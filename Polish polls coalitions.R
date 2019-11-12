# PREPARE WORKSPACE
rm(list=ls())
setwd('/Users/benstanley/Google Drive/Resources/R scripts/Pooling the Poles')
library(plyr); library(tidyverse); library(rjags); library(R2jags); library(R2WinBUGS); library(scales)
library(grid); library(foreign); library(memisc); library(MCMCpack); library(repmis); 
library(readxl); library(pander); library(coda); library(runjags); library(rgdal);
library(maptools); library(rgeos); library(gpclib); library(reshape2); 
library(gridExtra); library(grid); library(cowplot); library(scales); library(hrbrthemes); 
library(tidybayes); library(bayestestR); library(seatdist); library(knitr); library(kableExtra);
library(xtable)
gpclibPermit()

# d'Hondt function
countN <- function (v) {return (Reduce(function (x, y) x + y, ifelse(is.na(v), 0, 1)))}
dHondt <- function( candidates, votes, seats ){ 
  tmp <- data.frame( 
    candidates = rep( candidates, each = seats ), 
    scores     = as.vector(sapply( votes, function(x) x / 
                                     1:seats )) 
  ) 
  tmp <- tmp$candidates[order( - tmp$scores )] [1:seats] 
  table(tmp) 
}

# colours for plots
k1cols <- c("PiS"="blue4", "KO"="orange", "PSL-Kukiz"="darkgreen", "Konfederacja" = "midnightblue", "Lewica" = "red", "MN" = "yellow", "Other"="gray50")


## POOLED POLL MODEL
# read in, subset, adjust data
pollingdata <- read_excel('~/Google Drive/Resources/Polish materials/Poll data/pooledpolls_2019_k.xlsx')
pollingdata <- subset(pollingdata, select = -c(Source))
pollingdata$nDef <- round(((100-pollingdata$DK)/100)*pollingdata$n, digits=0)
pollingdata$KO <- 100/((100-pollingdata$DK))*pollingdata$KO
pollingdata$PiS <- 100/((100-pollingdata$DK))*pollingdata$PiS
pollingdata$PSL <- 100/((100-pollingdata$DK))*pollingdata$PSL
pollingdata$Lewica <- 100/((100-pollingdata$DK))*pollingdata$Lewica
pollingdata$Konfederacja <- 100/((100-pollingdata$DK))*pollingdata$Konfederacja
pollingdata$Other <- 100/((100-pollingdata$DK))*pollingdata$Other
pollingdata$nTot <- NULL
pollingdata$DK <- NULL
pollingdata$n <- NULL
pollingdata$pdate <- julian(as.Date(pollingdata$date, "%d/%m/%Y"), origin=as.Date("2019-04-01"))
pollingdata$pdate <- as.Date(pollingdata$pdate, origin=as.Date("2019-04-01"))
pollingdata <- pollingdata[which(pollingdata$pdate > 0),]
pollingdata <- pollingdata[!is.na(pollingdata$pdate),]
pollingdata <- pollingdata[order(pollingdata$pdate),]
pollingdata <- subset(pollingdata, as.integer(pdate) > as.integer(max(pdate)-150))
pollingdata$day <- as.integer(pollingdata$pdate)-as.integer(pollingdata$pdate)[1] + 1
pollingdata <- unite(pollingdata, agency, method, col="housef", sep="_")
pollingdata$housef <-as.factor(pollingdata$housef)

# create dataset for jags model
NUMPOLLS <- nrow(pollingdata)
PERIOD <- max(as.integer(pollingdata$day))
HOUSECOUNT <- length(levels(pollingdata$housef))
HOUSENAMES <- levels(pollingdata$housef)
PARTYNAMES <- c("KO","PiS","Lewica","PSL","Konfederacja","Other")
PARTIES <- length(PARTYNAMES)
Votes <- pollingdata[PARTYNAMES] * pollingdata$nDef * 0.01
Votes <- sapply(Votes, function(x) round(x,0))
day0 <- 0

data = list(PERIOD = PERIOD,
            HOUSECOUNT = HOUSECOUNT,
            NUMPOLLS = NUMPOLLS,
            PARTIES = PARTIES,
            Votes = Votes,
            pollDay = as.integer(pollingdata$day),
            house = as.integer(as.factor(pollingdata$housef)), 
            n = rowSums(Votes)
)

# jags model
model <- "
data {
    zero <- 0.0
}
model {

    #### -- observational model

    for(poll in 1:NUMPOLLS) { # for each poll result - rows
        adjusted_poll[poll, 1:PARTIES] <- walk[pollDay[poll], 1:PARTIES] +
            houseEffect[house[poll], 1:PARTIES]
        Votes[poll, 1:PARTIES] ~ dmulti(adjusted_poll[poll, 1:PARTIES], n[poll])
    }


    #### -- temporal model without discontinuity
#    tightness <- 50000 # kludge - today very much like yesterday
    tightness ~ dunif(100,90000)
# Estimate for first 6 weeks -> much lower at about 6700

    # no discontinuity
    for(day in 2:PERIOD) {
        # Note: use math not a distribution to generate the multinomial ...
        multinomial[day, 1:PARTIES] <- walk[day-1,  1:PARTIES] * tightness
        walk[day, 1:PARTIES] ~ ddirch(multinomial[day, 1:PARTIES])
    }

    ## -- weakly informative priors for first day.

   alpha[1] ~ dunif(250, 300) # KO
   alpha[2] ~ dunif(400, 450) # PiS
   alpha[3] ~ dunif(100, 150) # Lewica
   alpha[4] ~ dunif(50, 100) # PSL
   alpha[5] ~ dunif(50, 100) # Konfederacja
   alpha[6] ~ dunif(10, 20) # Other

   walk[1, 1:PARTIES] ~ ddirch(alpha[])             # initial starting point


    #### -- house effects model with two-way, sum-to-zero constraints

    ## -- vague priors ...
    for (h in 2:HOUSECOUNT) {
        for (p in 2:PARTIES) {
            # houseEffect[h, p] ~ dunif(-0.1, 0.1)
            houseEffect[h, p] ~ dnorm(0, pow(0.01, -2))
       }
    }

    ## -- sum to zero - but only in one direction for houseEffect[1, 1]
    for (p in 2:PARTIES) {
        houseEffect[1, p] <- 0 - sum( houseEffect[2:HOUSECOUNT, p] )
    }
    for(h in 1:HOUSECOUNT) {
        # includes constraint for houseEffect[1, 1], but only in one direction
        houseEffect[h, 1] <- 0 - sum( houseEffect[h, 2:PARTIES] )
    }

    ## -- the other direction constraint on houseEffect[1, 1]
    zero ~ dsum( houseEffect[1, 1], sum( houseEffect[2:HOUSECOUNT, 1] ) )
}
"

# run jags model and save results
results <- run.jags(model, monitor=c('walk','houseEffect','tightness'),data=data,n.chains=4,
                    burnin=4000,sample=20000,thin=5,method="parallel")
mysummary <- summary(results)
save(mysummary,file="ppsummary_NAT")


## HOUSE EFFECTS
ppframe <- data.frame(mysummary)
ppframe <- rownames_to_column(ppframe, var="n")
houseframe <- ppframe[-grep('tightness', ppframe$n),]
houseframe <- houseframe[-grep('walk', ppframe$n),]
houseframe$agency <- rep(HOUSENAMES, length(PARTYNAMES))
houseframe$party <- rep(PARTYNAMES, each=length(HOUSENAMES))
houseframe <- separate(houseframe, agency, c("house", "method"), sep="_")


## TREND DATA
ppframe <- data.frame(mysummary)
ppframe <- rownames_to_column(ppframe, var="n")
ppframe <- ppframe[-grep('tightness', ppframe$n),]
ppframe <- ppframe[-grep('houseEffect', ppframe$n),]

KOmean <- ppframe$Mean[grep('1]', ppframe$n)]*100
KOlow <- ppframe$Lower95[grep('1]', ppframe$n)]*100
KOhigh <- ppframe$Upper95[grep('1]', ppframe$n)]*100
KOest <- data.frame(KOmean, KOlow, KOhigh)

PiSmean <- ppframe$Mean[grep('2]', ppframe$n)]*100
PiSlow <- ppframe$Lower95[grep('2]', ppframe$n)]*100
PiShigh <- ppframe$Upper95[grep('2]', ppframe$n)]*100
PiSest <- data.frame(PiSmean, PiSlow, PiShigh)

Lewicamean <- ppframe$Mean[grep('3]', ppframe$n)]*100
Lewicalow <- ppframe$Lower95[grep('3]', ppframe$n)]*100
Lewicahigh <- ppframe$Upper95[grep('3]', ppframe$n)]*100
Lewicaest <- data.frame(Lewicamean, Lewicalow, Lewicahigh)

PSLmean <- ppframe$Mean[grep('4]', ppframe$n)]*100
PSLlow <- ppframe$Lower95[grep('4]', ppframe$n)]*100
PSLhigh <- ppframe$Upper95[grep('4]', ppframe$n)]*100
PSLest <- data.frame(PSLmean, PSLlow, PSLhigh)

Konfederacjamean <- ppframe$Mean[grep('5]', ppframe$n)]*100
Konfederacjalow <- ppframe$Lower95[grep('5]', ppframe$n)]*100
Konfederacjahigh <- ppframe$Upper95[grep('5]', ppframe$n)]*100
Konfederacjaest <- data.frame(Konfederacjamean, Konfederacjalow, Konfederacjahigh)

Othermean <- ppframe$Mean[grep('6]', ppframe$n)]*100
Otherlow <- ppframe$Lower95[grep('6]', ppframe$n)]*100
Otherhigh <- ppframe$Upper95[grep('6]', ppframe$n)]*100
Otherest <- data.frame(Othermean, Otherlow, Otherhigh)

plotdata <- cbind(PiSest, KOest, PSLest, Lewicaest, Konfederacjaest, Otherest)
plotdata$date <- as.Date(c(1:length(KOmean)), origin=as.Date(tail(pollingdata$pdate, n=1)-length(KOmean)))


# LATEST FIGURES
# prepare data frame
party <- c("PiS", "KO", "PSL-Kukiz", "Lewica", "Konfederacja", "Other")
alpha <- rep(1, length(party))
percent <- round(c(mean(tail(PiSest$PiSmean, n=7)),
                   mean(tail(KOest$KOmean, n=7)),
                   mean(tail(PSLest$PSLmean, n=7)),
                   mean(tail(Lewicaest$Lewicamean, n=7)),
                   mean(tail(Konfederacjaest$Konfederacjamean, n=7)),
                   mean(tail(Otherest$Othermean, n=7))))
votes <- round((percent*650)/100, digits=2)

# calculate means and HDIs
pooledframe <- data.frame(cbind(party, alpha, votes))
pos <- MCmultinomdirichlet(votes, alpha, mc=10000)
HDIs <- round(HPDinterval(pos)*100, digits=2)
colnames(pos) <- pooledframe$party
pos <- as.data.frame(pos)
postframe <- describe_posterior(as.data.frame(pos))
postframe <- arrange(postframe, desc(Median))
                     
means_pos <- round(apply(pos,2,mean)*100, digits=2)
pooledframe <- cbind(pooledframe, means_pos, HDIs)
pooledframe <- arrange(pooledframe, desc(means_pos))

# calculate probabilities
PiS.50.diff <- pos[,"PiS"] - 0.50
PiS.50.out <- sum(PiS.50.diff > 0) / length(PiS.50.diff)
PiS.50.out <- round(PiS.50.out, 2)

PiS.KO.diff <-pos[,"PiS"] - pos[,"KO"]
PiS.KO.diff.out <- sum(PiS.KO.diff > 0) / length(PiS.KO.diff)
PiS.KO.diff.out <- round(PiS.KO.diff.out, 2)

Lewica.thr <- pos[,"Lewica"] - 0.05
Lewica.thr.out <- sum(Lewica.thr > 0) / length(Lewica.thr)
Lewica.thr.out <- round(Lewica.thr.out, 2)

PSL.thr <- pos[,"PSL-Kukiz"] - 0.05
PSL.thr.out <- sum(PSL.thr > 0) / length(PSL.thr)
PSL.thr.out <- round(PSL.thr.out, 2)

Konfederacja.thr <- pos[,"Konfederacja"] - 0.05
Konfederacja.thr.out <- sum(Konfederacja.thr > 0) / length(Konfederacja.thr)
Konfederacja.thr.out <- round(Konfederacja.thr.out, 2)

# calculate latest figures
posfr <- data.frame()
for (i in 1:length(pos)){
  posfr <- pos[1:10000,]
}
posfrmelt <- melt(as.data.frame(posfr))


# SEAT SHARES
# read in 2019 coefficient data
weights <- read_excel('~/Google Drive/Resources/Polish materials/Poll data/2019_elec_percentages.xlsx')

means_pos <- data.frame(means_pos)
for(i in 1:length(party)) {
  if(means_pos[i,] > 4.99) {
    means_pos[i,] <- means_pos[i,] 
  } else { means_pos[i,] <- 0
  }
}

# reduce to zero if electoral coalitions below 8% threshold
# if(means_pos["Konfederacja",] > 7.99) {
# means_pos["Konfederacja",] <- means_pos["Konfederacja",] 
# } else { means_pos["Konfederacja",] <- 0
# }

means_pos <- 100/(sum(means_pos))*means_pos

# define party percentage per district according to most recent poll, include MN manually
PiSpct <- round(weights$PiScoef*means_pos[1,], digits=2)
KOpct <- round(weights$KOcoef*means_pos[2,], digits=2)
PSLpct <- round(weights$PSLcoef*means_pos[3,], digits=2)
Lewicapct <- round(weights$Lewicacoef*means_pos[4,], digits=2)
Konfederacjapct <- round(weights$Konfcoef*means_pos[5,], digits=2)
MNpct <- c(0.17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.90, 0, 0, 0, 0, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# estimate number of votes per party per district based on 2015 turnout
KOest <- (weights$validvotes/100)*KOpct
PiSest <- (weights$validvotes/100)*PiSpct
PSLest <- (weights$validvotes/100)*PSLpct
Lewicaest <- (weights$validvotes/100)*Lewicapct
Konfederacjaest <- (weights$validvotes/100)*Konfederacjapct
MNest <- (weights$validvotes/100)*MNpct

# estimate seat shares for each district
poldHondt <- data.frame(KO=rep(1,42), Konfederacja=rep(1,42), Lewica=rep(1,42),  MN=rep(1,42), PiS=rep(1,42),
                        PSL=rep(1,42))
for( i in 1 : 42 ) {
  poldHondt[i, ] <- dHondt(c("PiS", "KO", "PSL", "Lewica", "Konfederacja", "MN"), 
                           c(PiSest[i], KOest[i], PSLest[i], Lewicaest[i], Konfederacjaest[i], 
                             MNest[i]), weights$magnitude[i])
}

colnames(poldHondt) <- c("KO", "Konfederacja", "Lewica", "MN", "PiS", "PSL-Kukiz")

stlague <- data.frame(KO=rep(1,42), Konfederacja=rep(1,42), Lewica=rep(1,42),  MN=rep(1,42), PiS=rep(1,42),
                      PSL=rep(1,42))

for( i in 1 : 42 ) {
  stlague[i,] <- c(giveseats(v = c(KOest[i], Konfederacjaest[i], Lewicaest[i], MNest[i], PiSest[i], 
                                    PSLest[i]), ns = weights$magnitude[i], method="sl", thresh=5))$seats
}

colnames(stlague) <- c("KO", "Konfederacja", "Lewica", "MN", "PiS", "PSL-Kukiz")

# #Warsaw constituency - spread of projected seat shares
# PiS20 <- posfr[,1]*weights$PiScoef[20]
# PiS20est <- (weights$validvotes[20])*PiS20
# KO20 <- posfr[,2]*weights$KOcoef[20]
# KO20est <- (weights$validvotes[20])*KO20
# PSL20 <- posfr[,3]*weights$PSLcoef[20]
# PSL20est <- (weights$validvotes[20])*PSL20
# Lewica20 <- posfr[,4]*weights$Lewicacoef[20]
# Lewica20est <- (weights$validvotes[20])*Lewica20
# Konf20 <- posfr[,5]*weights$Konfcoef[20]
# Konf20est <- (weights$validvotes[20])*Konf20
# MN20est <- rep(0,10000)
# for( i in 1 : 10000 ) {
#   poldHondt[i, ] <- dHondt(c("PiS", "KO", "PSL", "Lewica", "Konfederacja", "MN"), 
#                            c(PiS20est[i], KO20est[i], PSL20est[i], Lewica20est[i], Konf20est[i], 
#                              MN20est[i]), weights$magnitude[20])
# }
# 
# prop.table(table(poldHondt[3]))*100
# warsaw_lewica <- table(poldHondt[3])

# create frame for plots
frame <- t(rbind(poldHondt[1,], colSums(poldHondt[2:42,])))
#frame <- t(rbind(stlague[1,], colSums(stlague[2:42,])))
frame <- data.frame(rownames(frame), frame)
colnames(frame) <- c("Party", "Unweighted", "Weighted")
frame <- frame[with(frame, order(-Weighted)),]
frame$in2019[frame$Party=="KO"] <- 134
frame$in2019[frame$Party=="PiS"] <- 235
frame$in2019[frame$Party=="Lewica"] <- 49
frame$in2019[frame$Party=="PSL-Kukiz"] <- 30
frame$in2019[frame$Party=="MN"] <- 1
frame$in2019[frame$Party=="Konfederacja"] <- 11
frame <- frame[frame$Weighted>0,]
frame$Party <- factor(frame$Party, levels=c("PiS", "KO", "PSL-Kukiz", "Lewica", "Konfederacja", "MN"))
frame$diffPres <- sprintf("%+d", (frame$Weighted - frame$in2019))
frame$diffPres <- sprintf("(%s)", frame$diffPres)
frame$diffPresUn <- sprintf("%+d", (frame$Unweighted - frame$in2019))
frame$diffPresUn <- sprintf("(%s)", frame$diffPresUn)
frame$Party <- reorder(frame$Party, -frame$Weighted)

## CREATE PLOTS
setwd('~/Google Drive/Resources/Polish materials/Plots')

#plot house effects
p <- ggplot() + 
  geom_abline(intercept=0, slope=0, colour="gray10", linetype=3) +
  geom_pointrange(data=houseframe, mapping=aes(x=party, y=Mean, ymin=Lower95, ymax=Upper95, color=house, shape=method), 
                  position = position_dodge(width=0.5)) +
  guides(color=guide_legend(override.aes=list(shape=15, size=1, linetype=0)))+
  labs(color="Pollster", shape="Mode", x="", y="Deviation from mean party vote share", 
       title="House and mode effects - coalitions", 
       caption = "@BDStanley; benstanley.org") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_houseeffects.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot most recent party support
p <- ggplot(posfrmelt, aes(y=variable, x = value, fill=variable)) +
  geom_vline(aes(xintercept=0.05), colour="gray60", linetype="dotted") +
  geom_vline(aes(xintercept=0.08), colour="gray60", linetype="dotted") +
  geom_halfeyeh(color=NA, scale="width") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PiS"]),0)), 
           y="PiS", x=mean(posfrmelt$value[posfrmelt$variable=="PiS"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="KO"]),0)), 
           y="KO", x=mean(posfrmelt$value[posfrmelt$variable=="KO"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Lewica"]),0)), 
           y="Lewica", x=mean(posfrmelt$value[posfrmelt$variable=="Lewica"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Konfederacja"]),0)), 
           y="Konfederacja", x=mean(posfrmelt$value[posfrmelt$variable=="Konfederacja"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Other"]),0)), 
           y="Other", x=mean(posfrmelt$value[posfrmelt$variable=="Other"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PSL-Kukiz"]),0)), 
           y="PSL-Kukiz", x=mean(posfrmelt$value[posfrmelt$variable=="PSL-Kukiz"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste("Pr(PiS > 50%)  = ", PiS.50.out), y=3, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(PiS > KO)  = ", PiS.KO.diff.out), y=2.75, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(Lewica > 5%)  = ", Lewica.thr.out), y=2.5, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(Konfederacja > 5%)  = ", Konfederacja.thr.out), y=2.25, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(PSL-Kukiz > 5%)  = ", PSL.thr.out), y=2, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  scale_y_discrete(name=" ", limits=rev(pooledframe$party)) +
  scale_fill_manual(name=" ", values=k1cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Latest poll estimates - coalitions",
       subtitle="Estimated using polls published by IPSOS, IBRIS, Estymator, Kantar, Pollster, IBSP, CBOS, Social Changes and Indicator") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_latest.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot trends
datl <- melt(plotdata, measure.vars=c("KOmean","PiSmean","Lewicamean","PSLmean","Konfederacjamean", "Othermean"))
levels(datl$variable) <- c("KO","PiS", "Lewica","PSL-Kukiz", "Konfederacja", "Other")
datl$variable <- factor(datl$variable, levels = c("PiS", "KO", "Lewica", "PSL-Kukiz", "Konfederacja", "Other"))

#datl_all <- datl[!(datl$variable %in% c("Other")),]
p <- ggplot(datl, aes(x=date, y=value, colour=factor(variable))) + geom_line() +
  geom_abline(intercept=5, slope=0, colour="gray60", linetype=3) +
  geom_ribbon(data=subset(datl, variable=="PiS"), aes(ymin=PiSlow, ymax=PiShigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="KO"), aes(ymin=KOlow, ymax=KOhigh), colour=NA, fill="orange", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Lewica"), aes(ymin=Lewicalow, ymax=Lewicahigh), colour=NA, fill="red", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="PSL-Kukiz"), aes(ymin=PSLlow, ymax=PSLhigh), colour=NA, fill="darkgreen", alpha=0.3) +  
  geom_ribbon(data=subset(datl, variable=="Konfederacja"), aes(ymin=Konfederacjalow, ymax=Konfederacjahigh), colour=NA, fill="midnightblue", alpha=0.3) +  
  geom_ribbon(data=subset(datl, variable=="Other"), aes(ymin=Otherlow, ymax=Otherhigh), colour=NA, fill="grey50", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PiS), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=KO), col="orange", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PSL), col="darkgreen", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Lewica), col="red", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Konfederacja), col="midnightblue", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Other), col="gray50", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=k1cols,
                      breaks=c("PiS", "KO", "Lewica", "PSL-Kukiz", "Konfederacja", "Other"),
                      labels=c("PiS", "KO", "Lewica", "PSL-Kukiz", "Konfederacja", "Other")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Pooled poll trends - coalitions", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "polls_trends.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

#current seat share
p <- ggplot(data=frame, mapping=aes(x=Party, y=Weighted, fill=Party)) +
  geom_bar(stat="identity", width=.75, show.legend = F) +
  geom_abline(intercept=231, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=276, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=307, slope=0, colour="gray10", linetype=3) +
  scale_y_continuous('Number of seats', limits=c(0,320), breaks=c(0, 50, 100, 150, 200, 231, 276, 307)) +
  scale_fill_manual(name="Party", values = k1cols)+
  geom_label(aes(x=2, y=231), label="Legislative majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed") +
  geom_label(aes(x=2, y=276), label="Overturn presidential veto", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed") +
  geom_label(aes(x=2, y=307), label="Constitutional majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed") +
  annotate("text", x=frame$Party, y=c(frame$Weighted+18), label=frame$Weighted, size=4, family="Roboto Condensed")+
  annotate("text", x=frame$Party, y=c(frame$Weighted+8), label=frame$diffPres, size=3, family="Roboto Condensed") +
  labs(x="", y="% of vote", title="Estimated share of seats - coalitions",
       subtitle="Figures in brackets refer to change in seat share since October 2019 election",
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "polls_seats.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# seats table
seats <- cbind(poldHondt, weights)
row.names(seats) <- weights$name
keep <- c("KO","PiS","PSL-Kukiz","Lewica", "Konfederacja", "MN")
seats <- seats[keep]
#colnames(seats) <- sprintf("%-8s", colnames(seats))
seats <- seats[-1,]
seats$id <- 1:41
seats$PiSKO <-abs(seats$PiS-seats$KO)
seats$PiSmKO <- seats$PiS-seats$KO

#regional maps
const=readOGR("/Users/benstanley/Google Drive/Resources/Polish materials/Regional data/GRED_beta2_20170530_Poland/shapefile/GRED_Poland_2011_beta2.shp")
const@data$id = rownames(const@data)
const.points = fortify(const, region="id")
const.df = join(const.points, const@data, by="id")
const.df$con[const.df$id==0] <- 13
const.df$con[const.df$id==1] <- 12
const.df$con[const.df$id==2] <- 14
const.df$con[const.df$id==3] <- 15
const.df$con[const.df$id==4] <- 3
const.df$con[const.df$id==5] <- 2
const.df$con[const.df$id==6] <- 1
const.df$con[const.df$id==7] <- 11
const.df$con[const.df$id==8] <- 9
const.df$con[const.df$id==9] <- 10
const.df$con[const.df$id==10] <- 33
const.df$con[const.df$id==11] <- 37
const.df$con[const.df$id==12] <- 38
const.df$con[const.df$id==13] <- 36
const.df$con[const.df$id==14] <- 39
const.df$con[const.df$id==15] <- 4
const.df$con[const.df$id==16] <- 5
const.df$con[const.df$id==17] <- 6
const.df$con[const.df$id==18] <- 7
const.df$con[const.df$id==19] <- 8
const.df$con[const.df$id==20] <- 18
const.df$con[const.df$id==21] <- 16
const.df$con[const.df$id==22] <- 17
const.df$con[const.df$id==23] <- 20
const.df$con[const.df$id==24] <- 19
const.df$con[const.df$id==25] <- 21
const.df$con[const.df$id==26] <- 24
const.df$con[const.df$id==27] <- 26
const.df$con[const.df$id==28] <- 25
const.df$con[const.df$id==29] <- 30
const.df$con[const.df$id==30] <- 31
const.df$con[const.df$id==31] <- 27
const.df$con[const.df$id==32] <- 32
const.df$con[const.df$id==33] <- 29
const.df$con[const.df$id==34] <- 28
const.df$con[const.df$id==35] <- 23
const.df$con[const.df$id==36] <- 22
const.df$con[const.df$id==37] <- 34
const.df$con[const.df$id==38] <- 35
const.df$con[const.df$id==39] <- 41
const.df$con[const.df$id==40] <- 40

label_points <- coordinates(const)
colnames(label_points) <- c("x","y")
label_points <- data.frame(label_points)
seats$label_point_x[seats$id==12] <- label_points$x[2]
seats$label_point_y[seats$id==12] <- label_points$y[2]
seats$label_point_x[seats$id==13] <- label_points$x[1]
seats$label_point_y[seats$id==13] <- label_points$y[1]
seats$label_point_x[seats$id==14] <- label_points$x[3]
seats$label_point_y[seats$id==14] <- label_points$y[3]
seats$label_point_x[seats$id==15] <- label_points$x[4]
seats$label_point_y[seats$id==15] <- label_points$y[4]
seats$label_point_x[seats$id==3] <- label_points$x[5]
seats$label_point_y[seats$id==3] <- label_points$y[5]
seats$label_point_x[seats$id==2] <- label_points$x[6]
seats$label_point_y[seats$id==2] <- label_points$y[6]
seats$label_point_x[seats$id==1] <- label_points$x[7]
seats$label_point_y[seats$id==1] <- label_points$y[7]
seats$label_point_x[seats$id==11] <- label_points$x[8]
seats$label_point_y[seats$id==11] <- label_points$y[8]
seats$label_point_x[seats$id==9] <- label_points$x[9]
seats$label_point_y[seats$id==9] <- label_points$y[9]
seats$label_point_x[seats$id==10] <- label_points$x[10]
seats$label_point_y[seats$id==10] <- label_points$y[10]
seats$label_point_x[seats$id==33] <- label_points$x[11]
seats$label_point_y[seats$id==33] <- label_points$y[11]
seats$label_point_x[seats$id==37] <- label_points$x[12]
seats$label_point_y[seats$id==37] <- label_points$y[12]
seats$label_point_x[seats$id==38] <- label_points$x[13]
seats$label_point_y[seats$id==38] <- label_points$y[13]
seats$label_point_x[seats$id==36] <- label_points$x[14]
seats$label_point_y[seats$id==36] <- label_points$y[14]
seats$label_point_x[seats$id==39] <- label_points$x[15]
seats$label_point_y[seats$id==39] <- label_points$y[15]
seats$label_point_x[seats$id==4] <- label_points$x[16]
seats$label_point_y[seats$id==4] <- label_points$y[16]
seats$label_point_x[seats$id==5] <- label_points$x[17]
seats$label_point_y[seats$id==5] <- label_points$y[17]
seats$label_point_x[seats$id==6] <- label_points$x[18]
seats$label_point_y[seats$id==6] <- label_points$y[18]
seats$label_point_x[seats$id==7] <- label_points$x[19]+0.2
seats$label_point_y[seats$id==7] <- label_points$y[19]
seats$label_point_x[seats$id==8] <- label_points$x[20]+0.2
seats$label_point_y[seats$id==8] <- label_points$y[20]
seats$label_point_x[seats$id==18] <- label_points$x[21]+0.2
seats$label_point_y[seats$id==18] <- label_points$y[21]
seats$label_point_x[seats$id==16] <- label_points$x[22]
seats$label_point_y[seats$id==16] <- label_points$y[22]
seats$label_point_x[seats$id==17] <- label_points$x[23]
seats$label_point_y[seats$id==17] <- label_points$y[23]
seats$label_point_x[seats$id==20] <- label_points$x[24]-0.4
seats$label_point_y[seats$id==20] <- label_points$y[24]
seats$label_point_x[seats$id==19] <- label_points$x[25]
seats$label_point_y[seats$id==19] <- label_points$y[25]
seats$label_point_x[seats$id==21] <- label_points$x[26]
seats$label_point_y[seats$id==21] <- label_points$y[26]
seats$label_point_x[seats$id==24] <- label_points$x[27]
seats$label_point_y[seats$id==24] <- label_points$y[27]
seats$label_point_x[seats$id==26] <- label_points$x[28]
seats$label_point_y[seats$id==26] <- label_points$y[28]
seats$label_point_x[seats$id==25] <- label_points$x[29]
seats$label_point_y[seats$id==25] <- label_points$y[29]
seats$label_point_x[seats$id==30] <- label_points$x[30]
seats$label_point_y[seats$id==30] <- label_points$y[30]
seats$label_point_x[seats$id==31] <- label_points$x[31]
seats$label_point_y[seats$id==31] <- label_points$y[31]
seats$label_point_x[seats$id==27] <- label_points$x[32]
seats$label_point_y[seats$id==27] <- label_points$y[32]
seats$label_point_x[seats$id==32] <- label_points$x[33]
seats$label_point_y[seats$id==32] <- label_points$y[33]
seats$label_point_x[seats$id==29] <- label_points$x[34]
seats$label_point_y[seats$id==29] <- label_points$y[34]
seats$label_point_x[seats$id==28] <- label_points$x[35]
seats$label_point_y[seats$id==28] <- label_points$y[35]
seats$label_point_x[seats$id==23] <- label_points$x[36]
seats$label_point_y[seats$id==23] <- label_points$y[36]
seats$label_point_x[seats$id==22] <- label_points$x[37]+0.1
seats$label_point_y[seats$id==22] <- label_points$y[37]
seats$label_point_x[seats$id==34] <- label_points$x[38]
seats$label_point_y[seats$id==34] <- label_points$y[38]
seats$label_point_x[seats$id==35] <- label_points$x[39]
seats$label_point_y[seats$id==35] <- label_points$y[39]
seats$label_point_x[seats$id==41] <- label_points$x[40]
seats$label_point_y[seats$id==41] <- label_points$y[40]
seats$label_point_x[seats$id==40] <- label_points$x[41]
seats$label_point_y[seats$id==40] <- label_points$y[41]

const.df$id <- const.df$con
plotdata <- merge(const.df,seats,by="id")

p <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Lewica)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Lewica", limits=c(min=0, max=20), low = "white", high = "red", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Lewica, label=Lewica), fill="white") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) + theme(aspect.ratio=1, legend.position="none",
                                                                                  axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title="Constituency-level share of seats for Lewica", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")
ggsave(p, file = "Lewica_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiS)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PiS", limits=c(min=0, max=20), low = "white", high = "blue4", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiS, label=PiS), fill="white") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) + theme(aspect.ratio=1, legend.position="none",
                                                                                  axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title="Constituency-level share of seats for PiS", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")
ggsave(p, file = "PiS_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(KO)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="KO", limits=c(min=0, max=20), low = "white", high = "orange", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=KO, label=KO), fill="white") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) + theme(aspect.ratio=1, legend.position="none",
                                                                                  axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title="Constituency-level share of seats for Koalicja Obywatelska", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")
ggsave(p, file = "KO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(`PSL-Kukiz`)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PSL-Kukiz", limits=c(min=0, max=20), low = "white", high = "darkgreen", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=`PSL-Kukiz`, label=`PSL-Kukiz`), fill="white") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) + theme(aspect.ratio=1, legend.position="none",
                                                                                  axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title="Constituency-level share of seats for PSL-Kukiz", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")
ggsave(p, file = "PSL_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Konfederacja)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Konfederacja", limits=c(min=0, max=20), low = "white", high = "midnightblue", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Konfederacja, label=Konfederacja), fill="white") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) + theme(aspect.ratio=1, legend.position="none",
                                                                                  axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title="Constituency-level share of seats for Konfederacja", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")
ggsave(p, file = "Konf_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiSmKO)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) + theme(aspect.ratio=1, legend.position="none",
                                                                                  axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_fill_gradient2(name="PiSKO", limits=c(min=-20, max=20), low = "orange", mid="white", high = "blue4", midpoint=0, guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiSKO, label=PiSKO), fill="white") +
  labs(title="Constituency-level differences in share of seats for PiS and Koalicja Obywatelska", subtitle="Constituencies in shades of blue have more PiS MPs; constituencies in orange have more KO MPs", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")
ggsave(p, file = "PiSKO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

seattable <- tibble(rownames(seats), seats[,2], seats[,1], seats[,4], seats[,3], seats[,5], seats[,6])
colnames(seattable) <- c("Constituency", "PiS", "KO", "Lewica", "PSL-Kukiz", "Konf.", "MN")
hlines <- c(-1, 0, nrow(seattable))
print(xtable(seattable, type = "latex", digits=0, align=c("l","l","c","c","c","c","c","c")), hline.after=hlines, booktabs=TRUE,
      include.rownames=FALSE, file='seat_table.tex')
# kable(seattable, format = "latex", booktabs=TRUE, longtable=TRUE) %>%
#   column_spec(column=c(2:7), width = "5em", monospace = TRUE) %>%
#   column_spec(column=c(1), width = "15em", monospace = TRUE, italic=TRUE) %>%
#   kable_styling(bootstrap_options = "striped", full_width = FALSE)

save.image("~/Desktop/Personal/PoolingthePoles.RData")
