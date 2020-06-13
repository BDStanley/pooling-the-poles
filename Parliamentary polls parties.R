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
options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}


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
pcols <- c("PO"="orange", "PiS"="blue4", "PSL"="darkgreen", 
           "Kukiz'15"="black", "KORWiN" = "midnightblue", "Wiosna" = "purple2", "Other"="gray50", 
           "MN"="yellow", "Nowoczesna"="blue", "SLD"="red", "Razem"="magenta")

## POOLED POLL MODEL
# read in, subset, adjust data
pollingdata <- read_excel('~/Google Drive/Resources/Polish materials/Poll data/pooledpolls_2019.xlsx')
pollingdata <- subset(pollingdata, select = -c(Source))
pollingdata$nDef <- round(((100-pollingdata$DK)/100)*pollingdata$n, digits=0)
pollingdata$PO <- 100/((100-pollingdata$DK))*pollingdata$PO
pollingdata$PiS <- 100/((100-pollingdata$DK))*pollingdata$PiS
pollingdata$PSL <- 100/((100-pollingdata$DK))*pollingdata$PSL
pollingdata$SLD <- 100/((100-pollingdata$DK))*pollingdata$SLD
pollingdata$Nowoczesna <- 100/((100-pollingdata$DK))*pollingdata$Nowoczesna
pollingdata$Kukiz <- 100/((100-pollingdata$DK))*pollingdata$Kukiz
pollingdata$KORWiN <- 100/((100-pollingdata$DK))*pollingdata$KORWiN
pollingdata$Wiosna <- 100/((100-pollingdata$DK))*pollingdata$Wiosna
pollingdata$Razem <- 100/((100-pollingdata$DK))*pollingdata$Razem
pollingdata$Other <- 100/((100-pollingdata$DK))*pollingdata$Other
pollingdata$nTot <- NULL
pollingdata$DK <- NULL
pollingdata$n <- NULL
pollingdata$pdate <- julian(as.Date(pollingdata$date, "%d/%m/%Y"), origin=as.Date("2019-11-03"))
pollingdata$pdate <- as.Date(pollingdata$pdate, origin=as.Date("2019-11-03"))
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
PARTYNAMES <- c("PO","PiS","SLD","PSL","Wiosna","Nowoczesna","Kukiz","Razem","KORWiN","Other")
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

   alpha[1] ~ dunif(250, 300) # PO
   alpha[2] ~ dunif(400, 450) # PiS
   alpha[3] ~ dunif(70, 130) # SLD
   alpha[4] ~ dunif(50, 100) # PSL
   alpha[5] ~ dunif(50, 100) # Wiosna
   alpha[6] ~ dunif(10, 50) # Nowoczesna
   alpha[7] ~ dunif(30, 70) # Kukiz
   alpha[8] ~ dunif(10, 40) # Razem
   alpha[9] ~ dunif(30, 70) # KORWiN
   alpha[10] ~ dunif(10, 20) # Other

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

POmean <- ppframe$Mean[grep('1]', ppframe$n)]*100
POlow <- ppframe$Lower95[grep('1]', ppframe$n)]*100
POhigh <- ppframe$Upper95[grep('1]', ppframe$n)]*100
POest <- data.frame(POmean, POlow, POhigh)

PiSmean <- ppframe$Mean[grep('2]', ppframe$n)]*100
PiSlow <- ppframe$Lower95[grep('2]', ppframe$n)]*100
PiShigh <- ppframe$Upper95[grep('2]', ppframe$n)]*100
PiSest <- data.frame(PiSmean, PiSlow, PiShigh)

SLDmean <- ppframe$Mean[grep('3]', ppframe$n)]*100
SLDlow <- ppframe$Lower95[grep('3]', ppframe$n)]*100
SLDhigh <- ppframe$Upper95[grep('3]', ppframe$n)]*100
SLDest <- data.frame(SLDmean, SLDlow, SLDhigh)

PSLmean <- ppframe$Mean[grep('4]', ppframe$n)]*100
PSLlow <- ppframe$Lower95[grep('4]', ppframe$n)]*100
PSLhigh <- ppframe$Upper95[grep('4]', ppframe$n)]*100
PSLest <- data.frame(PSLmean, PSLlow, PSLhigh)

Wiosnamean <- ppframe$Mean[grep('5]', ppframe$n)]*100
Wiosnalow <- ppframe$Lower95[grep('5]', ppframe$n)]*100
Wiosnahigh <- ppframe$Upper95[grep('5]', ppframe$n)]*100
Wiosnaest <- data.frame(Wiosnamean, Wiosnalow, Wiosnahigh)

Nowoczesnamean <- ppframe$Mean[grep('6]', ppframe$n)]*100
Nowoczesnalow <- ppframe$Lower95[grep('6]', ppframe$n)]*100
Nowoczesnahigh <- ppframe$Upper95[grep('6]', ppframe$n)]*100
Nowoczesnaest <- data.frame(Nowoczesnamean, Nowoczesnalow, Nowoczesnahigh)

Kukiz15mean <- ppframe$Mean[grep('7]', ppframe$n)]*100
Kukiz15low <- ppframe$Lower95[grep('7]', ppframe$n)]*100
Kukiz15high <- ppframe$Upper95[grep('7]', ppframe$n)]*100
Kukiz15est <- data.frame(Kukiz15mean, Kukiz15low, Kukiz15high)

Razemmean <- ppframe$Mean[grep('8]', ppframe$n)]*100
Razemlow <- ppframe$Lower95[grep('8]', ppframe$n)]*100
Razemhigh <- ppframe$Upper95[grep('8]', ppframe$n)]*100
Razemest <- data.frame(Razemmean, Razemlow, Razemhigh)

KORWiNmean <- ppframe$Mean[grep('9]', ppframe$n)]*100
KORWiNlow <- ppframe$Lower95[grep('9]', ppframe$n)]*100
KORWiNhigh <- ppframe$Upper95[grep('9]', ppframe$n)]*100
KORWiNest <- data.frame(KORWiNmean, KORWiNlow, KORWiNhigh)

Othermean <- ppframe$Mean[grep('10]', ppframe$n)]*100
Otherlow <- ppframe$Lower95[grep('10]', ppframe$n)]*100
Otherhigh <- ppframe$Upper95[grep('10]', ppframe$n)]*100
Otherest <- data.frame(Othermean, Otherlow, Otherhigh)

plotdata <- cbind(POest, PiSest, PSLest, SLDest, Wiosnaest, Nowoczesnaest, Kukiz15est, Razemest, Wiosnaest, KORWiNest, Otherest)
plotdata$date <- as.Date(c(1:length(POmean)), origin=as.Date(tail(pollingdata$pdate, n=1)-length(POmean)))

# LATEST FIGURES
# prepare data frame
party <- c("PO", "PiS", "SLD", "PSL", "KORWiN", "Kukiz15", "Nowoczesna", "Razem", "Wiosna", "Other")
alpha <- c(1,1,1,1,1,1,1,1,1,1)
percent <- round(c(mean(tail(POest$POmean, n=7)), mean(tail(PiSest$PiSmean, n=7)), 
                   mean(tail(SLDest$SLDmean, n=7)), mean(tail(PSLest$PSLmean, n=7)), 
                   mean(tail(KORWiNest$KORWiNmean, n=7)), mean(tail(Kukiz15est$Kukiz15mean, n=7)),
                   mean(tail(Nowoczesnaest$Nowoczesnamean, n=7)), mean(tail(Razemest$Razemmean, n=7)),
                   mean(tail(Wiosnaest$Wiosnamean, n=7)), mean(tail(Otherest$Othermean, n=7))))
votes <- round((percent*500)/100, digits=2)

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

PO.PiS.diff <-pos[,"PiS"] - pos[,"PO"]
PO.PiS.diff.out <- sum(PO.PiS.diff > 0) / length(PO.PiS.diff)
PO.PiS.diff.out <- round(PO.PiS.diff.out, 2)

SLD.thr <- pos[,"SLD"] - 0.05
SLD.thr.out <- sum(SLD.thr > 0) / length(SLD.thr)
SLD.thr.out <- round(SLD.thr.out, 2)

PSL.thr <- pos[,"PSL"] - 0.05
PSL.thr.out <- sum(PSL.thr > 0) / length(PSL.thr)
PSL.thr.out <- round(PSL.thr.out, 2)

KORWiN.thr <- pos[,"KORWiN"] - 0.05
KORWiN.thr.out <- sum(KORWiN.thr > 0) / length(KORWiN.thr)
KORWiN.thr.out <- round(KORWiN.thr.out, 2)

Kukiz15.thr <- pos[,"Kukiz15"] - 0.05
Kukiz15.thr.out <- sum(Kukiz15.thr > 0) / length(Kukiz15.thr)
Kukiz15.thr.out <- round(Kukiz15.thr.out, 2)

Nowoczesna.thr <- pos[,"Nowoczesna"] - 0.05
Nowoczesna.thr.out <- sum(Nowoczesna.thr > 0) / length(Nowoczesna.thr)
Nowoczesna.thr.out <- round(Nowoczesna.thr.out, 2)

Razem.thr <- pos[,"Razem"] - 0.05
Razem.thr.out <- sum(Razem.thr > 0) / length(Razem.thr)
Razem.thr.out <- round(Razem.thr.out, 2)

Wiosna.thr <- pos[,"Wiosna"] - 0.05
Wiosna.thr.out <- sum(Wiosna.thr > 0) / length(Wiosna.thr)
Wiosna.thr.out <- round(Wiosna.thr.out, 2)

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

means_pos <- 100/(sum(means_pos))*means_pos

# define party percentage per district according to most recent poll, include MN manually
PiSpct <- round(weights$PiScoef*means_pos[2,], digits=2)
POpct <- round(weights$KOcoef*means_pos[1,], digits=2)
SLDpct <- round(weights$Lewicacoef*means_pos[3,], digits=2)
PSLpct <- round(weights$PSLcoef*means_pos[4,], digits=2)
KORWiNpct <- round(weights$Konfcoef*means_pos[5,], digits=2)
Kukiz15pct <- round(weights$PSLcoef*means_pos[6,], digits=2)
Nowoczesnapct <- round(weights$KOcoef*means_pos[7,], digits=2)
Razempct <- round(weights$Lewicacoef*means_pos[8,], digits=2)
Wiosnapct <- round(weights$Lewicacoef*means_pos[9,], digits=2)
MNpct <- c(0.17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.90, 0, 0, 0, 0, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# estimate number of votes per party per district based on 2019 turnout
POest <- (weights$validvotes/100)*POpct
PiSest <- (weights$validvotes/100)*PiSpct
PSLest <- (weights$validvotes/100)*PSLpct
SLDest <- (weights$validvotes/100)*SLDpct
KORWiNest <- (weights$validvotes/100)*KORWiNpct
Kukiz15est <- (weights$validvotes/100)*Kukiz15pct
Razemest <- (weights$validvotes/100)*Razempct
Wiosnaest <- (weights$validvotes/100)*Wiosnapct
Nowoczesnaest <- (weights$validvotes/100)*Nowoczesnapct
MNest <- (weights$validvotes/100)*MNpct

# estimate seat shares for each district
poldHondt <- data.frame(KORWiN=rep(1,42), Kukiz15=rep(1,42), MN=rep(1,42), Nowoczesna=rep(1,42), PiS=rep(1,42), PO=rep(1,42),
                        PSL=rep(1,42), Razem=rep(1,42), SLD=rep(1,42), Wiosna=rep(1,42))

for( i in 1 : 42 ) {
  poldHondt[i, ] <- dHondt(c("PO", "PiS", "SLD", "PSL", "MN", "KORWiN", "Wiosna", "Kukiz15", "Nowoczesna", "Razem"), 
                           c(POest[i], PiSest[i], SLDest[i], PSLest[i], MNest[i], 
                             KORWiNest[i], Wiosnaest[i], Kukiz15est[i], Nowoczesnaest[i], 
                             Razemest[i]), weights$magnitude[i])
}

# create frame for plots
# create frame for plots
frame <- t(rbind(poldHondt[1,], colSums(poldHondt[2:42,])))
frame <- data.frame(rownames(frame), frame)
colnames(frame) <- c("Party", "Unweighted", "Weighted")
frame <- frame[with(frame, order(-Weighted)),]
frame <- frame[frame$Weighted>0,]
frame$Party <- factor(frame$Party, levels=c("PO", "PiS", "SLD", "PSL", "MN", "KORWiN", "Wiosna", "Kukiz15", "Nowoczesna", "Razem"))
frame$Party <- reorder(frame$Party, -frame$Weighted)

## CREATE PLOTS
setwd('~/Google Drive/Resources/Polish materials/Plots')
levels(posfrmelt$variable)[levels(posfrmelt$variable)=="Kukiz15"] <- "Kukiz'15"
levels(pooledframe$party)[levels(pooledframe$party)=="Kukiz15"] <- "Kukiz'15"

#plot house effects
p <- ggplot() + 
  geom_abline(intercept=0, slope=0, colour="gray10", linetype=3) +
  geom_pointrange(data=houseframe, mapping=aes(x=party, y=Mean, ymin=Lower95, ymax=Upper95, color=house, shape=method), 
                  position = position_dodge(width=0.5)) +
  guides(color=guide_legend(override.aes=list(shape=15, size=1, linetype=0)))+
  labs(color="Pollster", shape="Mode", x="", y="Deviation from mean party vote share", 
       title="House and mode effects for national election polls - all parties", 
       caption = "@BDStanley; benstanley.org") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_houseeffects_parties.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot most recent party support
p <- ggplot(posfrmelt, aes(y=variable, x = value, fill=variable)) +
  geom_vline(aes(xintercept=0.05), colour="gray60", linetype="dotted") +
  geom_halfeyeh(color=NA, scale="width") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PiS"]),0)), 
           y="PiS", x=mean(posfrmelt$value[posfrmelt$variable=="PiS"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PO"]),0)), 
           y="PO", x=mean(posfrmelt$value[posfrmelt$variable=="PO"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="SLD"]),0)), 
           y="SLD", x=mean(posfrmelt$value[posfrmelt$variable=="SLD"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="KORWiN"]),0)), 
           y="KORWiN", x=mean(posfrmelt$value[posfrmelt$variable=="KORWiN"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Razem"]),0)), 
           y="Razem", x=mean(posfrmelt$value[posfrmelt$variable=="Razem"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PSL"]),0)), 
           y="PSL", x=mean(posfrmelt$value[posfrmelt$variable=="PSL"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Wiosna"]),0)), 
           y="Wiosna", x=mean(posfrmelt$value[posfrmelt$variable=="Wiosna"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Nowoczesna"]),0)), 
           y="Nowoczesna", x=mean(posfrmelt$value[posfrmelt$variable=="Nowoczesna"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Kukiz'15"]),0)), 
           y="Kukiz'15", x=mean(posfrmelt$value[posfrmelt$variable=="Kukiz'15"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Other"]),0)), 
           y="Other", x=mean(posfrmelt$value[posfrmelt$variable=="Other"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste("Pr(PiS > 50%)  = ", PiS.50.out), y=6, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(PiS > PO)  = ", PO.PiS.diff.out), y=5.5, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(SLD > 5%)  = ", SLD.thr.out), y=5, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(PSL > 5%)  = ", PSL.thr.out), y=4.5, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(Wiosna > 5%)  = ", Wiosna.thr.out), y=4, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(Kukiz'15 > 5%)  = ", Kukiz15.thr.out), y=3.5, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(KORWiN > 5%)  = ", KORWiN.thr.out), y=3, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(Razem > 5%)  = ", Razem.thr.out), y=2.5, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Pr(Nowoczesna > 5%)  = ", Nowoczesna.thr.out), y=2, x=0.4, size=3.5, adj=c(0), family="Roboto Condensed") +
  scale_y_discrete(name=" ", limits=rev(pooledframe$party)) +
  scale_fill_manual(name=" ", values=pcols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Latest poll estimates - all parties",
       subtitle="Estimated using polls published by IPSOS, IBRIS, Estymator, Kantar, Pollster, IBSP, CBOS, Social Changes and Indicator") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_latest_parties.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot trends
datl <- melt(plotdata, measure.vars=c("POmean","PiSmean","SLDmean","PSLmean","KORWiNmean", "Wiosnamean", "Kukiz15mean", "Nowoczesnamean", "Razemmean", "Othermean"))
levels(datl$variable) <- c("PO","PiS", "SLD","PSL","KORWiN", "Wiosna", "Kukiz'15", "Nowoczesna", "Razem", "Other")
datl$variable <- factor(datl$variable, levels = c("PiS", "PO", "Nowoczesna", "Kukiz'15", "PSL", "SLD", "KORWiN", "Wiosna", "Razem", "Other"))

pollingdata <- rename(pollingdata, "Kukiz" = "Kukiz'15")
pdatl <- melt(pollingdata, measure.vars=c("PO","PiS","SLD","PSL","KORWiN", "Wiosna", "Kukiz'15", "Nowoczesna", "Razem", "Other"))
levels(pdatl$variable) <- c("PO","PiS", "SLD","PSL","KORWiN", "Wiosna", "Kukiz'15", "Nowoczesna", "Razem", "Other")
pdatl$variable <- factor(pdatl$variable, levels = c("PiS", "PO", "Nowoczesna", "Kukiz'15", "PSL", "SLD", "KORWiN", "Wiosna", "Razem", "Other"))

datl_all <- datl[!(datl$variable %in% c("Other")),]
p <- ggplot(datl_all, aes(x=date, y=value, colour=factor(variable))) + geom_line() + 
  geom_abline(intercept=5, slope=0, colour="gray60", linetype=3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PiS), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PO), col="orange", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=`Kukiz'15`), col="black", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Wiosna), col="purple4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PSL), col="darkgreen", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=SLD), col="red", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Nowoczesna), col="blue", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=KORWiN), col="midnightblue", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Razem), col="magenta", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Other), col="gray50", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=pcols, 
                      breaks=c("PiS", "PO", "Kukiz'15", "Wiosna", "PSL", "SLD", "Nowoczesna", "KORWiN", "Razem", "Other"),
                      labels=c("PiS", "PO", "Kukiz'15", "Wiosna", "PSL", "SLD", "Nowoczesna", "KORWiN", "Razem", "Other")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Pooled poll trends - all parties", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_trends_parties.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

datl_maj <- datl[!(datl$variable %in% c("Kukiz'15", "Nowoczesna", "PSL", "SLD", "KORWiN", "Wiosna", "Other", "Razem")),]
p <- ggplot(datl_maj, aes(x=date, y=value, colour=factor(variable))) + geom_line() + 
  geom_abline(intercept=5, slope=0, colour="gray60", linetype=3) +
  geom_ribbon(data=subset(datl, variable=="PiS"),aes(ymin=PiSlow, ymax=PiShigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="PO"),aes(ymin=POlow, ymax=POhigh), colour=NA, fill="orange", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PiS), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PO), col="orange", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=pcols, breaks=c("PiS", "PO")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Pooled poll trends - major parties", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_trends_major_parties.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

datl_medium <- datl[!(datl$variable %in% c("PO", "PiS", "KORWiN", "Nowoczesna", "Razem", "Kukiz'15", "Other")), ]
p <- ggplot(datl_medium, aes(x=date, y=value, colour=factor(variable))) + geom_line() + 
  geom_abline(intercept=5, slope=0, colour="gray60", linetype=2) +
  geom_ribbon(data=subset(datl, variable=="Wiosna"),aes(ymin=Wiosnalow, ymax=Wiosnahigh), colour=NA, fill="purple4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="PSL"),aes(ymin=PSLlow, ymax=PSLhigh), colour=NA, fill="darkgreen", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="SLD"),aes(ymin=SLDlow, ymax=SLDhigh), colour=NA, fill="red", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Wiosna), col="purple4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PSL), col="darkgreen", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=SLD), col="red", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=pcols, breaks=c("Wiosna", "PSL", "SLD"), 
                      labels=c("Wiosna", "PSL", "SLD")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Pooled poll trends - medium-sized parties", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_trends_medium_parties.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

datl_minor <- datl[!(datl$variable %in% c("PO", "PiS", "Kukiz'15", "Wiosna", "PSL", "SLD")), ]
p <- ggplot(datl_minor, aes(x=date, y=value, colour=factor(variable))) + geom_line() + geom_abline(intercept=5, slope=0, colour="black", linetype=2) +
  geom_ribbon(data=subset(datl, variable=="Kukiz'15"),aes(ymin=Kukiz15low, ymax=Kukiz15high), colour=NA, fill="black", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Nowoczesna"),aes(ymin=Nowoczesnalow, ymax=Nowoczesnahigh), colour=NA, fill="blue", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="KORWiN"),aes(ymin=KORWiNlow, ymax=KORWiNhigh), colour=NA, fill="midnightblue", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Razem"),aes(ymin=Razemlow, ymax=Razemhigh), colour=NA, fill="magenta", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Other"),aes(ymin=Otherlow, ymax=Otherhigh), colour=NA, fill="gray50", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=`Kukiz'15`), col="black", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Nowoczesna), col="blue", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=KORWiN), col="midnightblue", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Razem), col="magenta", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Other), col="gray50", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=pcols, breaks=c("Kukiz'15", "Nowoczesna", "KORWiN", "Razem", "Other"), 
                      labels=c("Kukiz'15", "Nowoczesna", "KORWiN", "Razem", "Other")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Pooled poll trends - minor parties", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_trends_minor_parties.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

p <- ggplot(datl, aes(x=date, y=value, colour=factor(variable))) +
  geom_line(show.legend = F) + geom_abline(intercept=5, slope=0, colour="gray60", linetype=3) +
  facet_wrap( ~ variable, nrow=3, scales="free_y") +
  geom_ribbon(data=subset(datl, variable=="PiS"), aes(ymin=PiSlow, ymax=PiShigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="PO"), aes(ymin=POlow, ymax=POhigh), colour=NA, fill="orange", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Kukiz'15"), aes(ymin=Kukiz15low, ymax=Kukiz15high), colour=NA, fill="black", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Wiosna"), aes(ymin=Wiosnalow, ymax=Wiosnahigh), colour=NA, fill="purple4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="PSL"), aes(ymin=PSLlow, ymax=PSLhigh), colour=NA, fill="darkgreen", alpha=0.3) +  
  geom_ribbon(data=subset(datl, variable=="SLD"), aes(ymin=SLDlow, ymax=SLDhigh), colour=NA, fill="red", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="KORWiN"), aes(ymin=KORWiNlow, ymax=KORWiNhigh), colour=NA, fill="midnightblue", alpha=0.3) +  
  geom_ribbon(data=subset(datl, variable=="Nowoczesna"), aes(ymin=Nowoczesnalow, ymax=Nowoczesnahigh), colour=NA, fill="blue", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Razem"), aes(ymin=Razemlow, ymax=Razemhigh), colour=NA, fill="magenta", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Other"), aes(ymin=Otherlow, ymax=Otherhigh), colour=NA, fill="grey50", alpha=0.3) +
  geom_point(data=subset(pdatl, variable=="PiS"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="blue4", size=1.5) +
  geom_point(data=subset(pdatl, variable=="PO"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="orange", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Kukiz'15"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="black", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Wiosna"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="purple4", size=1.5) +
  geom_point(data=subset(pdatl, variable=="PSL"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="darkgreen", size=1.5) +
  geom_point(data=subset(pdatl, variable=="SLD"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="red", size=1.5) +
  geom_point(data=subset(pdatl, variable=="KORWiN"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="midnightblue", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Nowoczesna"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="blue", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Razem"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="magenta", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Other"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="grey50", size=1.5) +
  theme(legend.position="none", panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none")+
  scale_x_date(labels=date_format("%d.%m"))+
  scale_colour_manual(name="", values=pcols) +
  labs(x="", y="% of vote", title="Pooled poll trends - all parties", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_trends_all_parties.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5)

#current seat share
p <- ggplot(data=frame, mapping=aes(x=Party, y=Weighted, fill=Party)) +
  geom_bar(stat="identity", width=.75, show.legend = F) +
  geom_abline(intercept=231, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=276, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=307, slope=0, colour="gray10", linetype=3) +
  scale_y_continuous('Number of seats', limits=c(0,320), breaks=c(0, 50, 100, 150, 200, 231, 276, 307)) +
  scale_fill_manual(name="Party", values = pcols, 
                    breaks=c("PO", "PiS", "SLD", "PSL", "Wiosna", "KORWiN", "MN", "Kukiz'15", "Nowoczesna", "Razem"), 
                    labels=c("PO", "PiS", "SLD", "PSL", "Wiosna", "KORWiN", "MN", "Kukiz'15", "Nowoczesna", "Razem"))+
  geom_label(aes(x=2, y=231), label="Legislative majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed") +
  geom_label(aes(x=2, y=276), label="Overturn presidential veto", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed") +
  geom_label(aes(x=2, y=307), label="Constitutional majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed") +    
  annotate("text", x=frame$Party, y=c(frame$Weighted+8), label=frame$Weighted, size=4, family="Roboto Condensed")+
  labs(x="", y="% of vote", title="Estimated share of seats - all parties", 
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_seats_parties.png", 
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
