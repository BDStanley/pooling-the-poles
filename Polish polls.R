# PREPARE WORKSPACE
rm(list=ls())
setwd('/Users/benstanley/Desktop/Personal/Dropbox/Resources/R scripts/Poland')
library(tidyverse); library(rjags); library(R2jags); library(R2WinBUGS); library(scales)
library(grid); library(foreign); library(memisc); library(MCMCpack); library(repmis); 
library(readxl); library(pander); library(coda); library(runjags); library(rgdal);
library(maptools); library(rgeos); library(gpclib); library(reshape2); library(plyr);
library(gridExtra); library(grid); library(cowplot); library(scales)
gpclibPermit()

#text

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
           "Kukiz'15"="black", "KORWiN" = "deepskyblue", "Wiosna" = "maroon", "Other"="gray50", 
           "MN"="yellow", "Nowoczesna"="blue", "SLD"="red", "Razem"="magenta")


## POOLED POLL MODEL
# read in, subset and adjust data
pollingdata <- read.csv('~/Desktop/Personal/Dropbox/Resources/Polish materials/Poll data/pooledpolls.csv')
pollingdata <- subset(pollingdata, select = -c(Source,vote,dkn,nvote))
pollingdata$nDef <- round(((100-pollingdata$DK)/100)*pollingdata$n, digits=0)
pollingdata$PO <- 100/((100-pollingdata$DK))*pollingdata$PO
pollingdata$PiS <- 100/((100-pollingdata$DK))*pollingdata$PiS
pollingdata$PSL <- 100/((100-pollingdata$DK))*pollingdata$PSL
pollingdata$SLD <- 100/((100-pollingdata$DK))*pollingdata$SLD
pollingdata$KORWiN <- 100/((100-pollingdata$DK))*pollingdata$KORWiN
pollingdata$Kukiz15 <- 100/((100-pollingdata$DK))*pollingdata$Kukiz15
pollingdata$Nowoczesna <- 100/((100-pollingdata$DK))*pollingdata$Nowoczesna
pollingdata$Razem <- 100/((100-pollingdata$DK))*pollingdata$Razem
pollingdata$Wiosna <- 100/((100-pollingdata$DK))*pollingdata$Wiosna
pollingdata$Other <- 100/((100-pollingdata$DK))*pollingdata$Other
pollingdata$nTot <- NULL
pollingdata$DK <- NULL
pollingdata$n <- NULL
pollingdata$pdate <- julian(as.Date(pollingdata$date, "%d/%m/%Y"), origin=as.Date("2015-10-31"))
pollingdata$pdate <- as.Date(pollingdata$pdate, origin=as.Date("2015-10-31"))
pollingdata <- pollingdata[which(pollingdata$pdate > 0),]
pollingdata <- pollingdata[!is.na(pollingdata$pdate),]
pollingdata <- pollingdata[order(pollingdata$pdate),]
pollingdata <- subset(pollingdata, as.integer(pdate) > as.integer(max(pdate)-150))
pollingdata$day <- as.integer(pollingdata$pdate)-as.integer(pollingdata$pdate)[1] + 1

# create dataset for jags model
NUMPOLLS <- nrow(pollingdata)
PERIOD <- max(as.integer(pollingdata$day))
HOUSECOUNT <- length(levels(pollingdata$agency))
HOUSENAMES <- levels(pollingdata$agency)
PARTYNAMES <- c("PO","PiS","SLD","PSL","KORWiN","Kukiz15","Nowoczesna","Razem", "Wiosna","Other")
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
            house = as.integer(as.factor(pollingdata$agency)), 
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

   alpha[1] ~ dunif(200, 300) # PO
   alpha[2] ~ dunif(350, 450) # PiS
   alpha[3] ~ dunif(50, 100) # SLD
   alpha[4] ~ dunif(50, 100) # PSL
   alpha[5] ~ dunif(20, 40) # KORWiN
   alpha[6] ~ dunif(50, 100) # Kukiz15
   alpha[7] ~ dunif(50, 100) # Nowoczesna
   alpha[8] ~ dunif(20, 40) # Razem
   alpha[9] ~ dunif(50, 100) # Wiosna
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
results <- run.jags(model, monitor=c('walk','houseEffect','tightness'),data=data,n.chains=4,adapt=10000,
                    burnin=20000,sample=20000,thin=5,method="parallel")
mysummary <- summary(results)
save(mysummary,file="ppsummary")


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

KORWiNmean <- ppframe$Mean[grep('5]', ppframe$n)]*100
KORWiNlow <- ppframe$Lower95[grep('5]', ppframe$n)]*100
KORWiNhigh <- ppframe$Upper95[grep('5]', ppframe$n)]*100
KORWiNest <- data.frame(KORWiNmean, KORWiNlow, KORWiNhigh)

Kukiz15mean <- ppframe$Mean[grep('6]', ppframe$n)]*100
Kukiz15low <- ppframe$Lower95[grep('6]', ppframe$n)]*100
Kukiz15high <- ppframe$Upper95[grep('6]', ppframe$n)]*100
Kukiz15est <- data.frame(Kukiz15mean, Kukiz15low, Kukiz15high)

Nowoczesnamean <- ppframe$Mean[grep('7]', ppframe$n)]*100
Nowoczesnalow <- ppframe$Lower95[grep('7]', ppframe$n)]*100
Nowoczesnahigh <- ppframe$Upper95[grep('7]', ppframe$n)]*100
Nowoczesnaest <- data.frame(Nowoczesnamean, Nowoczesnalow, Nowoczesnahigh)

Razemmean <- ppframe$Mean[grep('8]', ppframe$n)]*100
Razemlow <- ppframe$Lower95[grep('8]', ppframe$n)]*100
Razemhigh <- ppframe$Upper95[grep('8]', ppframe$n)]*100
Razemest <- data.frame(Razemmean, Razemlow, Razemhigh)

Wiosnamean <- ppframe$Mean[grep('9]', ppframe$n)]*100
Wiosnalow <- ppframe$Lower95[grep('9]', ppframe$n)]*100
Wiosnahigh <- ppframe$Upper95[grep('9]', ppframe$n)]*100
Wiosnaest <- data.frame(Wiosnamean, Wiosnalow, Wiosnahigh)

Othermean <- ppframe$Mean[grep('10]', ppframe$n)]*100
Otherlow <- ppframe$Lower95[grep('10]', ppframe$n)]*100
Otherhigh <- ppframe$Upper95[grep('10]', ppframe$n)]*100
Otherest <- data.frame(Othermean, Otherlow, Otherhigh)

plotdata <- cbind(PiSest, POest, PSLest, SLDest, KORWiNest, Kukiz15est, Nowoczesnaest, Razemest, Wiosnaest, Otherest)
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
colnames(pos) <- pooledframe$party
means_pos <- round(apply(pos,2,mean)*100, digits=2)
HDIs <- round(HPDinterval(pos)*100, digits=2)
pooledframe <- cbind(pooledframe, means_pos, HDIs)
pooledframe <- arrange(pooledframe, desc(means_pos))

# calculate probabilities
PiS.50.diff <- pos[,"PiS"] - 0.50
PiS.50.out <- sum(PiS.50.diff > 0) / length(PiS.50.diff)
PiS.50.out <- round(PiS.50.out, 2)

PO.PiS.diff <-pos[,"PiS"] - pos[,"PO"]
PO.PiS.diff.out <- sum(PO.PiS.diff > 0) / length(PO.PiS.diff)
PO.PiS.diff.out <- round(PO.PiS.diff.out, 2)

Nowoczesna.Kukiz15.diff <-pos[,"Nowoczesna"] - pos[,"Kukiz15"]
Nowoczesna.Kukiz15.diff.out <- sum(Nowoczesna.Kukiz15.diff > 0) / length(Nowoczesna.Kukiz15.diff)
Nowoczesna.Kukiz15.diff.out <- round(Nowoczesna.Kukiz15.diff.out, 2)

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
# read in 2015 coefficient data
data1 <- read_excel('~/Desktop/Personal/Dropbox/Resources/Polish materials/Poll data/2015percentages.xlsx')

# enter most recent average level of support and reduce to zero if below electoral threshold
means_pos <- data.frame(means_pos)
for(i in 1:10) {
  if(means_pos[i,] > 4.99) {
    means_pos[i,] <- means_pos[i,] 
  } else { means_pos[i,] <- 0
  }
}

#Threshold for electoral coalitions
#if(means_pos["SLD",] > 7.99) {
#means_pos["SLD",] <- means_pos["SLD",] 
#} else { means_pos["SLD",] <- 0
#}
means_pos <- 100/(sum(means_pos))*means_pos

# define party percentage per district according to most recent poll, include MN manually
POpct <- round(data1$POcoef*means_pos[1,], digits=2)
PiSpct <- round(data1$PiScoef*means_pos[2,], digits=2)
SLDpct <- round(data1$SLDcoef*means_pos[3,], digits=2)
PSLpct <- round(data1$PSLcoef*means_pos[4,], digits=2)
KORWiNpct <- round(data1$KORWiNcoef*means_pos[5,], digits=2)
Kukiz15pct <- round(data1$Kukiz15coef*means_pos[6,], digits=2)
Nowoczesnapct <- round(data1$Nowoczesnacoef*means_pos[7,], digits=2)
Razempct <- round(data1$Razemcoef*means_pos[8,], digits=2)
Wiosnapct <- round(data1$Wiosnacoef*means_pos[9,], digits=2)
MNpct <- c(0.18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8.14, 0, 0, 0, 0, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# estimate number of votes per party per district based on 2015 turnout
POest <- (data1$validvotes/100)*POpct
PiSest <- (data1$validvotes/100)*PiSpct
PSLest <- (data1$validvotes/100)*PSLpct
SLDest <- (data1$validvotes/100)*SLDpct
KORWiNest <- (data1$validvotes/100)*KORWiNpct
Kukiz15est <- (data1$validvotes/100)*Kukiz15pct
Nowoczesnaest <- (data1$validvotes/100)*Nowoczesnapct
Razemest <- (data1$validvotes/100)*Razempct
Wiosnaest <- (data1$validvotes/100)*Wiosnapct
MNest <- (data1$validvotes/100)*MNpct

# estimate seat shares for each district
poldHondt <- data.frame(KORWiN=rep(1,42), Kukiz15=rep(1,42), MN=rep(1,42), Nowoczesna=rep(1,42), PiS=rep(1,42), PO=rep(1,42),
                        PSL=rep(1,42), Razem=rep(1,42), SLD=rep(1,42), Wiosna=rep(1,42))
for( i in 1 : 42 ) {
  poldHondt[i, ] <- dHondt(c("PO", "PiS", "SLD", "PSL", "MN", "KORWiN", "Wiosna", "Kukiz15", "Nowoczesna", "Razem"), 
                           c(POest[i], PiSest[i], SLDest[i], PSLest[i], MNest[i], 
                             KORWiNest[i], Wiosnaest[i], Kukiz15est[i], Nowoczesnaest[i], 
                             Razemest[i]), data1$magnitude[i])
}

# create frame for plots
frame <- t(rbind(poldHondt[1,], colSums(poldHondt[2:42,])))
frame <- data.frame(rownames(frame), frame)
colnames(frame) <- c("Party", "Unweighted", "Weighted")
frame <- frame[with(frame, order(-Weighted)),]
frame$in2015[frame$Party=="PO"] <- 138
frame$in2015[frame$Party=="PiS"] <- 235
frame$in2015[frame$Party=="SLD"] <- 0
frame$in2015[frame$Party=="PSL"] <- 16
frame$in2015[frame$Party=="MN"] <- 1
frame$in2015[frame$Party=="KORWiN"] <- 0
frame$in2015[frame$Party=="Kukiz15"] <- 42
frame$in2015[frame$Party=="Nowoczesna"] <- 28
frame$in2015[frame$Party=="Razem"] <- 0
frame$in2015[frame$Party=="Wiosna"] <- 0
frame <- frame[frame$Weighted>0,]
frame$Party <- factor(frame$Party, levels=c("PO", "PiS", "SLD", "PSL", "MN", "KORWiN", "Wiosna", "Kukiz15", "Nowoczesna", "Razem"))
frame$diffPres <- sprintf("%+d", (frame$Weighted - frame$in2015))
frame$diffPres <- sprintf("(%s)", frame$diffPres)
frame$diffPresUn <- sprintf("%+d", (frame$Unweighted - frame$in2015))
frame$diffPresUn <- sprintf("(%s)", frame$diffPresUn)
frame$Party <- reorder(frame$Party, -frame$Weighted)


## CREATE PLOTS
setwd('~/Desktop/Personal/Dropbox/Resources/Polish materials/Plots')
levels(posfrmelt$variable)[levels(posfrmelt$variable)=="Kukiz15"] <- "Kukiz'15"
levels(pooledframe$party)[levels(pooledframe$party)=="Kukiz15"] <- "Kukiz'15"

# plot most recent party support
ggplot(data=posfrmelt, aes(variable, value)) + 
  geom_boxplot(aes(fill=variable), outlier.shape=NA) +
  coord_flip() +
  geom_hline(aes(yintercept=0.05), colour="black", linetype="dashed") +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position="none") +
  background_grid(major = "xy", minor = "none") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PO"]),0)), 
           x="PO", y=mean(posfrmelt$value[posfrmelt$variable=="PO"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PiS"]),0)), 
           x="PiS", y=mean(posfrmelt$value[posfrmelt$variable=="PiS"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="SLD"]),0)), 
           x="SLD", y=mean(posfrmelt$value[posfrmelt$variable=="SLD"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Kukiz'15"]),0)), 
           x="Kukiz'15", y=mean(posfrmelt$value[posfrmelt$variable=="Kukiz'15"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PSL"]),0)), 
           x="PSL", y=mean(posfrmelt$value[posfrmelt$variable=="PSL"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Nowoczesna"]),0)), 
           x="Nowoczesna", y=mean(posfrmelt$value[posfrmelt$variable=="Nowoczesna"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="KORWiN"]),0)), 
           x="KORWiN", y=mean(posfrmelt$value[posfrmelt$variable=="KORWiN"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Razem"]),0)), 
           x="Razem", y=mean(posfrmelt$value[posfrmelt$variable=="Razem"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Wiosna"]),0)), 
           x="Wiosna", y=mean(posfrmelt$value[posfrmelt$variable=="Wiosna"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Other"]),0)), 
           x="Other", y=mean(posfrmelt$value[posfrmelt$variable=="Other"]), size=3.5, hjust = "center", vjust=-2.7) +
  annotate(geom = "text", label=paste("Pr(PiS > 50%)  = ", PiS.50.out), x=5.5, y=0.25, size=3.5, adj=c(0)) +
  annotate(geom = "text", label=paste("Pr(PiS > PO)  = ", PO.PiS.diff.out), x=5, y=0.25, size=3.5, adj=c(0)) +
  annotate(geom = "text", label=paste("Pr(Wiosna > 5%)  = ", Wiosna.thr.out), x=4.5, y=0.25, size=3.5, adj=c(0)) +
  annotate(geom = "text", label=paste("Pr(SLD > 5%)  = ", SLD.thr.out), x=4, y=0.25, size=3.5, adj=c(0)) +
  annotate(geom = "text", label=paste("Pr(Kukiz'15 > 5%)  = ", Kukiz15.thr.out), x=3.5, y=0.25, size=3.5, adj=c(0)) +
  annotate(geom = "text", label=paste("Pr(Nowoczesna > 5%)  = ", Nowoczesna.thr.out), x=3, y=0.25, size=3.5, adj=c(0)) +
  annotate(geom = "text", label=paste("Pr(PSL > 5%)  = ", PSL.thr.out), x=2.5, y=0.25, size=3.5, adj=c(0)) +
  annotate(geom = "text", label=paste("Pr(KORWiN > 5%)  = ", KORWiN.thr.out), x=2, y=0.25, size=3.5, adj=c(0)) +  
  annotate(geom = "text", label=paste("Pr(Razem > 5%)  = ", Razem.thr.out), x=1.5, y=0.25, size=3.5, adj=c(0)) +
  scale_x_discrete(name=" ", limits=rev(pooledframe$party)) +
  scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  scale_fill_manual(values=pcols) +
  labs(caption="@BDStanley")
ggsave('Latest_figures.png')

# plot trends
datl <- melt(plotdata, measure.vars=c("POmean","PiSmean","SLDmean","PSLmean","KORWiNmean", "Wiosnamean", "Kukiz15mean", "Nowoczesnamean", "Razemmean", "Othermean"))
levels(datl$variable) <- c("PO","PiS", "SLD","PSL","KORWiN", "Wiosna", "Kukiz'15", "Nowoczesna", "Razem", "Other")
datl$variable <- factor(datl$variable, levels = c("PiS", "PO", "Nowoczesna", "Kukiz'15", "PSL", "SLD", "KORWiN", "Wiosna", "Razem", "Other"))

pdatl <- melt(pollingdata, measure.vars=c("PO","PiS","SLD","PSL","KORWiN", "Wiosna", "Kukiz15", "Nowoczesna", "Razem", "Other"))
levels(pdatl$variable) <- c("PO","PiS", "SLD","PSL","KORWiN", "Wiosna", "Kukiz'15", "Nowoczesna", "Razem", "Other")
pdatl$variable <- factor(pdatl$variable, levels = c("PiS", "PO", "Nowoczesna", "Kukiz'15", "PSL", "SLD", "KORWiN", "Wiosna", "Razem", "Other"))

datl_all <- datl[!(datl$variable %in% c("Other")),]
ggplot(datl_all, aes(x=date, y=value, colour=factor(variable))) + geom_line() + 
  geom_abline(intercept=5, slope=0, colour="black", linetype=3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PiS), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PO), col="orange", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Kukiz15), col="black", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Wiosna), col="maroon", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PSL), col="darkgreen", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=SLD), col="red", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Nowoczesna), col="blue", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=KORWiN), col="deepskyblue", size=1.5) +
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
  labs(caption="@BDStanley")
ggsave('all_trends.png')

datl_POPiS <- datl[!(datl$variable %in% c("Kukiz'15", "Nowoczesna", "PSL", "SLD", "KORWiN", "Wiosna", "Other", "Razem")),]
ggplot(datl_POPiS, aes(x=date, y=value, colour=factor(variable))) + geom_line() + 
  geom_abline(intercept=5, slope=0, colour="black", linetype=3) +
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
  labs(caption="@BDStanley")
ggsave('POPiS_trends.png')

datl_other <- datl[!(datl$variable %in% c("PO", "PiS", "KORWiN", "Nowoczesna", "Razem", "Other")), ]
ggplot(datl_other, aes(x=date, y=value, colour=factor(variable))) + geom_line() + 
  geom_abline(intercept=5, slope=0, colour="black", linetype=2) +
  geom_ribbon(data=subset(datl, variable=="Kukiz'15"),aes(ymin=Kukiz15low, ymax=Kukiz15high), colour=NA, fill="black", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Wiosna"),aes(ymin=Wiosnalow, ymax=Wiosnahigh), colour=NA, fill="maroon", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="PSL"),aes(ymin=PSLlow, ymax=PSLhigh), colour=NA, fill="darkgreen", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="SLD"),aes(ymin=SLDlow, ymax=SLDhigh), colour=NA, fill="red", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Kukiz15), col="black", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Wiosna), col="maroon", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PSL), col="darkgreen", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=SLD), col="red", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=pcols, breaks=c("Kukiz'15", "Wiosna", "PSL", "SLD"), 
                      labels=c("Kukiz'15", "Wiosna", "PSL", "SLD")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(caption="@BDStanley")
ggsave('Other_trends.png')

datl_minor <- datl[!(datl$variable %in% c("PO", "PiS", "Kukiz'15", "Wiosna", "PSL", "SLD")), ]
ggplot(datl_minor, aes(x=date, y=value, colour=factor(variable))) + geom_line() + 
  geom_abline(intercept=5, slope=0, colour="black", linetype=2) +
  geom_ribbon(data=subset(datl, variable=="Nowoczesna"),aes(ymin=Nowoczesnalow, ymax=Nowoczesnahigh), colour=NA, fill="blue", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="KORWiN"),aes(ymin=KORWiNlow, ymax=KORWiNhigh), colour=NA, fill="deepskyblue", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Razem"),aes(ymin=Razemlow, ymax=Razemhigh), colour=NA, fill="magenta", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Other"),aes(ymin=Otherlow, ymax=Otherhigh), colour=NA, fill="gray50", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Nowoczesna), col="blue", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=KORWiN), col="deepskyblue", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Razem), col="magenta", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Other), col="gray50", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=pcols, breaks=c("Nowoczesna", "KORWiN", "Razem", "Other"), 
                      labels=c("Nowoczesna", "KORWiN", "Razem", "Other")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(caption="@BDStanley")
ggsave('Minor_trends.png')

#levels(datl$variable) <- c("PiS", "PO", "Kukiz'15", "Wiosna", "PSL", "SLD", "KORWiN", "Nowoczesna", "Razem", "Other")
#levels(pdatl$variable) <- c("PiS", "PO", "Kukiz'15", "Wiosna", "PSL", "SLD", "KORWiN", "Nowoczesna", "Razem", "Other")
ggplot(datl, aes(x=date, y=value, colour=factor(variable))) + geom_line() +
  facet_wrap( ~ variable, nrow=3, scales="free_y") +
  geom_abline(intercept=5, slope=0, colour="black", linetype=3) +
  geom_ribbon(data=subset(datl, variable=="PiS"), aes(ymin=PiSlow, ymax=PiShigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="PO"), aes(ymin=POlow, ymax=POhigh), colour=NA, fill="orange", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Kukiz'15"), aes(ymin=Kukiz15low, ymax=Kukiz15high), colour=NA, fill="black", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Wiosna"), aes(ymin=Wiosnalow, ymax=Wiosnahigh), colour=NA, fill="maroon", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="PSL"), aes(ymin=PSLlow, ymax=PSLhigh), colour=NA, fill="darkgreen", alpha=0.3) +  
  geom_ribbon(data=subset(datl, variable=="SLD"), aes(ymin=SLDlow, ymax=SLDhigh), colour=NA, fill="red", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="KORWiN"), aes(ymin=KORWiNlow, ymax=KORWiNhigh), colour=NA, fill="deepskyblue", alpha=0.3) +  
  geom_ribbon(data=subset(datl, variable=="Nowoczesna"), aes(ymin=Nowoczesnalow, ymax=Nowoczesnahigh), colour=NA, fill="blue", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Razem"), aes(ymin=Razemlow, ymax=Razemhigh), colour=NA, fill="magenta", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Other"), aes(ymin=Otherlow, ymax=Otherhigh), colour=NA, fill="grey50", alpha=0.3) +
  geom_point(data=subset(pdatl, variable=="PiS"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="blue4", size=1.5) +
  geom_point(data=subset(pdatl, variable=="PO"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="orange", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Kukiz'15"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="black", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Wiosna"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="maroon", size=1.5) +
  geom_point(data=subset(pdatl, variable=="PSL"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="darkgreen", size=1.5) +
  geom_point(data=subset(pdatl, variable=="SLD"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="red", size=1.5) +
  geom_point(data=subset(pdatl, variable=="KORWiN"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="deepskyblue", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Nowoczesna"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="blue", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Razem"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="magenta", size=1.5) +
  geom_point(data=subset(pdatl, variable=="Other"), aes(x=as.Date(date, "%d/%m/%Y"), y=value), col="grey50", size=1.5) +
  theme(legend.position="none", panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none")+
  scale_x_date(labels=date_format("%d.%m"))+
  scale_colour_manual(name="", values=pcols) +
  labs(caption="@BDStanley")
ggsave('Overall_trends.png')

#current seat share without district weighting
levels(frame$Party)[levels(frame$Party)=="Kukiz15"] <- "Kukiz'15"
ggplot(data=frame, mapping=aes(x=Party, y=Unweighted, fill=Party)) +
  geom_bar(stat="identity", width=.75) +
  geom_abline(intercept=231, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=276, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=307, slope=0, colour="gray10", linetype=3) +
  scale_y_continuous('Number of seats', limits=c(0,320), breaks=c(0, 50, 100, 150, 200, 231, 276, 307)) +
  scale_fill_manual(name="Party", values = pcols, 
                    breaks=c("PO", "PiS", "SLD", "PSL", "Wiosna", "KORWiN", "MN", "Kukiz'15", "Nowoczesna", "Razem"), 
                    labels=c("PO", "PiS", "SLD", "PSL", "Wiosna", "KORWiN", "MN", "Kukiz'15", "Nowoczesna", "Razem"))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position="none", axis.text.x=element_text(colour="black", size=10), 
        axis.text.y= element_text(colour="black", size=10), axis.title.x = element_blank()) +
  geom_label(aes(x=2, y=231), label="Legislative majority", size=3, adj=c(0), fill="grey95") +
  geom_label(aes(x=2, y=276), label="Overturn presidential veto", size=3, adj=c(0), fill="grey95") +
  geom_label(aes(x=2, y=307), label="Constitutional majority", size=3, adj=c(0), fill="grey95") +    
  annotate("text", x=frame$Party, y=c(frame$Unweighted+18), label=frame$Unweighted, size=4)+
  annotate("text", x=frame$Party, y=c(frame$Unweighted+8), label=frame$diffPres, size=3) +
  labs(caption="@BDStanley")
ggsave('Seats_unweighted.png')

#current seat share with district weighting
ggplot(data=frame, mapping=aes(x=Party, y=Weighted, fill=Party)) +
  geom_bar(stat="identity", width=.75) +
  geom_abline(intercept=231, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=276, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=307, slope=0, colour="gray10", linetype=3) +
  scale_y_continuous('Number of seats', limits=c(0,320), breaks=c(0, 50, 100, 150, 200, 231, 276, 307)) +
  scale_fill_manual(name="Party", values = pcols, 
                    breaks=c("PO", "PiS", "SLD", "PSL", "Wiosna", "KORWiN", "MN", "Kukiz'15", "Nowoczesna", "Razem"), 
                    labels=c("PO", "PiS", "SLD", "PSL", "Wiosna", "KORWiN", "MN", "Kukiz'15", "Nowoczesna", "Razem"))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position="none", axis.text.x=element_text(colour="black", size=10), 
        axis.text.y= element_text(colour="black", size=10), axis.title.x = element_blank()) +
  geom_label(aes(x=2, y=231), label="Legislative majority", size=3, adj=c(0), fill="grey95") +
  geom_label(aes(x=2, y=276), label="Overturn presidential veto", size=3, adj=c(0), fill="grey95") +
  geom_label(aes(x=2, y=307), label="Constitutional majority", size=3, adj=c(0), fill="grey95") +    
  annotate("text", x=frame$Party, y=c(frame$Weighted+18), label=frame$Weighted, size=4)+
  annotate("text", x=frame$Party, y=c(frame$Weighted+8), label=frame$diffPres, size=3) +
  labs(caption="@BDStanley")
ggsave('Seats_weighted.png')

# seats table
seats <- cbind(poldHondt, data1)
row.names(seats) <- data1$name
keep <- c("PiS","PO","Kukiz15","Nowoczesna","SLD","PSL","KORWiN", "Wiosna","Razem")
seats <- seats[keep]
#colnames(seats) <- sprintf("%-8s", colnames(seats))
seats <- seats[-1,]
seats$id <- 1:41
seats$PiSPO <-abs(seats$PiS-seats$PO)
seats$PiSmPO <- seats$PiS-seats$PO

#regional maps
const=readOGR("/Users/benstanley/Desktop/Personal/Dropbox/Resources/Polish materials/Regional data/GRED_beta2_20170530_Poland/shapefile/GRED_Poland_2011_beta2.shp")
const@data$id = rownames(const@data)
const.points = fortify(const, region="id")
const.df = join(const.points, const@data, by="id")
const.df$con[const.df$id==0] <- 12
const.df$con[const.df$id==1] <- 13
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
seats$label_point_x[seats$id==12] <- label_points$x[1]
seats$label_point_y[seats$id==12] <- label_points$y[1]
seats$label_point_x[seats$id==13] <- label_points$x[2]
seats$label_point_y[seats$id==13] <- label_points$y[2]
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

ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Wiosna)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Wiosna", limits=c(min=0, max=20), low = "white", high = "maroon", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Wiosna, label=Wiosna), fill="white") +
  theme_void() + theme(aspect.ratio=1, legend.position="none") +
  labs(caption="@BDStanley")
ggsave('Wiosna_seats.png', width=7, height=7)

ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiS)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PiS", limits=c(min=0, max=20), low = "white", high = "blue4", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiS, label=PiS), fill="white") +
  theme_void() + theme(aspect.ratio=1, legend.position="none") +
  labs(caption="@BDStanley")
ggsave('PiS_seats.png', width=7, height=7)

ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PO)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PO", limits=c(min=0, max=20), low = "white", high = "orange", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PO, label=PO), fill="white") +
  theme_void() + theme(aspect.ratio=1, legend.position="none") +
  labs(caption="@BDStanley")
ggsave('PO_seats.png', width=7, height=7)

ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PSL)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PSL", limits=c(min=0, max=20), low = "white", high = "darkgreen", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PSL, label=PSL), fill="white") +
  theme_void() + theme(aspect.ratio=1, legend.position="none") +
  labs(caption="@BDStanley")
ggsave('PSL_seats.png', width=7, height=7)

ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(SLD)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="SLD", limits=c(min=0, max=20), low = "white", high = "red", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=SLD, label=SLD), fill="white") +
  theme_void() + theme(aspect.ratio=1, legend.position="none") +
  labs(caption="@BDStanley")
ggsave('SLD_seats.png', width=7, height=7)

ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Nowoczesna)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Nowoczesna", limits=c(min=0, max=20), low = "white", high = "blue", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Nowoczesna, label=Nowoczesna), fill="white") +
  theme_void() + theme(aspect.ratio=1, legend.position="none") +
  labs(caption="@BDStanley")
ggsave('Nowoczesna_seats.png', width=7, height=7)

ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Kukiz15)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Kukiz'15", limits=c(min=0, max=20), low = "white", high = "black", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Kukiz15, label=Kukiz15), fill="white") +
  theme_void() + theme(aspect.ratio=1, legend.position="none") +
  labs(caption="@BDStanley")
ggsave('Kukiz15_seats.png', width=7, height=7)

ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiSmPO)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient2(name="PiSPO", limits=c(min=-20, max=20), low = "orange", mid="white", high = "blue4", midpoint=0, guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiSPO, label=PiSPO), fill="white") +
  theme_void() + theme(aspect.ratio=1, legend.position="none") +
  labs(caption="@BDStanley")
ggsave('PiSPO_seats.png', width=7, height=7)

Wiosnaspread<- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Wiosna)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient(name="", limits=c(min=0, max=max(seats$Wiosna)), breaks=c(0, max(seats$Wiosna)),
                      low = "white", high = "maroon", guide="colorbar") +
  theme_void() + theme(aspect.ratio=1, legend.key.size = unit(0.7, "line")) +
  ggtitle("Wiosna") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  theme(plot.margin=grid::unit(c(5,0,0,0), "mm"))

PiSspread<- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiS)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient(name="", limits=c(min=0, max=max(seats$PiS)), breaks=c(0, max(seats$PiS)),
                      low = "white", high = "blue4", guide="colorbar") +
  theme_void() + theme(aspect.ratio=1, legend.key.size = unit(0.7, "line")) +
  ggtitle("PiS") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  theme(plot.margin=grid::unit(c(5,0,0,0), "mm"))

POspread <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PO)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient(name="", limits=c(min=0, max=max(seats$PO)), breaks=c(0, max(seats$PO)),
                      low = "white", high = "orange", guide="colorbar") +
  theme_void() + theme(aspect.ratio=1, legend.key.size = unit(0.7, "line")) +
  ggtitle("PO") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  theme(plot.margin=grid::unit(c(5,0,0,0), "mm"))

PSLspread <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PSL)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient(name="", limits=c(min=0, max=max(seats$PSL)), breaks=c(0, max(seats$PSL)),
                      low = "white", high = "darkgreen", guide="colorbar") +
  theme_void() + theme(aspect.ratio=1, legend.key.size = unit(0.7, "line")) +
  ggtitle("PSL") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  theme(plot.margin=grid::unit(c(5,0,0,0), "mm"))

SLDspread <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(SLD)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient(name="", limits=c(min=0, max=max(seats$SLD)), breaks=c(0, max(seats$SLD)),
                      low = "white", high = "red", guide="colorbar") +
  theme_void() + theme(aspect.ratio=1, legend.key.size = unit(0.7, "line")) +
  ggtitle("SLD") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  theme(plot.margin=grid::unit(c(5,0,0,0), "mm"))

Nowoczesnaspread <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Nowoczesna)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient(name="", limits=c(min=0, max=max(seats$Nowoczesna)), breaks=c(0, max(seats$Nowoczesna)),
                      low = "white", high = "blue", guide="colorbar") +
  theme_void() + theme(aspect.ratio=1, legend.key.size = unit(0.7, "line")) +
  ggtitle("Nowoczesna") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  theme(plot.margin=grid::unit(c(5,0,0,0), "mm"))

Kukiz15spread <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Kukiz15)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient(name="", limits=c(min=0, max=max(seats$Kukiz15)), breaks=c(0, max(seats$Kukiz15)),
                      low = "white", high = "black", guide="colorbar") +
  theme_void() + theme(aspect.ratio=1, legend.key.size = unit(0.7, "line")) +
  ggtitle("Kukiz'15") +
  theme(plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  theme(plot.margin=grid::unit(c(5,0,0,0), "mm"))

seat_spreads <- plot_grid(PiSspread, POspread, Wiosnaspread, Kukiz15spread, PSLspread, SLDspread) + 
  labs(caption="@BDStanley")
ggsave('Seat_spreads.png', seat_spreads)

save.image("~/Desktop/Personal/PoolingthePoles.RData")
