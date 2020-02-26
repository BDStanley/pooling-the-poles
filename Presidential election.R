# PREPARE WORKSPACE
rm(list=ls())
setwd('/Users/benstanley/Google Drive/Resources/R scripts/Pooling the Poles')
library(plyr); library(tidyverse); library(rjags); library(R2jags); library(R2WinBUGS); library(scales)
library(grid); library(foreign); library(memisc); library(MCMCpack); library(repmis); 
library(readxl); library(pander); library(coda); library(runjags); library(reshape2); 
library(gridExtra); library(grid); library(cowplot); library(scales); library(hrbrthemes); 
library(tidybayes); library(bayestestR); library(googledrive)


#####ROUND 1#####
#read in data
import <- drive_download(as_id("https://drive.google.com/file/d/1ICkfwpQnMbp-h0AKtfbZEAfpzg16_9zJ/view?usp=sharing"), overwrite=TRUE)
1
pollingdata <- read_excel('pooledpolls_pres_r1.xlsx')
pollingdata <- subset(pollingdata, select = -c(Source))
pollingdata$nDef <- round(((100-pollingdata$DK)/100)*pollingdata$n, digits=0)
pollingdata$Duda <- 100/((100-pollingdata$DK))*pollingdata$Duda
pollingdata$`Kidawa-Błońska` <- 100/((100-pollingdata$DK))*pollingdata$`Kidawa-Błońska`
pollingdata$`Kosiniak-Kamysz` <- 100/((100-pollingdata$DK))*pollingdata$`Kosiniak-Kamysz`
pollingdata$Biedroń <- 100/((100-pollingdata$DK))*pollingdata$Biedroń
pollingdata$Bosak <- 100/((100-pollingdata$DK))*pollingdata$Bosak
pollingdata$Hołownia <- 100/((100-pollingdata$DK))*pollingdata$Hołownia
pollingdata$Other <- 100/((100-pollingdata$DK))*pollingdata$Other
pollingdata$nTot <- NULL
pollingdata$DK <- NULL
pollingdata$n <- NULL
pollingdata$pdate <- julian(as.Date(pollingdata$date, "%d/%m/%Y"), origin=as.Date("2019-12-19"))
pollingdata$pdate <- as.Date(pollingdata$pdate, origin=as.Date("2019-12-19"))
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
PARTYNAMES <- c("Duda","Kidawa-Błońska", "Kosiniak-Kamysz", "Biedroń", "Bosak", "Hołownia", "Other")
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

   alpha[1] ~ dunif(350, 450) # Duda
   alpha[2] ~ dunif(200, 300) # Kidawa-Błońska
   alpha[3] ~ dunif(50, 150) # Kosiniak-Kamysz
   alpha[4] ~ dunif(50, 150) # Biedroń
   alpha[5] ~ dunif(40, 120) # Bosak
   alpha[6] ~ dunif(50, 150) # Hołownia
   alpha[7] ~ dunif(0, 50) # Other

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
                    burnin=4000,sample=40000,thin=5,method="parallel")
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

Dudamean <- ppframe$Mean[grep('1]', ppframe$n)]*100
Dudalow <- ppframe$Lower95[grep('1]', ppframe$n)]*100
Dudahigh <- ppframe$Upper95[grep('1]', ppframe$n)]*100
Dudaest <- data.frame(Dudamean, Dudalow, Dudahigh)

RBmean <- ppframe$Mean[grep('2]', ppframe$n)]*100
RBlow <- ppframe$Lower95[grep('2]', ppframe$n)]*100
RBhigh <- ppframe$Upper95[grep('2]', ppframe$n)]*100
RBest <- data.frame(RBmean, RBlow, RBhigh)

KKmean <- ppframe$Mean[grep('3]', ppframe$n)]*100
KKlow <- ppframe$Lower95[grep('3]', ppframe$n)]*100
KKhigh <- ppframe$Upper95[grep('3]', ppframe$n)]*100
KKest <- data.frame(KKmean, KKlow, KKhigh)

RBmean <- ppframe$Mean[grep('4]', ppframe$n)]*100
RBlow <- ppframe$Lower95[grep('4]', ppframe$n)]*100
RBhigh <- ppframe$Upper95[grep('4]', ppframe$n)]*100
RBest <- data.frame(RBmean, RBlow, RBhigh)

Bosakmean <- ppframe$Mean[grep('5]', ppframe$n)]*100
Bosaklow <- ppframe$Lower95[grep('5]', ppframe$n)]*100
Bosakhigh <- ppframe$Upper95[grep('5]', ppframe$n)]*100
Bosakest <- data.frame(Bosakmean, Bosaklow, Bosakhigh)

Holowniamean <- ppframe$Mean[grep('6]', ppframe$n)]*100
Holownialow <- ppframe$Lower95[grep('6]', ppframe$n)]*100
Holowniahigh <- ppframe$Upper95[grep('6]', ppframe$n)]*100
Holowniaest <- data.frame(Holowniamean, Holownialow, Holowniahigh)

Othermean <- ppframe$Mean[grep('7]', ppframe$n)]*100
Otherlow <- ppframe$Lower95[grep('7]', ppframe$n)]*100
Otherhigh <- ppframe$Upper95[grep('7]', ppframe$n)]*100
Otherest <- data.frame(Othermean, Otherlow, Otherhigh)

plotdata <- cbind(Dudaest, RBest, KKest, RBest, Bosakest, Holowniaest, Otherest)
plotdata$date <- as.Date(c(1:length(Dudamean)), origin=as.Date(tail(pollingdata$pdate, n=1)-length(Dudamean)))

#Latest figures
# prepare data frame
party <- c("Duda", "Kidawa-Błońska", "Kosiniak-Kamysz", "Biedroń", "Bosak", "Hołownia", "Other")
alpha <- rep(1, length(party))
percent <- round(c(mean(tail(Dudaest$Dudamean, n=7)),
                   mean(tail(RBest$RBmean, n=7)),
                   mean(tail(KKest$KKmean, n=7)),
                   mean(tail(RBest$RBmean, n=7)),
                   mean(tail(Bosakest$Bosakmean, n=7)),
                   mean(tail(Holowniaest$Holowniamean, n=7)),
                   mean(tail(Otherest$Othermean, n=7))))
votes <- round((percent*600)/100, digits=2)

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
Duda.50.diff <- pos[,"Duda"] - 0.50
Duda.50.out <- sum(Duda.50.diff > 0) / length(Duda.50.diff)
Duda.50.out <- round(Duda.50.out, 2)

Duda.RB.diff <-pos[,"Duda"] - pos[,"Kidawa-Błońska"]
Duda.RB.diff.out <- sum(Duda.RB.diff > 0) / length(Duda.RB.diff)
Duda.RB.diff.out <- round(Duda.RB.diff.out, 2)

# calculate latest figures
posfr <- data.frame()
for (i in 1:length(pos)){
  posfr <- pos[1:10000,]
}
posfrmelt <- melt(as.data.frame(posfr))

#plot house effects
p <- ggplot() + 
  geom_abline(intercept=0, slope=0, colour="gray10", linetype=3) +
  geom_pointrange(data=houseframe, mapping=aes(x=party, y=Mean, ymin=Lower95, ymax=Upper95, color=house, shape=method), 
                  position = position_dodge(width=0.5)) +
  guides(color=guide_legend(override.aes=list(shape=15, size=1, linetype=0)))+
  labs(color="Pollster", shape="Mode", x="", y="Deviation from mean party vote share", 
       title="House and mode effects", 
       caption = "@BDStanley; benstanley.org") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_houseeffects_pres_r1.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot most recent party support
# colours for plots
cols <- c("Duda"="blue4", "Kidawa-Błońska"="orange", "Kosiniak-Kamysz"="darkgreen", "Bosak" = "midnightblue", "Biedroń" = "red", "Hołownia" = "darkorchid1", "Other"="gray50")

p <- ggplot(posfrmelt, aes(y=variable, x = value, fill=variable)) +
  geom_vline(aes(xintercept=0.50), colour="gray60", linetype="dotted") +
  stat_slabh(normalize="xy") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Duda"]),0)), 
           y="Duda", x=mean(posfrmelt$value[posfrmelt$variable=="Duda"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Kidawa-Błońska"]),0)), 
           y="Kidawa-Błońska", x=mean(posfrmelt$value[posfrmelt$variable=="Kidawa-Błońska"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Kosiniak-Kamysz"]),0)), 
           y="Kosiniak-Kamysz", x=mean(posfrmelt$value[posfrmelt$variable=="Kosiniak-Kamysz"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Biedroń"]),0)), 
           y="Biedroń", x=mean(posfrmelt$value[posfrmelt$variable=="Biedroń"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Bosak"]),0)), 
           y="Bosak", x=mean(posfrmelt$value[posfrmelt$variable=="Bosak"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Hołownia"]),0)), 
           y="Hołownia", x=mean(posfrmelt$value[posfrmelt$variable=="Hołownia"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Other"]),0)), 
           y="Other", x=mean(posfrmelt$value[posfrmelt$variable=="Other"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste("Probability of Duda winning in the first round:", Duda.50.out), y=6.75, x=median(posfrmelt$value[posfrmelt$variable=="Duda"]), size=3.75, family="Roboto Condensed", hjust=0.5) +
  scale_y_discrete(name=" ", limits=rev(pooledframe$party)) +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Polish presidential elections, round 1: latest estimates",
       subtitle="Data from Estymator, IBRIS, IBSP, IPSOS, Kantar, Maison & Partners, Pollster, PPG and Social Changes") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_latest_pres_r1.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot trends
datl <- melt(plotdata, measure.vars=c("Dudamean","RBmean","KKmean","RBmean","Bosakmean","Holowniamean","Othermean"))
levels(datl$variable) <- c("Duda", "Kidawa-Błońska", "Kosiniak-Kamysz", "Biedroń", "Bosak", "Hołownia", "Other")
datl$variable <- factor(datl$variable, levels = c("Duda", "Kidawa-Błońska", "Kosiniak-Kamysz", "Biedroń", "Bosak", "Hołownia", "Other"))

p <- ggplot(datl, aes(x=date, y=value, colour=factor(variable))) + geom_line() +
  geom_abline(intercept=50, slope=0, colour="gray60", linetype=3) +
  geom_ribbon(data=subset(datl, variable=="Duda"), aes(ymin=Dudalow, ymax=Dudahigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Kidawa-Błońska"), aes(ymin=RBlow, ymax=RBhigh), colour=NA, fill="orange", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Kosiniak-Kamysz"), aes(ymin=KKlow, ymax=KKhigh), colour=NA, fill="darkgreen", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Biedroń"), aes(ymin=RBlow, ymax=RBhigh), colour=NA, fill="red", alpha=0.3) +  
  geom_ribbon(data=subset(datl, variable=="Bosak"), aes(ymin=Bosaklow, ymax=Bosakhigh), colour=NA, fill="midnightblue", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Hołownia"), aes(ymin=Holownialow, ymax=Holowniahigh), colour=NA, fill="midnightblue", alpha=0.3) +  
  geom_ribbon(data=subset(datl, variable=="Other"), aes(ymin=Otherlow, ymax=Otherhigh), colour=NA, fill="grey50", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Duda), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=`Kidawa-Błońska`), col="orange", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=`Kosiniak-Kamysz`), col="darkgreen", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Biedroń), col="red", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Bosak), col="midnightblue", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Hołownia), col="darkorchid1", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Other), col="gray50", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=cols,
                      breaks=c("Duda","Kidawa-Błońska", "Kosiniak-Kamysz", "Biedroń", "Bosak", "Hołownia", "Other"),
                      labels=c("Duda","Kidawa-Błońska", "Kosiniak-Kamysz", "Biedroń", "Bosak", "Hołownia", "Other")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Polish presidential elections, round 1: trends", 
       subtitle="Data from Estymator, IBRIS, IBSP, IPSOS, Kantar, Maison & Partners, Pollster, PPG and Social Changes", 
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "polls_trends_pres_r1.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####ROUND 2: Duda vs. Kidawa-Błońska#####
#read in data
import <- drive_download(as_id("https://drive.google.com/file/d/1J_rNuLiETmzPfNpuRlQDtfwWpsCARRIj/view?usp=sharing"), overwrite=TRUE)
1
pollingdata <- read_excel('pooledpolls_pres_r2.xlsx')
pollingdata <- subset(pollingdata, select = -c(Source))
pollingdata$nDef <- round(((100-pollingdata$DK)/100)*pollingdata$n, digits=0)
pollingdata$Duda <- 100/((100-pollingdata$DK))*pollingdata$Duda
pollingdata$`Kidawa-Błońska` <- 100/((100-pollingdata$DK))*pollingdata$`Kidawa-Błońska`
pollingdata$nTot <- NULL
pollingdata$DK <- NULL
pollingdata$n <- NULL
pollingdata$pdate <- julian(as.Date(pollingdata$date, "%d/%m/%Y"), origin=as.Date(min(pollingdata$date)))
pollingdata$pdate <- as.Date(pollingdata$pdate, origin=as.Date(min(pollingdata$date)))
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
PARTYNAMES <- c("Duda","Kidawa-Błońska")
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

   alpha[1] ~ dunif(45, 55) # Duda
   alpha[2] ~ dunif(40, 50) # Kidawa-Błońska

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
                    adapt=20000, burnin=6000,sample=50000,thin=5,method="parallel")
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

Dudamean <- ppframe$Mean[grep('1]', ppframe$n)]*100
Dudalow <- ppframe$Lower95[grep('1]', ppframe$n)]*100
Dudahigh <- ppframe$Upper95[grep('1]', ppframe$n)]*100
Dudaest <- data.frame(Dudamean, Dudalow, Dudahigh)

RBmean <- ppframe$Mean[grep('2]', ppframe$n)]*100
RBlow <- ppframe$Lower95[grep('2]', ppframe$n)]*100
RBhigh <- ppframe$Upper95[grep('2]', ppframe$n)]*100
RBest <- data.frame(RBmean, RBlow, RBhigh)

plotdata <- cbind(Dudaest, RBest)
plotdata$date <- as.Date(c(1:length(Dudamean)), origin=as.Date(tail(pollingdata$pdate, n=1)-length(Dudamean)))

#Latest figures
# prepare data frame
party <- c("Duda", "Kidawa-Błońska")
alpha <- rep(1, length(party))
percent <- round(c(mean(tail(Dudaest$Dudamean, n=7)),
                   mean(tail(RBest$RBmean, n=7))))
votes <- round((percent*1000)/100, digits=2)

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
Duda.50.diff <- pos[,"Duda"] - 0.50
Duda.50.out <- sum(Duda.50.diff > 0) / length(Duda.50.diff)
Duda.50.out <- round(Duda.50.out, 2)

RB.50.diff <- pos[,"Kidawa-Błońska"] - 0.50
RB.50.out <- sum(RB.50.diff > 0) / length(RB.50.diff)
RB.50.out <- round(RB.50.out, 2)

# calculate latest figures
posfr <- data.frame()
for (i in 1:length(pos)){
  posfr <- pos[1:10000,]
}
posfrmelt <- melt(as.data.frame(posfr))

#plot house effects
p <- ggplot() + 
  geom_abline(intercept=0, slope=0, colour="gray10", linetype=3) +
  geom_pointrange(data=houseframe, mapping=aes(x=party, y=Mean, ymin=Lower95, ymax=Upper95, color=house, shape=method), 
                  position = position_dodge(width=0.5)) +
  guides(color=guide_legend(override.aes=list(shape=15, size=1, linetype=0)))+
  labs(color="Pollster", shape="Mode", x="", y="Deviation from mean party vote share", 
       title="House and mode effects", 
       caption = "@BDStanley; benstanley.org") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_houseeffects_pres_r2.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot most recent support
# colours for plots
cols <- c("Duda"="blue4", "Kidawa-Błońska"="orange")

p <- ggplot(posfrmelt, aes(y=variable, x = value, fill=variable)) +
  geom_vline(aes(xintercept=0.50), colour="gray60", linetype="dotted") +
  stat_slabh(normalize="xy") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Duda"]),0)), 
           y="Duda", x=mean(posfrmelt$value[posfrmelt$variable=="Duda"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Kidawa-Błońska"]),0)), 
           y="Kidawa-Błońska", x=mean(posfrmelt$value[posfrmelt$variable=="Kidawa-Błońska"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste("Probability of victory:", Duda.50.out), 
           y=1.85, x=median(posfrmelt$value[posfrmelt$variable=="Duda"]), size=3.75, family="Roboto Condensed", hjust=0.5) +
  annotate(geom = "text", label=paste("Probability of victory:", RB.50.out), 
           y=0.85, x=median(posfrmelt$value[posfrmelt$variable=="Kidawa-Błońska"]), size=3.75, family="Roboto Condensed", hjust=0.5) +
  scale_y_discrete(name=" ", limits=rev(pooledframe$party)) +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Polish presidential elections, round 2, Duda vs. Kidawa-Błońska: latest estimates",
       subtitle="Data from IBRIS, IBSP, IPSOS, Kantar, Maison & Partners, Pollster, PPG and Social Changes") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_latest_pres_r2_kidawa.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot trends
datl <- melt(plotdata, measure.vars=c("Dudamean","RBmean"))
levels(datl$variable) <- c("Duda", "Kidawa-Błońska")
datl$variable <- factor(datl$variable, levels = c("Duda", "Kidawa-Błońska"))

p <- ggplot(datl, aes(x=date, y=value, colour=factor(variable))) + geom_line() +
  geom_abline(intercept=50, slope=0, colour="gray60", linetype=3) +
  geom_ribbon(data=subset(datl, variable=="Duda"), aes(ymin=Dudalow, ymax=Dudahigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Kidawa-Błońska"), aes(ymin=RBlow, ymax=RBhigh), colour=NA, fill="orange", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Duda), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=`Kidawa-Błońska`), col="orange", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=cols,
                      breaks=c("Duda","Kidawa-Błońska"),
                      labels=c("Duda","Kidawa-Błońska")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Polish presidential elections, round 2,  Duda vs. Kidawa-Błońska: trends", 
       subtitle="Data from IBRIS, IBSP, IPSOS, Kantar, Maison & Partners, Pollster, PPG and Social Changes", 
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "polls_trends_pres_r2_kidawa.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####ROUND 2: Duda vs. Biedroń#####
#read in data
import <- drive_download(as_id("https://drive.google.com/file/d/1tUwPTyxE8rCO6KJlqho1XFwsCicDS2Ie/view?usp=sharing"), overwrite=TRUE)
1
pollingdata <- read_excel('pooledpolls_pres_r2_biedron.xlsx')
pollingdata <- subset(pollingdata, select = -c(Source))
pollingdata$nDef <- round(((100-pollingdata$DK)/100)*pollingdata$n, digits=0)
pollingdata$Duda <- 100/((100-pollingdata$DK))*pollingdata$Duda
pollingdata$`Biedroń` <- 100/((100-pollingdata$DK))*pollingdata$`Biedroń`
pollingdata$nTot <- NULL
pollingdata$DK <- NULL
pollingdata$n <- NULL
pollingdata$pdate <- julian(as.Date(pollingdata$date, "%d/%m/%Y"), origin=as.Date(min(pollingdata$date)))
pollingdata$pdate <- as.Date(pollingdata$pdate, origin=as.Date(min(pollingdata$date)))
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
PARTYNAMES <- c("Duda","Biedroń")
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

   alpha[1] ~ dunif(45, 55) # Duda
   alpha[2] ~ dunif(40, 50) # Biedroń

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
                    adapt=20000, burnin=6000,sample=50000,thin=5,method="parallel")
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

Dudamean <- ppframe$Mean[grep('1]', ppframe$n)]*100
Dudalow <- ppframe$Lower95[grep('1]', ppframe$n)]*100
Dudahigh <- ppframe$Upper95[grep('1]', ppframe$n)]*100
Dudaest <- data.frame(Dudamean, Dudalow, Dudahigh)

RBmean <- ppframe$Mean[grep('2]', ppframe$n)]*100
RBlow <- ppframe$Lower95[grep('2]', ppframe$n)]*100
RBhigh <- ppframe$Upper95[grep('2]', ppframe$n)]*100
RBest <- data.frame(RBmean, RBlow, RBhigh)

plotdata <- cbind(Dudaest, RBest)
plotdata$date <- as.Date(c(1:length(Dudamean)), origin=as.Date(tail(pollingdata$pdate, n=1)-length(Dudamean)))

#Latest figures
# prepare data frame
party <- c("Duda", "Biedroń")
alpha <- rep(1, length(party))
percent <- round(c(mean(tail(Dudaest$Dudamean, n=7)),
                   mean(tail(RBest$RBmean, n=7))))
votes <- round((percent*1000)/100, digits=2)

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
Duda.50.diff <- pos[,"Duda"] - 0.50
Duda.50.out <- sum(Duda.50.diff > 0) / length(Duda.50.diff)
Duda.50.out <- round(Duda.50.out, 2)

RB.50.diff <- pos[,"Biedroń"] - 0.50
RB.50.out <- sum(RB.50.diff > 0) / length(RB.50.diff)
RB.50.out <- round(RB.50.out, 2)

# calculate latest figures
posfr <- data.frame()
for (i in 1:length(pos)){
  posfr <- pos[1:10000,]
}
posfrmelt <- melt(as.data.frame(posfr))

#plot house effects
p <- ggplot() + 
  geom_abline(intercept=0, slope=0, colour="gray10", linetype=3) +
  geom_pointrange(data=houseframe, mapping=aes(x=party, y=Mean, ymin=Lower95, ymax=Upper95, color=house, shape=method), 
                  position = position_dodge(width=0.5)) +
  guides(color=guide_legend(override.aes=list(shape=15, size=1, linetype=0)))+
  labs(color="Pollster", shape="Mode", x="", y="Deviation from mean party vote share", 
       title="House and mode effects", 
       caption = "@BDStanley; benstanley.org") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_houseeffects_pres_r2_biedron.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot most recent support
# colours for plots
cols <- c("Duda"="blue4", "Biedroń"="red")

p <- ggplot(posfrmelt, aes(y=variable, x = value, fill=variable)) +
  geom_vline(aes(xintercept=0.50), colour="gray60", linetype="dotted") +
  stat_slabh(normalize="xy") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Duda"]),0)), 
           y="Duda", x=mean(posfrmelt$value[posfrmelt$variable=="Duda"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Biedroń"]),0)), 
           y="Biedroń", x=mean(posfrmelt$value[posfrmelt$variable=="Biedroń"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste("Probability of victory:", Duda.50.out), 
           y=1.85, x=median(posfrmelt$value[posfrmelt$variable=="Duda"]), size=3.75, family="Roboto Condensed", hjust=0.5) +
  annotate(geom = "text", label=paste("Probability of victory:", RB.50.out), 
           y=0.85, x=median(posfrmelt$value[posfrmelt$variable=="Biedroń"]), size=3.75, family="Roboto Condensed", hjust=0.5) +
  scale_y_discrete(name=" ", limits=rev(pooledframe$party)) +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Polish presidential elections, round 2, Duda vs. Biedroń: latest estimates",
       subtitle="Data from IBRIS, IBSP, IPSOS, Kantar, PPG and Social Changes") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_latest_pres_r2_biedron.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot trends
datl <- melt(plotdata, measure.vars=c("Dudamean","RBmean"))
levels(datl$variable) <- c("Duda", "Biedroń")
datl$variable <- factor(datl$variable, levels = c("Duda", "Biedroń"))

p <- ggplot(datl, aes(x=date, y=value, colour=factor(variable))) + geom_line() +
  geom_abline(intercept=50, slope=0, colour="gray60", linetype=3) +
  geom_ribbon(data=subset(datl, variable=="Duda"), aes(ymin=Dudalow, ymax=Dudahigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Biedroń"), aes(ymin=RBlow, ymax=RBhigh), colour=NA, fill="red", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Duda), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=`Biedroń`), col="red", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=cols,
                      breaks=c("Duda","Biedroń"),
                      labels=c("Duda","Biedroń")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Polish presidential elections, round 2, Duda vs. Biedroń: trends", 
       subtitle="Data from IBRIS, IBSP, IPSOS, Kantar, PPG and Social Changes", 
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "polls_trends_pres_r2_biedron.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

#####ROUND 2: Duda vs. Kosiniak-Kamysz#####
#read in data
import <- drive_download(as_id("https://drive.google.com/file/d/16N-QiK-WPrFQEgXfQSmjEeeHJ78k8-zw/view?usp=sharing"), overwrite=TRUE)
1
pollingdata <- read_excel('pooledpolls_pres_r2_kkamysz.xlsx')
pollingdata <- subset(pollingdata, select = -c(Source))
pollingdata$nDef <- round(((100-pollingdata$DK)/100)*pollingdata$n, digits=0)
pollingdata$Duda <- 100/((100-pollingdata$DK))*pollingdata$Duda
pollingdata$`Kosiniak-Kamysz` <- 100/((100-pollingdata$DK))*pollingdata$`Kosiniak-Kamysz`
pollingdata$nTot <- NULL
pollingdata$DK <- NULL
pollingdata$n <- NULL
pollingdata$pdate <- julian(as.Date(pollingdata$date, "%d/%m/%Y"), origin=as.Date(min(pollingdata$date)))
pollingdata$pdate <- as.Date(pollingdata$pdate, origin=as.Date(min(pollingdata$date)))
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
PARTYNAMES <- c("Duda","Kosiniak-Kamysz")
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

   alpha[1] ~ dunif(45, 55) # Duda
   alpha[2] ~ dunif(40, 50) # Kosiniak-Kamysz

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
                    adapt=20000, burnin=6000,sample=50000,thin=5,method="parallel")
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

Dudamean <- ppframe$Mean[grep('1]', ppframe$n)]*100
Dudalow <- ppframe$Lower95[grep('1]', ppframe$n)]*100
Dudahigh <- ppframe$Upper95[grep('1]', ppframe$n)]*100
Dudaest <- data.frame(Dudamean, Dudalow, Dudahigh)

KKmean <- ppframe$Mean[grep('2]', ppframe$n)]*100
KKlow <- ppframe$Lower95[grep('2]', ppframe$n)]*100
KKhigh <- ppframe$Upper95[grep('2]', ppframe$n)]*100
KKest <- data.frame(KKmean, KKlow, KKhigh)

plotdata <- cbind(Dudaest, KKest)
plotdata$date <- as.Date(c(1:length(Dudamean)), origin=as.Date(tail(pollingdata$pdate, n=1)-length(Dudamean)))

#Latest figures
# prepare data frame
party <- c("Duda", "Kosiniak-Kamysz")
alpha <- rep(1, length(party))
percent <- round(c(mean(tail(Dudaest$Dudamean, n=7)),
                   mean(tail(KKest$KKmean, n=7))))
votes <- round((percent*1000)/100, digits=2)

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
Duda.50.diff <- pos[,"Duda"] - 0.50
Duda.50.out <- sum(Duda.50.diff > 0) / length(Duda.50.diff)
Duda.50.out <- round(Duda.50.out, 2)

KK.50.diff <- pos[,"Kosiniak-Kamysz"] - 0.50
KK.50.out <- sum(KK.50.diff > 0) / length(KK.50.diff)
KK.50.out <- round(KK.50.out, 2)

# calculate latest figures
posfr <- data.frame()
for (i in 1:length(pos)){
  posfr <- pos[1:10000,]
}
posfrmelt <- melt(as.data.frame(posfr))

#plot house effects
p <- ggplot() + 
  geom_abline(intercept=0, slope=0, colour="gray10", linetype=3) +
  geom_pointrange(data=houseframe, mapping=aes(x=party, y=Mean, ymin=Lower95, ymax=Upper95, color=house, shape=method), 
                  position = position_dodge(width=0.5)) +
  guides(color=guide_legend(override.aes=list(shape=15, size=1, linetype=0)))+
  labs(color="Pollster", shape="Mode", x="", y="Deviation from mean party vote share", 
       title="House and mode effects", 
       caption = "@BDStanley; benstanley.org") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_houseeffects_pres_r2_kkamysz.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot most recent support
# colours for plots
cols <- c("Duda"="blue4", "Kosiniak-Kamysz"="darkgreen")

p <- ggplot(posfrmelt, aes(y=variable, x = value, fill=variable)) +
  geom_vline(aes(xintercept=0.50), colour="gray60", linetype="dotted") +
  stat_slabh(normalize="xy") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Duda"]),0)), 
           y="Duda", x=mean(posfrmelt$value[posfrmelt$variable=="Duda"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Kosiniak-Kamysz"]),0)), 
           y="Kosiniak-Kamysz", x=mean(posfrmelt$value[posfrmelt$variable=="Kosiniak-Kamysz"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste("Probability of victory:", Duda.50.out), 
           y=1.85, x=median(posfrmelt$value[posfrmelt$variable=="Duda"]), size=3.75, family="Roboto Condensed", hjust=0.5) +
  annotate(geom = "text", label=paste("Probability of victory:", KK.50.out), 
           y=0.85, x=median(posfrmelt$value[posfrmelt$variable=="Kosiniak-Kamysz"]), size=3.75, family="Roboto Condensed", hjust=0.5) +
  scale_y_discrete(name=" ", limits=rev(pooledframe$party)) +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Polish presidential elections, round 2, Duda vs. Kosiniak-Kamysz: latest estimates",
       subtitle="Data from IBRIS, IBSP, IPSOS, Kantar, Maison & Partners, PPG and Social Changes") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_latest_pres_r2_kkamysz.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot trends
datl <- melt(plotdata, measure.vars=c("Dudamean","KKmean"))
levels(datl$variable) <- c("Duda", "Kosiniak-Kamysz")
datl$variable <- factor(datl$variable, levels = c("Duda", "Kosiniak-Kamysz"))

p <- ggplot(datl, aes(x=date, y=value, colour=factor(variable))) + geom_line() +
  geom_abline(intercept=50, slope=0, colour="gray60", linetype=3) +
  geom_ribbon(data=subset(datl, variable=="Duda"), aes(ymin=Dudalow, ymax=Dudahigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Kosiniak-Kamysz"), aes(ymin=KKlow, ymax=KKhigh), colour=NA, fill="darkgreen", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Duda), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=`Kosiniak-Kamysz`), col="darkgreen", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=cols,
                      breaks=c("Duda","Kosiniak-Kamysz"),
                      labels=c("Duda","Kosiniak-Kamysz")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Polish presidential elections, round 2, Duda vs. Kosiniak-Kamysz: trends", 
       subtitle="Data from IBRIS, IBSP, IPSOS, Kantar, Maison & Partners, PPG and Social Changes", 
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "polls_trends_pres_r2_kkamysz.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####ROUND 2: Duda vs. Hołownia#####
#read in data
import <- drive_download(as_id("https://drive.google.com/file/d/1YTtbmMPheT0RfUgS3u5rfoSiydU5Yoor/view?usp=sharing"), overwrite=TRUE)
1
pollingdata <- read_excel('pooledpolls_pres_r2_holownia.xlsx')
pollingdata <- subset(pollingdata, select = -c(Source))
pollingdata$nDef <- round(((100-pollingdata$DK)/100)*pollingdata$n, digits=0)
pollingdata$Duda <- 100/((100-pollingdata$DK))*pollingdata$Duda
pollingdata$`Hołownia` <- 100/((100-pollingdata$DK))*pollingdata$`Hołownia`
pollingdata$nTot <- NULL
pollingdata$DK <- NULL
pollingdata$n <- NULL
pollingdata$pdate <- julian(as.Date(pollingdata$date, "%d/%m/%Y"), origin=as.Date(min(pollingdata$date)))
pollingdata$pdate <- as.Date(pollingdata$pdate, origin=as.Date(min(pollingdata$date)))
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
PARTYNAMES <- c("Duda","Hołownia")
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

   alpha[1] ~ dunif(45, 55) # Duda
   alpha[2] ~ dunif(40, 50) # Hołownia

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
                    adapt=20000, burnin=6000,sample=50000,thin=5,method="parallel")
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

Dudamean <- ppframe$Mean[grep('1]', ppframe$n)]*100
Dudalow <- ppframe$Lower95[grep('1]', ppframe$n)]*100
Dudahigh <- ppframe$Upper95[grep('1]', ppframe$n)]*100
Dudaest <- data.frame(Dudamean, Dudalow, Dudahigh)

SHmean <- ppframe$Mean[grep('2]', ppframe$n)]*100
SHlow <- ppframe$Lower95[grep('2]', ppframe$n)]*100
SHhigh <- ppframe$Upper95[grep('2]', ppframe$n)]*100
SHest <- data.frame(SHmean, SHlow, SHhigh)

plotdata <- cbind(Dudaest, SHest)
plotdata$date <- as.Date(c(1:length(Dudamean)), origin=as.Date(tail(pollingdata$pdate, n=1)-length(Dudamean)))

#Latest figures
# prepare data frame
party <- c("Duda", "Hołownia")
alpha <- rep(1, length(party))
percent <- round(c(mean(tail(Dudaest$Dudamean, n=7)),
                   mean(tail(SHest$SHmean, n=7))))
votes <- round((percent*1000)/100, digits=2)

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
Duda.50.diff <- pos[,"Duda"] - 0.50
Duda.50.out <- sum(Duda.50.diff > 0) / length(Duda.50.diff)
Duda.50.out <- round(Duda.50.out, 2)

SH.50.diff <- pos[,"Hołownia"] - 0.50
SH.50.out <- sum(SH.50.diff > 0) / length(SH.50.diff)
SH.50.out <- round(SH.50.out, 2)

# calculate latest figures
posfr <- data.frame()
for (i in 1:length(pos)){
  posfr <- pos[1:10000,]
}
posfrmelt <- melt(as.data.frame(posfr))

#plot house effects
p <- ggplot() + 
  geom_abline(intercept=0, slope=0, colour="gray10", linetype=3) +
  geom_pointrange(data=houseframe, mapping=aes(x=party, y=Mean, ymin=Lower95, ymax=Upper95, color=house, shape=method), 
                  position = position_dodge(width=0.5)) +
  guides(color=guide_legend(override.aes=list(shape=15, size=1, linetype=0)))+
  labs(color="Pollster", shape="Mode", x="", y="Deviation from mean party vote share", 
       title="House and mode effects", 
       caption = "@BDStanley; benstanley.org") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_houseeffects_pres_r2_holownia.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot most recent support
# colours for plots
cols <- c("Duda"="blue4", "Hołownia"="darkorchid1")

p <- ggplot(posfrmelt, aes(y=variable, x = value, fill=variable)) +
  geom_vline(aes(xintercept=0.50), colour="gray60", linetype="dotted") +
  stat_slabh(normalize="xy") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Duda"]),0)), 
           y="Duda", x=mean(posfrmelt$value[posfrmelt$variable=="Duda"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Hołownia"]),0)), 
           y="Hołownia", x=mean(posfrmelt$value[posfrmelt$variable=="Hołownia"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste("Probability of victory:", Duda.50.out), 
           y=1.85, x=median(posfrmelt$value[posfrmelt$variable=="Duda"]), size=3.75, family="Roboto Condensed", hjust=0.5) +
  annotate(geom = "text", label=paste("Probability of victory:", SH.50.out), 
           y=0.85, x=median(posfrmelt$value[posfrmelt$variable=="Hołownia"]), size=3.75, family="Roboto Condensed", hjust=0.5) +
  scale_y_discrete(name=" ", limits=rev(pooledframe$party)) +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Polish presidential elections, round 2, Duda vs. Hołownia: latest estimates",
       subtitle="Data from IBRIS, IBSP, IPSOS, Kantar, Maison & Partners, PPG and Social Changes") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "polls_latest_pres_r2_holownia.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

# plot trends
datl <- melt(plotdata, measure.vars=c("Dudamean","SHmean"))
levels(datl$variable) <- c("Duda", "Hołownia")
datl$variable <- factor(datl$variable, levels = c("Duda", "Hołownia"))

p <- ggplot(datl, aes(x=date, y=value, colour=factor(variable))) + geom_line() +
  geom_abline(intercept=50, slope=0, colour="gray60", linetype=3) +
  geom_ribbon(data=subset(datl, variable=="Duda"), aes(ymin=Dudalow, ymax=Dudahigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="Hołownia"), aes(ymin=SHlow, ymax=SHhigh), colour=NA, fill="darkorchid1", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=Duda), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=`Hołownia`), col="darkorchid1", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m.%y"))+
  scale_colour_manual(name="", values=cols,
                      breaks=c("Duda","Hołownia"),
                      labels=c("Duda","Hołownia")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="", y="% of vote", title="Polish presidential elections, round 2, Duda vs. Hołownia: trends", 
       subtitle="Data from IBRIS, IBSP, IPSOS, Kantar, Maison & Partners, PPG and Social Changes", 
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "polls_trends_pres_r2_holownia.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)
