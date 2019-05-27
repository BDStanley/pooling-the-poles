# PREPARE WORKSPACE
rm(list=ls())
setwd('/Users/benstanley/Desktop/Personal/Dropbox/Resources/R scripts/Pooling the Poles')
library(tidyverse); library(rjags); library(R2jags); library(R2WinBUGS); library(scales)
library(grid); library(foreign); library(memisc); library(MCMCpack); library(repmis); 
library(readxl); library(pander); library(coda); library(runjags); library(rgdal);
library(maptools); library(rgeos); library(gpclib); library(reshape2); library(plyr);
library(gridExtra); library(grid); library(cowplot); library(coalitions); library(xtable);
library(hrbrthemes); library(ggrepel); library(tidybayes)
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

## POOLED POLL MODEL
# read in, subset and adjust data
pollingdata <- read.csv('~/Desktop/Personal/Dropbox/Resources/Polish materials/Poll data/pooledpollsPE.csv')
pollingdata <- subset(pollingdata, select = -c(Source))
pollingdata$nDef <- round(((100-pollingdata$DK)/100)*pollingdata$n, digits=0)
pollingdata$KE <- 100/((100-pollingdata$DK))*pollingdata$KE
pollingdata$PiS <- 100/((100-pollingdata$DK))*pollingdata$PiS
pollingdata$KP <- 100/((100-pollingdata$DK))*pollingdata$KP
pollingdata$Kukiz15 <- 100/((100-pollingdata$DK))*pollingdata$Kukiz15
pollingdata$Razem <- 100/((100-pollingdata$DK))*pollingdata$Razem
pollingdata$Wiosna <- 100/((100-pollingdata$DK))*pollingdata$Wiosna
pollingdata$Other <- 100/((100-pollingdata$DK))*pollingdata$Other
pollingdata$nTot <- NULL
pollingdata$DK <- NULL
pollingdata$n <- NULL
pollingdata$pdate <- julian(as.Date(pollingdata$date, "%d/%m/%Y"), origin=as.Date("2019-02-17"))
pollingdata$pdate <- as.Date(pollingdata$pdate, origin=as.Date("2019-02-17"))
pollingdata <- pollingdata[which(pollingdata$pdate > 0),]
pollingdata <- pollingdata[!is.na(pollingdata$pdate),]
pollingdata <- pollingdata[order(pollingdata$pdate),]
pollingdata$day <- as.integer(pollingdata$pdate)-as.integer(pollingdata$pdate)[1] + 1

# create dataset for jags model
NUMPOLLS <- nrow(pollingdata)
PERIOD <- max(as.integer(pollingdata$day))
HOUSECOUNT <- length(levels(pollingdata$agency))
HOUSENAMES <- levels(pollingdata$agency)
PARTYNAMES <- c("KE","PiS","Wiosna","Kukiz15","KP","Razem","Other")
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

   alpha[1] ~ dunif(350, 400) # KE
   alpha[2] ~ dunif(350, 400) # PiS
   alpha[3] ~ dunif(50, 150) # Wiosna
   alpha[4] ~ dunif(50, 100) # Kukiz15
   alpha[5] ~ dunif(20, 80) # KP
   alpha[6] ~ dunif(10, 30) # Razem
   alpha[7] ~ dunif(10, 30) # Other

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
save(mysummary,file="ppsummary_EP")


## TREND DATA
ppframe <- data.frame(mysummary)
ppframe <- rownames_to_column(ppframe, var="n")
ppframe <- ppframe[-grep('tightness', ppframe$n),]
ppframe <- ppframe[-grep('houseEffect', ppframe$n),]

KEmean <- ppframe$Mean[grep('1]', ppframe$n)]*100
KElow <- ppframe$Lower95[grep('1]', ppframe$n)]*100
KEhigh <- ppframe$Upper95[grep('1]', ppframe$n)]*100
KEest <- data.frame(KEmean, KElow, KEhigh)

PiSmean <- ppframe$Mean[grep('2]', ppframe$n)]*100
PiSlow <- ppframe$Lower95[grep('2]', ppframe$n)]*100
PiShigh <- ppframe$Upper95[grep('2]', ppframe$n)]*100
PiSest <- data.frame(PiSmean, PiSlow, PiShigh)

Wiosnamean <- ppframe$Mean[grep('3]', ppframe$n)]*100
Wiosnalow <- ppframe$Lower95[grep('3]', ppframe$n)]*100
Wiosnahigh <- ppframe$Upper95[grep('3]', ppframe$n)]*100
Wiosnaest <- data.frame(Wiosnamean, Wiosnalow, Wiosnahigh)

Kukiz15mean <- ppframe$Mean[grep('4]', ppframe$n)]*100
Kukiz15low <- ppframe$Lower95[grep('4]', ppframe$n)]*100
Kukiz15high <- ppframe$Upper95[grep('4]', ppframe$n)]*100
Kukiz15est <- data.frame(Kukiz15mean, Kukiz15low, Kukiz15high)

KPmean <- ppframe$Mean[grep('5]', ppframe$n)]*100
KPlow <- ppframe$Lower95[grep('5]', ppframe$n)]*100
KPhigh <- ppframe$Upper95[grep('5]', ppframe$n)]*100
KPest <- data.frame(KPmean, KPlow, KPhigh)

Razemmean <- ppframe$Mean[grep('6]', ppframe$n)]*100
Razemlow <- ppframe$Lower95[grep('6]', ppframe$n)]*100
Razemhigh <- ppframe$Upper95[grep('6]', ppframe$n)]*100
Razemest <- data.frame(Razemmean, Razemlow, Razemhigh)

Othermean <- ppframe$Mean[grep('7]', ppframe$n)]*100
Otherlow <- ppframe$Lower95[grep('7]', ppframe$n)]*100
Otherhigh <- ppframe$Upper95[grep('7]', ppframe$n)]*100
Otherest <- data.frame(Othermean, Otherlow, Otherhigh)

plotdata <- cbind(KEest, PiSest, Wiosnaest, Kukiz15est, KPest, Razemest, Otherest)
plotdata$date <- as.Date(c(1:length(KEmean)), origin=as.Date(tail(pollingdata$pdate, n=1)-length(KEmean)))

# LATEST FIGURES
# prepare data frame
party <- c("KE", "PiS", "Wiosna", "Kukiz15", "KP", "Razem", "Other")
alpha <- c(1,1,1,1,1,1,1)
percent <- round(c(mean(tail(KEest$KEmean, n=7)), mean(tail(PiSest$PiSmean, n=7)), 
                   mean(tail(Wiosnaest$Wiosnamean, n=7)), mean(tail(Kukiz15est$Kukiz15mean, n=7)),
                   mean(tail(KPest$KPmean, n=7)), mean(tail(Razemest$Razemmean, n=7)),
                   mean(tail(Otherest$Othermean, n=7))))
votes <- round((percent*500)/100, digits=2)

# calculate means and HDIs
pooledframe <- data.frame(cbind(party, alpha, votes))
pos <- MCmultinomdirichlet(votes, alpha, mc=10000)
colnames(pos) <- pooledframe$party
means_pos <- round(apply(pos,2,mean)*100, digits=2)
HDIs <- round(HPDinterval(pos)*100, digits=2)
pooledframe <- cbind(pooledframe, means_pos, HDIs)
pooledframe <- arrange(pooledframe, desc(means_pos))

# calculate latest figures
posfr <- data.frame()
for (i in 1:length(pos)){
  posfr <- pos[1:10000,]
}
posfrmelt <- melt(as.data.frame(posfr))

# SEAT SHARES
# read in 2019 coefficient data
data1 <- read_excel('~/Desktop/Personal/Dropbox/Resources/Polish materials/Poll data/2019percentages.xlsx')

# enter most recent average level of support and reduce to zero if below electoral threshold
means_pos <- data.frame(means_pos)
for(i in 1:7) {
  if(means_pos[i,] > 4.99) {
    means_pos[i,] <- means_pos[i,] 
  } else { means_pos[i,] <- 0
  }
}

# define party percentage per district according to most recent polls
KEpct <- round(data1$KEcoef*means_pos[1,], digits=2)
PiSpct <- round(data1$PiScoef*means_pos[2,], digits=2)
Wiosnapct <- round(data1$Wiosnacoef*means_pos[3,], digits=2)
Kukiz15pct <- round(data1$Kukiz15coef*means_pos[4,], digits=2)
KPpct <- round(data1$KPcoef*means_pos[5,], digits=2)
Razempct <- round(data1$Razemcoef*means_pos[6,], digits=2)

# estimate number of votes per party per district based on 2015 turnout
KEest <- (data1$validvotes/100)*KEpct
PiSest <- (data1$validvotes/100)*PiSpct
Wiosnaest <- (data1$validvotes/100)*Wiosnapct
Kukiz15est <- (data1$validvotes/100)*Kukiz15pct
KPest <- (data1$validvotes/100)*KPpct
Razemest <- (data1$validvotes/100)*Razempct

# estimate seat shares
seats <- dHondt(c("KE", "PiS", "Wiosna", "Kukiz15", "KP", "Razem"), 
                c(KEest[1], PiSest[1], Wiosnaest[1], Kukiz15est[1], KPest[1], Razemest[1]), 51)
seats <- t(rbind(seats))

# distribute seats in constituencies
const <- data1$name[2:14]

votesKE <- KEest[2:14]
seatsKE <- seats[1]
distribKE <- hare_niemeyer(votesKE, const, seatsKE)

votesKP <- KPest[2:14]
seatsKP <- seats[2]
distribKP <- hare_niemeyer(votesKP, const, seatsKP)
if (seatsKP==0) {
  distribKP <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
}

votesKukiz15 <- Kukiz15est[2:14]
seatsKukiz15 <- seats[3]
distribKukiz15 <- hare_niemeyer(votesKukiz15, const, seatsKukiz15)
if (seatsKukiz15==0) {
  distribKukiz15 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
}

votesPiS <- PiSest[2:14]
seatsPiS <- seats[4]
distribPiS <- hare_niemeyer(votesPiS, const, seatsPiS)

votesRazem <- Razemest[2:14]
seatsRazem <- seats[5]
distribRazem <- hare_niemeyer(votesRazem, const, seatsRazem)
if (seatsRazem==0) {
  distribRazem <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
}

votesWiosna <- Wiosnaest[2:14]
seatsWiosna <- seats[6]
distribWiosna <- hare_niemeyer(votesWiosna, const, seatsWiosna)

#create table
seatsEP <- tibble(const, distribKE, distribPiS, distribWiosna, distribKukiz15, distribKP, distribRazem)
seatsEP <- add_row(seatsEP, const = "Mandaty w skali kraju", distribKE = sum(distribKE), distribPiS = sum(distribPiS), distribWiosna = sum(distribWiosna),
        distribKukiz15 = sum(distribKukiz15), distribKP = sum(distribKP), distribRazem = sum(distribRazem), .before=1)
seatsEP <- add_row(seatsEP, const = "Głosy (%)", distribKE = pooledframe$means_pos[pooledframe$party=="KE"], distribPiS = pooledframe$means_pos[pooledframe$party=="PiS"],
                   distribWiosna = pooledframe$means_pos[pooledframe$party=="Wiosna"], distribKukiz15 = pooledframe$means_pos[pooledframe$party=="Kukiz15"],
                   distribKP = pooledframe$means_pos[pooledframe$party=="KP"], distribRazem = pooledframe$means_pos[pooledframe$party=="Razem"], .before=1)
seatsEP = seatsEP %>% mutate(Suma = rowSums(seatsEP[2:7]))
colnames(seatsEP) <- c("", "KE", "PiS", "Wiosna", "Kukiz'15", "Konf.", "Razem", "Suma")
hlines <- c(-1, 0, 2, nrow(seatsEP))
print(xtable(seatsEP, type = "latex", digits=0, align=c("p{1cm}","p{5.5cm}","c","c","c","c","c","c","c")), hline.after=hlines, booktabs=TRUE,
      include.rownames=FALSE, file='~/Desktop/Personal/Dropbox/Resources/Polish materials/Plots/EP_seats.tex')

#plot trend
setwd('~/Desktop/Personal/Dropbox/Resources/Polish materials/Plots')
prcols <- c("KE"="orange", "PiS"="blue4", "Kukiz'15"="black", "Wiosna" = "maroon", "Konf." = "goldenrod1", "Razem"="magenta", "Other"="grey")
pcols <- c("KE"="orange", "PiS"="blue4", "Kukiz15"="black", "Wiosna" = "maroon", "KP" = "goldenrod1", "Razem"="magenta", "Other"="grey")

datl <- melt(plotdata, measure.vars=c("KEmean","PiSmean","Wiosnamean","Kukiz15mean","KPmean", "Razemmean", "Othermean"))
levels(datl$variable) <- c("KE", "PiS", "Wiosna", "Kukiz15", "KP", "Razem", "Other")
datl$variable <- factor(datl$variable, levels = c("KE", "PiS", "Wiosna", "Kukiz15", "KP", "Razem", "Other"))

pdatl <- melt(pollingdata, measure.vars=c("KE", "PiS", "Wiosna", "Kukiz15", "KP", "Razem", "Other"))
levels(pdatl$variable) <- c("KE", "PiS", "Wiosna", "Kukiz15", "KP", "Razem", "Other")
pdatl$variable <- factor(pdatl$variable, levels = c("KE", "PiS", "Wiosna", "Kukiz15", "KP", "Razem", "Other"))

datl_KE <- datl[!(datl$variable %in% c("Other")),]
p <- ggplot(datl_KE, aes(x=date, y=value, colour=factor(variable))) + geom_line() + 
  geom_abline(intercept=5, slope=0, colour="gray60", linetype=3) +
  geom_point(data=pdatl, aes(x=as.Date(pdatl$date, "%d/%m/%Y", y=variable)), size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m"))+
  scale_colour_manual(name="", values=pcols, breaks=c("PiS", "KE", "Wiosna", "Kukiz15", "KP", "Razem"), 
                      labels=c("PiS", "KE", "Wiosna", "Kukiz'15", "Konf.", "Razem")) + 
  labs(x="", y="% of vote", title="Pooled poll results for parties and coalitions (EP election, Poland)", 
       subtitle="Estimated using polls published by IPSOS, IBRIS, Estymator, Kantar, Pollster, IBSP, Ariadne, Social Changes and Dobra Opinia.",
       caption = "@BDStanley; benstanley.org")+
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "EP_trends.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

datl_KE_maj <- datl[!(datl$variable %in% c("Other", "Wiosna", "Kukiz15", "KP", "Razem")),]
p <- ggplot(datl_KE_maj, aes(x=date, y=value, colour=factor(variable))) + geom_line() + 
  geom_abline(intercept=5, slope=0, colour="gray60", linetype=3) +
  geom_ribbon(data=subset(datl, variable=="PiS"),aes(ymin=PiSlow, ymax=PiShigh), colour=NA, fill="blue4", alpha=0.3) +
  geom_ribbon(data=subset(datl, variable=="KE"),aes(ymin=KElow, ymax=KEhigh), colour=NA, fill="orange", alpha=0.3) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=PiS), col="blue4", size=1.5) +
  geom_point(data=pollingdata, aes(x=as.Date(pollingdata$date, "%d/%m/%Y"), y=KE), col="orange", size=1.5) +
  theme(panel.background = element_rect(colour="white"),  axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,3,1,1), "lines"), strip.text.x = element_text(size = 10))+
  background_grid(major = "xy", minor = "none") +
  scale_x_date(labels=date_format("%d.%m"))+
  scale_colour_manual(name="", values=pcols, breaks=c("PiS", "KE"), 
                      labels=c("PiS", "KE")) + 
  labs(x="", y="% of vote", title="Pooled poll results for PiS and KE (EP election, Poland)", 
       subtitle="Estimated using polls published by IPSOS, IBRIS, Estymator, Kantar, Pollster, IBSP, Ariadne, Social Changes and Dobra Opinia.",
       caption = "@BDStanley; benstanley.org")+
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "EP_trends_PiS_KE.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

levels(posfrmelt$variable)[levels(posfrmelt$variable)=="Kukiz15"] <- "Kukiz'15"
levels(posfrmelt$variable)[levels(posfrmelt$variable)=="KP"] <- "Konf."
levels(pooledframe$party)[levels(pooledframe$party)=="Kukiz15"] <- "Kukiz'15"
levels(pooledframe$party)[levels(pooledframe$party)=="KP"] <- "Konf."

# p <- ggplot(data=posfrmelt, aes(variable, value)) +
#   geom_hline(aes(yintercept=0.05), colour="gray60", linetype="dashed") +
#   geom_boxplot(aes(fill=variable, color=variable), outlier.shape=NA, show.legend = F, fatten=0) +
#   stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
#                fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
#   coord_flip() +
#   annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="KE"]),0)), 
#             x="KE", y=mean(posfrmelt$value[posfrmelt$variable=="KE"]), size=3.5, hjust = "center", vjust=-3.6, family="Roboto Condensed") +
#   annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PiS"]),0)), 
#            x="PiS", y=mean(posfrmelt$value[posfrmelt$variable=="PiS"]), size=3.5, hjust = "center", vjust=-3.6, family="Roboto Condensed") +
#   annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Wiosna"]),0)), 
#             x="Wiosna", y=mean(posfrmelt$value[posfrmelt$variable=="Wiosna"]), size=3.5, hjust = "center", vjust=-3.6, family="Roboto Condensed") +
#   annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Kukiz'15"]),0)), 
#             x="Kukiz'15", y=mean(posfrmelt$value[posfrmelt$variable=="Kukiz'15"]), size=3.5, hjust = "center", vjust=-3.6, family="Roboto Condensed") +
#   annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Konf."]),0)), 
#             x="Konf.", y=mean(posfrmelt$value[posfrmelt$variable=="Konf."]), size=3.5, hjust = "center", vjust=-3.6, family="Roboto Condensed") +
#   annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Razem"]),0)), 
#             x="Razem", y=mean(posfrmelt$value[posfrmelt$variable=="Razem"]), size=3.5, hjust = "center", vjust=-3.6, family="Roboto Condensed") +
#   annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Other"]),0)), 
#             x="Other", y=mean(posfrmelt$value[posfrmelt$variable=="Other"]), size=3.5, hjust = "center", vjust=-3.6, family="Roboto Condensed") +
#   scale_x_discrete(name=" ", limits=rev(pooledframe$party)) +
#   scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
#   theme_minimal() +
#   theme_ipsum_rc() +
#   scale_fill_manual(values=prcols) +
#   scale_color_manual(values=prcols) +
#   labs(caption="@BDStanley; benstanley.org", y="", title="Latest poll estimates (EP elections, Poland)",
#        subtitle="Estimated using polls published by IPSOS, IBRIS, Estymator, Kantar, Pollster, IBSP, Ariadne, Social Changes and Dobra Opinia.")
# ggsave(p, file = "EP_latest.png", 
#        width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

p <- ggplot(posfrmelt, aes(y=variable, x = value, fill=variable)) +
  geom_vline(aes(xintercept=0.05), colour="gray60", linetype="dashed") +
  geom_halfeyeh(color=NA, scale="width") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="PiS"]),0)), 
           y="PiS", x=mean(posfrmelt$value[posfrmelt$variable=="PiS"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="KE"]),0)), 
           y="KE", x=mean(posfrmelt$value[posfrmelt$variable=="KE"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Wiosna"]),0)), 
           y="Wiosna", x=mean(posfrmelt$value[posfrmelt$variable=="Wiosna"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Kukiz'15"]),0)), 
           y="Kukiz'15", x=mean(posfrmelt$value[posfrmelt$variable=="Kukiz'15"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Konf."]),0)), 
           y="Konf.", x=mean(posfrmelt$value[posfrmelt$variable=="Konf."]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Razem"]),0)), 
           y="Razem", x=mean(posfrmelt$value[posfrmelt$variable=="Razem"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*mean(posfrmelt$value[posfrmelt$variable=="Other"]),0)), 
           y="Other", x=mean(posfrmelt$value[posfrmelt$variable=="Other"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  scale_y_discrete(name=" ", limits=rev(pooledframe$party)) +
  scale_fill_manual(name=" ", values=prcols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Latest poll estimates (EP elections, Poland)",
       subtitle="Estimated using polls published by IPSOS, IBRIS, Estymator, Kantar, Pollster, IBSP, Ariadne, Social Changes and Dobra Opinia.") +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(p, file = "EP_latest.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)
