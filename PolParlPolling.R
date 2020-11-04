#Set up workspace
rm(list=ls())
library("rstan")
library("tidyverse")
library("lubridate")
library("stringr")
library("googledrive")
library("rio")
library("readxl")
library("hrbrthemes")
library("sjlabelled")
library("seatdist")
library("rgdal")
library("maptools") 
library("rgeos") 
library("gpclib")
library("tidybayes")
library("xtable")

options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.3") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

theme_changes <- theme(axis.title.y = element_text(family="Roboto Condensed Light"), axis.title.x = element_text(family="Roboto Condensed Light"),
                       axis.text.x = element_text(size=10, family="Roboto Condensed Light"), axis.text.y = element_text(size=10, family="Roboto Condensed Light"),
                       strip.text.x = element_text(size = 10, family="Roboto Condensed Light"), legend.text = element_text(size=9, family="Roboto Condensed Light"), 
                       legend.title = element_text(size=10, family="Roboto Condensed Light"), plot.title = element_text(size=14, family="Roboto Condensed Bold"),
                       plot.subtitle = element_text(size=12, family="Roboto Condensed Light"))

theme_changes_map <- theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                       axis.text.x = element_blank(), axis.text.y = element_blank(),
                       strip.text.x = element_text(size = 10, family="Roboto Condensed Light"), legend.text = element_text(size=9, family="Roboto Condensed Light"), 
                       legend.title = element_text(size=10, family="Roboto Condensed Light"), plot.title = element_text(size=14, family="Roboto Condensed Bold"),
                       plot.subtitle = element_text(size=12, family="Roboto Condensed Light"), aspect.ratio=1, legend.position="none")

#####Read in, adjust and subset data#####
import <- drive_download(as_id("https://drive.google.com/file/d/1ZiaHdyGqkeWaQwpADjBloRSPnfAAhXrC/view?usp=sharing"), overwrite=TRUE)
1
polls <- read_excel('pooledpolls_parl_P50.xlsx')

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + difftime(endDate, startDate)),
         midDate_int=as.integer(midDate)) %>%
  filter(midDate_int > (max(midDate_int)-150)) %>%
  mutate(PiS = 100/((100-DK))*PiS,
         PiS_se = PiS * (100 - PiS) / sampleSize,
         KO = 100/((100-DK))*KO,
         KO_se = KO * (100 - KO) / sampleSize,
         Lewica = 100/((100-DK))*Lewica,
         Lewica_se = Lewica * (100 - Lewica) / sampleSize,
         `PSL-Kukiz` = 100/((100-DK))*`PSL-Kukiz`,
         `PSL-Kukiz_se` = `PSL-Kukiz` * (100 - `PSL-Kukiz`) / sampleSize,
         Konfederacja = 100/((100-DK))*Konfederacja,
         Konfederacja_se = Konfederacja * (100 - Konfederacja) / sampleSize,
         `Polska 2050` = 100/((100-DK))*`Polska 2050`,
         `Polska 2050_se` = `Polska 2050` * (100 - `Polska 2050`) / sampleSize,
         Other = 100/((100-DK))*Other,
         Other_se = Other * (100 - Other) / sampleSize,
         time = as.integer(difftime(midDate, min(midDate)-1, units = "days")) + 1L,
         pollster = as.integer(factor(org)))

START_DATE <- min(polls$midDate)-7
END_DATE <- max(polls$midDate)

cols <- c("PiS"="blue4", "KO"="orange", "PSL-Kukiz"="darkgreen", "Konfederacja" = "midnightblue", "Lewica" = "red", "MN" = "yellow", "Other"="gray50", "Polska 2050"="darkgoldenrod")
names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
housenames <- fct_recode(names$house, "Kantar" = "Kantar") %>%
  fct_collapse(., Kantar=c("Kantar"))
names <- paste0(get_labels(housenames), collapse=", ")


#####Stan model#####
write("data {
          int N;
          int T;
          vector[N] y;
          vector[N] s;
          int time[N];
          int H;
          int house[N];
          // initial and final values
          real xi_init;
          real xi_final;
          real delta_loc;
          real zeta_scale;
          real tau_scale;
        }
        parameters {
          vector[T - 1] omega;
          real tau;
          vector[H] delta_raw;
          real zeta;
        }
        transformed parameters {
          vector[H] delta;
          vector[T - 1] xi;
          vector[N] mu;
          // this is necessary. If not centered the model is unidentified
          delta = (delta_raw - mean(delta_raw)) / sd(delta_raw) * zeta;
          xi[1] = xi_init;
          for (i in 2:(T - 1)) {
            xi[i] = xi[i - 1] + tau * omega[i - 1];
          }
          for (i in 1:N) {
            mu[i] = xi[time[i]] + delta[house[i]];
          }
        }
        model {
          // house effects
          delta_raw ~ normal(0., 1.);
          zeta ~ normal(0., zeta_scale);
          // latent state innovations
          omega ~ normal(0., 1.);
          // scale of innovations
          tau ~ cauchy(0, tau_scale);
          // final known effect
          xi_final ~ normal(xi[T - 1], 10);
          // daily polls
          y ~ normal(mu, s);
        }",
      "polls.stan")

model <- "polls.stan"

#xi_final ~ normal(xi[T - 1], tau);


#####PiS#####
PiS_data <- within(list(), {
  y <- polls$PiS
  s <- polls$PiS_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- median(head(polls$PiS, 7))
  xi_final <- median(tail(polls$PiS, 7))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

PiS_fit <- stan(model, data = PiS_data, chains = 6, control = list(adapt_delta=0.999999), iter=20000)

#####KO fit#####
KO_data <- within(list(), {
  y <- polls$KO
  s <- polls$KO_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
  xi_init <- median(head(polls$KO, 7))
  xi_final <- median(tail(polls$KO, 7))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

KO_fit <- stan(model, data = KO_data, chains = 6, control = list(adapt_delta=0.999999), iter=20000)

#####PSL-Kukiz fit#####
`PSL-Kukiz_data` <- within(list(), {
  y <- polls$`PSL-Kukiz`
  s <- polls$`PSL-Kukiz_se`
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
  xi_init <- median(head(polls$`PSL-Kukiz`, 7))
  xi_final <- median(tail(polls$`PSL-Kukiz`, 7))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

`PSL-Kukiz_fit` <- stan(model, data = `PSL-Kukiz_data`, chains = 6, control = list(adapt_delta=0.999999), iter=20000)

#####Lewica fit#####
Lewica_data <- within(list(), {
  y <- polls$Lewica
  s <- polls$Lewica_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
  xi_init <- median(head(polls$Lewica, 7))
  xi_final <- median(tail(polls$Lewica, 7))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Lewica_fit <- stan(model, data = Lewica_data, chains = 4, control = list(adapt_delta=0.999999), iter=20000)

#####Konfederacja fit#####
Konfederacja_data <- within(list(), {
  y <- polls$Konfederacja
  s <- polls$Konfederacja_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
  xi_init <- median(head(polls$Konfederacja, 7))
  xi_final <- median(tail(polls$Konfederacja, 7))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Konfederacja_fit <- stan(model, data = Konfederacja_data, chains = 6, control = list(adapt_delta=0.999999, max_treedepth=15), iter=20000, init=1)

#####Polska 2050 fit#####
`Polska 2050_data` <- within(list(), {
  y <- polls$`Polska 2050`
  s <- polls$`Polska 2050_se`
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- median(head(polls$`Polska 2050`, 7))
  xi_final <- median(tail(polls$`Polska 2050`, 7))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

`Polska 2050_fit` <- stan(model, data = `Polska 2050_data`, chains = 6, control = list(adapt_delta=0.999999, max_treedepth=15), iter=20000, init=1)

# #####Other fit#####
# Other_data <- within(list(), {
#   y <- polls$Other+0.01
#   s <- polls$Other_se+0.001
#   time <- polls$time
#   house <- polls$pollster
#   H <- max(polls$pollster)
#   N <- length(y)
#   T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
#   xi_init <- head(polls$Other, 1)
#   xi_final <- tail(polls$Other, 1)
#   delta_loc <- 0
#   tau_scale <- sd(y)
#   zeta_scale <- 5
# })
# 
# Other_fit <- stan(model, data = Other_data, chains = 4, control = list(adapt_delta=0.99), iter=4000)


#####Trend plot#####
PiS_draws <- tidybayes::spread_draws(PiS_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "PiS")

KO_draws <- tidybayes::spread_draws(KO_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "KO")

`PSL-Kukiz_draws` <- tidybayes::spread_draws(`PSL-Kukiz_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "PSL-Kukiz")

Konfederacja_draws <- tidybayes::spread_draws(Konfederacja_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Konfederacja")

Lewica_draws <- tidybayes::spread_draws(Lewica_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Lewica")

`Polska 2050_draws` <- tidybayes::spread_draws(`Polska 2050_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Polska 2050")

plot_trends <- rbind(PiS_draws, KO_draws, `PSL-Kukiz_draws`, Lewica_draws, Konfederacja_draws, `Polska 2050_draws`)

plot_trends$candidate <- fct_reorder(plot_trends$candidate, plot_trends$xi, .fun=median, .desc=TRUE)

plot_points <- polls %>%
  pivot_longer(c(PiS, KO, Lewica, `PSL-Kukiz`, Konfederacja, `Polska 2050`), names_to="candidate", values_to="percent") 

plot_points$candidate <- fct_reorder(plot_points$candidate, plot_points$percent, .fun=median, .desc=TRUE)

plot_trends_parl <- ggplot(plot_trends) +
  stat_lineribbon(aes(x = time, y = xi, color=candidate, fill=candidate), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  geom_point(data=plot_points, aes(x = midDate, y = percent, color=candidate), alpha = 1/3, size = 2, show.legend = FALSE) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "% of vote", x="", title = "Trends", 
       subtitle=str_c("Data from ", names), color="", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() +
  theme_changes +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave(plot_trends_parl, file = "plot_trends_parl.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####Latest plot#####
PiS_draws <- tidybayes::spread_draws(PiS_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "PiS") %>%
  filter(., time==END_DATE)

KO_draws <- tidybayes::spread_draws(KO_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "KO") %>%
  filter(., time==END_DATE)

`Polska 2050_draws` <- tidybayes::spread_draws(`Polska 2050_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Polska 2050") %>%
  filter(., time==END_DATE)

Lewica_draws <- tidybayes::spread_draws(Lewica_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Lewica") %>%
  filter(., time==END_DATE) %>%
  mutate(over_5 = xi - 5,
         over_5 = sum(over_5 > 0) / length(over_5),
         over_8 = xi - 8,
         over_8 = sum(over_8 > 0) / length(over_8))

`PSL-Kukiz_draws` <- tidybayes::spread_draws(`PSL-Kukiz_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "PSL-Kukiz") %>%
  filter(., time==END_DATE) %>%
  mutate(over_5 = xi - 5,
         over_5 = sum(over_5 > 0) / length(over_5),
         over_8 = xi - 8,
         over_8 = sum(over_8 > 0) / length(over_8))

Konfederacja_draws <- tidybayes::spread_draws(Konfederacja_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Konfederacja") %>%
  filter(., time==END_DATE) %>%
  mutate(over_5 = xi - 5,
         over_5 = sum(over_5 > 0) / length(over_5),
         over_8 = xi - 8,
         over_8 = sum(over_8 > 0) / length(over_8))

PiS.KO.diff <- PiS_draws$xi - KO_draws$xi
PiS.KO.diff <- sum(PiS.KO.diff > 0) / length(PiS.KO.diff)
PiS.KO.diff <- round(PiS.KO.diff, 2)

KO.P50.diff <- KO_draws$xi - `Polska 2050_draws`$xi
KO.P50.diff <- sum(KO.P50.diff > 0) / length(KO.P50.diff)
KO.P50.diff <- round(KO.P50.diff, 2)

plot_latest <- rbind(PiS_draws, KO_draws, `PSL-Kukiz_draws`, Lewica_draws, Konfederacja_draws, `Polska 2050_draws`)

plot_latest$candidate <- fct_reorder(plot_latest$candidate, plot_latest$xi, .fun=median, .desc=TRUE)

plot_latest_parl <- ggplot(plot_latest, aes(y=candidate, x = xi, fill=candidate)) +
  geom_vline(aes(xintercept=5), colour="gray40", linetype="dotted") +
  geom_vline(aes(xintercept=8), colour="gray40", linetype="dashed") +
  stat_slabh(aes(y=reorder(candidate, dplyr::desc(candidate)), x=xi, fill=candidate), normalize="xy") +
  scale_y_discrete(name="", position="right") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="PiS"]),0)), 
           y="PiS", x=mean(plot_latest$xi[plot_latest$candidate=="PiS"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="KO"]),0)), 
           y="KO", x=mean(plot_latest$xi[plot_latest$candidate=="KO"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="Lewica"]),0)), 
           y="Lewica", x=mean(plot_latest$xi[plot_latest$candidate=="Lewica"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="Konfederacja"]),0)), 
           y="Konfederacja", x=mean(plot_latest$xi[plot_latest$candidate=="Konfederacja"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="PSL-Kukiz"]),0)), 
           y="PSL-Kukiz", x=mean(plot_latest$xi[plot_latest$candidate=="PSL-Kukiz"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="Polska 2050"]),0)), 
           y="Polska 2050", x=mean(plot_latest$xi[plot_latest$candidate=="Polska 2050"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste("Pr(PiS > KO)  = ", PiS.KO.diff), y="PiS", 
           x=quantile(plot_latest$xi[plot_latest$candidate=="PiS"], 0.005), size=3.5, adj=c(1), vjust=-3, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(KO > Polska 2050)  = ", KO.P50.diff), y="KO", 
           x=quantile(plot_latest$xi[plot_latest$candidate=="KO"], 0.005), size=3.5, adj=c(1), vjust=-3, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(Lewica > 5%)  = ", round(median(plot_latest$over_5[plot_latest$candidate=="Lewica"]),2)), y="Lewica", 
           x=quantile(plot_latest$xi[plot_latest$candidate=="Lewica"], 0.999), size=3.5, adj=c(0), vjust=-4, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(PSL-Kukiz > 5%)  = ", round(median(plot_latest$over_5[plot_latest$candidate=="PSL-Kukiz"]),2)), y="PSL-Kukiz", 
           x=quantile(plot_latest$xi[plot_latest$candidate=="PSL-Kukiz"], 0.999), size=3.5, adj=c(0), vjust=-4, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(Konfederacja > 5%)  = ", round(median(plot_latest$over_5[plot_latest$candidate=="Konfederacja"]),2)), y="Konfederacja", 
           x=quantile(plot_latest$xi[plot_latest$candidate=="Konfederacja"], 0.999), size=3.5, adj=c(0), vjust=-4, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(Lewica > 8%)  = ", round(median(plot_latest$over_8[plot_latest$candidate=="Lewica"]),2)), y="Lewica", 
           x=quantile(plot_latest$xi[plot_latest$candidate=="Lewica"], 0.999), size=3.5, adj=c(0), vjust=-2, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(PSL-Kukiz > 8%)  = ", round(median(plot_latest$over_8[plot_latest$candidate=="PSL-Kukiz"]),2)), y="PSL-Kukiz", 
           x=quantile(plot_latest$xi[plot_latest$candidate=="PSL-Kukiz"], 0.999), size=3.5, adj=c(0), vjust=-2, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(Konfederacja > 8%)  = ", round(median(plot_latest$over_8[plot_latest$candidate=="Konfederacja"]),2)), y="Konfederacja", 
           x=quantile(plot_latest$xi[plot_latest$candidate=="Konfederacja"], 0.999), size=3.5, adj=c(0), vjust=-2, family="Roboto Condensed Light") +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 5, 8, 10, 20, 30, 40, 50), labels=c("0", "5", "8", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Latest estimates",
       subtitle=str_c("Data from ", names)) +
  theme_minimal() +
  theme_ipsum_rc() +
  theme_changes +
ggsave(plot_latest_parl, file = "polls_latest_parl.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

#####Seats plot#####
weights <- read_excel('~/Google Drive/Resources/Polish materials/Poll data/2019_elec_percentages.xlsx')
median_PiS <- ifelse(median(PiS_draws$xi >=5), median(PiS_draws$xi), 0)
median_KO <- ifelse(median(KO_draws$xi >=5), median(KO_draws$xi), 0)
`median_PSL-Kukiz` <- ifelse(median(`PSL-Kukiz_draws`$xi >=5), median(`PSL-Kukiz_draws`$xi), 0)
median_Lewica <-ifelse(median(Lewica_draws$xi >=5), median(Lewica_draws$xi), 0)
median_Konfederacja <-  ifelse(median(Konfederacja_draws$xi >=5), median(Konfederacja_draws$xi), 0)
`median_Polska 2050` <- ifelse(median(`Polska 2050_draws`$xi >=5), median(`Polska 2050_draws`$xi), 0)

PiSpct <- round(weights$PiScoef*median_PiS, digits=2)
KOpct <- round(weights$KOcoef*median_KO, digits=2)
PSLpct <- round(weights$PSLcoef*`median_PSL-Kukiz`, digits=2)
Lewicapct <- round(weights$Lewicacoef*median_Lewica, digits=2)
Konfederacjapct <- round(weights$Konfcoef*median_Konfederacja, digits=2)
`Polska 2050pct` <- round(weights$KOcoef*`median_Polska 2050`, digits=2)
MNpct <- c(0.17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.90, 0, 0, 0, 0, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

KOest <- (weights$validvotes/100)*KOpct
PiSest <- (weights$validvotes/100)*PiSpct
PSLest <- (weights$validvotes/100)*PSLpct
Lewicaest <- (weights$validvotes/100)*Lewicapct
Konfederacjaest <- (weights$validvotes/100)*Konfederacjapct
`Polska 2050est` <- (weights$validvotes/100)*`Polska 2050pct`
MNest <- (weights$validvotes/100)*MNpct

poldHondt <- data.frame(KO=rep(1,42), Konfederacja=rep(1,42), Lewica=rep(1,42),  MN=rep(1,42), PiS=rep(1,42),
                        `Polska 2050`=rep(1,42), PSL=rep(1,42))

for( i in 1 : 42 ) {
  poldHondt[i,] <- c(giveseats(v = c(KOest[i], Konfederacjaest[i], Lewicaest[i], MNest[i], PiSest[i], 
                                     `Polska 2050est`[i], PSLest[i]), ns = weights$magnitude[i], method="dh", thresh=5))$seats
}

colnames(poldHondt) <- c("KO", "Konfederacja", "Lewica", "MN", "PiS", "Polska 2050", "PSL-Kukiz")

frame <- t(rbind(poldHondt[1,], colSums(poldHondt[2:42,])))
frame <- data.frame(rownames(frame), frame)
colnames(frame) <- c("Party", "Unweighted", "Weighted")
frame <- frame[with(frame, order(-Weighted)),]
frame$in2019[frame$Party=="KO"] <- 134
frame$in2019[frame$Party=="PiS"] <- 235
frame$in2019[frame$Party=="Lewica"] <- 49
frame$in2019[frame$Party=="PSL-Kukiz"] <- 30
frame$in2019[frame$Party=="MN"] <- 1
frame$in2019[frame$Party=="Konfederacja"] <- 11
frame$in2019[frame$Party=="Polska 2050"] <- 0
frame <- frame[frame$Weighted>0,]
frame$Party <- factor(frame$Party, levels=c("PiS", "KO", "PSL-Kukiz", "Lewica", "Konfederacja", "Polska 2050", "MN"))
frame$diffPres <- sprintf("%+d", (frame$Weighted - frame$in2019))
frame$diffPres <- sprintf("(%s)", frame$diffPres)
frame$diffPresUn <- sprintf("%+d", (frame$Unweighted - frame$in2019))
frame$diffPresUn <- sprintf("(%s)", frame$diffPresUn)
frame$Party <- reorder(frame$Party, -frame$Weighted)

plot_seats_parl <- ggplot(data=frame, mapping=aes(x=Party, y=Weighted, fill=Party)) +
  geom_bar(stat="identity", width=.75, show.legend = F) +
  geom_abline(intercept=231, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=276, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=307, slope=0, colour="gray10", linetype=3) +
  scale_y_continuous('Number of seats', limits=c(0,320), breaks=c(0, 50, 100, 150, 200, 231, 276, 307)) +
  scale_fill_manual(name="Party", values = cols)+
  geom_label(aes(x=2, y=231), label="Legislative majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed Light") +
  geom_label(aes(x=2, y=276), label="Overturn presidential veto", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed Light") +
  geom_label(aes(x=2, y=307), label="Constitutional majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed Light") +
  annotate("text", x=frame$Party, y=c(frame$Weighted+18), label=frame$Weighted, size=4, family="Roboto Condensed Light")+
  annotate("text", x=frame$Party, y=c(frame$Weighted+8), label=frame$diffPres, size=3, family="Roboto Condensed Light") +
  labs(x="", y="% of vote", title="Estimated share of seats",
       subtitle="Figures in brackets refer to change in seat share since October 2019 election",
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() +
  theme_changes
ggsave(plot_seats_parl, file = "plot_seats_parl.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####Seat maps#####
# seats table
seats <- cbind(poldHondt, weights)
row.names(seats) <- weights$name
keep <- c("KO","PiS","PSL-Kukiz","Lewica", "Konfederacja", "MN", "Polska 2050")
seats <- seats[keep]
seats <- seats[-1,]
seats$id <- 1:41
seats$PiSKO <-abs(seats$PiS-seats$KO)
seats$PiSmKO <- seats$PiS-seats$KO

#regional maps
const=readOGR("/Users/benstanley/Google Drive/Resources/Polish materials/Regional data/GRED_beta2_20170530_Poland/shapefile/GRED_Poland_2011_beta2.shp")
const@data$id = rownames(const@data)
const.points = fortify(const, region="id")
const.df = full_join(const.points, const@data, by="id")
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

p_p2050 <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(`Polska 2050`)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Polska 2050", limits=c(min=0, max=20), low = "white", high = "darkgoldenrod", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=`Polska 2050`, label=`Polska 2050`), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for Polska 2050", subtitle="Seat distribution reflects regional levels of support for Szymon Hołownia at 2020 presidential election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_p2050, file = "Polska_2050_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_lewica <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Lewica)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Lewica", limits=c(min=0, max=20), low = "white", high = "red", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Lewica, label=Lewica), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for Lewica", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_lewica, file = "Lewica_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_pis <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiS)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PiS", limits=c(min=0, max=20), low = "white", high = "blue4", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiS, label=PiS), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for PiS", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_pis, file = "PiS_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_ko <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(KO)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="KO", limits=c(min=0, max=20), low = "white", high = "orange", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=KO, label=KO), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for Koalicja Obywatelska", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_ko, file = "KO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_psl <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(`PSL-Kukiz`)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PSL-Kukiz", limits=c(min=0, max=20), low = "white", high = "darkgreen", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=`PSL-Kukiz`, label=`PSL-Kukiz`), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for PSL-Kukiz", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_psl, file = "PSL_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_konf <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Konfederacja)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Konfederacja", limits=c(min=0, max=20), low = "white", high = "midnightblue", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Konfederacja, label=Konfederacja), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for Konfederacja", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_konf, file = "Konf_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_pis_ko <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiSmKO)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient2(name="PiSKO", limits=c(min=-20, max=20), low = "orange", mid="white", high = "blue4", midpoint=0, guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiSKO, label=PiSKO), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level differences in share of seats for PiS and Koalicja Obywatelska", subtitle="Constituencies in shades of blue have more PiS MPs; constituencies in orange have more KO MPs", 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_pis_ko, file = "PiSKO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

seattable <- tibble(rownames(seats), seats[,2], seats[,1], seats[,4], seats[,3], seats[,5], seats[,6])
colnames(seattable) <- c("Constituency", "PiS", "KO", "Lewica", "PSL-Kukiz", "Konf.", "MN")
hlines <- c(-1, 0, nrow(seattable))
print(xtable(seattable, type = "latex", digits=0, align=c("l","l","c","c","c","c","c","c")), hline.after=hlines, booktabs=TRUE,
      include.rownames=FALSE, file='seat_table.tex')

PiS_house <- tidybayes::spread_draws(PiS_fit, delta[term]) %>%
  mutate(party="PiS")
KO_house <- tidybayes::spread_draws(KO_fit, delta[term]) %>%
  mutate(party="KO")
`PSL-Kukiz_house` <- tidybayes::spread_draws(`PSL-Kukiz_fit`, delta[term]) %>%
  mutate(party="PSL-Kukiz")
Lewica_house <- tidybayes::spread_draws(Lewica_fit, delta[term]) %>%
  mutate(party="Lewica")
Konfederacja_house <- tidybayes::spread_draws(Konfederacja_fit, delta[term]) %>%
  mutate(party="Konfederacja")
`Polska 2050_house` <- tidybayes::spread_draws(`Polska 2050_fit`, delta[term]) %>%
  mutate(party="Polska 2050")

houselevels <- get_labels(droplevels(polls$org))

houseeff <- bind_rows(PiS_house, KO_house, `PSL-Kukiz_house`, Lewica_house, Konfederacja_house, `Polska 2050_house`) %>%
  mutate(house = as_factor(term)) %>%
  set_labels(house, labels=houselevels) %>%
  group_by(house, party) %>%
  ggplot(aes(x = party, y = delta, color=house)) +
  stat_pointinterval(position = position_dodge(width = .7)) +
  scale_size_continuous(guide=FALSE) 

p_house <- bind_rows(PiS_house, KO_house, `PSL-Kukiz_house`, Lewica_house, Konfederacja_house, `Polska 2050_house`) %>%
  mutate(house = factor(term, levels=c(1:max(.$term)), labels=houselevels)) %>%
  separate(., house, c("house", "method"), sep="_") %>%
  group_by(house, method, party) %>%
  ggplot(aes(x = party, y = delta, color=house, shape=method)) +
  geom_abline(intercept=0, slope=0, colour="gray10", linetype=3) +
  stat_pointinterval(position = position_dodge(width = .7)) +
  guides(color = guide_legend(override.aes = list(size = 5, shape="|"), keywidth=0.3, keyheight=0.3, default.unit="inch")) +
  guides(shape = guide_legend(override.aes = list(size = 5, linetype=NULL), keywidth=0.3, keyheight=0.3, default.unit="inch")) +
  labs(color="Pollster", shape="Mode", x="", y="Deviation from mean party vote share (percent)", 
       title="House and mode effects for national election polls - all parties", 
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() +
  theme_changes
ggsave(p_house, file = "p_house.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)









#####Including Don't Knows#####
import <- drive_download(as_id("https://drive.google.com/file/d/1ZiaHdyGqkeWaQwpADjBloRSPnfAAhXrC/view?usp=sharing"), overwrite=TRUE)
1
polls <- read_excel('pooledpolls_parl_P50.xlsx')

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + difftime(endDate, startDate)),
         midDate_int=as.integer(midDate)) %>%
  filter(midDate_int > (max(midDate_int)-60)) %>%
  mutate(PiS_se = PiS * (100 - PiS) / sampleSize,
         KO_se = KO * (100 - KO) / sampleSize,
         Lewica_se = Lewica * (100 - Lewica) / sampleSize,
         `PSL-Kukiz_se` = `PSL-Kukiz` * (100 - `PSL-Kukiz`) / sampleSize,
         Konfederacja_se = Konfederacja * (100 - Konfederacja) / sampleSize,
         `Polska 2050_se` = `Polska 2050` * (100 - `Polska 2050`) / sampleSize,
         Other_se = Other * (100 - Other) / sampleSize,
         DK = DK+0.001,
         DK_se = DK * (100 - DK) / sampleSize,
         time = as.integer(difftime(midDate, min(midDate)-1, units = "days")) + 1L,
         pollster = as.integer(factor(org)))

START_DATE <- min(polls$midDate)-7
END_DATE <- max(polls$midDate)

write("data {
          int N;
          int T;
          vector[N] y;
          vector[N] s;
          int time[N];
          int H;
          int house[N];
          // initial and final values
          real xi_init;
          real xi_final;
          real delta_loc;
          real zeta_scale;
          real tau_scale;
        }
        parameters {
          vector[T - 1] omega;
          real tau;
          vector[H] delta_raw;
          real zeta;
        }
        transformed parameters {
          vector[H] delta;
          vector[T - 1] xi;
          vector[N] mu;
          // this is necessary. If not centered the model is unidentified
          delta = (delta_raw - mean(delta_raw)) / sd(delta_raw) * zeta;
          xi[1] = xi_init;
          for (i in 2:(T - 1)) {
            xi[i] = xi[i - 1] + tau * omega[i - 1];
          }
          for (i in 1:N) {
            mu[i] = xi[time[i]] + delta[house[i]];
          }
        }
        model {
          // house effects
          delta_raw ~ normal(0., 1.);
          zeta ~ normal(0., zeta_scale);
          // latent state innovations
          omega ~ normal(0., 1.);
          // scale of innovations
          tau ~ cauchy(0, tau_scale);
          // final known effect
          xi_final ~ normal(xi[T - 1], 0.5);
          // daily polls
          y ~ normal(mu, s);
        }",
      "polls.stan")

model <- "polls.stan"

#xi_final ~ normal(xi[T - 1], tau);

#####PiS#####
PiS_data <- within(list(), {
  y <- polls$PiS
  s <- polls$PiS_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- median(head(polls$PiS, 7))
  xi_final <- median(tail(polls$PiS, 7))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

PiS_fit <- stan(model, data = PiS_data, chains = 6, control = list(adapt_delta=0.999999), iter=20000)

#####KO#####
KO_data <- within(list(), {
  y <- polls$KO
  s <- polls$KO_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
  xi_init <- median(head(polls$KO, 1))
  xi_final <- median(tail(polls$KO, 1))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

KO_fit <- stan(model, data = KO_data, chains = 6, control = list(adapt_delta=0.999999), iter=20000)

#####`PSL-Kukiz`#####
`PSL-Kukiz_data` <- within(list(), {
  y <- polls$`PSL-Kukiz`
  s <- polls$`PSL-Kukiz_se`
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
  xi_init <- median(head(polls$`PSL-Kukiz`, 1))
  xi_final <- median(tail(polls$`PSL-Kukiz`, 1))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

`PSL-Kukiz_fit` <- stan(model, data = `PSL-Kukiz_data`, chains = 6, control = list(adapt_delta=0.999999), iter=20000)

#####Lewica#####
Lewica_data <- within(list(), {
  y <- polls$Lewica
  s <- polls$Lewica_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
  xi_init <- median(head(polls$Lewica, 1))
  xi_final <- median(tail(polls$Lewica, 1))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Lewica_fit <- stan(model, data = Lewica_data, chains = 4, control = list(adapt_delta=0.999999), iter=20000)

#####Konfederacja#####
Konfederacja_data <- within(list(), {
  y <- polls$Konfederacja
  s <- polls$Konfederacja_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
  xi_init <- median(head(polls$Konfederacja, 1))
  xi_final <- median(tail(polls$Konfederacja, 1))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Konfederacja_fit <- stan(model, data = Konfederacja_data, chains = 6, control = list(adapt_delta=0.999999), iter=20000)

#####`Polska 2050`#####
`Polska 2050_data` <- within(list(), {
  y <- polls$`Polska 2050`
  s <- polls$`Polska 2050_se`
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- median(head(polls$`Polska 2050`, 1))
  xi_final <- median(tail(polls$`Polska 2050`, 1))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

`Polska 2050_fit` <- stan(model, data = `Polska 2050_data`, chains = 6, control = list(adapt_delta=0.999999), iter=20000)

#####Don't knows#####
DK_data <- within(list(), {
  y <- polls$DK
  s <- polls$DK_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- median(head(polls$DK, 1))
  xi_final <- median(tail(polls$DK, 1))
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

DK_fit <- stan(model, data = DK_data, chains = 6, control = list(adapt_delta=0.99999999999999), iter=500000, init=1)

cols <- c("PiS"="blue4", "KO"="orange", "PSL-Kukiz"="darkgreen", "Konfederacja" = "midnightblue", "Lewica" = "red", "MN" = "yellow", "DK"="gray50", "Polska 2050"="darkgoldenrod")
names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
housenames <- fct_recode(names$house, "Kantar" = "Kantar") %>%
  fct_collapse(., Kantar=c("Kantar"))
names <- paste0(get_labels(housenames), collapse=", ")


#####Trend plot#####
PiS_draws <- tidybayes::spread_draws(PiS_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "PiS")

KO_draws <- tidybayes::spread_draws(KO_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "KO")

`PSL-Kukiz_draws` <- tidybayes::spread_draws(`PSL-Kukiz_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "PSL-Kukiz")

Konfederacja_draws <- tidybayes::spread_draws(Konfederacja_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Konfederacja")

Lewica_draws <- tidybayes::spread_draws(Lewica_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Lewica")

`Polska 2050_draws` <- tidybayes::spread_draws(`Polska 2050_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Polska 2050")

DK_draws <- tidybayes::spread_draws(DK_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "DK")

plot_trends <- rbind(PiS_draws, KO_draws, `PSL-Kukiz_draws`, Lewica_draws, Konfederacja_draws, `Polska 2050_draws`, DK_draws)

plot_trends$candidate <- fct_reorder(plot_trends$candidate, plot_trends$xi, .fun=median, .desc=TRUE)

plot_points <- polls %>%
  pivot_longer(c(PiS, KO, Lewica, `PSL-Kukiz`, Konfederacja, `Polska 2050`, DK), names_to="candidate", values_to="percent") 

plot_points$candidate <- fct_reorder(plot_points$candidate, plot_points$percent, .fun=median, .desc=TRUE)

plot_trends_parl_P50_DK <- plot_trends %>%
  filter(., candidate %in% c("PiS", "KO", "DK")) %>%
  ggplot() +
  stat_lineribbon(aes(x = time, y = xi, color=candidate, fill=candidate), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  geom_point(data=filter(plot_points, candidate %in% c("PiS", "KO", "DK")), aes(x = midDate, y = percent, color=candidate), alpha = 1, size = 2, show.legend = FALSE) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "% of vote", x="", title = "Polish parliamentary elections: trends (including undecided voters)", 
       subtitle=str_c("Data from ", names), color="", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave(plot_trends_parl_P50_DK, file = "plot_trends_parl_P50_DK.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####Latest plot#####
PiS_draws <- tidybayes::spread_draws(PiS_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "PiS") %>%
  filter(., time==END_DATE)

KO_draws <- tidybayes::spread_draws(KO_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "KO") %>%
  filter(., time==END_DATE)

`Polska 2050_draws` <- tidybayes::spread_draws(`Polska 2050_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Polska 2050") %>%
  filter(., time==END_DATE)

Lewica_draws <- tidybayes::spread_draws(Lewica_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Lewica") %>%
  filter(., time==END_DATE) %>%
  mutate(over_5 = xi - 5,
         over_5 = sum(over_5 > 0) / length(over_5),
         over_8 = xi - 8,
         over_8 = sum(over_8 > 0) / length(over_8))

`PSL-Kukiz_draws` <- tidybayes::spread_draws(`PSL-Kukiz_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "PSL-Kukiz") %>%
  filter(., time==END_DATE) %>%
  mutate(over_5 = xi - 5,
         over_5 = sum(over_5 > 0) / length(over_5),
         over_8 = xi - 8,
         over_8 = sum(over_8 > 0) / length(over_8))

Konfederacja_draws <- tidybayes::spread_draws(Konfederacja_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Konfederacja") %>%
  filter(., time==END_DATE) %>%
  mutate(over_5 = xi - 5,
         over_5 = sum(over_5 > 0) / length(over_5),
         over_8 = xi - 8,
         over_8 = sum(over_8 > 0) / length(over_8))

DK_draws <- tidybayes::spread_draws(DK_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "DK") %>%
  filter(., time==END_DATE) %>%
  mutate(over_5 = xi - 5,
         over_5 = sum(over_5 > 0) / length(over_5),
         over_8 = xi - 8,
         over_8 = sum(over_8 > 0) / length(over_8))

plot_latest <- rbind(PiS_draws, KO_draws, `PSL-Kukiz_draws`, Lewica_draws, Konfederacja_draws, `Polska 2050_draws`, DK_draws)

plot_latest$candidate <- fct_reorder(plot_latest$candidate, plot_latest$xi, .fun=median, .desc=TRUE)

plot_latest_parl_P50_DK <- ggplot(plot_latest, aes(y=candidate, x = xi, fill=candidate)) +
  stat_slabh(aes(y=reorder(candidate, dplyr::desc(candidate)), x=xi, fill=candidate), normalize="xy") +
  scale_y_discrete(name="", position="right") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="PiS"]),0)), 
           y="PiS", x=mean(plot_latest$xi[plot_latest$candidate=="PiS"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="KO"]),0)), 
           y="KO", x=mean(plot_latest$xi[plot_latest$candidate=="KO"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="Lewica"]),0)), 
           y="Lewica", x=mean(plot_latest$xi[plot_latest$candidate=="Lewica"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="Konfederacja"]),0)), 
           y="Konfederacja", x=mean(plot_latest$xi[plot_latest$candidate=="Konfederacja"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="PSL-Kukiz"]),0)), 
           y="PSL-Kukiz", x=mean(plot_latest$xi[plot_latest$candidate=="PSL-Kukiz"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="Polska 2050"]),0)), 
           y="Polska 2050", x=mean(plot_latest$xi[plot_latest$candidate=="Polska 2050"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(mean(plot_latest$xi[plot_latest$candidate=="DK"]),0)), 
           y="DK", x=mean(plot_latest$xi[plot_latest$candidate=="Polska 2050"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 10, 20, 30, 40), labels=c("0","10", "20", "30", "40")) +
  expand_limits(x = 0) +
  labs(caption="@BDStanley; benstanley.org", x="", title="Polish parliamentary elections: latest estimates (including undecided voters)",
       subtitle=str_c("Data from ", names)) +
  theme_minimal() +
  theme_ipsum_rc() 
ggsave(plot_latest_parl_P50_DK, file = "polls_latest_parl_P50_DK.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

#####Save image out#####
save.image("~/Desktop/PoolingthePoles.RData")
