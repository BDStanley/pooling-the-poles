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
library("tidybayes")

options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

import <- drive_download(as_id("https://drive.google.com/file/d/1jgkmVxkddCV-ERanROEZeZA0FM_xvnTj/view?usp=sharing"), overwrite=TRUE)
1
polls <- read_excel('pooledpolls_pres_r1_new.xlsx')

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + difftime(endDate, startDate)),
         Duda = 100/((100-DK))*Duda,
         Duda_se = Duda * (100 - Duda) / sampleSize,
         Trzaskowski = 100/((100-DK))*Trzaskowski,
         Trzaskowski_se = Trzaskowski * (100 - Trzaskowski) / sampleSize,
        `Kosiniak-Kamysz` = 100/((100-DK))*`Kosiniak-Kamysz`,
        `Kosiniak-Kamysz_se` = `Kosiniak-Kamysz` * (100 - `Kosiniak-Kamysz`) / sampleSize,
         Hołownia = 100/((100-DK))*Hołownia,
         Hołownia_se = Hołownia * (100 - Hołownia) / sampleSize,
         Bosak = 100/((100-DK))*Bosak,
         Bosak_se = Bosak * (100 - Bosak) / sampleSize,
         Biedroń = 100/((100-DK))*Biedroń,
         Biedroń_se = Biedroń * (100 - Biedroń) / sampleSize,
         Other = 100/((100-DK))*Other,
         Other_se = Other * (100 - Other) / sampleSize,
         time = as.integer(difftime(midDate, min(midDate)-1, units = "days")) + 1L,
         pollster = as.integer(factor(org)))

START_DATE <- min(polls$midDate)-1
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
          xi_final ~ normal(xi[T - 1], tau);
          // daily polls
          y ~ normal(mu, s);
        }",
        "polls.stan")

model <- "polls.stan"


#####Duda#####
Duda_data <- within(list(), {
  y <- polls$Duda
  s <- polls$Duda_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- head(polls$Duda, 1)
  xi_final <- tail(polls$Duda, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})
  

Duda_fit <- stan(model, data = Duda_data, iter=4000,
                     chains = 4, control = list(adapt_delta=0.99))

#####Trzaskowski#####
Trzaskowski_data <- within(list(), {
  y <- polls$Trzaskowski
  s <- polls$Trzaskowski_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- head(polls$Trzaskowski, 1)
  xi_final <- tail(polls$Trzaskowski, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Trzaskowski_fit <- stan(model, data = Trzaskowski_data, pars="xi",
                        iter = 4000, chains = 4, control = list(adapt_delta=0.99))

#####Kosiniak-Kamysz#####
`Kosiniak-Kamysz_data` <- within(list(), {
  y <- polls$`Kosiniak-Kamysz`
  s <- polls$`Kosiniak-Kamysz_se`
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- head(polls$`Kosiniak-Kamysz`, 1)
  xi_final <- tail(polls$`Kosiniak-Kamysz`, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

`Kosiniak-Kamysz_fit` <- stan(model, data = `Kosiniak-Kamysz_data`, pars="xi",
                              iter = 4000, chains = 4, control = list(adapt_delta=0.99))

#####Biedroń#####
Biedroń_data <- within(list(), {
  y <- polls$Biedroń
  s <- polls$Biedroń_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- head(polls$Biedroń, 1)
  xi_final <- tail(polls$Biedroń, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Biedroń_fit <- stan(model, data = Biedroń_data, pars="xi",
                    iter = 4000, chains = 4, control = list(adapt_delta=0.99))

#####Hołownia#####
Hołownia_data <- within(list(), {
  y <- polls$Hołownia
  s <- polls$Hołownia_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- head(polls$Hołownia, 1)
  xi_final <- tail(polls$Hołownia, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Hołownia_fit <- stan(model, data = Hołownia_data, pars="xi",
                     iter = 4000, chains = 4, control = list(adapt_delta=0.99))

#####Bosak#####
Bosak_data <- within(list(), {
  y <- polls$Bosak
  s <- polls$Bosak_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- head(polls$Bosak, 1)
  xi_final <- tail(polls$Bosak, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Bosak_fit <- stan(model, data = Bosak_data, pars="xi",
                  iter = 4000, chains = 4, control = list(adapt_delta=0.99))

# #####Other#####
# Other_data <- within(list(), {
#   y <- polls$Other+0.01
#   s <- polls$Other_se+0.00000001
#   time <- polls$time
#   house <- polls$pollster
#   H <- max(polls$pollster)
#   N <- length(y)
#   T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
#   xi_init <- 1
#   xi_final <- 1
#   delta_loc <- 0
#   tau_scale <- sd(y)
#   zeta_scale <- 5
# })
# 
# Other_fit <- stan(model, data = Other_data, pars="xi",
#                   iter = 10000, chains = 4, control = list(adapt_delta=0.99))


cols <- c("Duda"="blue4", "Trzaskowski"="orange", "Kosiniak-Kamysz"="darkgreen", "Bosak" = "midnightblue", "Biedroń" = "red", "Hołownia" = "darkorchid1", "Other"="gray50")
names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
names$house <- fct_recode(names$house, "Maison & Partners" = "Maison", "Kantar" = "Kantar") %>%
  fct_collapse(., Kantar=c("Kantar"))
names <- paste0(get_labels(names$house), collapse=", ")

#####Trend plot#####

Trzaskowski_draws <- tidybayes::spread_draws(Trzaskowski_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Trzaskowski")

Duda_draws <- tidybayes::spread_draws(Duda_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Duda")

Hołownia_draws <- tidybayes::spread_draws(Hołownia_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Hołownia")

`Kosiniak-Kamysz_draws` <- tidybayes::spread_draws(`Kosiniak-Kamysz_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Kosiniak-Kamysz")

Biedroń_draws <- tidybayes::spread_draws(Biedroń_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Biedroń")

Bosak_draws <- tidybayes::spread_draws(Bosak_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Bosak")

plot_trends <- rbind(Duda_draws, Trzaskowski_draws, Hołownia_draws, `Kosiniak-Kamysz_draws`, Biedroń_draws, Bosak_draws)

plot_trends$candidate <- fct_reorder(plot_trends$candidate, plot_trends$xi, .fun=median, .desc=TRUE)

plot_points <- polls %>%
  pivot_longer(c(Duda, Trzaskowski, Biedroń, `Kosiniak-Kamysz`, Bosak, Hołownia), names_to="candidate", values_to="percent") 
  
plot_points$candidate <- fct_reorder(plot_points$candidate, plot_points$percent, .fun=median, .desc=TRUE)

plot_trends_facet_r1 <- ggplot(plot_trends) +
  stat_lineribbon(aes(x = time, y = xi, color=candidate, fill=candidate), .width=c(0.5, 0.66, 0.95), alpha=1/4, show.legend = FALSE) +
  geom_point(data=plot_points, aes(x = midDate, y = percent, color=candidate), alpha = 1, size = 2, show.legend = FALSE) +
  scale_color_manual(values=cols, guide=FALSE) +
  scale_fill_manual(values=cols, guide=FALSE) +
  facet_wrap( ~ candidate, scales="free") + 
  labs(y = "% of vote", x="", title = "Polish presidential elections, round 1: trends", 
       subtitle=str_c("Data from ", names), color="", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave(plot_trends_facet_r1, file = "plot_trends_facet_r1.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

plot_trends_r1 <- ggplot(plot_trends) +
  stat_lineribbon(aes(x = time, y = xi, color=candidate, fill=candidate), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  geom_point(data=plot_points, aes(x = midDate, y = percent, color=candidate), alpha = 1, size = 2, show.legend = FALSE) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "% of vote", x="", title = "Polish presidential elections, round 1: trends", 
       subtitle=str_c("Data from ", names), color="", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave(plot_trends_r1, file = "plot_trends_r1.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####Latest plot#####
Trzaskowski_draws <- tidybayes::spread_draws(Trzaskowski_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Trzaskowski") %>%
  filter(., time==END_DATE) %>%
  mutate(over_50 = xi - 50,
         over_50 = sum(over_50 > 0) / length(over_50))

Duda_draws <- tidybayes::spread_draws(Duda_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
        candidate = "Duda") %>%
  filter(., time==END_DATE) %>%
  mutate(over_50 = xi - 50,
         over_50 = sum(over_50 > 0) / length(over_50))

`Kosiniak-Kamysz_draws` <- tidybayes::spread_draws(`Kosiniak-Kamysz_fit`, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Kosiniak-Kamysz") %>%
  filter(., time==END_DATE) %>%
  mutate(over_50 = xi - 50,
         over_50 = sum(over_50 > 0) / length(over_50))

Biedroń_draws <- tidybayes::spread_draws(Biedroń_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Biedroń") %>%
  filter(., time==END_DATE) %>%
  mutate(over_50 = xi - 50,
         over_50 = sum(over_50 > 0) / length(over_50))

Hołownia_draws <- tidybayes::spread_draws(Hołownia_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Hołownia") %>%
  filter(., time==END_DATE) %>%
  mutate(over_50 = xi - 50,
         over_50 = sum(over_50 > 0) / length(over_50))

Bosak_draws <- tidybayes::spread_draws(Bosak_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Bosak") %>%
  filter(., time==END_DATE) %>%
  mutate(over_50 = xi - 50,
         over_50 = sum(over_50 > 0) / length(over_50))

Duda.Trzaskowski.diff <- Duda_draws$xi - Trzaskowski_draws$xi
Duda.Trzaskowski.diff <- sum(Duda.Trzaskowski.diff > 0) / length(Duda.Trzaskowski.diff)
Duda.Trzaskowski.diff <- round(Duda.Trzaskowski.diff, 2)

plot_latest <- rbind(Duda_draws, Trzaskowski_draws, Hołownia_draws, `Kosiniak-Kamysz_draws`, Biedroń_draws, Bosak_draws)

plot_latest$candidate <- fct_reorder(plot_latest$candidate, plot_latest$xi, .fun=median, .desc=TRUE)

plot_latest_r1 <- ggplot(plot_latest) +
  geom_vline(aes(xintercept=50), colour="gray60", linetype="dotted") +
  stat_slabh(aes(y=reorder(candidate, desc(candidate)), x=xi, fill=candidate), normalize="xy") +
  scale_y_discrete(name="") +
  scale_fill_manual(name="", values=cols, guide=FALSE) +
  annotate(geom = "text", label=paste("Probability of first-round win:", 
                                      round(median(plot_latest$over_50[plot_latest$candidate=="Duda"]),2)), 
           y="Duda", x=median(plot_latest$xi[plot_latest$candidate=="Duda"]), 
           size=3.75, family="Roboto Condensed Light", vjust=1.4) +
  annotate(geom = "text", label=paste("Probability Duda leads Trzaskowski:", 
                                      round(median(Duda.Trzaskowski.diff),2)), 
           y="Duda", x=median(plot_latest$xi[plot_latest$candidate=="Duda"]), 
           size=3.75, family="Roboto Condensed Light", vjust=3) +
  annotate(geom = "text", label=paste("Probability of first-round win:", 
                                      round(median(plot_latest$over_50[plot_latest$candidate=="Trzaskowski"]),2)), 
           y="Trzaskowski", x=median(plot_latest$xi[plot_latest$candidate=="Trzaskowski"]), 
           size=3.75, family="Roboto Condensed Light", vjust=1.4) +
  annotate(geom = "text", label=paste(round(median(plot_latest$xi[plot_latest$candidate=="Duda"]),0)), 
           y="Duda", x=median(plot_latest$xi[plot_latest$candidate=="Duda"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(median(plot_latest$xi[plot_latest$candidate=="Trzaskowski"]),0)), 
           y="Trzaskowski", x=median(plot_latest$xi[plot_latest$candidate=="Trzaskowski"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(median(plot_latest$xi[plot_latest$candidate=="Hołownia"]),0)), 
           y="Hołownia", x=median(plot_latest$xi[plot_latest$candidate=="Hołownia"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(median(plot_latest$xi[plot_latest$candidate=="Bosak"]),0)), 
           y="Bosak", x=median(plot_latest$xi[plot_latest$candidate=="Bosak"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(median(plot_latest$xi[plot_latest$candidate=="Kosiniak-Kamysz"]),0)), 
           y="Kosiniak-Kamysz", x=median(plot_latest$xi[plot_latest$candidate=="Kosiniak-Kamysz"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(median(plot_latest$xi[plot_latest$candidate=="Biedroń"]),0)), 
           y="Biedroń", x=median(plot_latest$xi[plot_latest$candidate=="Biedroń"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  labs(x = "% of vote", y="", title = "Polish presidential elections, round 1: latest estimates", 
       subtitle=str_c("Data from ", names), color="", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot_latest_r1, file = "plot_latest_r1.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####Second round#####
import <- drive_download(as_id("https://drive.google.com/file/d/1MYtcJEZ4ougvrpCqFj5TkLtVT9UfOm7M/view?usp=sharing"), overwrite=TRUE)
1
polls <- read_excel('pooledpolls_pres_r2_new.xlsx')

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + difftime(endDate, startDate)),
         Duda = 100/((100-DK))*Duda,
         Duda_se = Duda * (100 - Duda) / sampleSize,
         Trzaskowski = 100/((100-DK))*Trzaskowski,
         Trzaskowski_se = Trzaskowski * (100 - Trzaskowski) / sampleSize,
         time = as.integer(difftime(midDate, min(midDate)-1, units = "days")) + 1L,
         pollster = as.integer(factor(org)))

START_DATE <- min(polls$midDate)-1
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
          xi_final ~ normal(xi[T - 1], tau);
          // daily polls
          y ~ normal(mu, s);
        }",
      "polls.stan")

model <- "polls.stan"

#####Duda#####
Duda_data <- within(list(), {
  y <- polls$Duda
  s <- polls$Duda_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) +1
  xi_init <- head(polls$Duda, 1)
  xi_final <- tail(polls$Duda, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Duda_fit <- stan(model, data = Duda_data, iter=10000,
                 chains = 4, control = list(adapt_delta=0.99))

#####Trzaskowski#####
Trzaskowski_data <- within(list(), {
  y <- polls$Trzaskowski
  s <- polls$Trzaskowski_se
  time <- polls$time
  house <- polls$pollster
  H <- max(polls$pollster)
  N <- length(y)
  T <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  xi_init <- head(polls$Trzaskowski, 1)
  xi_final <- tail(polls$Trzaskowski, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

Trzaskowski_fit <- stan(model, data = Trzaskowski_data, pars="xi",
                        iter = 10000, chains = 4, control = list(adapt_delta=0.99))

names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
names$house <- fct_recode(names$house, "Maison & Partners" = "Maison", "Kantar" = "Kantar") %>%
  fct_collapse(., Kantar=c("Kantar"))
names <- paste0(get_labels(names$house), collapse=", ")

#####Trend plot#####
Trzaskowski_draws <- tidybayes::spread_draws(Trzaskowski_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Trzaskowski")

Duda_draws <- tidybayes::spread_draws(Duda_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Duda")

plot_trends <- rbind(Duda_draws, Trzaskowski_draws)

plot_trends$candidate <- fct_reorder(plot_trends$candidate, plot_trends$xi, .fun=median, .desc=TRUE)

plot_points <- polls %>%
  pivot_longer(c(Duda, Trzaskowski), names_to="candidate", values_to="percent") 

plot_points$candidate <- fct_reorder(plot_points$candidate, plot_points$percent, .fun=median, .desc=TRUE)

plot_trends_r2 <- ggplot(plot_trends) +
  stat_lineribbon(aes(x = time, y = xi, color=candidate, fill=candidate), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  geom_point(data=plot_points, aes(x = midDate, y = percent, color=candidate), alpha = 1, size = 2, show.legend = FALSE) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "% of vote", x="", title = "Polish presidential elections, round 2: trends", 
       subtitle=str_c("Data from ", names), color="", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave(plot_trends_r2, file = "plot_trends_r2.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

#####Latest plot#####
Trzaskowski_draws <- tidybayes::spread_draws(Trzaskowski_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Trzaskowski") %>%
  filter(., time==END_DATE) %>%
  mutate(over_50 = xi - 50,
         over_50 = sum(over_50 > 0) / length(over_50))

Duda_draws <- tidybayes::spread_draws(Duda_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Duda") %>%
  filter(., time==END_DATE) %>%
  mutate(over_50 = xi - 50,
         over_50 = sum(over_50 > 0) / length(over_50))

plot_latest <- rbind(Duda_draws, Trzaskowski_draws)

plot_latest$candidate <- fct_reorder(plot_latest$candidate, plot_latest$xi, .fun=median, .desc=TRUE)

plot_latest_r2 <- ggplot(plot_latest) +
  geom_vline(aes(xintercept=50), colour="gray60", linetype="dotted") +
  stat_slabh(aes(y=reorder(candidate, desc(candidate)), x=xi, fill=candidate), normalize="xy") +
  scale_y_discrete(name="") +
  scale_fill_manual(name="", values=cols, guide=FALSE) +
  annotate(geom = "text", label=paste("Probability of Duda winning:", 
                                      round(mean(plot_latest$over_50[plot_latest$candidate=="Duda"]),2)), 
           y="Duda", x=median(plot_latest$xi[plot_latest$candidate=="Duda"]), 
           size=3.75, family="Roboto Condensed Light", vjust=1.4) +
  annotate(geom = "text", label=paste("Probability of Trzaskowski winning:", 
                                      round(mean(plot_latest$over_50[plot_latest$candidate=="Trzaskowski"]),2)), 
           y="Trzaskowski", x=median(plot_latest$xi[plot_latest$candidate=="Trzaskowski"]), 
           size=3.75, family="Roboto Condensed Light", vjust=1.4) +
  annotate(geom = "text", label=paste(round(median(plot_latest$xi[plot_latest$candidate=="Duda"]),0)), 
           y="Duda", x=median(plot_latest$xi[plot_latest$candidate=="Duda"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(median(plot_latest$xi[plot_latest$candidate=="Trzaskowski"]),0)), 
           y="Trzaskowski", x=median(plot_latest$xi[plot_latest$candidate=="Trzaskowski"]), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  labs(x = "% of vote", y="", title = "Polish presidential elections, round 2: latest estimates", 
       subtitle=str_c("Data from ", names), color="", caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(plot_latest_r2, file = "plot_latest_r2.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)