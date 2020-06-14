library("rstan")
library("tidyverse")
library("lubridate")
library("stringr")
library("googledrive")
library("rio")
library("readxl")

options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

import <- drive_download(as_id("https://drive.google.com/file/d/1MYtcJEZ4ougvrpCqFj5TkLtVT9UfOm7M/view?usp=sharing"), overwrite=TRUE)
1
PolPresPolling <- read_excel('pooledpolls_pres_r2_dudatrzask.xlsx')

import <- drive_download(as_id("https://drive.google.com/file/d/1jgkmVxkddCV-ERanROEZeZA0FM_xvnTj/view?usp=sharing"), overwrite=TRUE)
1
PolPresPolling <- read_excel('pooledpolls_pres_r1_new.xlsx')

START_DATE <- "2020-05-14"
PolPresPolling$startDate <- as.Date(PolPresPolling$startDate)
PolPresPolling$endDate <- as.Date(PolPresPolling$endDate)


PolPresPolling <-
  PolPresPolling %>%
  mutate(midDate = as.Date(startDate + difftime(endDate, startDate)),
         Duda = 100/((100-PolPresPolling$DK))*PolPresPolling$Duda,
         Duda_se = Duda * (100 - Duda) / sampleSize,
         Trzaskowski = 100/((100-PolPresPolling$DK))*PolPresPolling$Trzaskowski,
         Trzaskowski_se = Trzaskowski * (100 - Trzaskowski) / sampleSize,
         time = as.integer(difftime(midDate, START_DATE, units = "days")) + 1L,
         pollster = as.integer(factor(org)))


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
          xi_final ~ normal(0, 0.01);
          // daily polls
          y ~ normal(mu, s);
        }",
        "PolPresPolling.stan")

model <- "PolPresPolling.stan"


#####Duda#####
duda_data <- within(list(), {
  y <- PolPresPolling$Duda
  s <- PolPresPolling$Duda_se
  time <- PolPresPolling$time
  house <- PolPresPolling$pollster
  H <- max(PolPresPolling$pollster)
  N <- length(y)
  T <- as.integer(difftime("2020-06-28", "2020-05-15", units = "days")) + 1
  xi_init <- head(PolPresPolling$Duda, 1)
  xi_final <- tail(PolPresPolling$Duda, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})
  

duda_fit <- stan(model, data = duda_data, pars="xi",
                     chains = 4, control = list(adapt_delta=0.99))

duda_plot <- summary(duda_fit, par = "xi")$summary %>%
  as.data.frame() %>%
  rownames_to_column("parameter")
duda_plot$date <- seq.Date(as.Date(START_DATE), by="days", length.out=length(duda_plot$mean))

ggplot() +
  geom_ribbon(data = duda_plot, mapping = aes(x = date, ymin = mean - 2 * sd, ymax = mean + 2 * sd), alpha = 0.3) +
  geom_line(data = duda_plot, mapping = aes(x = date, y = mean)) +
  geom_point(data = PolPresPolling,
             mapping = aes(x = midDate, y = Duda, colour = org))
  

#####Trzaskowski#####
trzaskowski_data <- within(list(), {
  y <- PolPresPolling$Trzaskowski
  s <- PolPresPolling$Trzaskowski_se
  time <- PolPresPolling$time
  house <- PolPresPolling$pollster
  H <- max(PolPresPolling$pollster)
  N <- length(y)
  T <- as.integer(difftime("2020-06-28", "2020-05-15", units = "days")) + 1
  xi_init <- head(PolPresPolling$Trzaskowski, 1)
  xi_final <- tail(PolPresPolling$Trzaskowski, 1)
  delta_loc <- 0
  tau_scale <- sd(y)
  zeta_scale <- 5
})

trzaskowski_fit <- stan(model, data = trzaskowski_data, pars="xi",
                     chains = 4, control = list(adapt_delta=0.99))

trzaskowski_plot <- summary(trzaskowski_fit, par = "xi")$summary %>%
  as.data.frame() %>%
  rownames_to_column("parameter")
trzaskowski_plot$date <- seq.Date(as.Date(START_DATE), by="days", length.out=length(trzaskowski_plot$mean))

ggplot() +
  geom_ribbon(data = trzaskowski_plot, mapping = aes(x = date, ymin = mean - 2 * sd, ymax = mean + 2 * sd), alpha = 0.3) +
  geom_line(data = trzaskowski_plot, mapping = aes(x = date, y = mean)) +
  geom_point(data = PolPresPolling,
             mapping = aes(x = midDate, y = Trzaskowski, colour = org))




trzask <- tidybayes::spread_draws(trzaskowski_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Trzaskowski") %>%
  ggplot() +
  stat_lineribbon(aes(x = time, y = xi), .width=c(0.5, 0.66, 0.95), alpha=1/4)

duda <- tidybayes::spread_draws(duda_fit, xi[term]) %>%
  mutate(time = as.Date(term, origin=START_DATE),
         candidate = "Duda") %>%
  ggplot() +
  stat_lineribbon(aes(x = time, y = xi), .width=c(0.5, 0.66, 0.95), alpha=1/4)
