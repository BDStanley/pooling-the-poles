library("rstan")
library("tidyverse")
library("lubridate")
library("stringr")
library("googledrive")
library("rio")
library("readxl")
library("tidybayes")
library("hrbrthemes")

options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

import <- drive_download(as_id("https://drive.google.com/file/d/1MYtcJEZ4ougvrpCqFj5TkLtVT9UfOm7M/view?usp=sharing"), overwrite=TRUE)
1
polls <- read_excel('pooledpolls_pres_r2_dudatrzask.xlsx')

START_DATE <- "2020-05-10"
END_DATE <- max(as.Date(START_DATE, polls$time))
polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

candidates <- tibble(candidate = c(
  "Duda",
  "Trzaskowski")) %>% 
  mutate(c = 1:2)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + difftime(endDate, startDate)),
         Duda = 100/((100-polls$DK))*polls$Duda,
         Trzaskowski = 100/((100-polls$DK))*polls$Trzaskowski,
         time = as.integer(difftime(midDate, START_DATE, units = "days") + 1L),
         pollster = as.integer(factor(org))) %>%
  gather(candidate, percent, Duda, Trzaskowski, na.rm = FALSE, convert = FALSE) %>%
  left_join(candidates, by = c(candidate = "candidate")) 

stan_data <- list(
  C = max(polls$c),
  T = max(polls$time) + as.integer(difftime(Sys.Date(), max(as.Date(polls$time, origin=START_DATE)), units = "days")),
  N = nrow(polls),
  y = round((polls$percent / 100) * polls$sampleSize),
  sample_size = polls$sampleSize,
  get_t_i = polls$time,
  get_c_i = polls$c
)

write("data {
  int<lower=0> T; // Number of timepoints
  int<lower=0> C; // Number of candidates
  int<lower=0> N; // Number of poll yervations
  
  int sample_size[N]; // Sample size of each poll
  int y[N]; // Number of respondents in poll for candidate (approximate)
  int<lower=1, upper=T> get_t_i[N]; // timepoint for ith observation
  int<lower=1, upper=C> get_c_i[N]; // candidate for ith observation
}
parameters {
  matrix[C, T] delta_logit; // Percent for candidate c at time t
  real<lower=0, upper=1> phi[N]; // Percent of participants in poll for candidate
  real<lower=0> tau; // Random walk variance
  real<lower=0,upper=0.5> sigma; // Overdispersion of observations
}
model {
  // Priors
  tau ~ normal(0, 0.2);
  sigma ~ normal(0, 1);
  
  // Random walk
  for(c in 1:C) {
    delta_logit[c, 2:T] ~ normal(delta_logit[c, 1:(T - 1)], tau);
  }
  
  // Observed data
  y ~ binomial(sample_size, phi);
  for(i in 1:N) {
    // Overdispersion
    delta_logit[get_c_i[i], get_t_i[i]] ~ normal(logit(phi[i]), sigma);
  }
}
generated quantities {
  matrix[C, T] delta = inv_logit(delta_logit);
}",
  "poll_model.stan")

poll_model <- "poll_model.stan"

fit <- stan(poll_model, data=stan_data, iter = 4000, chains=4, control = list(adapt_delta = 0.99, max_treedepth = 12))

cols <- c("Duda"="blue4", "Trzaskowski"="orange", "Kosiniak-Kamysz"="darkgreen", "Bosak" = "midnightblue", "Biedroń" = "red", "Hołownia" = "darkorchid1", "Other"="gray50")

p_trends <- tidybayes::spread_draws(fit, delta[c, time]) %>%
  left_join(candidates) %>%
  group_by(candidate) %>%
  mutate(time = as.Date(time, origin=START_DATE)) %>%
  ggplot() +
  stat_lineribbon(aes(x = time, y = delta*100, fill=candidate, color=candidate), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  geom_point(data = polls, aes(x = midDate, y = percent, color=candidate), alpha = 1, size = 2) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "% of vote", x="",
       subtitle = "",
       color="",
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


p_latest_data <- tidybayes::spread_draws(fit, delta[c, time]) %>%
  left_join(candidates) %>%
  group_by(candidate) %>%
  mutate(time = as.Date(time, origin=START_DATE)) %>%
  filter(time==max(time)) %>%
  mutate(over_50 = delta - 0.5,
         over_50 = sum(over_50 > 0) / length(over_50))


p_latest <- ggplot(p_latest_data) +
  stat_slabh(aes(y=reorder(candidate, desc(candidate)), x=delta*100, fill=candidate), normalize="xy") +
  scale_y_discrete(name="") +
  scale_fill_manual(name="", values=cols, guide=FALSE) +
  annotate(geom = "text", label=paste("Probability of Duda winning:", 
                                      round(mean(p_latest_data$over_50[p_latest_data$candidate=="Duda"]),2)), 
                                      y=1.95, x=median(p_latest_data$delta[p_latest_data$candidate=="Duda"])*100, 
                                      size=3.75, family="Roboto Condensed") +
  annotate(geom = "text", label=paste("Probability of Trzaskowski winning:", 
                                      round(mean(p_latest_data$over_50[p_latest_data$candidate=="Trzaskowski"]),2)), 
           y=0.95, x=median(p_latest_data$delta[p_latest_data$candidate=="Trzaskowski"])*100, 
           size=3.75, family="Roboto Condensed") +
  annotate(geom = "text", label=paste(round(100*median(p_latest_data$delta[p_latest_data$candidate=="Duda"]),0)), 
           y="Duda", x=mean(round(100*median(p_latest_data$delta[p_latest_data$candidate=="Duda"]),0)), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(100*median(p_latest_data$delta[p_latest_data$candidate=="Trzaskowski"]),0)), 
           y="Trzaskowski", x=mean(round(100*median(p_latest_data$delta[p_latest_data$candidate=="Trzaskowski"]),0)), size=4, hjust = "center", vjust=-1, 
           family="Roboto Condensed", color="white") +
  labs(x = "% of vote", y="",
       subtitle = "",
       color="",
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()


delta_post_summarized <- tidybayes::spread_draws(fit, delta[c, time]) %>%
  left_join(candidates) %>%
  group_by(time, candidate) %>%
  mutate(time = as.Date(time, origin=START_DATE)) %>%
  summarize(posterior_median = quantile(delta, 0.5),
            posterior_2.5    = quantile(delta, 0.025),
            posterior_25     = quantile(delta, 0.25),
            posterior_75     = quantile(delta, 0.75),
            posterior_97.5   = quantile(delta, 0.975))
