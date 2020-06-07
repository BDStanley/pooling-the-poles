#####Prepare workspace#####
rm(list=ls())
library(tidyverse); library(rio); library(sjPlot); library(easystats); library(googledrive); library(sjPlot)
library(sjlabelled); library(brms); library(nnet); library(ggeffects); library(hrbrthemes); library(tidybayes)
library(modelr); library(broom)
options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

cols <- c("Duda"="blue4", "Trzaskowski"="orange", "Kosiniak-Kamysz"="darkgreen", "Bosak" = "midnightblue", 
          "Biedroń" = "red", "Hołownia" = "darkorchid1", "Nie wiem"="gray50", "Nie wezmę udziału"="gray50")

drive_download(as_id('https://drive.google.com/file/d/1I8UxoqznS51_S41gvUueHFNknMhrgOBg/view?usp=sharing'), overwrite=TRUE)
1
fala_7 <- import('Fala 7_weight.sav')

fala_7$F6_VOTE <- factorize(fala_7$F6_VOTE)
fala_7$F6_VOTE <- fct_relevel(fala_7$F6_VOTE, c("Andrzej Duda", "Rafał Trzaskowski", "Szymon Hołownia", "Krzysztof Bosak",
                                                "Władysław Kosiniak-Kamysz", "Robert Biedroń", "Biorę pod uwagę głosowania na innego kandydata/nie wiem jeszcze na kogo zagłosuję"))
fala_7$F6_VOTE[1:7] <- levels(fala_7$F6_VOTE)[1:7] <- c("Duda", "Trzaskowski", "Hołownia", "Bosak", "Kosiniak-Kamysz", "Biedroń", "Nie wiem")

fala_7 <- fala_7 %>%
  set_labels(F7_vote2t_A, labels=c("Trzaskowski", "Duda", "Nie wezmę udziału"))

bprior <- c(prior_string("normal(0,2.5)", class="b"))
#bprior <- c(prior(normal(-3,1), class="b", coef="F6_VOTETrzaskowski", dpar="mu3"),
            #prior(normal(0,1), class="b", coef="F6_VOTETrzaskowski", dpar="mu2"))

fit <- brm(F7_vote2t_A | weights(waga) ~ F6_VOTE, family=categorical(), data=fala_7, prior=bprior, sample_prior=TRUE)
plot(hypothesis(fit, "mu3_F6_VOTEBosak=0"))

medians <- fala_7 %>%
  data_grid(F6_VOTE, .model=fit) %>%
  add_fitted_draws(fit) %>%
  median_qi(.value)

print(medians, n=21)

p <- fala_7 %>%
  data_grid(F6_VOTE, .model=fit) %>%
  add_fitted_draws(fit) %>%
  ggplot(aes(x = F6_VOTE, y = .value, color=.category)) +
  stat_pointinterval(position = position_dodge(width = .4)) +
  scale_color_manual(breaks = c("2", "1", "3"), values = c("blue4", "orange", "grey50"), labels=c("Duda", "Trzaskowski", "Nie wezmę udziału")) +
  labs(x = "Candidate voted for in first round", y = "Probability of vote in second round", 
       subtitle = "Second-round vote choice based on first-round choices",
       color="",
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "ADRT.png", width = 8, height = 5, units = "cm", dpi = 320, scale = 4.5)


fit <- brm(F7_vote2t_B | weights(waga) ~ F6_VOTE, family=categorical(), data=fala_7, prior=bprior, sample_prior=TRUE)
medians <- fala_7 %>%
  data_grid(F6_VOTE, .model=fit) %>%
  add_fitted_draws(fit) %>%
  median_qi(.value)

print(medians, n=21)

p <- fala_7 %>%
  data_grid(F6_VOTE, .model=fit) %>%
  add_fitted_draws(fit) %>%
  ggplot(aes(x = F6_VOTE, y = .value, color=.category)) +
  stat_pointinterval(position = position_dodge(width = .4)) +
  scale_color_manual(breaks = c("2", "1", "3"), values = c("blue4", "darkorchid1", "grey50"), labels=c("Duda", "Hołownia", "Nie wezmę udziału")) +
  labs(x = "Candidate voted for in first round", y = "Probability of vote in second round", 
       subtitle = "Second-round vote choice based on first-round choices",
       color="",
       caption = "@BDStanley; benstanley.org") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "ADSH.png", width = 8, height = 5, units = "cm", dpi = 320, scale = 4.5)


