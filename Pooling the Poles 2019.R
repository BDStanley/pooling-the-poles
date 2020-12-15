#####Load packages#####
library(tidyverse)
library(patchwork)
library(tidybayes)
library(lubridate)
library(htmltab)
library(jbmisc)
library(brms)
library(here)
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

set.seed(780045)

url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Polish_parliamentary_election"
dta1 <- htmltab(doc = url, which = 1, rm_nodata_cols = F)
dta1 <- dta1 %>%
  tibble(., .name_repair = "universal") %>%
  select(., -Kukiz.15)

dta2 <- htmltab(doc = url, which = 2, rm_nodata_cols = F)
dta2 <- tibble(dta2, .name_repair = "universal")

dta <- bind_rows(dta1, dta2)

polls <- dta %>%
  filter(!is.na(Polling.Firm.Link)) %>%
  filter(!str_detect(Polling.Firm.Link, 'Presidential')) %>%
  filter(!is.na(Poland2050)) %>%
  select(-Lead, -CivicCoalition...6, -SampleSize) %>%
  mutate_at('Poland2050', as.numeric) %>%
  mutate_at('UnitedRight', as.numeric) %>%
  mutate_at('CivicCoalition...5', as.numeric) %>%
  mutate_at('The.Left', as.numeric) %>%
  mutate_at('PolishCoalition', as.numeric) %>%
  mutate_at('Confederation', as.numeric) %>%
  mutate_at('Others..Don.t.know', as.numeric) %>%
  select(org = Polling.Firm.Link, date = FieldworkPeriod, PiS = UnitedRight, KO = CivicCoalition...5, `Polska 2050` = Poland2050, 
         Lewica = The.Left, PSL = PolishCoalition, Konfederacja = Confederation, DK = Others..Don.t.know) %>%
  mutate(
    date =
      date %>%
      str_remove(".*(–|-)") %>%
      dmy()
  ) %>%
  mutate(
    org =
      org %>%
      str_remove("(/|/).*") %>%
      str_trim(.) %>%
      as_factor(.)
  ) %>%
  mutate(time = as.integer(date, units = "days"),
         pollster = as.integer(org)) %>%
  mutate(DK = DK+0.00001) %>%
  mutate(total = PiS + KO + Lewica +  PSL + Konfederacja + `Polska 2050` + DK) %>%
  filter(!is.na(total)) %>%
  mutate(PiS = (PiS/total)*100,
         KO = (KO/total)*100,
         Lewica = (Lewica/total)*100,
         PSL = (PSL/total)*100,
         Konfederacja = (Konfederacja/total)*100,
         `Polska 2050` = (`Polska 2050`/total)*100,
         DK = (DK/total)*100) %>%
  filter(., total<=100)

cols <- c("PiS"="blue4", "KO"="orange", "PSL"="darkgreen", "Konfederacja" = "midnightblue", "Lewica" = "red", "MN" = "yellow", "DK"="gray50", "Polska 2050"="darkgoldenrod")
names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
housenames <- fct_recode(names$house, "Kantar" = "Kantar") %>%
  fct_collapse(., Kantar=c("Kantar"))
names <- paste0(get_labels(housenames), collapse=", ")


polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska 2050", "DK")] <-
  polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska 2050", "DK")] %>%
  mutate_all(function(x) (as.numeric(str_remove(x, "%"))/100))

polls <-
  polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska 2050", "DK")])
  )

#####Run model#####
m1 <-
  brm(formula = bf(outcome ~ 1 + s(time, k = 10) + (1 | pollster)),
      family = dirichlet(link = "logit", refcat = "PiS"),
      prior =
        prior(normal(0, 1.5), class = "Intercept", dpar = "muKO") +
        prior(normal(0, 0.5), class = "b", dpar = "muKO") +
        prior(exponential(2), class = "sd", dpar = "muKO") +
        prior(exponential(2), class = "sds", dpar = "muKO") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muLewica") +
        prior(normal(0, 0.5), class = "b", dpar = "muLewica") +
        prior(exponential(2), class = "sd", dpar = "muLewica") +
        prior(exponential(2), class = "sds", dpar = "muLewica") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muPSL") +
        prior(normal(0, 0.5), class = "b", dpar = "muPSL") +
        prior(exponential(2), class = "sd", dpar = "muPSL") +
        prior(exponential(2), class = "sds", dpar = "muPSL") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muKonfederacja") +
        prior(normal(0, 0.5), class = "b", dpar = "muKonfederacja") +
        prior(exponential(2), class = "sd", dpar = "muKonfederacja") +
        prior(exponential(2), class = "sds", dpar = "muKonfederacja") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muPolska2050") +
        prior(normal(0, 0.5), class = "b", dpar = "muPolska2050") +
        prior(exponential(2), class = "sd", dpar = "muPolska2050") +
        prior(exponential(2), class = "sds", dpar = "muPolska2050") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muDK") +
        prior(normal(0, 0.5), class = "b", dpar = "muDK") +
        prior(exponential(2), class = "sd", dpar = "muDK") +
        prior(exponential(2), class = "sds", dpar = "muDK") +
        prior(gamma(1, 0.01), class = "phi"),
      data = polls,
      seed = 780045,
      iter = 5000,
      chains = 6,
      cores = 6,
      refresh = 5,
      control =
        list(
          adapt_delta = .95,
          max_treedepth = 15
        )
  )

#####Trend plot#####
today <- interval(min(polls$date), Sys.Date())/years(1)

pred_dta <-
  tibble(
    time = seq(0, today, length.out = nrow(polls)),
    date = as.Date(time*365, origin = min(polls$date))
  )

pred_dta <-
  add_fitted_draws(
    model = m1,
    newdata = pred_dta,
    re_formula = NA
  ) %>%
  group_by(date, .category) %>%
  rename(party = .category) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska 2050", "DK"),
        labels = c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska 2050", "DK")))

point_dta <-
  polls[names(polls) %in% c("date", "PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska 2050", "DK")] %>%
  pivot_longer(
    cols = -date,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska 2050", "DK"),
        labels = c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska 2050", "DK")))


plot_trends_parl_all <-
  ggplot() +
  #geom_point(data=point_dta, aes(x = date, y = est, colour = party, fill = party), alpha = .5, size = 1, show.legend=FALSE) +
  stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(xlim = c(min(polls$date), max(polls$date)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "", x="", title = "Trends", 
       subtitle=str_c("Data from ", names), color="", caption = "Ben Stanley (@BDStanley; benstanley.org). Model based on code written by Jack Bailey (@PoliSciJack).") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_changes
ggsave(plot_trends_parl_all, file = "plot_trends_parl_all.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)