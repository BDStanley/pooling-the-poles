#####Prepare workspace
system("git pull")
library(tidyverse); library(ggrepel)

set.seed(780045)

theme_plots <- function() {
  theme_minimal(base_family = "IBM Plex Sans Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size=8),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}

theme_plots_map <- function() {
  theme_minimal(base_family = "IBM Plex Sans Condensed") +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          strip.text.x = element_text(size = 10), legend.text = element_text(size=9), 
          legend.title = element_text(face="bold"), plot.title = element_text(face="bold"),
          plot.subtitle = element_text(size=8), aspect.ratio=1, legend.position="none")
}


update_geom_defaults("label", 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults(ggtext::GeomRichText, 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults("label_repel", 
                     list(family = "IBM Plex Sans Condensed"))

options(mc.cores = parallel::detectCores())

#####Read in, adjust and subset data#####
library(googledrive)
library(rio)
library(readxl)

import <- drive_download(as_id("https://docs.google.com/spreadsheets/d/1aI_JqiQuuO0WmzkMBzSZvufrf-XGr-vmNddDCadGi7Q/edit?usp=sharing"), overwrite=TRUE)
1
polls <- read_excel('polldata_gs.xlsx')

import <- drive_download(as_id("https://docs.google.com/spreadsheets/d/1cOC1mY4xf0iXgPavA1CNQeSFwqi_0U_m/edit?usp=sharing&ouid=111487015973215379663&rtpof=true&sd=true"), overwrite=TRUE)
1
weights <- read_excel('2019_elec_percentages.xlsx')

import <- drive_download(as_id("https://drive.google.com/file/d/1JmF3bjRA_sTaJZ4rqPd1WAQyGm-XM-l7/view?usp=sharing"), overwrite=TRUE)
1
const <- readRDS('constituencies')

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + (difftime(endDate, startDate)/2)),
         midDate_int=as.integer(midDate)) %>%
  filter(midDate >= as.Date('2022-01-01')) %>%
  mutate(PSL = PSL,
         Polska2050 = `Polska 2050`,
         time = as.integer(difftime(midDate, min(midDate), units = "days")),
         pollster = as.integer(factor(org)))

cols <- c("PiS"="blue4", "KO"="orange", "PSL"="darkgreen", "Konfederacja" = "midnightblue", "Lewica" = "red",
          "MN" = "yellow", "Other"="gray50", "Polska 2050"="darkgoldenrod", "Don't know" = "gray50")
names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
housenames <- fct_recode(names$house, "Kantar" = "Kantar") %>%
  fct_collapse(., Kantar=c("Kantar"))
names <- glue_collapse(get_labels(housenames), ", ", last = " and ")
polls$org <- str_replace_all(polls$org, "_", ", ")

polls <-
  polls %>%
  mutate(time = interval(min(midDate), midDate)/years(1))

polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska2050", "DK")] <-
  polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska2050", "DK")] %>%
  mutate_all(function(x) (as.numeric(str_remove(x, "%"))/100)-0.001) #to ensure no zeroes in model

polls <-
  polls %>%
  mutate(DK = 1 - c(PiS + KO + Lewica + PSL + Konfederacja + Polska2050))

polls <-
  polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska2050", "DK")])
  )

#####Run model#####
m1 <-
  brm(formula = bf(outcome ~ 1 + s(time, k = 12) + (1 | pollster)),
      family = dirichlet(link = "logit", refcat = "PSL"),
      prior =
        prior(normal(0, 1.5), class = "Intercept", dpar = "muPiS") +
        prior(normal(0, 0.5), class = "b", dpar = "muPiS") +
        prior(exponential(2), class = "sd", dpar = "muPiS") +
        prior(exponential(2), class = "sds", dpar = "muPiS") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muKO") +
        prior(normal(0, 0.5), class = "b", dpar = "muKO") +
        prior(exponential(2), class = "sd", dpar = "muKO") +
        prior(exponential(2), class = "sds", dpar = "muKO") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muLewica") +
        prior(normal(0, 0.5), class = "b", dpar = "muLewica") +
        prior(exponential(2), class = "sd", dpar = "muLewica") +
        prior(exponential(2), class = "sds", dpar = "muLewica") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muDK") +
        prior(normal(0, 0.5), class = "b", dpar = "muDK") +
        prior(exponential(2), class = "sd", dpar = "muDK") +
        prior(exponential(2), class = "sds", dpar = "muDK") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muKonfederacja") +
        prior(normal(0, 0.5), class = "b", dpar = "muKonfederacja") +
        prior(exponential(2), class = "sd", dpar = "muKonfederacja") +
        prior(exponential(2), class = "sds", dpar = "muKonfederacja") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muPolska2050") +
        prior(normal(0, 0.5), class = "b", dpar = "muPolska2050") +
        prior(exponential(2), class = "sd", dpar = "muPolska2050") +
        prior(exponential(2), class = "sds", dpar = "muPolska2050") +
        prior(gamma(1, 0.01), class = "phi"),
      data = polls,
      seed = 780045,
      iter = 5000,
      backend="cmdstanr", chains=3, cores=3, threads = threading(3),
      refresh = 5,
      control =
        list(
          adapt_delta = .95,
          max_treedepth = 15
        )
  )




today <- interval(min(polls$midDate), Sys.Date())/years(1)

pred_dta <-
  tibble(
    time = seq(0, today, length.out = nrow(polls)),
    date = as.Date(time*365, origin = min(polls$midDate))
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
          levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "DK"),
          labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Don't know")
      )
  )

point_dta <-
  polls[names(polls) %in% c("midDate", "PiS", "KO", "Lewica", "Konfederacja", "DK", "PSL", "Polska2050")] %>%
  pivot_longer(
    cols = -midDate,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "DK"),
        labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Don't know")
      )
  )

plot_trends_parl_DK <-
  ggplot() +
  geom_point(data=point_dta %>% filter(., party %in% c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Don't know")),
             aes(x = midDate, y = est, colour = party, fill = party), alpha = .5, size = 1, show.legend=FALSE) +
  stat_lineribbon(data=pred_dta %>% filter(., party %in% c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Don't know")),
                  aes(x = date, y = .value, color=party, fill=party), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  facet_wrap(~party, nrow=3) +
  labs(y = "% of vote", x="", title = "Trends (including undecided voters)",
       subtitle=str_c("Data from ", names, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
  theme_changes
ggsave(plot_trends_parl_DK, file = "plot_trends_parl_DK.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")

facet_labels <- c(
  `PiS` = "PiS",
  `KO` = "KO",
  `Polska 2050` = "Polska 2050",
  `Lewica` = "Lewica",
  `Konfederacja` = "Konfederacja",
  `PSL` = "PSL",
  `Don't know` = "Nie wiem / Trudno powiedzieć"
)

Sys.setlocale("LC_TIME", "pl_PL.UTF-8")
plot_trends_parl_DK_PL <-
  ggplot() +
  geom_point(data=point_dta %>% filter(., party %in% c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Don't know")),
             aes(x = midDate, y = est, colour = party, fill = party), alpha = .5, size = 1, show.legend=FALSE) +
  stat_lineribbon(data=pred_dta %>% filter(., party %in% c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Don't know")),
                  aes(x = date, y = .value, color=party, fill=party), .width=c(0.95), alpha=1/2, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  facet_wrap(~party, nrow=3, labeller = as_labeller(facet_labels)) +
  labs(y = "", x="", title = "Trendy (w tym wyborcy niezdecydowani)",
       subtitle=str_to_upper(str_c("Dane: ", names_PL, ".")), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
  theme_changes
ggsave(plot_trends_parl_DK_PL, file = "plot_trends_parl_DK_PL.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")



