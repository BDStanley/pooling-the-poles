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
          strip.background = element_rect(fill = "grey95", color = NA),
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

my_date_format <- function()
{
  function(x)
  {
    m <- format(x,"%b")
    y <- format(x,"\n%Y")
    ifelse(duplicated(y),m,paste(m,y))
  }
}

options(mc.cores = parallel::detectCores())

#####Read in, adjust and subset data#####
library(googledrive)
library(rio)
library(readxl)
library(sjlabelled)
library(glue)
library(lubridate)

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
names_PL <- glue_collapse(get_labels(housenames), ", ", last = " i ")
polls$org <- str_replace_all(polls$org, "_", ", ")

polls <-
  polls %>%
  mutate(time = interval(min(midDate), midDate)/years(1))

polls$Other <- polls$Other+0.001
polls$DK <- polls$DK+0.001

polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska2050", "Other", "DK")] <-
  polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska2050", "Other", "DK")] %>%
  mutate_all(function(x) (as.numeric(str_remove(x, "%"))/100))

polls <-
  polls %>%
  mutate(All = PiS + KO + Lewica + PSL + Konfederacja + Polska2050 + Other + DK,
         PiS = (PiS/All)*1,
         KO = (KO/All)*1,
         Lewica = (Lewica/All)*1,
         PSL = (PSL/All)*1,
         Konfederacja = (Konfederacja/All)*1,
         Polska2050 = (Polska2050/All)*1,
         Other = (Other/All)*1,
         DK = (DK/All)*1
  )

polls <-
  polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska2050", "Other", "DK")])
  )

#####Run model#####
library(brms)
m1 <-
  brm(formula = bf(outcome ~ 1 + s(time, k = 24) + (1 | pollster)),
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
        prior(normal(0, 1.5), class = "Intercept", dpar = "muOther") +
        prior(normal(0, 0.5), class = "b", dpar = "muOther") +
        prior(exponential(2), class = "sd", dpar = "muOther") +
        prior(exponential(2), class = "sds", dpar = "muOther") +
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


#####Trend plot#####
library(stringr)
library(tidybayes)

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
          levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "Other", "DK"),
          labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other", "Don't know")
      )
  )

point_dta <-
  polls[names(polls) %in% c("midDate", "PiS", "KO", "Lewica", "Konfederacja", "DK", "PSL", "Polska2050", "Other")] %>%
  pivot_longer(
    cols = -midDate,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "Other", "DK"),
        labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other", "Don't know")
      )
  )

plot_trends_parl_DK <-
  ggplot() +
  geom_point(data=point_dta %>% filter(., party %in% c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other", "Don't know")),
             aes(x = midDate, y = est, colour = party, fill = party), alpha = .5, size = 1, show.legend=FALSE) +
  stat_lineribbon(data=pred_dta %>% filter(., party %in% c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other", "Don't know")),
                  aes(x = date, y = .value, color=party, fill=party), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               labels = my_date_format()) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  facet_wrap(~party, nrow=3) +
  labs(y = "% of vote", x="", title = "Trends (including undecided voters)",
       subtitle=str_c("Data from ", names, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA)))
ggsave(plot_trends_parl_DK, file = "plot_trends_parl_DK.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")

facet_labels <- c(
  `PiS` = "PiS",
  `KO` = "KO",
  `Polska 2050` = "Polska 2050",
  `Lewica` = "Lewica",
  `Konfederacja` = "Konfederacja",
  `PSL` = "PSL",
  `Other` = "Inni",
  `Don't know` = "Nie wiem / Trudno powiedzieć"
)

Sys.setlocale("LC_TIME", "pl_PL.UTF-8")
plot_trends_parl_DK_PL <-
  ggplot() +
  geom_point(data=point_dta %>% filter(., party %in% c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other", "Don't know")),
             aes(x = midDate, y = est, colour = party, fill = party), alpha = .5, size = 1, show.legend=FALSE) +
  stat_lineribbon(data=pred_dta %>% filter(., party %in% c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other", "Don't know")),
                  aes(x = date, y = .value, color=party, fill=party), .width=c(0.95), alpha=1/2, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               labels = my_date_format()) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  facet_wrap(~party, nrow=3, labeller = as_labeller(facet_labels)) +
  labs(y = "", x="", title = "Trendy (w tym wyborcy niezdecydowani)",
       subtitle=str_c("Dane: ", names_PL, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) 
ggsave(plot_trends_parl_DK_PL, file = "plot_trends_parl_DK_PL.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")


#####Latest plot#####
library(ggdist)
library(ggblend)

plotdraws <- add_fitted_draws(
  model = m1,
  newdata =
    tibble(time = today),
  re_formula = NA
) %>%
  group_by(.category) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "Other", "DK"),
                            labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other", "Don't know")))

medians <- plotdraws %>%
  summarise(est = median(.value)*100, .groups = "drop")

plot_latest_parl_DK <-
  add_fitted_draws(
    model = m1,
    newdata =
      tibble(time = today),
    re_formula = NA
  ) %>%
  group_by(.category) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "Other", "DK"),
                            labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other", "Don't know"))) %>%
  ggplot(aes(y=reorder(.category, dplyr::desc(-.value)), 
             x=.value, color=.category)) +
  geom_vline(aes(xintercept=0.05), colour="gray40", linetype="dotted") +
  stat_interval(aes(x=.value, color_ramp = stat(.width)), .width = ppoints(100)) %>%
  partition(vars(.category)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_discrete(name="", position="right") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PiS"],0)),
           y="PiS", x=medians$est[medians$.category=="PiS"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="KO"],0)),
           y="KO", x=medians$est[medians$.category=="KO"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Polska 2050"],0)),
           y="Polska 2050", x=medians$est[medians$.category=="Polska 2050"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Lewica"],0)),
           y="Lewica", x=medians$est[medians$.category=="Lewica"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Konfederacja"],0)),
           y="Konfederacja", x=medians$est[medians$.category=="Konfederacja"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PSL"],0)),
           y="PSL", x=medians$est[medians$.category=="PSL"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Other"],0)),
           y="Other", x=medians$est[medians$.category=="Other"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Don't know"],0)),
           y="Don't know", x=medians$est[medians$.category=="Don't know"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  scale_color_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Latest estimates",
       subtitle=str_c("Data from ", names,".")) +
  theme_plots()
ggsave(plot_latest_parl_DK, file = "polls_latest_parl_DK.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")

medians <- medians %>%
  mutate(., .category = recode(.category, "Don't know" = 'Nie wiem', "Other" = 'Inni'))

Sys.setlocale("LC_TIME", "pl_PL.UTF-8")
plot_latest_parl_DK_PL <-
  add_fitted_draws(
    model = m1,
    newdata =
      tibble(time = today),
    re_formula = NA
  ) %>%
  group_by(.category) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "Other", "DK"),
                            labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Inni", "Nie wiem"))) %>%
  ggplot(aes(y=reorder(.category, dplyr::desc(-.value)), 
             x=.value, color=.category)) +
  geom_vline(aes(xintercept=0.05), colour="gray40", linetype="dotted") +
  stat_interval(aes(x=.value, color_ramp = stat(.width)), .width = ppoints(100)) %>%
  partition(vars(.category)) +
  scale_color_manual(values=cols, guide=FALSE) +
  scale_fill_manual(values=cols, guide=FALSE) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_discrete(name="", position="right") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PiS"],0)),
           y="PiS", x=medians$est[medians$.category=="PiS"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="KO"],0)),
           y="KO", x=medians$est[medians$.category=="KO"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Polska 2050"],0)),
           y="Polska 2050", x=medians$est[medians$.category=="Polska 2050"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Lewica"],0)),
           y="Lewica", x=medians$est[medians$.category=="Lewica"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Konfederacja"],0)),
           y="Konfederacja", x=medians$est[medians$.category=="Konfederacja"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PSL"],0)),
           y="PSL", x=medians$est[medians$.category=="PSL"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Inni"],0)),
           y="Inni", x=medians$est[medians$.category=="Inni"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Nie wiem"],0)),
           y="Nie wiem", x=medians$est[medians$.category=="Nie wiem"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Szacunkowe wyniki", color="",
       subtitle=str_c("Dane: ", names_PL,".")) +
  theme_plots()
ggsave(plot_latest_parl_DK_PL, file = "polls_latest_parl_DK_PL.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")


#####Upload to Github#####
system("git add -A")
system("git commit -m 'PTP new'")
system("git pull")
system("git push")
