#####Prepare workspace
system("git pull")
library(tidyverse); library(ggrepel); library(googledrive); library(rio); library(readxl); library(sf); library(glue); library(brms);
library(sjlabelled); library(lubridate); library(stringr); library(hrbrthemes); library(tidybayes); library(ggdist); library(ggblend)

set.seed(780045)

theme_plots <- function(base_size = 11, base_family = "Jost") {
  theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill = "#ffffff", colour = NA),
          title = element_text(size = rel(1), family = "Jost", face = "bold"),
          plot.subtitle = element_text(size = rel(0.8),
                                       family = "Jost", face = "plain"),
          plot.caption = element_text(margin = margin(t = 10), size = rel(0.6),
                                      family = "Jost", face = "plain"),
          panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.15),
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = rel(0.8),
                                    family = "Jost", face = "bold"),
          axis.title.x = element_text(hjust = 0, margin = margin(t = 10)),
          axis.title.y = element_text(hjust = 1, margin = margin(r = 10)),
          legend.position = "bottom",
          legend.title = element_text(size = rel(0.8), vjust = 0.5,
                                      family = "Jost", face = "bold"),
          legend.key.size = unit(0.7, "line"),
          legend.key = element_blank(),
          legend.spacing = unit(0.1, "lines"),
          legend.justification = "left",
          legend.margin = margin(t = -5, b = 0, l = 0, r = 0),
          strip.text = element_text(size = rel(0.9), hjust = 0,
                                    family = "Jost", face = "plain"),
          strip.background = element_rect(fill = "white", colour = NA),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )
}

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

#####Round 1#####
drive_deauth()
import <- drive_download(as_id("https://docs.google.com/spreadsheets/d/1YXiP38k4JYVvJcuAm1E2uYGJFK2foldaJPQXq8c9H8o/edit?usp=share_link"), overwrite=TRUE)
polls <- read_excel('polldata_pres_R1.xlsx')

polls <- polls %>%
  dplyr::select(startDate, endDate, org, remark, Nawrocki, Trzaskowski, Hołownia, Biejat, Mentzen, Other, DK)

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + (difftime(endDate, startDate, units="days")/2)),
         midDate_int=as.integer(midDate)) %>%
  mutate(Nawrocki = 100/((100-DK))*Nawrocki,
         Trzaskowski = 100/((100-DK))*Trzaskowski,
         Biejat = 100/((100-DK))*Biejat,
         Hołownia = 100/((100-DK))*Hołownia,
         Mentzen = 100/((100-DK))*Mentzen,
         Other = 100/((100-DK))*Other,
         time = as.integer(difftime(midDate, min(midDate), units = "days")),
         pollster = as.integer(factor(org)))

cols <- c("Nawrocki"="blue", "Trzaskowski"="orange", "Hołownia"="goldenrod", "Mentzen" = "midnightblue", "Biejat" = "red",  "Other"="gray50")

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

polls[names(polls) %in% c("Nawrocki", "Trzaskowski", "Biejat", "Mentzen", "Hołownia")] <-
  polls[names(polls) %in% c("Nawrocki", "Trzaskowski", "Biejat", "Mentzen", "Hołownia")] %>%
  mutate_all(function(x) (as.numeric(str_remove(x, "%"))/100)-0.001)

polls <-
  polls %>%
  mutate(Other = 1 - (Nawrocki + Trzaskowski + Biejat + Mentzen + Hołownia))

polls <- polls %>%
  rename(
    Holownia = Hołownia
  )

polls <-
  polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("Nawrocki", "Trzaskowski", "Biejat", "Mentzen", "Holownia", "Other")])
  )

m1 <-
  brm(formula = bf(outcome ~ 1 + s(time, k = 6) + (1 | pollster)),
      family = dirichlet(link = "logit", refcat = "Other"),
      data = polls,
      prior =
        prior(normal(0, 1.5), class = "Intercept", dpar = "muNawrocki") +
        prior(normal(0, 0.5), class = "b", dpar = "muNawrocki") +
        prior(exponential(2), class = "sd", dpar = "muNawrocki") +
        prior(exponential(2), class = "sds", dpar = "muNawrocki") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muTrzaskowski") +
        prior(normal(0, 0.5), class = "b", dpar = "muTrzaskowski") +
        prior(exponential(2), class = "sd", dpar = "muTrzaskowski") +
        prior(exponential(2), class = "sds", dpar = "muTrzaskowski") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muBiejat") +
        prior(normal(0, 0.5), class = "b", dpar = "muBiejat") +
        prior(exponential(2), class = "sd", dpar = "muBiejat") +
        prior(exponential(2), class = "sds", dpar = "muBiejat") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muHolownia") +
        prior(normal(0, 0.5), class = "b", dpar = "muHolownia") +
        prior(exponential(2), class = "sd", dpar = "muHolownia") +
        prior(exponential(2), class = "sds", dpar = "muHolownia") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muMentzen") +
        prior(normal(0, 0.5), class = "b", dpar = "muMentzen") +
        prior(exponential(2), class = "sd", dpar = "muMentzen") +
        prior(exponential(2), class = "sds", dpar = "muMentzen") +
        prior(gamma(1, 0.01), class = "phi"),
      seed = 780045,
      iter = 5000,
      backend="cmdstanr", threads = threading(3),
      chains = 3, cores = 12,
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
        levels = c("Nawrocki", "Trzaskowski", "Biejat", "Mentzen", "Holownia", "Other"),
        labels = c("Nawrocki", "Trzaskowski", "Biejat", "Mentzen", "Hołownia", "Other")
      )
  )

point_dta <-
  polls[names(polls) %in% c("midDate", "Nawrocki", "Trzaskowski", "Biejat", "Mentzen", "Holownia", "Other")] %>%
  pivot_longer(
    cols = -midDate,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("Nawrocki", "Trzaskowski", "Biejat", "Mentzen", "Holownia", "Other"),
        labels = c("Nawrocki", "Trzaskowski", "Biejat", "Mentzen", "Hołownia", "Other")
      )
  )

# trends_pres_R1 <-
#   ggplot() +
#   geom_point(data=point_dta, aes(x = midDate, y = est, colour = party, fill=party), size = 1, show.legend=FALSE) +
#   stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0.8), alpha=1/2) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_x_date(date_breaks = "1 month",
#                labels = my_date_format()) +
#   coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
#                   ylim = c(0, .5)) +
#   scale_color_manual(values=cols) +
#   scale_fill_manual(values=cols, guide=FALSE) +
#   labs(y = "", x="", title = "Polish presidential election, round 1", 
#        subtitle=str_c("Data from ", names, "."), color="", caption = "") +
#   guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
#   theme_plots() +
#   theme(legend.position = "bottom")
# ggsave(trends_pres_R1, file = "trends_pres_R1.png", 
#        width = 7, height = 5, units = "cm", dpi = 600, scale = 3, bg="white")

trends_pres_R1 <- pred_dta %>%
  ggplot(aes(x = date, color=party, fill=party)) +
  ggdist::stat_lineribbon(
    aes(y = .value, fill_ramp = stat(.width)),
    .width = seq(0, 0.8, 0.01)
  ) |> partition(vars(party)) |> blend("multiply") +
  geom_point(data=point_dta, aes(x = midDate, y = est, colour = party, fill=party), size = 1, show.legend=FALSE) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  ggdist::scale_fill_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               labels = my_date_format()) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  labs(y = "", x="", title = "Polish presidential election, round 1",
       subtitle=str_c("Data from ", names, "."), color="", caption = "") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
  theme_plots() +
  theme(legend.position = "bottom")
ggsave(trends_pres_R1, file = "trends_pres_R1.png",
       width = 7, height = 5, units = "cm", dpi = 600, scale = 3, bg="white", device=png(type="cairo"))

#####Round 2#####
drive_deauth()
import <- drive_download(as_id("https://docs.google.com/spreadsheets/d/1_-yU4OFLhKuBzhONq_923cg6Qwc5DkUzCoIdxzcGEbk/edit?usp=share_link"), overwrite=TRUE)
polls <- read_excel('polldata_pres_R2.xlsx')

polls <- polls %>%
  dplyr::select(startDate, endDate, org, remark, Nawrocki, Trzaskowski, DK)

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + (difftime(endDate, startDate, units="days")/2)),
         midDate_int=as.integer(midDate)) %>%
  #filter(midDate >= as.Date('2023-10-15')) %>%
  mutate(Nawrocki = 100/((100-DK))*Nawrocki,
         Trzaskowski = 100/((100-DK))*Trzaskowski,
         time = as.integer(difftime(midDate, min(midDate), units = "days")),
         pollster = as.integer(factor(org)))

cols <- c("Nawrocki"="blue", "Trzaskowski"="orange")

names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
names <- glue_collapse(get_labels(housenames), ", ", last = " and ")
names_PL <- glue_collapse(get_labels(housenames), ", ", last = " i ")
polls$org <- str_replace_all(polls$org, "_", ", ")

polls <-
  polls %>%
  mutate(time = interval(min(midDate), midDate)/years(1))

polls[names(polls) %in% c("Nawrocki", "Trzaskowski")] <-
  polls[names(polls) %in% c("Nawrocki", "Trzaskowski")] %>%
  mutate_all(function(x) (as.numeric(str_remove(x, "%"))/100))

polls <-
  polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("Nawrocki", "Trzaskowski")])
  )

m1 <-
  brm(formula = bf(outcome ~ 1 + s(time, k = 5) + (1 | pollster)),
      family = dirichlet(link = "logit", refcat = "Nawrocki"),
      data = polls,
      prior =
        prior(normal(0, 1.5), class = "Intercept", dpar = "muTrzaskowski") +
        prior(normal(0, 0.5), class = "b", dpar = "muTrzaskowski") +
        prior(exponential(2), class = "sd", dpar = "muTrzaskowski") +
        prior(exponential(2), class = "sds", dpar = "muTrzaskowski") +
        prior(gamma(1, 0.01), class = "phi"),
      seed = 780045,
      iter = 5000,
      backend="cmdstanr", threads = threading(3),
      chains = 3, cores = 12,
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
        levels = c("Nawrocki", "Trzaskowski"),
        labels = c("Nawrocki", "Trzaskowski")
      )
  )

point_dta <-
  polls[names(polls) %in% c("midDate", "Nawrocki", "Trzaskowski")] %>%
  pivot_longer(
    cols = -midDate,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("Nawrocki", "Trzaskowski"),
        labels = c("Nawrocki", "Trzaskowski")
      )
  )

trends_pres_R2 <-
  ggplot() +
  geom_point(data=point_dta, aes(x = midDate, y = est, colour = party, fill=party), size = 1, show.legend=FALSE) +
  stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0.8), alpha=1/2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               labels = my_date_format()) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate))) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "", x="", title = "Polish presidential election, round 2", 
       subtitle=str_c("Data from ", names, "."), color="", caption = "") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
  theme_plots()+
  theme(legend.position = "bottom")
ggsave(trends_pres_R2, file = "trends_pres_R2.png", 
       width = 7, height = 5, units = "cm", dpi = 600, scale = 3, bg="white")

trends_pres_R2 <- pred_dta %>%
  ggplot(aes(x = date, color=party, fill=party)) +
  ggdist::stat_lineribbon(
    aes(y = .value, fill_ramp = stat(.width)),
    .width = seq(0, 0.8, 0.01)
  ) |> partition(vars(party)) |> blend("multiply") +
  geom_point(data=point_dta, aes(x = midDate, y = est, colour = party, fill=party), size = 1, show.legend=FALSE) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  ggdist::scale_fill_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               labels = my_date_format()) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate))) +
  labs(y = "", x="", title = "Polish presidential election, round 2",
       subtitle=str_c("Data from ", names, "."), color="", caption = "") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
  theme_plots()+
  theme(legend.position = "bottom")
ggsave(trends_pres_R2, file = "trends_pres_R2.png",
       width = 7, height = 5, units = "cm", dpi = 600, scale = 3, bg="white", device=png(type="cairo"))


#####Upload to Github and sync with website folder#####
system("git pull")
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
system("rsync -av --include='*.png' --exclude='*' '/Users/benstanley/R scripts/Pooling the Poles/' '/Users/benstanley/R scripts/Website/images/'")
