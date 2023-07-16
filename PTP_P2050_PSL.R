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

import <- drive_download(as_id("https://docs.google.com/spreadsheets/d/1aI_JqiQuuO0WmzkMBzSZvufrf-XGr-vmNddDCadGi7Q/edit?usp=sharing"), overwrite=TRUE)
1
polls_old <- read_excel('polldata_gs.xlsx')

polls_old <- polls_old %>%
  mutate(PO2050PSL = `Polska 2050` + PSL) %>%
  select(startDate, endDate, org, remark, PiS, KO, Lewica, PO2050PSL, Konfederacja, Other, DK)

import <- drive_download(as_id("https://docs.google.com/spreadsheets/d/1DzFWrLA34CWzXDuxIncracoNG0kmMuo1DjvLR0PQUEs/edit?usp=sharing"), overwrite=TRUE)
1
polls <- read_excel('polldata_gs_p2050_psl.xlsx')

polls <- bind_rows(polls_old, polls)

import <- drive_download(as_id("https://docs.google.com/spreadsheets/d/1cOC1mY4xf0iXgPavA1CNQeSFwqi_0U_m/edit?usp=sharing&ouid=111487015973215379663&rtpof=true&sd=true"), overwrite=TRUE)
1
weights <- read_excel('2019_elec_percentages.xlsx')

import <- drive_download(as_id("https://drive.google.com/file/d/1JmF3bjRA_sTaJZ4rqPd1WAQyGm-XM-l7/view?usp=sharing"), overwrite=TRUE)
1
const <- readRDS('constituencies')

# polls <- polls %>% 
#   filter(., org!="Social Changes") %>%
#   filter(., org!="CBOS") %>%
#   filter(., org!="Kantar")

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + (difftime(endDate, startDate, units="days")/2)),
         midDate_int=as.integer(midDate)) %>%
  filter(midDate >= as.Date('2023-01-01')) %>%
  #filter(midDate_int > (max(midDate_int)-365)) %>%
  mutate(PiS = 100/((100-DK))*PiS,
         KO = 100/((100-DK))*KO,
         Lewica = 100/((100-DK))*Lewica,
         PO2050PSL = 100/((100-DK))*PO2050PSL,
         Konfederacja = 100/((100-DK))*Konfederacja,
         Other = 100/((100-DK))*Other,
         time = as.integer(difftime(midDate, min(midDate), units = "days")),
         pollster = as.integer(factor(org)))

cols <- c("PiS"="blue", "KO"="orange", "Trzecia Droga"="darkgreen", "Konfederacja" = "midnightblue", "Lewica" = "red", "MN" = "yellow", "Other"="gray50")

library(glue)
library(sjlabelled)
library(lubridate)

names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
housenames <- fct_recode(names$house, "Kantar" = "Kantar") %>%
  fct_collapse(., Kantar=c("Kantar"))
#names <- paste0(get_labels(housenames), collapse=", ")
names <- glue_collapse(get_labels(housenames), ", ", last = " and ")
names_PL <- glue_collapse(get_labels(housenames), ", ", last = " i ")
polls$org <- str_replace_all(polls$org, "_", ", ")

polls <-
  polls %>%
  mutate(time = interval(min(midDate), midDate)/years(1))

polls[names(polls) %in% c("PiS", "KO", "Lewica", "Konfederacja", "PO2050PSL")] <-
  polls[names(polls) %in% c("PiS", "KO", "Lewica", "Konfederacja", "PO2050PSL")] %>%
  mutate_all(function(x) (as.numeric(str_remove(x, "%"))/100)-0.001)

polls <-
  polls %>%
  mutate(Other = 1 - (PiS + KO + Lewica + Konfederacja + PO2050PSL))

polls <-
  polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("PiS", "KO", "Lewica", "PO2050PSL", "Konfederacja", "Other")])
  )



#####Run model#####
library(brms)
m1 <-
  brm(formula = bf(outcome ~ 1 + s(time, k = 10) + (1 | pollster)),
      family = dirichlet(link = "logit", refcat = "Other"),
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
        prior(normal(0, 1.5), class = "Intercept", dpar = "muPO2050PSL") +
        prior(normal(0, 0.5), class = "b", dpar = "muPO2050PSL") +
        prior(exponential(2), class = "sd", dpar = "muPO2050PSL") +
        prior(exponential(2), class = "sds", dpar = "muPO2050PSL") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muKonfederacja") +
        prior(normal(0, 0.5), class = "b", dpar = "muKonfederacja") +
        prior(exponential(2), class = "sd", dpar = "muKonfederacja") +
        prior(exponential(2), class = "sds", dpar = "muKonfederacja") +
        prior(gamma(1, 0.01), class = "phi"),
      data = polls,
      seed = 780045,
      iter = 5000,
      #backend="cmdstanr", chains=3, cores=3, threads = threading(3),
      refresh = 5,
      control =
        list(
          adapt_delta = .95,
          max_treedepth = 15
        )
  )


#####Trend plot#####
library(stringr)
library(hrbrthemes)
library(tidybayes)

today <- interval(min(polls$midDate), Sys.Date())/years(1)

# time <- seq(0, today, length.out = nrow(polls))
# 
# pred_dta <- polls %>%  
#   modelr::data_grid(time, date = as.Date(time*365, origin=min(polls$midDate)),
#                     pollster = pollster) %>% 
#   add_fitted_draws(m1, re_formula = NA)

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
        levels = c("PiS", "KO", "PO2050PSL", "Lewica", "Konfederacja", "Other"),
        labels = c("PiS", "KO", "Trzecia Droga", "Lewica", "Konfederacja", "Other")
      )
  )

point_dta <-
  polls[names(polls) %in% c("midDate", "PiS", "KO", "Lewica", "Konfederacja", "Other", "PO2050PSL")] %>%
  pivot_longer(
    cols = -midDate,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "PO2050PSL", "Lewica", "Konfederacja", "Other"),
        labels = c("PiS", "KO", "Trzecia Droga", "Lewica", "Konfederacja", "Other")
      )
  )

plot_trends_parl <-
  ggplot() +
  geom_point(data=point_dta, aes(x = midDate, y = est, colour = party, fill=party), alpha = .5, size = 1, show.legend=FALSE) +
  #stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0.95), alpha=1/2) +
  stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               labels = my_date_format()) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "", x="", title = "Trends", 
       subtitle=str_c("Data from ", names, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
  theme_plots()
ggsave(plot_trends_parl, file = "plot_trends_parl.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")

library(ggdist)
library(ggblend)

# trends_blend <- pred_dta %>%
#   ggplot(aes(x = date, color=party, fill=party)) +
#   ggdist::stat_lineribbon(
#     aes(y = .value, fill_ramp = stat(.width)),
#     .width = ppoints(100)
#   ) |> partition(vars(party)) |> blend("multiply") +
#   geom_point(data=point_dta, aes(x = midDate, y = est, colour = party, fill=party), size = 1, show.legend=FALSE) +
#   scale_color_manual(values=cols) +
#   scale_fill_manual(values=cols, guide=FALSE) +
#   ggdist::scale_fill_ramp_continuous(range = c(1, 0), guide=FALSE) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_x_date(date_breaks = "1 month",
#                labels = my_date_format()) +
#   coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
#                   ylim = c(0, .5)) +
#   labs(y = "", x="", title = "Trends",
#        subtitle=str_c("Data from ", names, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
#   guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
#   theme_plots()
# 
# ggsave(trends_blend, file = "trends_blend.pdf",
#        width = 7, height = 5, units = "cm", dpi = 320, scale = 5, bg="white", device=cairo_pdf)

Sys.setlocale("LC_TIME", "pl_PL.UTF-8")
plot_trends_parl_PL <-
  ggplot() +
  geom_point(data=point_dta, aes(x = midDate, y = est, colour = party, fill=party), alpha = .5, size = 1, show.legend=FALSE) +
  #stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0.95), alpha=1/2) +
  stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               labels = my_date_format()) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=c("PiS"="blue", "KO"="orange", "Lewica" = "red", "Konfederacja" = "midnightblue", "Trzecia Droga"="darkgreen", "Inni"="gray50")) +
  scale_fill_manual(values=c("PiS"="blue", "KO"="orange", "Lewica" = "red", "Konfederacja" = "midnightblue", "Trzecia Droga"="darkgreen", "Inni"="gray50"), guide=FALSE) +
  labs(y = "", x="", title = "Trendy",
       subtitle=str_c("Dane: ", names_PL, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
  theme_plots()
ggsave(plot_trends_parl_PL, file = "plot_trends_parl_PL.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")

#####Latest plot#####
plotdraws <- add_fitted_draws(
  model = m1,
  newdata =
    tibble(time = today),
  re_formula = NA
) %>%
  group_by(.category) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PO2050PSL"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "Trzecia Droga")))

medians <- plotdraws %>%
  summarise(est = median(.value)*100, .groups = "drop")

PiS.KO.diff <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(., PiSKO = PiS-KO,
         PiSKO = sum((PiSKO > 0) / length(PiSKO)),
         PiSKO = round(PiSKO, 2)) %>%
  pull(PiSKO) %>%
  last(.)

plot_latest_parl <-
  add_fitted_draws(
    model = m1,
    newdata =
      tibble(time = today),
    re_formula = NA
  ) %>%
  group_by(.category) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PO2050PSL"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "Trzecia Droga"))) %>%
  ggplot(aes(y=reorder(.category, dplyr::desc(-.value)), 
             x=.value, color=.category)) +
  geom_vline(aes(xintercept=0.05), colour="gray40", linetype="dotted") +
  stat_interval(aes(x=.value, color_ramp = stat(.width)), .width = ppoints(100)) %>%
  partition(vars(.category)) +
  scale_fill_manual(values=cols, guide=FALSE) +
  scale_color_manual(name=" ", values=cols, guide=FALSE) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_discrete(name="", position="right") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PiS"],0)),
           y="PiS", x=medians$est[medians$.category=="PiS"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="KO"],0)),
           y="KO", x=medians$est[medians$.category=="KO"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Trzecia Droga"],0)),
           y="Trzecia Droga", x=medians$est[medians$.category=="Trzecia Droga"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Lewica"],0)),
           y="Lewica", x=medians$est[medians$.category=="Lewica"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Konfederacja"],0)),
           y="Konfederacja", x=medians$est[medians$.category=="Konfederacja"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Other"],0)),
           y="Other", x=medians$est[medians$.category=="Other"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste("Pr(PiS > KO)  = ", PiS.KO.diff), y="PiS",
           x=quantile(plotdraws$.value[plotdraws$.category=="PiS"], 0.005), size=3.5, adj=c(1), family="IBM Plex Sans Condensed Light") +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Latest estimates",
       subtitle=str_c("Data from ", names,".")) +
  theme_plots()
ggsave(plot_latest_parl, file = "polls_latest_parl.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")

Sys.setlocale("LC_TIME", "pl_PL.UTF-8")
plot_latest_parl_PL <-
  add_fitted_draws(
    model = m1,
    newdata =
      tibble(time = today),
    re_formula = NA
  ) %>%
  group_by(.category) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PO2050PSL"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Inni", "Trzecia Droga"))) %>%
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
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Trzecia Droga"],0)),
           y="Trzecia Droga", x=medians$est[medians$.category=="Trzecia Droga"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Lewica"],0)),
           y="Lewica", x=medians$est[medians$.category=="Lewica"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Konfederacja"],0)),
           y="Konfederacja", x=medians$est[medians$.category=="Konfederacja"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Other"],0)),
           y="Inni", x=medians$est[medians$.category=="Other"]/100, size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste("Pr(PiS > KO)  = ", PiS.KO.diff), y="PiS",
           x=quantile(plotdraws$.value[plotdraws$.category=="PiS"], 0.005), size=3.5, adj=c(1), family="IBM Plex Sans Condensed Light") +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Poparcie dla partii politycznych", color="",
       subtitle=str_c("Dane: ", names_PL,".")) +
  theme_plots()
ggsave(plot_latest_parl_PL, file = "polls_latest_parl_PL.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")


#####Seat maps#####
library(seatdist)
library(maptools) 
library(rgeos) 
#library(gpclib)

median_PiS <- ifelse(medians$est[medians$.category=="PiS"] >=5, medians$est[medians$.category=="PiS"], 0)
median_KO <- ifelse(medians$est[medians$.category=="KO"] >=5, medians$est[medians$.category=="KO"], 0)
median_Lewica <- ifelse(medians$est[medians$.category=="Lewica"] >=5, medians$est[medians$.category=="Lewica"], 0)
median_Konfederacja <- ifelse(medians$est[medians$.category=="Konfederacja"] >=5, medians$est[medians$.category=="Konfederacja"], 0)
`median_Trzecia Droga` <- ifelse(medians$est[medians$.category=="Trzecia Droga"] >=5, medians$est[medians$.category=="Trzecia Droga"], 0)
PiSpct <- round(weights$PiScoef*median_PiS, digits=2)
KOpct <- round(weights$KOcoef*median_KO, digits=2)
Lewicapct <- round(weights$Lewicacoef*median_Lewica, digits=2)
Konfederacjapct <- round(weights$Konfcoef*median_Konfederacja, digits=2)
`Trzecia Drogapct` <- round(weights$PSLcoef*`median_Trzecia Droga`, digits=2)
MNpct <- c(0.17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
KOest <- (weights$validvotes/100)*KOpct
PiSest <- (weights$validvotes/100)*PiSpct
Lewicaest <- (weights$validvotes/100)*Lewicapct
Konfederacjaest <- (weights$validvotes/100)*Konfederacjapct
`Trzecia Drogaest` <- (weights$validvotes/100)*`Trzecia Drogapct`
MNest <- (weights$validvotes/100)*MNpct
poldHondt <- data.frame(KO=rep(1,42), Konfederacja=rep(1,42), Lewica=rep(1,42),  MN=rep(1,42), PiS=rep(1,42), `Trzecia Droga`=rep(1,42))

for( i in 1 : 42 ) {
  poldHondt[i,] <- c(giveseats(v = c(KOest[i], Konfederacjaest[i], Lewicaest[i], MNest[i], PiSest[i], 
                                     `Trzecia Drogaest`[i]), ns = weights$magnitude[i], method="dh", thresh=5))$seats
}

#seats table
seats <- cbind(poldHondt, weights)
row.names(seats) <- weights$name
keep <- c("KO","Konfederacja","Lewica","MN", "PiS", "Trzecia Droga")
colnames(seats) <- c("KO", "Konfederacja","Lewica","MN", "PiS", "Trzecia Droga")
seats <- seats[keep]
seats <- seats[-1,]
seats$id <- 1:41
seats$PiSKO <-abs(seats$PiS-seats$KO)
seats$PiSmKO <- seats$PiS-seats$KO

#regional maps
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
  aes(long,lat,group=group,fill=as.integer(`Trzecia Droga`)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Trzecia Droga", limits=c(min=0, max=20), low = "white", high = "darkgreen", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=`Trzecia Droga`, label=`Trzecia Droga`), fill="white") +
  labs(title="Constituency-level share of seats for Trzecia Droga", subtitle="Seat distribution reflects regional levels of support for PSL in 2019", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_p2050, file = "Polska_2050-PSL_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4, bg="white")

p_lewica <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Lewica)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Lewica", limits=c(min=0, max=20), low = "white", high = "red", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Lewica, label=Lewica), fill="white") +
  labs(title="Constituency-level share of seats for Lewica", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_lewica, file = "Lewica_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4, bg="white")

p_pis <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiS)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PiS", limits=c(min=0, max=20), low = "white", high = "blue", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiS, label=PiS), fill="white") +
  labs(title="Constituency-level share of seats for PiS", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_pis, file = "PiS_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4, bg="white")

p_ko <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(KO)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="KO", limits=c(min=0, max=20), low = "white", high = "orange", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=KO, label=KO), fill="white") +
  labs(title="Constituency-level share of seats for Koalicja Obywatelska", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_ko, file = "KO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4, bg="white")

p_konf <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Konfederacja)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Konfederacja", limits=c(min=0, max=20), low = "white", high = "midnightblue", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Konfederacja, label=Konfederacja), fill="white") +
  labs(title="Constituency-level share of seats for Konfederacja", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).")+
  theme_plots_map()
ggsave(p_konf, file = "Konf_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4, bg="white")

p_pis_ko <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiSmKO)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient2(name="PiSKO", limits=c(min=-20, max=20), low = "orange", mid="white", high = "blue", midpoint=0, guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiSKO, label=PiSKO), fill="white") +
  labs(title="Constituency-level differences in share of seats for PiS and Koalicja Obywatelska", subtitle="Constituencies in shades of blue have more PiS MPs; constituencies in orange have more KO MPs", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_pis_ko, file = "PiSKO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4, bg="white")


#####Seats plot#####
plotdraws <- add_fitted_draws(
  model = m1,
  newdata =
    tibble(time = today),
  re_formula = NA,
  n=1000
) %>%
  mutate(.draw = c(1:1000)) %>%
  group_by(.category, .draw) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL", "PO2050PSL"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL","Trzecia Droga")))

plotdraws <- plotdraws %>%
  pivot_wider(names_from = .category, values_from = .value) %>%
  mutate(magnitude=460) %>%
  select(., c(.draw, PiS, KO, Lewica, Konfederacja, `Trzecia Droga`, magnitude)) 

plotdraws$MN <- rnorm(1000, mean=0.079, sd=0.00001)

plotdraws <- plotdraws %>%
  mutate(., PiS = ifelse(PiS<0.05, 0, PiS),
         KO = ifelse(KO<0.05, 0, KO),
         Konfederacja = ifelse(Konfederacja<0.05, 0, Konfederacja),
         Lewica = ifelse(Lewica<0.05, 0, Lewica),
         `Trzecia Droga` = ifelse(`Trzecia Droga`<0.05, 0, `Trzecia Droga`)
  )

consts <- uncount(tibble(plotdraws), 41, .id="okreg")

consts <- consts %>%
  mutate(magnitude = case_when(okreg==1 ~ weights$magnitude[weights$okreg==1],
                               okreg==2 ~ weights$magnitude[weights$okreg==2],
                               okreg==3 ~ weights$magnitude[weights$okreg==3],
                               okreg==4 ~ weights$magnitude[weights$okreg==4],
                               okreg==5 ~ weights$magnitude[weights$okreg==5],
                               okreg==6 ~ weights$magnitude[weights$okreg==6],
                               okreg==7 ~ weights$magnitude[weights$okreg==7],
                               okreg==8 ~ weights$magnitude[weights$okreg==8],
                               okreg==9 ~ weights$magnitude[weights$okreg==9],
                               okreg==10 ~ weights$magnitude[weights$okreg==10],
                               okreg==11 ~ weights$magnitude[weights$okreg==11],
                               okreg==12 ~ weights$magnitude[weights$okreg==12],
                               okreg==13 ~ weights$magnitude[weights$okreg==13],
                               okreg==14 ~ weights$magnitude[weights$okreg==14],
                               okreg==15 ~ weights$magnitude[weights$okreg==15],
                               okreg==16 ~ weights$magnitude[weights$okreg==16],
                               okreg==17 ~ weights$magnitude[weights$okreg==17],
                               okreg==18 ~ weights$magnitude[weights$okreg==18],
                               okreg==19 ~ weights$magnitude[weights$okreg==19],
                               okreg==20 ~ weights$magnitude[weights$okreg==20],
                               okreg==21 ~ weights$magnitude[weights$okreg==21],
                               okreg==22 ~ weights$magnitude[weights$okreg==22],
                               okreg==23 ~ weights$magnitude[weights$okreg==23],
                               okreg==24 ~ weights$magnitude[weights$okreg==24],
                               okreg==25 ~ weights$magnitude[weights$okreg==25],
                               okreg==26 ~ weights$magnitude[weights$okreg==26],
                               okreg==27 ~ weights$magnitude[weights$okreg==27],
                               okreg==28 ~ weights$magnitude[weights$okreg==28],
                               okreg==29 ~ weights$magnitude[weights$okreg==29],
                               okreg==30 ~ weights$magnitude[weights$okreg==30],
                               okreg==31 ~ weights$magnitude[weights$okreg==31],
                               okreg==32 ~ weights$magnitude[weights$okreg==32],
                               okreg==33 ~ weights$magnitude[weights$okreg==33],
                               okreg==34 ~ weights$magnitude[weights$okreg==34],
                               okreg==35 ~ weights$magnitude[weights$okreg==35],
                               okreg==36 ~ weights$magnitude[weights$okreg==36],
                               okreg==37 ~ weights$magnitude[weights$okreg==37],
                               okreg==38 ~ weights$magnitude[weights$okreg==38],
                               okreg==39 ~ weights$magnitude[weights$okreg==39],
                               okreg==40 ~ weights$magnitude[weights$okreg==40],
                               okreg==41 ~ weights$magnitude[weights$okreg==41]
  ),
  PiS = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*PiS*weights$PiScoef[weights$okreg==1],
                  okreg==2 ~ (weights$validvotes[weights$okreg==2])*PiS*weights$PiScoef[weights$okreg==2],
                  okreg==3 ~ (weights$validvotes[weights$okreg==3])*PiS*weights$PiScoef[weights$okreg==3],
                  okreg==4 ~ (weights$validvotes[weights$okreg==4])*PiS*weights$PiScoef[weights$okreg==4],
                  okreg==5 ~ (weights$validvotes[weights$okreg==5])*PiS*weights$PiScoef[weights$okreg==5],
                  okreg==6 ~ (weights$validvotes[weights$okreg==6])*PiS*weights$PiScoef[weights$okreg==6],
                  okreg==7 ~ (weights$validvotes[weights$okreg==7])*PiS*weights$PiScoef[weights$okreg==7],
                  okreg==8 ~ (weights$validvotes[weights$okreg==8])*PiS*weights$PiScoef[weights$okreg==8],
                  okreg==9 ~ (weights$validvotes[weights$okreg==9])*PiS*weights$PiScoef[weights$okreg==9],
                  okreg==10 ~ (weights$validvotes[weights$okreg==10])*PiS*weights$PiScoef[weights$okreg==10],
                  okreg==11 ~ (weights$validvotes[weights$okreg==11])*PiS*weights$PiScoef[weights$okreg==11],
                  okreg==12 ~ (weights$validvotes[weights$okreg==12])*PiS*weights$PiScoef[weights$okreg==12],
                  okreg==13 ~ (weights$validvotes[weights$okreg==13])*PiS*weights$PiScoef[weights$okreg==13],
                  okreg==14 ~ (weights$validvotes[weights$okreg==14])*PiS*weights$PiScoef[weights$okreg==14],
                  okreg==15 ~ (weights$validvotes[weights$okreg==15])*PiS*weights$PiScoef[weights$okreg==15],
                  okreg==16 ~ (weights$validvotes[weights$okreg==16])*PiS*weights$PiScoef[weights$okreg==16],
                  okreg==17 ~ (weights$validvotes[weights$okreg==17])*PiS*weights$PiScoef[weights$okreg==17],
                  okreg==18 ~ (weights$validvotes[weights$okreg==18])*PiS*weights$PiScoef[weights$okreg==18],
                  okreg==19 ~ (weights$validvotes[weights$okreg==19])*PiS*weights$PiScoef[weights$okreg==19],
                  okreg==20 ~ (weights$validvotes[weights$okreg==20])*PiS*weights$PiScoef[weights$okreg==20],
                  okreg==21 ~ (weights$validvotes[weights$okreg==21])*PiS*weights$PiScoef[weights$okreg==21],
                  okreg==22 ~ (weights$validvotes[weights$okreg==22])*PiS*weights$PiScoef[weights$okreg==22],
                  okreg==23 ~ (weights$validvotes[weights$okreg==23])*PiS*weights$PiScoef[weights$okreg==23],
                  okreg==24 ~ (weights$validvotes[weights$okreg==24])*PiS*weights$PiScoef[weights$okreg==24],
                  okreg==25 ~ (weights$validvotes[weights$okreg==25])*PiS*weights$PiScoef[weights$okreg==25],
                  okreg==26 ~ (weights$validvotes[weights$okreg==26])*PiS*weights$PiScoef[weights$okreg==26],
                  okreg==27 ~ (weights$validvotes[weights$okreg==27])*PiS*weights$PiScoef[weights$okreg==27],
                  okreg==28 ~ (weights$validvotes[weights$okreg==28])*PiS*weights$PiScoef[weights$okreg==28],
                  okreg==29 ~ (weights$validvotes[weights$okreg==29])*PiS*weights$PiScoef[weights$okreg==29],
                  okreg==30 ~ (weights$validvotes[weights$okreg==30])*PiS*weights$PiScoef[weights$okreg==30],
                  okreg==31 ~ (weights$validvotes[weights$okreg==31])*PiS*weights$PiScoef[weights$okreg==31],
                  okreg==32 ~ (weights$validvotes[weights$okreg==32])*PiS*weights$PiScoef[weights$okreg==32],
                  okreg==33 ~ (weights$validvotes[weights$okreg==33])*PiS*weights$PiScoef[weights$okreg==33],
                  okreg==34 ~ (weights$validvotes[weights$okreg==34])*PiS*weights$PiScoef[weights$okreg==34],
                  okreg==35 ~ (weights$validvotes[weights$okreg==35])*PiS*weights$PiScoef[weights$okreg==35],
                  okreg==36 ~ (weights$validvotes[weights$okreg==36])*PiS*weights$PiScoef[weights$okreg==36],
                  okreg==37 ~ (weights$validvotes[weights$okreg==37])*PiS*weights$PiScoef[weights$okreg==37],
                  okreg==38 ~ (weights$validvotes[weights$okreg==38])*PiS*weights$PiScoef[weights$okreg==38],
                  okreg==39 ~ (weights$validvotes[weights$okreg==39])*PiS*weights$PiScoef[weights$okreg==39],
                  okreg==40 ~ (weights$validvotes[weights$okreg==40])*PiS*weights$PiScoef[weights$okreg==40],
                  okreg==41 ~ (weights$validvotes[weights$okreg==41])*PiS*weights$PiScoef[weights$okreg==41]
  ),
  KO = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*KO*weights$KOcoef[weights$okreg==1],
                 okreg==2 ~ (weights$validvotes[weights$okreg==2])*KO*weights$KOcoef[weights$okreg==2],
                 okreg==3 ~ (weights$validvotes[weights$okreg==3])*KO*weights$KOcoef[weights$okreg==3],
                 okreg==4 ~ (weights$validvotes[weights$okreg==4])*KO*weights$KOcoef[weights$okreg==4],
                 okreg==5 ~ (weights$validvotes[weights$okreg==5])*KO*weights$KOcoef[weights$okreg==5],
                 okreg==6 ~ (weights$validvotes[weights$okreg==6])*KO*weights$KOcoef[weights$okreg==6],
                 okreg==7 ~ (weights$validvotes[weights$okreg==7])*KO*weights$KOcoef[weights$okreg==7],
                 okreg==8 ~ (weights$validvotes[weights$okreg==8])*KO*weights$KOcoef[weights$okreg==8],
                 okreg==9 ~ (weights$validvotes[weights$okreg==9])*KO*weights$KOcoef[weights$okreg==9],
                 okreg==10 ~ (weights$validvotes[weights$okreg==10])*KO*weights$KOcoef[weights$okreg==10],
                 okreg==11 ~ (weights$validvotes[weights$okreg==11])*KO*weights$KOcoef[weights$okreg==11],
                 okreg==12 ~ (weights$validvotes[weights$okreg==12])*KO*weights$KOcoef[weights$okreg==12],
                 okreg==13 ~ (weights$validvotes[weights$okreg==13])*KO*weights$KOcoef[weights$okreg==13],
                 okreg==14 ~ (weights$validvotes[weights$okreg==14])*KO*weights$KOcoef[weights$okreg==14],
                 okreg==15 ~ (weights$validvotes[weights$okreg==15])*KO*weights$KOcoef[weights$okreg==15],
                 okreg==16 ~ (weights$validvotes[weights$okreg==16])*KO*weights$KOcoef[weights$okreg==16],
                 okreg==17 ~ (weights$validvotes[weights$okreg==17])*KO*weights$KOcoef[weights$okreg==17],
                 okreg==18 ~ (weights$validvotes[weights$okreg==18])*KO*weights$KOcoef[weights$okreg==18],
                 okreg==19 ~ (weights$validvotes[weights$okreg==19])*KO*weights$KOcoef[weights$okreg==19],
                 okreg==20 ~ (weights$validvotes[weights$okreg==20])*KO*weights$KOcoef[weights$okreg==20],
                 okreg==21 ~ (weights$validvotes[weights$okreg==21])*KO*weights$KOcoef[weights$okreg==21],
                 okreg==22 ~ (weights$validvotes[weights$okreg==22])*KO*weights$KOcoef[weights$okreg==22],
                 okreg==23 ~ (weights$validvotes[weights$okreg==23])*KO*weights$KOcoef[weights$okreg==23],
                 okreg==24 ~ (weights$validvotes[weights$okreg==24])*KO*weights$KOcoef[weights$okreg==24],
                 okreg==25 ~ (weights$validvotes[weights$okreg==25])*KO*weights$KOcoef[weights$okreg==25],
                 okreg==26 ~ (weights$validvotes[weights$okreg==26])*KO*weights$KOcoef[weights$okreg==26],
                 okreg==27 ~ (weights$validvotes[weights$okreg==27])*KO*weights$KOcoef[weights$okreg==27],
                 okreg==28 ~ (weights$validvotes[weights$okreg==28])*KO*weights$KOcoef[weights$okreg==28],
                 okreg==29 ~ (weights$validvotes[weights$okreg==29])*KO*weights$KOcoef[weights$okreg==29],
                 okreg==30 ~ (weights$validvotes[weights$okreg==30])*KO*weights$KOcoef[weights$okreg==30],
                 okreg==31 ~ (weights$validvotes[weights$okreg==31])*KO*weights$KOcoef[weights$okreg==31],
                 okreg==32 ~ (weights$validvotes[weights$okreg==32])*KO*weights$KOcoef[weights$okreg==32],
                 okreg==33 ~ (weights$validvotes[weights$okreg==33])*KO*weights$KOcoef[weights$okreg==33],
                 okreg==34 ~ (weights$validvotes[weights$okreg==34])*KO*weights$KOcoef[weights$okreg==34],
                 okreg==35 ~ (weights$validvotes[weights$okreg==35])*KO*weights$KOcoef[weights$okreg==35],
                 okreg==36 ~ (weights$validvotes[weights$okreg==36])*KO*weights$KOcoef[weights$okreg==36],
                 okreg==37 ~ (weights$validvotes[weights$okreg==37])*KO*weights$KOcoef[weights$okreg==37],
                 okreg==38 ~ (weights$validvotes[weights$okreg==38])*KO*weights$KOcoef[weights$okreg==38],
                 okreg==39 ~ (weights$validvotes[weights$okreg==39])*KO*weights$KOcoef[weights$okreg==39],
                 okreg==40 ~ (weights$validvotes[weights$okreg==40])*KO*weights$KOcoef[weights$okreg==40],
                 okreg==41 ~ (weights$validvotes[weights$okreg==41])*KO*weights$KOcoef[weights$okreg==41]
  ),
`Trzecia Droga` = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==1],
                  okreg==2 ~ (weights$validvotes[weights$okreg==2])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==2],
                  okreg==3 ~ (weights$validvotes[weights$okreg==3])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==3],
                  okreg==4 ~ (weights$validvotes[weights$okreg==4])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==4],
                  okreg==5 ~ (weights$validvotes[weights$okreg==5])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==5],
                  okreg==6 ~ (weights$validvotes[weights$okreg==6])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==6],
                  okreg==7 ~ (weights$validvotes[weights$okreg==7])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==7],
                  okreg==8 ~ (weights$validvotes[weights$okreg==8])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==8],
                  okreg==9 ~ (weights$validvotes[weights$okreg==9])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==9],
                  okreg==10 ~ (weights$validvotes[weights$okreg==10])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==10],
                  okreg==11 ~ (weights$validvotes[weights$okreg==11])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==11],
                  okreg==12 ~ (weights$validvotes[weights$okreg==12])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==12],
                  okreg==13 ~ (weights$validvotes[weights$okreg==13])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==13],
                  okreg==14 ~ (weights$validvotes[weights$okreg==14])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==14],
                  okreg==15 ~ (weights$validvotes[weights$okreg==15])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==15],
                  okreg==16 ~ (weights$validvotes[weights$okreg==16])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==16],
                  okreg==17 ~ (weights$validvotes[weights$okreg==17])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==17],
                  okreg==18 ~ (weights$validvotes[weights$okreg==18])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==18],
                  okreg==19 ~ (weights$validvotes[weights$okreg==19])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==19],
                  okreg==20 ~ (weights$validvotes[weights$okreg==20])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==20],
                  okreg==21 ~ (weights$validvotes[weights$okreg==21])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==21],
                  okreg==22 ~ (weights$validvotes[weights$okreg==22])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==22],
                  okreg==23 ~ (weights$validvotes[weights$okreg==23])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==23],
                  okreg==24 ~ (weights$validvotes[weights$okreg==24])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==24],
                  okreg==25 ~ (weights$validvotes[weights$okreg==25])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==25],
                  okreg==26 ~ (weights$validvotes[weights$okreg==26])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==26],
                  okreg==27 ~ (weights$validvotes[weights$okreg==27])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==27],
                  okreg==28 ~ (weights$validvotes[weights$okreg==28])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==28],
                  okreg==29 ~ (weights$validvotes[weights$okreg==29])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==29],
                  okreg==30 ~ (weights$validvotes[weights$okreg==30])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==30],
                  okreg==31 ~ (weights$validvotes[weights$okreg==31])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==31],
                  okreg==32 ~ (weights$validvotes[weights$okreg==32])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==32],
                  okreg==33 ~ (weights$validvotes[weights$okreg==33])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==33],
                  okreg==34 ~ (weights$validvotes[weights$okreg==34])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==34],
                  okreg==35 ~ (weights$validvotes[weights$okreg==35])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==35],
                  okreg==36 ~ (weights$validvotes[weights$okreg==36])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==36],
                  okreg==37 ~ (weights$validvotes[weights$okreg==37])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==37],
                  okreg==38 ~ (weights$validvotes[weights$okreg==38])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==38],
                  okreg==39 ~ (weights$validvotes[weights$okreg==39])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==39],
                  okreg==40 ~ (weights$validvotes[weights$okreg==40])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==40],
                  okreg==41 ~ (weights$validvotes[weights$okreg==41])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==41]
  ),
  Konfederacja = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*Konfederacja*weights$Konfcoef[weights$okreg==1],
                           okreg==2 ~ (weights$validvotes[weights$okreg==2])*Konfederacja*weights$Konfcoef[weights$okreg==2],
                           okreg==3 ~ (weights$validvotes[weights$okreg==3])*Konfederacja*weights$Konfcoef[weights$okreg==3],
                           okreg==4 ~ (weights$validvotes[weights$okreg==4])*Konfederacja*weights$Konfcoef[weights$okreg==4],
                           okreg==5 ~ (weights$validvotes[weights$okreg==5])*Konfederacja*weights$Konfcoef[weights$okreg==5],
                           okreg==6 ~ (weights$validvotes[weights$okreg==6])*Konfederacja*weights$Konfcoef[weights$okreg==6],
                           okreg==7 ~ (weights$validvotes[weights$okreg==7])*Konfederacja*weights$Konfcoef[weights$okreg==7],
                           okreg==8 ~ (weights$validvotes[weights$okreg==8])*Konfederacja*weights$Konfcoef[weights$okreg==8],
                           okreg==9 ~ (weights$validvotes[weights$okreg==9])*Konfederacja*weights$Konfcoef[weights$okreg==9],
                           okreg==10 ~ (weights$validvotes[weights$okreg==10])*Konfederacja*weights$Konfcoef[weights$okreg==10],
                           okreg==11 ~ (weights$validvotes[weights$okreg==11])*Konfederacja*weights$Konfcoef[weights$okreg==11],
                           okreg==12 ~ (weights$validvotes[weights$okreg==12])*Konfederacja*weights$Konfcoef[weights$okreg==12],
                           okreg==13 ~ (weights$validvotes[weights$okreg==13])*Konfederacja*weights$Konfcoef[weights$okreg==13],
                           okreg==14 ~ (weights$validvotes[weights$okreg==14])*Konfederacja*weights$Konfcoef[weights$okreg==14],
                           okreg==15 ~ (weights$validvotes[weights$okreg==15])*Konfederacja*weights$Konfcoef[weights$okreg==15],
                           okreg==16 ~ (weights$validvotes[weights$okreg==16])*Konfederacja*weights$Konfcoef[weights$okreg==16],
                           okreg==17 ~ (weights$validvotes[weights$okreg==17])*Konfederacja*weights$Konfcoef[weights$okreg==17],
                           okreg==18 ~ (weights$validvotes[weights$okreg==18])*Konfederacja*weights$Konfcoef[weights$okreg==18],
                           okreg==19 ~ (weights$validvotes[weights$okreg==19])*Konfederacja*weights$Konfcoef[weights$okreg==19],
                           okreg==20 ~ (weights$validvotes[weights$okreg==20])*Konfederacja*weights$Konfcoef[weights$okreg==20],
                           okreg==21 ~ (weights$validvotes[weights$okreg==21])*Konfederacja*weights$Konfcoef[weights$okreg==21],
                           okreg==22 ~ (weights$validvotes[weights$okreg==22])*Konfederacja*weights$Konfcoef[weights$okreg==22],
                           okreg==23 ~ (weights$validvotes[weights$okreg==23])*Konfederacja*weights$Konfcoef[weights$okreg==23],
                           okreg==24 ~ (weights$validvotes[weights$okreg==24])*Konfederacja*weights$Konfcoef[weights$okreg==24],
                           okreg==25 ~ (weights$validvotes[weights$okreg==25])*Konfederacja*weights$Konfcoef[weights$okreg==25],
                           okreg==26 ~ (weights$validvotes[weights$okreg==26])*Konfederacja*weights$Konfcoef[weights$okreg==26],
                           okreg==27 ~ (weights$validvotes[weights$okreg==27])*Konfederacja*weights$Konfcoef[weights$okreg==27],
                           okreg==28 ~ (weights$validvotes[weights$okreg==28])*Konfederacja*weights$Konfcoef[weights$okreg==28],
                           okreg==29 ~ (weights$validvotes[weights$okreg==29])*Konfederacja*weights$Konfcoef[weights$okreg==29],
                           okreg==30 ~ (weights$validvotes[weights$okreg==30])*Konfederacja*weights$Konfcoef[weights$okreg==30],
                           okreg==31 ~ (weights$validvotes[weights$okreg==31])*Konfederacja*weights$Konfcoef[weights$okreg==31],
                           okreg==32 ~ (weights$validvotes[weights$okreg==32])*Konfederacja*weights$Konfcoef[weights$okreg==32],
                           okreg==33 ~ (weights$validvotes[weights$okreg==33])*Konfederacja*weights$Konfcoef[weights$okreg==33],
                           okreg==34 ~ (weights$validvotes[weights$okreg==34])*Konfederacja*weights$Konfcoef[weights$okreg==34],
                           okreg==35 ~ (weights$validvotes[weights$okreg==35])*Konfederacja*weights$Konfcoef[weights$okreg==35],
                           okreg==36 ~ (weights$validvotes[weights$okreg==36])*Konfederacja*weights$Konfcoef[weights$okreg==36],
                           okreg==37 ~ (weights$validvotes[weights$okreg==37])*Konfederacja*weights$Konfcoef[weights$okreg==37],
                           okreg==38 ~ (weights$validvotes[weights$okreg==38])*Konfederacja*weights$Konfcoef[weights$okreg==38],
                           okreg==39 ~ (weights$validvotes[weights$okreg==39])*Konfederacja*weights$Konfcoef[weights$okreg==39],
                           okreg==40 ~ (weights$validvotes[weights$okreg==40])*Konfederacja*weights$Konfcoef[weights$okreg==40],
                           okreg==41 ~ (weights$validvotes[weights$okreg==41])*Konfederacja*weights$Konfcoef[weights$okreg==41]
  ),
  Lewica = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*Lewica*weights$Lewicacoef[weights$okreg==1],
                     okreg==2 ~ (weights$validvotes[weights$okreg==2])*Lewica*weights$Lewicacoef[weights$okreg==2],
                     okreg==3 ~ (weights$validvotes[weights$okreg==3])*Lewica*weights$Lewicacoef[weights$okreg==3],
                     okreg==4 ~ (weights$validvotes[weights$okreg==4])*Lewica*weights$Lewicacoef[weights$okreg==4],
                     okreg==5 ~ (weights$validvotes[weights$okreg==5])*Lewica*weights$Lewicacoef[weights$okreg==5],
                     okreg==6 ~ (weights$validvotes[weights$okreg==6])*Lewica*weights$Lewicacoef[weights$okreg==6],
                     okreg==7 ~ (weights$validvotes[weights$okreg==7])*Lewica*weights$Lewicacoef[weights$okreg==7],
                     okreg==8 ~ (weights$validvotes[weights$okreg==8])*Lewica*weights$Lewicacoef[weights$okreg==8],
                     okreg==9 ~ (weights$validvotes[weights$okreg==9])*Lewica*weights$Lewicacoef[weights$okreg==9],
                     okreg==10 ~ (weights$validvotes[weights$okreg==10])*Lewica*weights$Lewicacoef[weights$okreg==10],
                     okreg==11 ~ (weights$validvotes[weights$okreg==11])*Lewica*weights$Lewicacoef[weights$okreg==11],
                     okreg==12 ~ (weights$validvotes[weights$okreg==12])*Lewica*weights$Lewicacoef[weights$okreg==12],
                     okreg==13 ~ (weights$validvotes[weights$okreg==13])*Lewica*weights$Lewicacoef[weights$okreg==13],
                     okreg==14 ~ (weights$validvotes[weights$okreg==14])*Lewica*weights$Lewicacoef[weights$okreg==14],
                     okreg==15 ~ (weights$validvotes[weights$okreg==15])*Lewica*weights$Lewicacoef[weights$okreg==15],
                     okreg==16 ~ (weights$validvotes[weights$okreg==16])*Lewica*weights$Lewicacoef[weights$okreg==16],
                     okreg==17 ~ (weights$validvotes[weights$okreg==17])*Lewica*weights$Lewicacoef[weights$okreg==17],
                     okreg==18 ~ (weights$validvotes[weights$okreg==18])*Lewica*weights$Lewicacoef[weights$okreg==18],
                     okreg==19 ~ (weights$validvotes[weights$okreg==19])*Lewica*weights$Lewicacoef[weights$okreg==19],
                     okreg==20 ~ (weights$validvotes[weights$okreg==20])*Lewica*weights$Lewicacoef[weights$okreg==20],
                     okreg==21 ~ (weights$validvotes[weights$okreg==21])*Lewica*weights$Lewicacoef[weights$okreg==21],
                     okreg==22 ~ (weights$validvotes[weights$okreg==22])*Lewica*weights$Lewicacoef[weights$okreg==22],
                     okreg==23 ~ (weights$validvotes[weights$okreg==23])*Lewica*weights$Lewicacoef[weights$okreg==23],
                     okreg==24 ~ (weights$validvotes[weights$okreg==24])*Lewica*weights$Lewicacoef[weights$okreg==24],
                     okreg==25 ~ (weights$validvotes[weights$okreg==25])*Lewica*weights$Lewicacoef[weights$okreg==25],
                     okreg==26 ~ (weights$validvotes[weights$okreg==26])*Lewica*weights$Lewicacoef[weights$okreg==26],
                     okreg==27 ~ (weights$validvotes[weights$okreg==27])*Lewica*weights$Lewicacoef[weights$okreg==27],
                     okreg==28 ~ (weights$validvotes[weights$okreg==28])*Lewica*weights$Lewicacoef[weights$okreg==28],
                     okreg==29 ~ (weights$validvotes[weights$okreg==29])*Lewica*weights$Lewicacoef[weights$okreg==29],
                     okreg==30 ~ (weights$validvotes[weights$okreg==30])*Lewica*weights$Lewicacoef[weights$okreg==30],
                     okreg==31 ~ (weights$validvotes[weights$okreg==31])*Lewica*weights$Lewicacoef[weights$okreg==31],
                     okreg==32 ~ (weights$validvotes[weights$okreg==32])*Lewica*weights$Lewicacoef[weights$okreg==32],
                     okreg==33 ~ (weights$validvotes[weights$okreg==33])*Lewica*weights$Lewicacoef[weights$okreg==33],
                     okreg==34 ~ (weights$validvotes[weights$okreg==34])*Lewica*weights$Lewicacoef[weights$okreg==34],
                     okreg==35 ~ (weights$validvotes[weights$okreg==35])*Lewica*weights$Lewicacoef[weights$okreg==35],
                     okreg==36 ~ (weights$validvotes[weights$okreg==36])*Lewica*weights$Lewicacoef[weights$okreg==36],
                     okreg==37 ~ (weights$validvotes[weights$okreg==37])*Lewica*weights$Lewicacoef[weights$okreg==37],
                     okreg==38 ~ (weights$validvotes[weights$okreg==38])*Lewica*weights$Lewicacoef[weights$okreg==38],
                     okreg==39 ~ (weights$validvotes[weights$okreg==39])*Lewica*weights$Lewicacoef[weights$okreg==39],
                     okreg==40 ~ (weights$validvotes[weights$okreg==40])*Lewica*weights$Lewicacoef[weights$okreg==40],
                     okreg==41 ~ (weights$validvotes[weights$okreg==41])*Lewica*weights$Lewicacoef[weights$okreg==41]
  ),
  MN = case_when(okreg==1 ~ 0,
                 okreg==2 ~ 0,
                 okreg==3 ~ 0,
                 okreg==4 ~ 0,
                 okreg==5 ~ 0,
                 okreg==6 ~ 0,
                 okreg==7 ~ 0,
                 okreg==8 ~ 0,
                 okreg==9 ~ 0,
                 okreg==10 ~ 0,
                 okreg==11 ~ 0,
                 okreg==12 ~ 0,
                 okreg==13 ~ 0,
                 okreg==14 ~ 0,
                 okreg==15 ~ 0,
                 okreg==16 ~ 0,
                 okreg==17 ~ 0,
                 okreg==18 ~ 0,
                 okreg==19 ~ 0,
                 okreg==20 ~ 0,
                 okreg==21 ~ (weights$validvotes[weights$okreg==21])*MN,
                 okreg==22 ~ 0,
                 okreg==23 ~ 0,
                 okreg==24 ~ 0,
                 okreg==25 ~ 0,
                 okreg==26 ~ 0,
                 okreg==27 ~ 0,
                 okreg==28 ~ 0,
                 okreg==29 ~ 0,
                 okreg==30 ~ 0,
                 okreg==31 ~ 0,
                 okreg==32 ~ 0,
                 okreg==33 ~ 0,
                 okreg==34 ~ 0,
                 okreg==35 ~ 0,
                 okreg==36 ~ 0,
                 okreg==37 ~ 0,
                 okreg==38 ~ 0,
                 okreg==39 ~ 0,
                 okreg==40 ~ 0,
                 okreg==41 ~ 0
  )
  )

poldHondt <- data.frame(KO=rep(1,41000), Konfederacja=rep(1,41000), Lewica=rep(1,41000), MN=rep(1,41000),  
                        PiS=rep(1,41000), `Trzecia Droga`=rep(1,41000))

for(i in 1:41000) { 
  poldHondt[i,] <- giveseats(v = c(consts$KO[i], consts$Konfederacja[i], consts$Lewica[i], consts$MN[i],
                                   consts$PiS[i], consts$`Trzecia Droga`[i]), ns = consts$magnitude[i], method="dh", thresh=0)$seats
}

poldHondt <- cbind(poldHondt, consts$okreg, consts$.draw)

colnames(poldHondt) <- c("KO", "Konfederacja", "Lewica", "MN", "PiS", "Trzecia Droga", "okreg", "draw")

poldHondt <- poldHondt %>% 
  group_by(draw) %>% 
  summarise(KO = sum(KO),
            PiS = sum(PiS),
            Konfederacja = sum(Konfederacja),
            `Trzecia Droga` = sum(`Trzecia Droga`),
            MN = sum(MN),
            Lewica = sum(Lewica))

poldHondt <- poldHondt %>%
  pivot_longer(., cols=c("KO", "Konfederacja", "Lewica", "MN", "PiS", "Trzecia Droga"), names_to="party", values_to="seats")

PiS_seats <- poldHondt %>%
  filter(., party=="PiS") %>%
  hypothesis(., "seats>230")

frame <- poldHondt %>%
  group_by(party) %>%
  summarise(mean_qi(seats)) %>%
  mutate(., y = round(y, 0),
         ymin = round(ymin, 0),
         ymax = round(ymax, 0))

frame$in2019[frame$party=="KO"] <- 134
frame$in2019[frame$party=="PiS"] <- 235
frame$in2019[frame$party=="Lewica"] <- 49
frame$in2019[frame$party=="MN"] <- 1
frame$in2019[frame$party=="Konfederacja"] <- 11
frame$in2019[frame$party=="Trzecia Droga"] <- 30
frame$party <- factor(frame$party, levels=c("PiS", "KO", "Lewica", "Konfederacja", "Trzecia Droga", "MN"))
frame$diffPres <- sprintf("%+d", (frame$y - frame$in2019))
frame$diffPres <- sprintf("(%s)", frame$diffPres)
frame$party <- reorder(frame$party, -frame$y)

plotdraws <- add_fitted_draws(
  model = m1,
  newdata =
    tibble(time = today),
  re_formula = NA,
  n=1000
) %>%
  mutate(.draw = c(1:1000)) %>%
  group_by(.category, .draw) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PO2050PSL"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "Trzecia Droga")))

plotdraws <- plotdraws %>%
  pivot_wider(names_from = .category, values_from = .value) %>%
  mutate(magnitude=460) %>%
  select(., c(.draw, PiS, KO, Lewica, Konfederacja, `Trzecia Droga`, magnitude)) 

plotdraws$MN <- rnorm(1000, mean=0.079, sd=0.00001)

plotdraws <- plotdraws %>%
  mutate(., PiS = ifelse(PiS<0.05, 0, PiS),
         KO = ifelse(KO<0.05, 0, KO),
         Konfederacja = ifelse(Konfederacja<0.05, 0, Konfederacja),
         Lewica = ifelse(Lewica<0.05, 0, Lewica),
         `Trzecia Droga` = ifelse(`Trzecia Droga`<0.05, 0, `Trzecia Droga`),
         KO = ifelse(median_KO<5, 0, KO),
         Konfederacja = ifelse(median_Konfederacja<5, 0, Konfederacja),
         Lewica = ifelse(median_Lewica<5, 0, Lewica),
         `Trzecia Droga` = ifelse(`median_Trzecia Droga`<5, 0, `Trzecia Droga`)
  )

consts <- uncount(tibble(plotdraws), 41, .id="okreg")

consts <- consts %>%
  mutate(magnitude = case_when(okreg==1 ~ weights$magnitude[weights$okreg==1],
                               okreg==2 ~ weights$magnitude[weights$okreg==2],
                               okreg==3 ~ weights$magnitude[weights$okreg==3],
                               okreg==4 ~ weights$magnitude[weights$okreg==4],
                               okreg==5 ~ weights$magnitude[weights$okreg==5],
                               okreg==6 ~ weights$magnitude[weights$okreg==6],
                               okreg==7 ~ weights$magnitude[weights$okreg==7],
                               okreg==8 ~ weights$magnitude[weights$okreg==8],
                               okreg==9 ~ weights$magnitude[weights$okreg==9],
                               okreg==10 ~ weights$magnitude[weights$okreg==10],
                               okreg==11 ~ weights$magnitude[weights$okreg==11],
                               okreg==12 ~ weights$magnitude[weights$okreg==12],
                               okreg==13 ~ weights$magnitude[weights$okreg==13],
                               okreg==14 ~ weights$magnitude[weights$okreg==14],
                               okreg==15 ~ weights$magnitude[weights$okreg==15],
                               okreg==16 ~ weights$magnitude[weights$okreg==16],
                               okreg==17 ~ weights$magnitude[weights$okreg==17],
                               okreg==18 ~ weights$magnitude[weights$okreg==18],
                               okreg==19 ~ weights$magnitude[weights$okreg==19],
                               okreg==20 ~ weights$magnitude[weights$okreg==20],
                               okreg==21 ~ weights$magnitude[weights$okreg==21],
                               okreg==22 ~ weights$magnitude[weights$okreg==22],
                               okreg==23 ~ weights$magnitude[weights$okreg==23],
                               okreg==24 ~ weights$magnitude[weights$okreg==24],
                               okreg==25 ~ weights$magnitude[weights$okreg==25],
                               okreg==26 ~ weights$magnitude[weights$okreg==26],
                               okreg==27 ~ weights$magnitude[weights$okreg==27],
                               okreg==28 ~ weights$magnitude[weights$okreg==28],
                               okreg==29 ~ weights$magnitude[weights$okreg==29],
                               okreg==30 ~ weights$magnitude[weights$okreg==30],
                               okreg==31 ~ weights$magnitude[weights$okreg==31],
                               okreg==32 ~ weights$magnitude[weights$okreg==32],
                               okreg==33 ~ weights$magnitude[weights$okreg==33],
                               okreg==34 ~ weights$magnitude[weights$okreg==34],
                               okreg==35 ~ weights$magnitude[weights$okreg==35],
                               okreg==36 ~ weights$magnitude[weights$okreg==36],
                               okreg==37 ~ weights$magnitude[weights$okreg==37],
                               okreg==38 ~ weights$magnitude[weights$okreg==38],
                               okreg==39 ~ weights$magnitude[weights$okreg==39],
                               okreg==40 ~ weights$magnitude[weights$okreg==40],
                               okreg==41 ~ weights$magnitude[weights$okreg==41]
  ),
  PiS = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*PiS*weights$PiScoef[weights$okreg==1],
                  okreg==2 ~ (weights$validvotes[weights$okreg==2])*PiS*weights$PiScoef[weights$okreg==2],
                  okreg==3 ~ (weights$validvotes[weights$okreg==3])*PiS*weights$PiScoef[weights$okreg==3],
                  okreg==4 ~ (weights$validvotes[weights$okreg==4])*PiS*weights$PiScoef[weights$okreg==4],
                  okreg==5 ~ (weights$validvotes[weights$okreg==5])*PiS*weights$PiScoef[weights$okreg==5],
                  okreg==6 ~ (weights$validvotes[weights$okreg==6])*PiS*weights$PiScoef[weights$okreg==6],
                  okreg==7 ~ (weights$validvotes[weights$okreg==7])*PiS*weights$PiScoef[weights$okreg==7],
                  okreg==8 ~ (weights$validvotes[weights$okreg==8])*PiS*weights$PiScoef[weights$okreg==8],
                  okreg==9 ~ (weights$validvotes[weights$okreg==9])*PiS*weights$PiScoef[weights$okreg==9],
                  okreg==10 ~ (weights$validvotes[weights$okreg==10])*PiS*weights$PiScoef[weights$okreg==10],
                  okreg==11 ~ (weights$validvotes[weights$okreg==11])*PiS*weights$PiScoef[weights$okreg==11],
                  okreg==12 ~ (weights$validvotes[weights$okreg==12])*PiS*weights$PiScoef[weights$okreg==12],
                  okreg==13 ~ (weights$validvotes[weights$okreg==13])*PiS*weights$PiScoef[weights$okreg==13],
                  okreg==14 ~ (weights$validvotes[weights$okreg==14])*PiS*weights$PiScoef[weights$okreg==14],
                  okreg==15 ~ (weights$validvotes[weights$okreg==15])*PiS*weights$PiScoef[weights$okreg==15],
                  okreg==16 ~ (weights$validvotes[weights$okreg==16])*PiS*weights$PiScoef[weights$okreg==16],
                  okreg==17 ~ (weights$validvotes[weights$okreg==17])*PiS*weights$PiScoef[weights$okreg==17],
                  okreg==18 ~ (weights$validvotes[weights$okreg==18])*PiS*weights$PiScoef[weights$okreg==18],
                  okreg==19 ~ (weights$validvotes[weights$okreg==19])*PiS*weights$PiScoef[weights$okreg==19],
                  okreg==20 ~ (weights$validvotes[weights$okreg==20])*PiS*weights$PiScoef[weights$okreg==20],
                  okreg==21 ~ (weights$validvotes[weights$okreg==21])*PiS*weights$PiScoef[weights$okreg==21],
                  okreg==22 ~ (weights$validvotes[weights$okreg==22])*PiS*weights$PiScoef[weights$okreg==22],
                  okreg==23 ~ (weights$validvotes[weights$okreg==23])*PiS*weights$PiScoef[weights$okreg==23],
                  okreg==24 ~ (weights$validvotes[weights$okreg==24])*PiS*weights$PiScoef[weights$okreg==24],
                  okreg==25 ~ (weights$validvotes[weights$okreg==25])*PiS*weights$PiScoef[weights$okreg==25],
                  okreg==26 ~ (weights$validvotes[weights$okreg==26])*PiS*weights$PiScoef[weights$okreg==26],
                  okreg==27 ~ (weights$validvotes[weights$okreg==27])*PiS*weights$PiScoef[weights$okreg==27],
                  okreg==28 ~ (weights$validvotes[weights$okreg==28])*PiS*weights$PiScoef[weights$okreg==28],
                  okreg==29 ~ (weights$validvotes[weights$okreg==29])*PiS*weights$PiScoef[weights$okreg==29],
                  okreg==30 ~ (weights$validvotes[weights$okreg==30])*PiS*weights$PiScoef[weights$okreg==30],
                  okreg==31 ~ (weights$validvotes[weights$okreg==31])*PiS*weights$PiScoef[weights$okreg==31],
                  okreg==32 ~ (weights$validvotes[weights$okreg==32])*PiS*weights$PiScoef[weights$okreg==32],
                  okreg==33 ~ (weights$validvotes[weights$okreg==33])*PiS*weights$PiScoef[weights$okreg==33],
                  okreg==34 ~ (weights$validvotes[weights$okreg==34])*PiS*weights$PiScoef[weights$okreg==34],
                  okreg==35 ~ (weights$validvotes[weights$okreg==35])*PiS*weights$PiScoef[weights$okreg==35],
                  okreg==36 ~ (weights$validvotes[weights$okreg==36])*PiS*weights$PiScoef[weights$okreg==36],
                  okreg==37 ~ (weights$validvotes[weights$okreg==37])*PiS*weights$PiScoef[weights$okreg==37],
                  okreg==38 ~ (weights$validvotes[weights$okreg==38])*PiS*weights$PiScoef[weights$okreg==38],
                  okreg==39 ~ (weights$validvotes[weights$okreg==39])*PiS*weights$PiScoef[weights$okreg==39],
                  okreg==40 ~ (weights$validvotes[weights$okreg==40])*PiS*weights$PiScoef[weights$okreg==40],
                  okreg==41 ~ (weights$validvotes[weights$okreg==41])*PiS*weights$PiScoef[weights$okreg==41]
  ),
  KO = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*KO*weights$KOcoef[weights$okreg==1],
                 okreg==2 ~ (weights$validvotes[weights$okreg==2])*KO*weights$KOcoef[weights$okreg==2],
                 okreg==3 ~ (weights$validvotes[weights$okreg==3])*KO*weights$KOcoef[weights$okreg==3],
                 okreg==4 ~ (weights$validvotes[weights$okreg==4])*KO*weights$KOcoef[weights$okreg==4],
                 okreg==5 ~ (weights$validvotes[weights$okreg==5])*KO*weights$KOcoef[weights$okreg==5],
                 okreg==6 ~ (weights$validvotes[weights$okreg==6])*KO*weights$KOcoef[weights$okreg==6],
                 okreg==7 ~ (weights$validvotes[weights$okreg==7])*KO*weights$KOcoef[weights$okreg==7],
                 okreg==8 ~ (weights$validvotes[weights$okreg==8])*KO*weights$KOcoef[weights$okreg==8],
                 okreg==9 ~ (weights$validvotes[weights$okreg==9])*KO*weights$KOcoef[weights$okreg==9],
                 okreg==10 ~ (weights$validvotes[weights$okreg==10])*KO*weights$KOcoef[weights$okreg==10],
                 okreg==11 ~ (weights$validvotes[weights$okreg==11])*KO*weights$KOcoef[weights$okreg==11],
                 okreg==12 ~ (weights$validvotes[weights$okreg==12])*KO*weights$KOcoef[weights$okreg==12],
                 okreg==13 ~ (weights$validvotes[weights$okreg==13])*KO*weights$KOcoef[weights$okreg==13],
                 okreg==14 ~ (weights$validvotes[weights$okreg==14])*KO*weights$KOcoef[weights$okreg==14],
                 okreg==15 ~ (weights$validvotes[weights$okreg==15])*KO*weights$KOcoef[weights$okreg==15],
                 okreg==16 ~ (weights$validvotes[weights$okreg==16])*KO*weights$KOcoef[weights$okreg==16],
                 okreg==17 ~ (weights$validvotes[weights$okreg==17])*KO*weights$KOcoef[weights$okreg==17],
                 okreg==18 ~ (weights$validvotes[weights$okreg==18])*KO*weights$KOcoef[weights$okreg==18],
                 okreg==19 ~ (weights$validvotes[weights$okreg==19])*KO*weights$KOcoef[weights$okreg==19],
                 okreg==20 ~ (weights$validvotes[weights$okreg==20])*KO*weights$KOcoef[weights$okreg==20],
                 okreg==21 ~ (weights$validvotes[weights$okreg==21])*KO*weights$KOcoef[weights$okreg==21],
                 okreg==22 ~ (weights$validvotes[weights$okreg==22])*KO*weights$KOcoef[weights$okreg==22],
                 okreg==23 ~ (weights$validvotes[weights$okreg==23])*KO*weights$KOcoef[weights$okreg==23],
                 okreg==24 ~ (weights$validvotes[weights$okreg==24])*KO*weights$KOcoef[weights$okreg==24],
                 okreg==25 ~ (weights$validvotes[weights$okreg==25])*KO*weights$KOcoef[weights$okreg==25],
                 okreg==26 ~ (weights$validvotes[weights$okreg==26])*KO*weights$KOcoef[weights$okreg==26],
                 okreg==27 ~ (weights$validvotes[weights$okreg==27])*KO*weights$KOcoef[weights$okreg==27],
                 okreg==28 ~ (weights$validvotes[weights$okreg==28])*KO*weights$KOcoef[weights$okreg==28],
                 okreg==29 ~ (weights$validvotes[weights$okreg==29])*KO*weights$KOcoef[weights$okreg==29],
                 okreg==30 ~ (weights$validvotes[weights$okreg==30])*KO*weights$KOcoef[weights$okreg==30],
                 okreg==31 ~ (weights$validvotes[weights$okreg==31])*KO*weights$KOcoef[weights$okreg==31],
                 okreg==32 ~ (weights$validvotes[weights$okreg==32])*KO*weights$KOcoef[weights$okreg==32],
                 okreg==33 ~ (weights$validvotes[weights$okreg==33])*KO*weights$KOcoef[weights$okreg==33],
                 okreg==34 ~ (weights$validvotes[weights$okreg==34])*KO*weights$KOcoef[weights$okreg==34],
                 okreg==35 ~ (weights$validvotes[weights$okreg==35])*KO*weights$KOcoef[weights$okreg==35],
                 okreg==36 ~ (weights$validvotes[weights$okreg==36])*KO*weights$KOcoef[weights$okreg==36],
                 okreg==37 ~ (weights$validvotes[weights$okreg==37])*KO*weights$KOcoef[weights$okreg==37],
                 okreg==38 ~ (weights$validvotes[weights$okreg==38])*KO*weights$KOcoef[weights$okreg==38],
                 okreg==39 ~ (weights$validvotes[weights$okreg==39])*KO*weights$KOcoef[weights$okreg==39],
                 okreg==40 ~ (weights$validvotes[weights$okreg==40])*KO*weights$KOcoef[weights$okreg==40],
                 okreg==41 ~ (weights$validvotes[weights$okreg==41])*KO*weights$KOcoef[weights$okreg==41]
  ),
  `Trzecia Droga` = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==1],
                                okreg==2 ~ (weights$validvotes[weights$okreg==2])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==2],
                                okreg==3 ~ (weights$validvotes[weights$okreg==3])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==3],
                                okreg==4 ~ (weights$validvotes[weights$okreg==4])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==4],
                                okreg==5 ~ (weights$validvotes[weights$okreg==5])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==5],
                                okreg==6 ~ (weights$validvotes[weights$okreg==6])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==6],
                                okreg==7 ~ (weights$validvotes[weights$okreg==7])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==7],
                                okreg==8 ~ (weights$validvotes[weights$okreg==8])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==8],
                                okreg==9 ~ (weights$validvotes[weights$okreg==9])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==9],
                                okreg==10 ~ (weights$validvotes[weights$okreg==10])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==10],
                                okreg==11 ~ (weights$validvotes[weights$okreg==11])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==11],
                                okreg==12 ~ (weights$validvotes[weights$okreg==12])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==12],
                                okreg==13 ~ (weights$validvotes[weights$okreg==13])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==13],
                                okreg==14 ~ (weights$validvotes[weights$okreg==14])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==14],
                                okreg==15 ~ (weights$validvotes[weights$okreg==15])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==15],
                                okreg==16 ~ (weights$validvotes[weights$okreg==16])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==16],
                                okreg==17 ~ (weights$validvotes[weights$okreg==17])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==17],
                                okreg==18 ~ (weights$validvotes[weights$okreg==18])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==18],
                                okreg==19 ~ (weights$validvotes[weights$okreg==19])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==19],
                                okreg==20 ~ (weights$validvotes[weights$okreg==20])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==20],
                                okreg==21 ~ (weights$validvotes[weights$okreg==21])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==21],
                                okreg==22 ~ (weights$validvotes[weights$okreg==22])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==22],
                                okreg==23 ~ (weights$validvotes[weights$okreg==23])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==23],
                                okreg==24 ~ (weights$validvotes[weights$okreg==24])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==24],
                                okreg==25 ~ (weights$validvotes[weights$okreg==25])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==25],
                                okreg==26 ~ (weights$validvotes[weights$okreg==26])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==26],
                                okreg==27 ~ (weights$validvotes[weights$okreg==27])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==27],
                                okreg==28 ~ (weights$validvotes[weights$okreg==28])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==28],
                                okreg==29 ~ (weights$validvotes[weights$okreg==29])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==29],
                                okreg==30 ~ (weights$validvotes[weights$okreg==30])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==30],
                                okreg==31 ~ (weights$validvotes[weights$okreg==31])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==31],
                                okreg==32 ~ (weights$validvotes[weights$okreg==32])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==32],
                                okreg==33 ~ (weights$validvotes[weights$okreg==33])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==33],
                                okreg==34 ~ (weights$validvotes[weights$okreg==34])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==34],
                                okreg==35 ~ (weights$validvotes[weights$okreg==35])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==35],
                                okreg==36 ~ (weights$validvotes[weights$okreg==36])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==36],
                                okreg==37 ~ (weights$validvotes[weights$okreg==37])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==37],
                                okreg==38 ~ (weights$validvotes[weights$okreg==38])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==38],
                                okreg==39 ~ (weights$validvotes[weights$okreg==39])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==39],
                                okreg==40 ~ (weights$validvotes[weights$okreg==40])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==40],
                                okreg==41 ~ (weights$validvotes[weights$okreg==41])*`Trzecia Droga`*weights$PSLcoef[weights$okreg==41]
  ),
  Konfederacja = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*Konfederacja*weights$Konfcoef[weights$okreg==1],
                           okreg==2 ~ (weights$validvotes[weights$okreg==2])*Konfederacja*weights$Konfcoef[weights$okreg==2],
                           okreg==3 ~ (weights$validvotes[weights$okreg==3])*Konfederacja*weights$Konfcoef[weights$okreg==3],
                           okreg==4 ~ (weights$validvotes[weights$okreg==4])*Konfederacja*weights$Konfcoef[weights$okreg==4],
                           okreg==5 ~ (weights$validvotes[weights$okreg==5])*Konfederacja*weights$Konfcoef[weights$okreg==5],
                           okreg==6 ~ (weights$validvotes[weights$okreg==6])*Konfederacja*weights$Konfcoef[weights$okreg==6],
                           okreg==7 ~ (weights$validvotes[weights$okreg==7])*Konfederacja*weights$Konfcoef[weights$okreg==7],
                           okreg==8 ~ (weights$validvotes[weights$okreg==8])*Konfederacja*weights$Konfcoef[weights$okreg==8],
                           okreg==9 ~ (weights$validvotes[weights$okreg==9])*Konfederacja*weights$Konfcoef[weights$okreg==9],
                           okreg==10 ~ (weights$validvotes[weights$okreg==10])*Konfederacja*weights$Konfcoef[weights$okreg==10],
                           okreg==11 ~ (weights$validvotes[weights$okreg==11])*Konfederacja*weights$Konfcoef[weights$okreg==11],
                           okreg==12 ~ (weights$validvotes[weights$okreg==12])*Konfederacja*weights$Konfcoef[weights$okreg==12],
                           okreg==13 ~ (weights$validvotes[weights$okreg==13])*Konfederacja*weights$Konfcoef[weights$okreg==13],
                           okreg==14 ~ (weights$validvotes[weights$okreg==14])*Konfederacja*weights$Konfcoef[weights$okreg==14],
                           okreg==15 ~ (weights$validvotes[weights$okreg==15])*Konfederacja*weights$Konfcoef[weights$okreg==15],
                           okreg==16 ~ (weights$validvotes[weights$okreg==16])*Konfederacja*weights$Konfcoef[weights$okreg==16],
                           okreg==17 ~ (weights$validvotes[weights$okreg==17])*Konfederacja*weights$Konfcoef[weights$okreg==17],
                           okreg==18 ~ (weights$validvotes[weights$okreg==18])*Konfederacja*weights$Konfcoef[weights$okreg==18],
                           okreg==19 ~ (weights$validvotes[weights$okreg==19])*Konfederacja*weights$Konfcoef[weights$okreg==19],
                           okreg==20 ~ (weights$validvotes[weights$okreg==20])*Konfederacja*weights$Konfcoef[weights$okreg==20],
                           okreg==21 ~ (weights$validvotes[weights$okreg==21])*Konfederacja*weights$Konfcoef[weights$okreg==21],
                           okreg==22 ~ (weights$validvotes[weights$okreg==22])*Konfederacja*weights$Konfcoef[weights$okreg==22],
                           okreg==23 ~ (weights$validvotes[weights$okreg==23])*Konfederacja*weights$Konfcoef[weights$okreg==23],
                           okreg==24 ~ (weights$validvotes[weights$okreg==24])*Konfederacja*weights$Konfcoef[weights$okreg==24],
                           okreg==25 ~ (weights$validvotes[weights$okreg==25])*Konfederacja*weights$Konfcoef[weights$okreg==25],
                           okreg==26 ~ (weights$validvotes[weights$okreg==26])*Konfederacja*weights$Konfcoef[weights$okreg==26],
                           okreg==27 ~ (weights$validvotes[weights$okreg==27])*Konfederacja*weights$Konfcoef[weights$okreg==27],
                           okreg==28 ~ (weights$validvotes[weights$okreg==28])*Konfederacja*weights$Konfcoef[weights$okreg==28],
                           okreg==29 ~ (weights$validvotes[weights$okreg==29])*Konfederacja*weights$Konfcoef[weights$okreg==29],
                           okreg==30 ~ (weights$validvotes[weights$okreg==30])*Konfederacja*weights$Konfcoef[weights$okreg==30],
                           okreg==31 ~ (weights$validvotes[weights$okreg==31])*Konfederacja*weights$Konfcoef[weights$okreg==31],
                           okreg==32 ~ (weights$validvotes[weights$okreg==32])*Konfederacja*weights$Konfcoef[weights$okreg==32],
                           okreg==33 ~ (weights$validvotes[weights$okreg==33])*Konfederacja*weights$Konfcoef[weights$okreg==33],
                           okreg==34 ~ (weights$validvotes[weights$okreg==34])*Konfederacja*weights$Konfcoef[weights$okreg==34],
                           okreg==35 ~ (weights$validvotes[weights$okreg==35])*Konfederacja*weights$Konfcoef[weights$okreg==35],
                           okreg==36 ~ (weights$validvotes[weights$okreg==36])*Konfederacja*weights$Konfcoef[weights$okreg==36],
                           okreg==37 ~ (weights$validvotes[weights$okreg==37])*Konfederacja*weights$Konfcoef[weights$okreg==37],
                           okreg==38 ~ (weights$validvotes[weights$okreg==38])*Konfederacja*weights$Konfcoef[weights$okreg==38],
                           okreg==39 ~ (weights$validvotes[weights$okreg==39])*Konfederacja*weights$Konfcoef[weights$okreg==39],
                           okreg==40 ~ (weights$validvotes[weights$okreg==40])*Konfederacja*weights$Konfcoef[weights$okreg==40],
                           okreg==41 ~ (weights$validvotes[weights$okreg==41])*Konfederacja*weights$Konfcoef[weights$okreg==41]
  ),
  Lewica = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*Lewica*weights$Lewicacoef[weights$okreg==1],
                     okreg==2 ~ (weights$validvotes[weights$okreg==2])*Lewica*weights$Lewicacoef[weights$okreg==2],
                     okreg==3 ~ (weights$validvotes[weights$okreg==3])*Lewica*weights$Lewicacoef[weights$okreg==3],
                     okreg==4 ~ (weights$validvotes[weights$okreg==4])*Lewica*weights$Lewicacoef[weights$okreg==4],
                     okreg==5 ~ (weights$validvotes[weights$okreg==5])*Lewica*weights$Lewicacoef[weights$okreg==5],
                     okreg==6 ~ (weights$validvotes[weights$okreg==6])*Lewica*weights$Lewicacoef[weights$okreg==6],
                     okreg==7 ~ (weights$validvotes[weights$okreg==7])*Lewica*weights$Lewicacoef[weights$okreg==7],
                     okreg==8 ~ (weights$validvotes[weights$okreg==8])*Lewica*weights$Lewicacoef[weights$okreg==8],
                     okreg==9 ~ (weights$validvotes[weights$okreg==9])*Lewica*weights$Lewicacoef[weights$okreg==9],
                     okreg==10 ~ (weights$validvotes[weights$okreg==10])*Lewica*weights$Lewicacoef[weights$okreg==10],
                     okreg==11 ~ (weights$validvotes[weights$okreg==11])*Lewica*weights$Lewicacoef[weights$okreg==11],
                     okreg==12 ~ (weights$validvotes[weights$okreg==12])*Lewica*weights$Lewicacoef[weights$okreg==12],
                     okreg==13 ~ (weights$validvotes[weights$okreg==13])*Lewica*weights$Lewicacoef[weights$okreg==13],
                     okreg==14 ~ (weights$validvotes[weights$okreg==14])*Lewica*weights$Lewicacoef[weights$okreg==14],
                     okreg==15 ~ (weights$validvotes[weights$okreg==15])*Lewica*weights$Lewicacoef[weights$okreg==15],
                     okreg==16 ~ (weights$validvotes[weights$okreg==16])*Lewica*weights$Lewicacoef[weights$okreg==16],
                     okreg==17 ~ (weights$validvotes[weights$okreg==17])*Lewica*weights$Lewicacoef[weights$okreg==17],
                     okreg==18 ~ (weights$validvotes[weights$okreg==18])*Lewica*weights$Lewicacoef[weights$okreg==18],
                     okreg==19 ~ (weights$validvotes[weights$okreg==19])*Lewica*weights$Lewicacoef[weights$okreg==19],
                     okreg==20 ~ (weights$validvotes[weights$okreg==20])*Lewica*weights$Lewicacoef[weights$okreg==20],
                     okreg==21 ~ (weights$validvotes[weights$okreg==21])*Lewica*weights$Lewicacoef[weights$okreg==21],
                     okreg==22 ~ (weights$validvotes[weights$okreg==22])*Lewica*weights$Lewicacoef[weights$okreg==22],
                     okreg==23 ~ (weights$validvotes[weights$okreg==23])*Lewica*weights$Lewicacoef[weights$okreg==23],
                     okreg==24 ~ (weights$validvotes[weights$okreg==24])*Lewica*weights$Lewicacoef[weights$okreg==24],
                     okreg==25 ~ (weights$validvotes[weights$okreg==25])*Lewica*weights$Lewicacoef[weights$okreg==25],
                     okreg==26 ~ (weights$validvotes[weights$okreg==26])*Lewica*weights$Lewicacoef[weights$okreg==26],
                     okreg==27 ~ (weights$validvotes[weights$okreg==27])*Lewica*weights$Lewicacoef[weights$okreg==27],
                     okreg==28 ~ (weights$validvotes[weights$okreg==28])*Lewica*weights$Lewicacoef[weights$okreg==28],
                     okreg==29 ~ (weights$validvotes[weights$okreg==29])*Lewica*weights$Lewicacoef[weights$okreg==29],
                     okreg==30 ~ (weights$validvotes[weights$okreg==30])*Lewica*weights$Lewicacoef[weights$okreg==30],
                     okreg==31 ~ (weights$validvotes[weights$okreg==31])*Lewica*weights$Lewicacoef[weights$okreg==31],
                     okreg==32 ~ (weights$validvotes[weights$okreg==32])*Lewica*weights$Lewicacoef[weights$okreg==32],
                     okreg==33 ~ (weights$validvotes[weights$okreg==33])*Lewica*weights$Lewicacoef[weights$okreg==33],
                     okreg==34 ~ (weights$validvotes[weights$okreg==34])*Lewica*weights$Lewicacoef[weights$okreg==34],
                     okreg==35 ~ (weights$validvotes[weights$okreg==35])*Lewica*weights$Lewicacoef[weights$okreg==35],
                     okreg==36 ~ (weights$validvotes[weights$okreg==36])*Lewica*weights$Lewicacoef[weights$okreg==36],
                     okreg==37 ~ (weights$validvotes[weights$okreg==37])*Lewica*weights$Lewicacoef[weights$okreg==37],
                     okreg==38 ~ (weights$validvotes[weights$okreg==38])*Lewica*weights$Lewicacoef[weights$okreg==38],
                     okreg==39 ~ (weights$validvotes[weights$okreg==39])*Lewica*weights$Lewicacoef[weights$okreg==39],
                     okreg==40 ~ (weights$validvotes[weights$okreg==40])*Lewica*weights$Lewicacoef[weights$okreg==40],
                     okreg==41 ~ (weights$validvotes[weights$okreg==41])*Lewica*weights$Lewicacoef[weights$okreg==41]
  ),
  MN = case_when(okreg==1 ~ 0,
                 okreg==2 ~ 0,
                 okreg==3 ~ 0,
                 okreg==4 ~ 0,
                 okreg==5 ~ 0,
                 okreg==6 ~ 0,
                 okreg==7 ~ 0,
                 okreg==8 ~ 0,
                 okreg==9 ~ 0,
                 okreg==10 ~ 0,
                 okreg==11 ~ 0,
                 okreg==12 ~ 0,
                 okreg==13 ~ 0,
                 okreg==14 ~ 0,
                 okreg==15 ~ 0,
                 okreg==16 ~ 0,
                 okreg==17 ~ 0,
                 okreg==18 ~ 0,
                 okreg==19 ~ 0,
                 okreg==20 ~ 0,
                 okreg==21 ~ (weights$validvotes[weights$okreg==21])*MN,
                 okreg==22 ~ 0,
                 okreg==23 ~ 0,
                 okreg==24 ~ 0,
                 okreg==25 ~ 0,
                 okreg==26 ~ 0,
                 okreg==27 ~ 0,
                 okreg==28 ~ 0,
                 okreg==29 ~ 0,
                 okreg==30 ~ 0,
                 okreg==31 ~ 0,
                 okreg==32 ~ 0,
                 okreg==33 ~ 0,
                 okreg==34 ~ 0,
                 okreg==35 ~ 0,
                 okreg==36 ~ 0,
                 okreg==37 ~ 0,
                 okreg==38 ~ 0,
                 okreg==39 ~ 0,
                 okreg==40 ~ 0,
                 okreg==41 ~ 0
  )
  )

poldHondt <- data.frame(KO=rep(1,41000), Konfederacja=rep(1,41000), Lewica=rep(1,41000), MN=rep(1,41000),  
                        PiS=rep(1,41000), `Trzecia Droga`=rep(1,41000))

for(i in 1:41000) { 
  poldHondt[i,] <- giveseats(v = c(consts$KO[i], consts$Konfederacja[i], consts$Lewica[i], consts$MN[i],
                                   consts$PiS[i], consts$`Trzecia Droga`[i]), ns = consts$magnitude[i], method="dh", thresh=0)$seats
}

poldHondt <- cbind(poldHondt, consts$okreg, consts$.draw)

colnames(poldHondt) <- c("KO", "Konfederacja", "Lewica", "MN", "PiS", "Trzecia Droga", "okreg", "draw")

poldHondt <- poldHondt %>% 
  group_by(draw) %>% 
  summarise(KO = sum(KO),
            PiS = sum(PiS),
            Konfederacja = sum(Konfederacja),
            `Trzecia Droga` = sum(`Trzecia Droga`),
            MN = sum(MN),
            Lewica = sum(Lewica))

poldHondt <- poldHondt %>%
  pivot_longer(., cols=c("KO", "Konfederacja", "Lewica", "MN", "PiS", "Trzecia Droga"), names_to="party", values_to="seats")

frame2 <- poldHondt %>%
  group_by(party) %>%
  summarise(mean_qi(seats)) %>%
  mutate(., y = round(y, 0),
         ymin = round(ymin, 0),
         ymax = round(ymax, 0))

frame2$in2019[frame2$party=="KO"] <- 134
frame2$in2019[frame2$party=="PiS"] <- 235
frame2$in2019[frame2$party=="Lewica"] <- 49
frame2$in2019[frame2$party=="MN"] <- 1
frame2$in2019[frame2$party=="Konfederacja"] <- 11
frame2$in2019[frame2$party=="Trzecia Droga"] <- 30
frame2$party <- factor(frame2$party, levels=c("PiS", "KO", "Lewica", "Konfederacja", "Trzecia Droga", "MN"))
frame2$diffPres <- sprintf("%+d", (frame2$y - frame2$in2019))
frame2$diffPres <- sprintf("(%s)", frame2$diffPres)
frame2$party <- reorder(frame2$party, -frame2$y)

frame$y <- frame2$y
frame$party <- reorder(frame$party, -frame$y)

plot_seats_parl <- ggplot(data=frame, mapping=aes(x=party, y=y, fill=party)) +
  geom_bar(stat="identity", width=.75, show.legend = F) +
  geom_abline(intercept=231, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=276, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=307, slope=0, colour="gray10", linetype=3) +
  scale_y_continuous('Number of seats', limits=c(0,320), breaks=c(0, 50, 100, 150, 200, 231, 276, 307)) +
  scale_fill_manual(name="Party", values = cols)+
  geom_label(aes(x=2, y=231), label="Legislative majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="IBM Plex Sans Condensed Light") +
  geom_label(aes(x=2, y=276), label="Overturn presidential veto", size=3, adj=c(0), label.size=NA, fill="grey95", family="IBM Plex Sans Condensed Light") +
  geom_label(aes(x=2, y=307), label="Constitutional majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="IBM Plex Sans Condensed Light") +
  annotate("text", x=frame$party, y=c(frame$y+18), label=frame$y, size=4, family="IBM Plex Sans Condensed Light")+
  annotate("text", x=frame$party, y=c(frame$y+8), label=paste("(",round(frame$ymin,0), "\u2013",round(frame$ymax,0),")", sep=""), size=3, family="IBM Plex Sans Condensed Light") +
  labs(x="", y="% of vote", title="Estimated share of seats",
       subtitle="Mean estimated seat share with 95% credible intervals. Sum total may not equal 460.",
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots()
ggsave(plot_seats_parl, file = "plot_seats_parl.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")

plot_seats_parl_PL <- ggplot(data=frame, mapping=aes(x=party, y=y, fill=party)) +
  geom_bar(stat="identity", width=.75, show.legend = F) +
  geom_abline(intercept=231, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=276, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=307, slope=0, colour="gray10", linetype=3) +
  scale_y_continuous('Liczba miejsc', limits=c(0,320), breaks=c(0, 50, 100, 150, 200, 231, 276, 307)) +
  scale_fill_manual(name="Party", values = cols)+
  geom_label(aes(x=2, y=231), label="Większość ustawodawcza", size=3, adj=c(0), label.size=NA, fill="grey95", family="IBM Plex Sans Condensed Light") +
  geom_label(aes(x=2, y=276), label="Większość pozwalająca obalić weto prezydenta", size=3, adj=c(0), label.size=NA, fill="grey95", family="IBM Plex Sans Condensed Light") +
  geom_label(aes(x=2, y=307), label="Konstytucyjna większość", size=3, adj=c(0), label.size=NA, fill="grey95", family="IBM Plex Sans Condensed Light") +
  annotate("text", x=frame$party, y=c(frame$y+18), label=frame$y, size=4, family="IBM Plex Sans Condensed Light")+
  annotate("text", x=frame$party, y=c(frame$y+8), label=paste("(",round(frame$ymin,0), "\u2013",round(frame$ymax,0),")", sep=""), size=3, family="IBM Plex Sans Condensed Light") +
  labs(x="", y="", title="Rozkład mandatów w Sejmie",
       subtitle="Średni szacowany udział miejsc z 95% przedziałami wiarygodności. Suma może nie być równa 460.",
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots()
ggsave(plot_seats_parl_PL, file = "plot_seats_parl_PL.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")


#####By pollster#####
tab <- table(polls$org)
polls <- polls[polls$org %in% names(tab)[tab >= 5], ]

names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
housenames <- fct_recode(names$house, "Kantar" = "Kantar") %>%
  fct_collapse(., Kantar=c("Kantar"))
orgnames <- glue_collapse(get_labels(housenames), ", ", last = " and ")
polls$org <- str_replace_all(polls$org, "_", ", ")

today <- interval(min(polls$midDate), Sys.Date())/years(1)

pred_dta <-
  tibble(
    time = seq(0, today, length.out = nrow(polls)),
    date = as.Date(time*365, origin = min(polls$midDate)),
    org = polls$org,
    pollster = polls$pollster
  )

pred_dta <-
  add_fitted_draws(
    model = m1,
    newdata = pred_dta
  ) %>%
  group_by(date, .category, org) %>%
  rename(party = .category) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "PO2050PSL", "Lewica", "Konfederacja", "Other"),
        labels = c("PiS", "KO", "Trzecia Droga", "Lewica", "Konfederacja", "Other")
      )
  )

point_dta <- polls %>%
  select(org, midDate, PiS, KO, Lewica, Konfederacja, Other, PO2050PSL) %>%
  pivot_longer(
    cols = c(-midDate, -org),
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "PO2050PSL", "Lewica", "Konfederacja", "Other"),
        labels = c("PiS", "KO", "Trzecia Droga", "Lewica", "Konfederacja", "Other")
      )
  )


plot_trends_pollster <-
  ggplot() +
  geom_point(data=point_dta, aes(x = midDate, y = est, colour=party, fill=party), alpha = .5, size = 1, show.legend=FALSE) +
  #stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "3 month",
               labels = my_date_format()) +
  facet_wrap(~org, nrow=3) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "% of vote", x="", title = "Trends by pollster",
       subtitle="Only pollsters with at least five polls are included.", color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) 
ggsave(plot_trends_pollster , file = "plot_trends_pollster.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5, bg="white")

Sys.setlocale("LC_TIME", "pl_PL.UTF-8")
plot_trends_pollster_PL <-
  ggplot() +
  geom_point(data=point_dta, aes(x = midDate, y = est, colour=party, fill=party), alpha = .5, size = 1, show.legend=FALSE) +
  #stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "3 month",
               labels = my_date_format()) +
  facet_wrap(~org, nrow=3) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=c("PiS"="blue", "KO"="orange", "Lewica" = "red", "Konfederacja" = "midnightblue", "Trzecia Droga"="darkgreen", "Inni"="gray50")) +
  scale_fill_manual(values=c("PiS"="blue", "KO"="orange", "Lewica" = "red", "Konfederacja" = "midnightblue", "Trzecia Droga"="darkgreen", "Inni"="gray50"), guide=FALSE) +
  labs(y = "", x="", title = "Trendy według ośrodku badawczego",
       subtitle=str_c("Dane: ", names_PL, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA)))
ggsave(plot_trends_pollster_PL , file = "plot_trends_pollster_PL.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5, bg="white")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")


plotdraws <- polls %>%  
  modelr::data_grid(time = today, pollster = pollster) %>%
  add_fitted_draws(m1)

medians <- plotdraws %>%
  summarise(est = median(.value)*100, .groups = "drop") 

names <- data.frame(as.factor(get_labels(polls$org)))
names$house <- as.factor(names$as.factor.get_labels.polls.org..)
names <- separate(names, as.factor.get_labels.polls.org.., sep=",", into=c('org', "label"))
orgnames <- glue_collapse(get_labels(sort(names$org)), ", ", last = " and ")
orgnames_PL <- glue_collapse(get_labels(sort(names$org)), ", ", last = " i ")

plot_latest_parl_pollster <- polls %>%  
  modelr::data_grid(time = today, pollster = factor(pollster)) %>%
  add_fitted_draws(m1) %>%
  group_by(.category, pollster) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PO2050PSL"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "Trzecia Droga")),
         pollster = factor(pollster,
                           levels = get_labels(factor(polls$pollster)),
                           labels = get_labels(factor(polls$org)))
  ) %>%
  ggplot(aes(y=reorder(.category, dplyr::desc(-.value)), 
             x=.value, color=.category)) +
  geom_vline(aes(xintercept=0.05), colour="gray40", linetype="dotted") +
  stat_interval(aes(x=.value, color_ramp = stat(.width)), .width = ppoints(100)) %>%
  partition(vars(.category)) +
  scale_color_manual(values=cols, guide=FALSE) +
  scale_fill_manual(values=cols, guide=FALSE) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_discrete(name="", position="right") +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  facet_wrap(vars(pollster), ) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Latest estimates by pollster",
       subtitle=str_c("Data from ", orgnames,"."), color="") +
  theme_plots()

plot_latest_parl_pollster <- plot_latest_parl_pollster +
  geom_text(data=medians %>%
              mutate(.category = factor(.category,
                                        levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PO2050PSL"),
                                        labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "Trzecia Droga")),
                     pollster = factor(pollster,
                                       levels = get_labels(factor(polls$pollster)),
                                       labels = get_labels(factor(polls$org)))
              ), 
            aes(x=est/100, y=.category, label=round(est,0)), check_overlap = TRUE,
            size=3, hjust=1.5,
            family="IBM Plex Sans Condensed Light", color="black")

ggsave(plot_latest_parl_pollster, file = "polls_latest_parl_pollster.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5, bg="white")


plot_latest_parl_pollster_PL <- polls %>%  
  modelr::data_grid(time = today, pollster = factor(pollster)) %>%
  add_fitted_draws(m1) %>%
  group_by(.category, pollster) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PO2050PSL"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Inni", "Trzecia Droga")),
         pollster = factor(pollster,
                           levels = get_labels(factor(polls$pollster)),
                           labels = get_labels(factor(polls$org)))
  ) %>%
  #filter(., pollster=="Kantar, CAPI" | pollster=="CBOS, Mixed") %>%
  ggplot(aes(y=reorder(.category, dplyr::desc(-.value)), 
             x=.value, color=.category)) +
  geom_vline(aes(xintercept=0.05), colour="gray40", linetype="dotted") +
  stat_interval(aes(x=.value, color_ramp = stat(.width)), .width = ppoints(100)) %>%
  partition(vars(.category)) +
  scale_color_manual(values=cols, guide=FALSE) +
  scale_fill_manual(values=cols, guide=FALSE) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_discrete(name="", position="right") +
  scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  facet_wrap(vars(pollster), ) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Szacunkowe wyniki według ośrodku badawczego",
       #subtitle=str_to_upper(str_c("Dane: ", orgnames_PL,"."), color="")
       ) +
  theme_plots()

plot_latest_parl_pollster_PL <- plot_latest_parl_pollster_PL +
  geom_text(data=medians %>%
              mutate(.category = factor(.category,
                                        levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PO2050PSL"),
                                        labels = c("PiS", "KO", "Lewica", "Konfederacja", "Inni", "Trzecia Droga")),
                     pollster = factor(pollster,
                                       levels = get_labels(factor(polls$pollster)),
                                       labels = get_labels(factor(polls$org)))
                     ), 
            aes(x=est/100, y=.category, label=round(est,0)), check_overlap=TRUE,
            size=3, hjust = 1.5,
            family="IBM Plex Sans Condensed Light", color="black")

ggsave(plot_latest_parl_pollster_PL, file = "polls_latest_parl_pollster_PL.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 5, bg="white")


#####Upload to Github#####
library(rmarkdown)
render('index.Rmd')
system("git add -A")
system("git commit -m 'PTP new'")
system("git pull")
system("git push")
