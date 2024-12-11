#####Prepare workspace
system("git pull")
library(tidyverse); library(ggrepel)

set.seed(780045)

theme_plots <- function(base_size = 11, base_family = "Gill Sans MT") {
  theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill = "#ffffff", colour = NA),
          title = element_text(size = rel(1), family = "Gill Sans MT", face = "bold"),
          plot.subtitle = element_text(size = rel(0.8),
                                       family = "Gill Sans MT", face = "plain"),
          plot.caption = element_text(margin = margin(t = 10), size = rel(0.6),
                                      family = "Gill Sans MT", face = "plain"),
          panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.15),
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = rel(0.8),
                                    family = "Gill Sans MT", face = "bold"),
          axis.title.x = element_text(hjust = 0, margin = margin(t = 10)),
          axis.title.y = element_text(hjust = 1, margin = margin(r = 10)),
          legend.position = "bottom",
          legend.title = element_text(size = rel(0.8), vjust = 0.5,
                                      family = "Gill Sans MT", face = "bold"),
          legend.key.size = unit(0.7, "line"),
          legend.key = element_blank(),
          legend.spacing = unit(0.1, "lines"),
          legend.justification = "left",
          legend.margin = margin(t = -5, b = 0, l = 0, r = 0),
          strip.text = element_text(size = rel(0.9), hjust = 0,
                                    family = "Gill Sans MT"),
          strip.background = element_rect(fill = "white", colour = NA),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )
}

theme_plots_map <- function() {
  theme_minimal(base_family = "Gill Sans MT") +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          strip.text.x = element_text(size = 10), legend.text = element_text(size=9), 
          legend.title = element_text(face="bold"), plot.title = element_text(face="bold"),
          plot.subtitle = element_text(size=8), aspect.ratio=1, legend.position="none")
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

#####Read in, adjust and subset data#####
library(googledrive)
library(rio)
library(readxl)
library(sf)

drive_deauth()
import <- drive_download(as_id("https://docs.google.com/spreadsheets/d/1KtOZ4s-YDw5gyC8UcyGe8Y_FFd7dRaLI2pLuywP9wlk/edit?usp=sharing"), overwrite=TRUE)
polls <- read_excel('polldata_2023.xlsx')

polls <- polls %>%
  select(startDate, endDate, org, remark, PiS, KO, Lewica, TD, Konfederacja, Other, DK)

weights <- read_excel('2023_elec_percentages.xlsx')
const <- st_read('GRED_20190215_Poland_2011.shp')

# polls <- polls %>%
#   filter(., org!="CBOS") 
polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + (difftime(endDate, startDate, units="days")/2)),
         midDate_int=as.integer(midDate)) %>%
  filter(midDate >= as.Date('2023-10-15')) %>%
  mutate(PiS = 100/((100-DK))*PiS,
         KO = 100/((100-DK))*KO,
         Lewica = 100/((100-DK))*Lewica,
         TD = 100/((100-DK))*TD,
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

polls[names(polls) %in% c("PiS", "KO", "Lewica", "Konfederacja", "TD")] <-
  polls[names(polls) %in% c("PiS", "KO", "Lewica", "Konfederacja", "TD")] %>%
  mutate_all(function(x) (as.numeric(str_remove(x, "%"))/100)-0.001)

polls <-
  polls %>%
  mutate(Other = 1 - (PiS + KO + Lewica + Konfederacja + TD))

polls <-
  polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("PiS", "KO", "Lewica", "TD", "Konfederacja", "Other")])
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
        prior(normal(0, 1.5), class = "Intercept", dpar = "muTD") +
        prior(normal(0, 0.5), class = "b", dpar = "muTD") +
        prior(exponential(2), class = "sd", dpar = "muTD") +
        prior(exponential(2), class = "sds", dpar = "muTD") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muKonfederacja") +
        prior(normal(0, 0.5), class = "b", dpar = "muKonfederacja") +
        prior(exponential(2), class = "sd", dpar = "muKonfederacja") +
        prior(exponential(2), class = "sds", dpar = "muKonfederacja") +
        prior(gamma(1, 0.01), class = "phi"),
      data = polls,
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


#####Trend plot#####
library(stringr)
library(hrbrthemes)
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
        levels = c("PiS", "KO", "TD", "Lewica", "Konfederacja", "Other"),
        labels = c("PiS", "KO", "Trzecia Droga", "Lewica", "Konfederacja", "Other")
      )
  )

point_dta <-
  polls[names(polls) %in% c("midDate", "PiS", "KO", "Lewica", "Konfederacja", "Other", "TD")] %>%
  pivot_longer(
    cols = -midDate,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "TD", "Lewica", "Konfederacja", "Other"),
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
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")

library(ggdist)
library(ggblend)

trends_blend <- pred_dta %>%
  ggplot(aes(x = date, color=party, fill=party)) +
  ggdist::stat_lineribbon(
    aes(y = .value, fill_ramp = stat(.width)),
    .width = ppoints(100)
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
  labs(y = "", x="", title = "Trends",
       subtitle=str_c("Data from ", names, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
  theme_plots()

ggsave(trends_blend, file = "trends_blend.png",
        width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white", device=png(type="cairo"))

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
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")
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
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "TD"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "Trzecia Droga")))

medians <- plotdraws %>%
  summarise(est = median(.value)*100, .groups = "drop")

PiS.KO.diff <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(., PiSKO = KO-PiS,
         PiSKO = sum((PiSKO > 0) / length(PiSKO)),
         PiSKO = round(PiSKO, 2)) %>%
  pull(PiSKO) %>%
  last(.)

trzecia_droga_thresh <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(., `Trzecia Droga` = sum((`Trzecia Droga` > 0.08) / length(`Trzecia Droga`)),
         `Trzecia Droga` = round(`Trzecia Droga`, 2)) %>%
  pull(`Trzecia Droga`) %>%
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
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "TD"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "Trzecia Droga"))) %>%
  ggplot(aes(y=reorder(.category, dplyr::desc(-.value)), 
             x=.value, color=.category)) +
  stat_interval(aes(x=.value, color_ramp = stat(.width)), .width = ppoints(100)) %>%
  partition(vars(.category)) +
  scale_fill_manual(values=cols, guide=FALSE) +
  scale_color_manual(name=" ", values=cols, guide=FALSE) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_discrete(name="") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PiS"],0)),
           y="PiS", x=medians$est[medians$.category=="PiS"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="KO"],0)),
           y="KO", x=medians$est[medians$.category=="KO"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Trzecia Droga"],0)),
           y="Trzecia Droga", x=medians$est[medians$.category=="Trzecia Droga"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Lewica"],0)),
           y="Lewica", x=medians$est[medians$.category=="Lewica"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Konfederacja"],0)),
           y="Konfederacja", x=medians$est[medians$.category=="Konfederacja"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Other"],0)),
           y="Other", x=medians$est[medians$.category=="Other"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT", fontface="plain") +
  annotate(geom = "text", label=paste("Pr(KO > PiS)  = ", PiS.KO.diff), y="KO",
           x=quantile(plotdraws$.value[plotdraws$.category=="KO"], 0.005), adj=c(1), family="Gill Sans MT", fontface="plain", size=3.5) +
  scale_x_continuous(breaks=c(0, 0.05, 0.08, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "5", "8", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Latest estimates",
       subtitle=str_c("Data from ", names,".")) +
  theme_plots()
ggsave(plot_latest_parl, file = "polls_latest_parl.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")

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
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "TD"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Inni", "Trzecia Droga"))) %>%
  ggplot(aes(y=reorder(.category, dplyr::desc(-.value)), 
             x=.value, color=.category)) +
  stat_interval(aes(x=.value, color_ramp = stat(.width)), .width = ppoints(100)) %>%
  partition(vars(.category)) +
  scale_color_manual(values=cols, guide=FALSE) +
  scale_fill_manual(values=cols, guide=FALSE) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_discrete(name="") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PiS"],0)),
           y="PiS", x=medians$est[medians$.category=="PiS"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="KO"],0)),
           y="KO", x=medians$est[medians$.category=="KO"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Trzecia Droga"],0)),
           y="Trzecia Droga", x=medians$est[medians$.category=="Trzecia Droga"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Lewica"],0)),
           y="Lewica", x=medians$est[medians$.category=="Lewica"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Konfederacja"],0)),
           y="Konfederacja", x=medians$est[medians$.category=="Konfederacja"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Inni"],0)),
           y="Inni", x=medians$est[medians$.category=="Inni"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Gill Sans MT") +
  annotate(geom = "text", label=paste("Pr(KO > PiS)  = ", PiS.KO.diff), y="KO",
           x=quantile(plotdraws$.value[plotdraws$.category=="KO"], 0.005), adj=c(1), family="Gill Sans MT", fontface="plain", size=3.5) +
  scale_x_continuous(breaks=c(0, 0.05, 0.08, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "5", "8", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Poparcie dla partii politycznych", color="",
       subtitle=str_c("Dane: ", names_PL,".")) +
  theme_plots()
ggsave(plot_latest_parl_PL, file = "polls_latest_parl_PL.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")


#####Seat maps#####
library(seatdist)

median_PiS <- ifelse(medians$est[medians$.category=="PiS"] >=5, medians$est[medians$.category=="PiS"], 0)
median_KO <- ifelse(medians$est[medians$.category=="KO"] >=5, medians$est[medians$.category=="KO"], 0)
median_Lewica <- ifelse(medians$est[medians$.category=="Lewica"] >=5, medians$est[medians$.category=="Lewica"], 0)
median_Konfederacja <- ifelse(medians$est[medians$.category=="Konfederacja"] >=5, medians$est[medians$.category=="Konfederacja"], 0)
`median_Trzecia Droga` <- ifelse(medians$est[medians$.category=="Trzecia Droga"] >=5, medians$est[medians$.category=="Trzecia Droga"], 0)
PiSpct <- round(weights$PiScoef*median_PiS, digits=2)
KOpct <- round(weights$KOcoef*median_KO, digits=2)
Lewicapct <- round(weights$Lewicacoef*median_Lewica, digits=2)
Konfederacjapct <- round(weights$Konfcoef*median_Konfederacja, digits=2)
TDpct <- round(weights$TDcoef*`median_Trzecia Droga`, digits=2)
MNpct <- c(0.12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.37, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
KOest <- (weights$validvotes/100)*KOpct
PiSest <- (weights$validvotes/100)*PiSpct
Lewicaest <- (weights$validvotes/100)*Lewicapct
Konfederacjaest <- (weights$validvotes/100)*Konfederacjapct
`Trzecia Drogaest` <- (weights$validvotes/100)*TDpct
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
const$id <- 0
const$id[const$cst==1] <- 24
const$id[const$cst==2] <- 27
const$id[const$cst==3] <- 4
const$id[const$cst==4] <- 7
const$id[const$cst==5] <- 28
const$id[const$cst==6] <- 34
const$id[const$cst==7] <- 25
const$id[const$cst==8] <- 26
const$id[const$cst==9] <- 29
const$id[const$cst==10] <- 36
const$id[const$cst==11] <- 31
const$id[const$cst==12] <- 33
const$id[const$cst==13] <- 37
const$id[const$cst==14] <- 40
const$id[const$cst==15] <- 13
const$id[const$cst==16] <- 12
const$id[const$cst==17] <- 22
const$id[const$cst==18] <- 1
const$id[const$cst==19] <- 6
const$id[const$cst==20] <- 14
const$id[const$cst==21] <- 35
const$id[const$cst==22] <- 21
const$id[const$cst==23] <- 10
const$id[const$cst==24] <- 38
const$id[const$cst==25] <- 39
const$id[const$cst==26] <- 16
const$id[const$cst==27] <- 17
const$id[const$cst==28] <- 30
const$id[const$cst==29] <- 23
const$id[const$cst==30] <- 18
const$id[const$cst==31] <- 11
const$id[const$cst==32] <- 32
const$id[const$cst==33] <- 41
const$id[const$cst==34] <- 15
const$id[const$cst==35] <- 5
const$id[const$cst==36] <- 19
const$id[const$cst==37] <- 20
const$id[const$cst==38] <- 2
const$id[const$cst==39] <- 3
const$id[const$cst==40] <- 8
const$id[const$cst==41] <- 9

label_points <- st_point_on_surface(const) %>%
  arrange(., id)
label_points <- st_coordinates(label_points) %>%
  as_tibble() %>%
  mutate(id = 1:n())
colnames(label_points) <- c("x","y","id")

plotdata <- merge(const, seats, by="id")
plotdata <- merge(plotdata, label_points, by="id")

p_TD <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(`Trzecia Droga`)))+
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Trzecia Droga", limits=c(min=0, max=20), low = "white", high = "darkgreen", guide="colorbar") +
  geom_label(aes(x=x, y=y, group=`Trzecia Droga`, label=`Trzecia Droga`), fill="white") +
  labs(title="Constituency-level share of seats for Trzecia Droga", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_TD, file = "TD_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 3.2, bg="white")

p_lewica <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(Lewica)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=Lewica, label=Lewica), fill="white") +
  scale_fill_gradient(name="Lewica", limits=c(min=0, max=20), low = "white", high = "red", guide="colorbar") +
  labs(title="Constituency-level share of seats for Lewica", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_lewica, file = "Lewica_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 3.2, bg="white")

p_pis <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(PiS)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=PiS, label=PiS), fill="white") +
  scale_fill_gradient(name="PiS", limits=c(min=0, max=20), low = "white", high = "blue", guide="colorbar") +
  labs(title="Constituency-level share of seats for PiS", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_pis, file = "PiS_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 3.2, bg="white")

p_ko <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(KO)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=KO, label=KO), fill="white") +
  scale_fill_gradient(name="KO", limits=c(min=0, max=20), low = "white", high = "orange", guide="colorbar") +
  labs(title="Constituency-level share of seats for Koalicja Obywatelska", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_ko, file = "KO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 3.2, bg="white")

p_konf <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(Konfederacja)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=Konfederacja, label=Konfederacja), fill="white") +
  scale_fill_gradient(name="Konfederacja", limits=c(min=0, max=20), low = "white", high = "midnightblue", guide="colorbar") +
  labs(title="Constituency-level share of seats for Konfederacja", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).")+
  theme_plots_map()
ggsave(p_konf, file = "Konf_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 3.2, bg="white")

p_pis_ko <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(PiSmKO)))+
  theme(aspect.ratio=1) +
  #geom_label(aes(x=x, y=y, group=PiSmKO, label=PiSmKO), fill="white") +
  scale_fill_gradient2(name="PiSKO", limits=c(min=-20, max=20), low = "orange", mid="white", high = "blue", midpoint=0, guide="colorbar") +
  labs(title="Constituency-level differences in share of seats for PiS and Koalicja Obywatelska", subtitle="Constituencies in shades of blue have more PiS MPs; constituencies in orange have more KO MPs;\nconstituencies in white have equal numbers of PiS and KO MPs", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots_map()
ggsave(p_pis_ko, file = "PiSKO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 3.2, bg="white")


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
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL", "TD"),
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
         `Trzecia Droga` = ifelse(`Trzecia Droga`<0.08, 0, `Trzecia Droga`)
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
  `Trzecia Droga` = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*`Trzecia Droga`*weights$TDcoef[weights$okreg==1],
                              okreg==2 ~ (weights$validvotes[weights$okreg==2])*`Trzecia Droga`*weights$TDcoef[weights$okreg==2],
                              okreg==3 ~ (weights$validvotes[weights$okreg==3])*`Trzecia Droga`*weights$TDcoef[weights$okreg==3],
                              okreg==4 ~ (weights$validvotes[weights$okreg==4])*`Trzecia Droga`*weights$TDcoef[weights$okreg==4],
                              okreg==5 ~ (weights$validvotes[weights$okreg==5])*`Trzecia Droga`*weights$TDcoef[weights$okreg==5],
                              okreg==6 ~ (weights$validvotes[weights$okreg==6])*`Trzecia Droga`*weights$TDcoef[weights$okreg==6],
                              okreg==7 ~ (weights$validvotes[weights$okreg==7])*`Trzecia Droga`*weights$TDcoef[weights$okreg==7],
                              okreg==8 ~ (weights$validvotes[weights$okreg==8])*`Trzecia Droga`*weights$TDcoef[weights$okreg==8],
                              okreg==9 ~ (weights$validvotes[weights$okreg==9])*`Trzecia Droga`*weights$TDcoef[weights$okreg==9],
                              okreg==10 ~ (weights$validvotes[weights$okreg==10])*`Trzecia Droga`*weights$TDcoef[weights$okreg==10],
                              okreg==11 ~ (weights$validvotes[weights$okreg==11])*`Trzecia Droga`*weights$TDcoef[weights$okreg==11],
                              okreg==12 ~ (weights$validvotes[weights$okreg==12])*`Trzecia Droga`*weights$TDcoef[weights$okreg==12],
                              okreg==13 ~ (weights$validvotes[weights$okreg==13])*`Trzecia Droga`*weights$TDcoef[weights$okreg==13],
                              okreg==14 ~ (weights$validvotes[weights$okreg==14])*`Trzecia Droga`*weights$TDcoef[weights$okreg==14],
                              okreg==15 ~ (weights$validvotes[weights$okreg==15])*`Trzecia Droga`*weights$TDcoef[weights$okreg==15],
                              okreg==16 ~ (weights$validvotes[weights$okreg==16])*`Trzecia Droga`*weights$TDcoef[weights$okreg==16],
                              okreg==17 ~ (weights$validvotes[weights$okreg==17])*`Trzecia Droga`*weights$TDcoef[weights$okreg==17],
                              okreg==18 ~ (weights$validvotes[weights$okreg==18])*`Trzecia Droga`*weights$TDcoef[weights$okreg==18],
                              okreg==19 ~ (weights$validvotes[weights$okreg==19])*`Trzecia Droga`*weights$TDcoef[weights$okreg==19],
                              okreg==20 ~ (weights$validvotes[weights$okreg==20])*`Trzecia Droga`*weights$TDcoef[weights$okreg==20],
                              okreg==21 ~ (weights$validvotes[weights$okreg==21])*`Trzecia Droga`*weights$TDcoef[weights$okreg==21],
                              okreg==22 ~ (weights$validvotes[weights$okreg==22])*`Trzecia Droga`*weights$TDcoef[weights$okreg==22],
                              okreg==23 ~ (weights$validvotes[weights$okreg==23])*`Trzecia Droga`*weights$TDcoef[weights$okreg==23],
                              okreg==24 ~ (weights$validvotes[weights$okreg==24])*`Trzecia Droga`*weights$TDcoef[weights$okreg==24],
                              okreg==25 ~ (weights$validvotes[weights$okreg==25])*`Trzecia Droga`*weights$TDcoef[weights$okreg==25],
                              okreg==26 ~ (weights$validvotes[weights$okreg==26])*`Trzecia Droga`*weights$TDcoef[weights$okreg==26],
                              okreg==27 ~ (weights$validvotes[weights$okreg==27])*`Trzecia Droga`*weights$TDcoef[weights$okreg==27],
                              okreg==28 ~ (weights$validvotes[weights$okreg==28])*`Trzecia Droga`*weights$TDcoef[weights$okreg==28],
                              okreg==29 ~ (weights$validvotes[weights$okreg==29])*`Trzecia Droga`*weights$TDcoef[weights$okreg==29],
                              okreg==30 ~ (weights$validvotes[weights$okreg==30])*`Trzecia Droga`*weights$TDcoef[weights$okreg==30],
                              okreg==31 ~ (weights$validvotes[weights$okreg==31])*`Trzecia Droga`*weights$TDcoef[weights$okreg==31],
                              okreg==32 ~ (weights$validvotes[weights$okreg==32])*`Trzecia Droga`*weights$TDcoef[weights$okreg==32],
                              okreg==33 ~ (weights$validvotes[weights$okreg==33])*`Trzecia Droga`*weights$TDcoef[weights$okreg==33],
                              okreg==34 ~ (weights$validvotes[weights$okreg==34])*`Trzecia Droga`*weights$TDcoef[weights$okreg==34],
                              okreg==35 ~ (weights$validvotes[weights$okreg==35])*`Trzecia Droga`*weights$TDcoef[weights$okreg==35],
                              okreg==36 ~ (weights$validvotes[weights$okreg==36])*`Trzecia Droga`*weights$TDcoef[weights$okreg==36],
                              okreg==37 ~ (weights$validvotes[weights$okreg==37])*`Trzecia Droga`*weights$TDcoef[weights$okreg==37],
                              okreg==38 ~ (weights$validvotes[weights$okreg==38])*`Trzecia Droga`*weights$TDcoef[weights$okreg==38],
                              okreg==39 ~ (weights$validvotes[weights$okreg==39])*`Trzecia Droga`*weights$TDcoef[weights$okreg==39],
                              okreg==40 ~ (weights$validvotes[weights$okreg==40])*`Trzecia Droga`*weights$TDcoef[weights$okreg==40],
                              okreg==41 ~ (weights$validvotes[weights$okreg==41])*`Trzecia Droga`*weights$TDcoef[weights$okreg==41]
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
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "TD"),
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
  `Trzecia Droga` = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*`Trzecia Droga`*weights$TDcoef[weights$okreg==1],
                              okreg==2 ~ (weights$validvotes[weights$okreg==2])*`Trzecia Droga`*weights$TDcoef[weights$okreg==2],
                              okreg==3 ~ (weights$validvotes[weights$okreg==3])*`Trzecia Droga`*weights$TDcoef[weights$okreg==3],
                              okreg==4 ~ (weights$validvotes[weights$okreg==4])*`Trzecia Droga`*weights$TDcoef[weights$okreg==4],
                              okreg==5 ~ (weights$validvotes[weights$okreg==5])*`Trzecia Droga`*weights$TDcoef[weights$okreg==5],
                              okreg==6 ~ (weights$validvotes[weights$okreg==6])*`Trzecia Droga`*weights$TDcoef[weights$okreg==6],
                              okreg==7 ~ (weights$validvotes[weights$okreg==7])*`Trzecia Droga`*weights$TDcoef[weights$okreg==7],
                              okreg==8 ~ (weights$validvotes[weights$okreg==8])*`Trzecia Droga`*weights$TDcoef[weights$okreg==8],
                              okreg==9 ~ (weights$validvotes[weights$okreg==9])*`Trzecia Droga`*weights$TDcoef[weights$okreg==9],
                              okreg==10 ~ (weights$validvotes[weights$okreg==10])*`Trzecia Droga`*weights$TDcoef[weights$okreg==10],
                              okreg==11 ~ (weights$validvotes[weights$okreg==11])*`Trzecia Droga`*weights$TDcoef[weights$okreg==11],
                              okreg==12 ~ (weights$validvotes[weights$okreg==12])*`Trzecia Droga`*weights$TDcoef[weights$okreg==12],
                              okreg==13 ~ (weights$validvotes[weights$okreg==13])*`Trzecia Droga`*weights$TDcoef[weights$okreg==13],
                              okreg==14 ~ (weights$validvotes[weights$okreg==14])*`Trzecia Droga`*weights$TDcoef[weights$okreg==14],
                              okreg==15 ~ (weights$validvotes[weights$okreg==15])*`Trzecia Droga`*weights$TDcoef[weights$okreg==15],
                              okreg==16 ~ (weights$validvotes[weights$okreg==16])*`Trzecia Droga`*weights$TDcoef[weights$okreg==16],
                              okreg==17 ~ (weights$validvotes[weights$okreg==17])*`Trzecia Droga`*weights$TDcoef[weights$okreg==17],
                              okreg==18 ~ (weights$validvotes[weights$okreg==18])*`Trzecia Droga`*weights$TDcoef[weights$okreg==18],
                              okreg==19 ~ (weights$validvotes[weights$okreg==19])*`Trzecia Droga`*weights$TDcoef[weights$okreg==19],
                              okreg==20 ~ (weights$validvotes[weights$okreg==20])*`Trzecia Droga`*weights$TDcoef[weights$okreg==20],
                              okreg==21 ~ (weights$validvotes[weights$okreg==21])*`Trzecia Droga`*weights$TDcoef[weights$okreg==21],
                              okreg==22 ~ (weights$validvotes[weights$okreg==22])*`Trzecia Droga`*weights$TDcoef[weights$okreg==22],
                              okreg==23 ~ (weights$validvotes[weights$okreg==23])*`Trzecia Droga`*weights$TDcoef[weights$okreg==23],
                              okreg==24 ~ (weights$validvotes[weights$okreg==24])*`Trzecia Droga`*weights$TDcoef[weights$okreg==24],
                              okreg==25 ~ (weights$validvotes[weights$okreg==25])*`Trzecia Droga`*weights$TDcoef[weights$okreg==25],
                              okreg==26 ~ (weights$validvotes[weights$okreg==26])*`Trzecia Droga`*weights$TDcoef[weights$okreg==26],
                              okreg==27 ~ (weights$validvotes[weights$okreg==27])*`Trzecia Droga`*weights$TDcoef[weights$okreg==27],
                              okreg==28 ~ (weights$validvotes[weights$okreg==28])*`Trzecia Droga`*weights$TDcoef[weights$okreg==28],
                              okreg==29 ~ (weights$validvotes[weights$okreg==29])*`Trzecia Droga`*weights$TDcoef[weights$okreg==29],
                              okreg==30 ~ (weights$validvotes[weights$okreg==30])*`Trzecia Droga`*weights$TDcoef[weights$okreg==30],
                              okreg==31 ~ (weights$validvotes[weights$okreg==31])*`Trzecia Droga`*weights$TDcoef[weights$okreg==31],
                              okreg==32 ~ (weights$validvotes[weights$okreg==32])*`Trzecia Droga`*weights$TDcoef[weights$okreg==32],
                              okreg==33 ~ (weights$validvotes[weights$okreg==33])*`Trzecia Droga`*weights$TDcoef[weights$okreg==33],
                              okreg==34 ~ (weights$validvotes[weights$okreg==34])*`Trzecia Droga`*weights$TDcoef[weights$okreg==34],
                              okreg==35 ~ (weights$validvotes[weights$okreg==35])*`Trzecia Droga`*weights$TDcoef[weights$okreg==35],
                              okreg==36 ~ (weights$validvotes[weights$okreg==36])*`Trzecia Droga`*weights$TDcoef[weights$okreg==36],
                              okreg==37 ~ (weights$validvotes[weights$okreg==37])*`Trzecia Droga`*weights$TDcoef[weights$okreg==37],
                              okreg==38 ~ (weights$validvotes[weights$okreg==38])*`Trzecia Droga`*weights$TDcoef[weights$okreg==38],
                              okreg==39 ~ (weights$validvotes[weights$okreg==39])*`Trzecia Droga`*weights$TDcoef[weights$okreg==39],
                              okreg==40 ~ (weights$validvotes[weights$okreg==40])*`Trzecia Droga`*weights$TDcoef[weights$okreg==40],
                              okreg==41 ~ (weights$validvotes[weights$okreg==41])*`Trzecia Droga`*weights$TDcoef[weights$okreg==41]
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
  geom_label(aes(x=2, y=231), label="Legislative majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Gill Sans MT") +
  geom_label(aes(x=2, y=276), label="Overturn presidential veto", size=3, adj=c(0), label.size=NA, fill="grey95", family="Gill Sans MT") +
  geom_label(aes(x=2, y=307), label="Constitutional majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Gill Sans MT") +
  annotate("text", x=frame$party, y=c(frame$y+18), label=frame$y, size=4, family="Gill Sans MT")+
  annotate("text", x=frame$party, y=c(frame$y+8), label=paste("(",round(frame$ymin,0), "\u2013",round(frame$ymax,0),")", sep=""), size=3, family="Gill Sans MT") +
  labs(x="", y="% of vote", title="Estimated share of seats",
       subtitle="Mean estimated seat share with 95% credible intervals. Sum total may not equal 460.",
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots()
ggsave(plot_seats_parl, file = "plot_seats_parl.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")

plot_seats_parl_PL <- ggplot(data=frame, mapping=aes(x=party, y=y, fill=party)) +
  geom_bar(stat="identity", width=.75, show.legend = F) +
  geom_abline(intercept=231, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=276, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=307, slope=0, colour="gray10", linetype=3) +
  scale_y_continuous(limits=c(0,320), breaks=c(0, 50, 100, 150, 200, 231, 276, 307)) +
  scale_fill_manual(name="Party", values = cols)+
  geom_label(aes(x=2, y=231), label="Większość ustawodawcza", size=3, adj=c(0), label.size=NA, fill="grey95", family="Gill Sans MT") +
  geom_label(aes(x=2, y=276), label="Większość pozwalająca obalić weto prezydenta", size=3, adj=c(0), label.size=NA, fill="grey95", family="Gill Sans MT") +
  geom_label(aes(x=2, y=307), label="Konstytucyjna większość", size=3, adj=c(0), label.size=NA, fill="grey95", family="Gill Sans MT") +
  annotate("text", x=frame$party, y=c(frame$y+18), label=frame$y, size=4, family="Gill Sans MT") +
  annotate("text", x=frame$party, y=c(frame$y+8), label=paste("(",round(frame$ymin,0), "\u2013",round(frame$ymax,0),")", sep=""), size=3, family="Gill Sans MT") +
  labs(x="", y="Liczba miejsc", title="Rozkład mandatów w Sejmie",
       subtitle="Średni szacowany udział miejsc z 95% przedziałami wiarygodności. Suma może nie być równa 460.",
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots()
ggsave(plot_seats_parl_PL, file = "plot_seats_parl_PL.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")


#####By pollster#####
tab <- table(polls$org)
#polls <- polls[polls$org %in% names(tab)[tab >= 5], ]

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
        levels = c("PiS", "KO", "TD", "Lewica", "Konfederacja", "Other"),
        labels = c("PiS", "KO", "Trzecia Droga", "Lewica", "Konfederacja", "Other")
      )
  )

point_dta <- polls %>%
  select(org, midDate, PiS, KO, Lewica, Konfederacja, Other, TD) %>%
  pivot_longer(
    cols = c(-midDate, -org),
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "TD", "Lewica", "Konfederacja", "Other"),
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
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")

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
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")
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
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "TD"),
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
                                        levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "TD"),
                                        labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "Trzecia Droga")),
                     pollster = factor(pollster,
                                       levels = get_labels(factor(polls$pollster)),
                                       labels = get_labels(factor(polls$org)))
              ), 
            aes(x=est/100, y=.category, label=round(est,0)), check_overlap = TRUE,
            size=3, hjust=1.5,
            family="Gill Sans MT", color="black")

ggsave(plot_latest_parl_pollster, file = "polls_latest_parl_pollster.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")


plot_latest_parl_pollster_PL <- polls %>%  
  modelr::data_grid(time = today, pollster = factor(pollster)) %>%
  add_fitted_draws(m1) %>%
  group_by(.category, pollster) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "TD"),
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
                                        levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "TD"),
                                        labels = c("PiS", "KO", "Lewica", "Konfederacja", "Inni", "Trzecia Droga")),
                     pollster = factor(pollster,
                                       levels = get_labels(factor(polls$pollster)),
                                       labels = get_labels(factor(polls$org)))
              ), 
            aes(x=est/100, y=.category, label=round(est,0)), check_overlap=TRUE,
            size=3, hjust = 1.5,
            family="Gill Sans MT", color="black")

ggsave(plot_latest_parl_pollster_PL, file = "polls_latest_parl_pollster_PL.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3.2, bg="white")

#####Upload to Github#####
library(rmarkdown)
render('index.Rmd')
system("git add -A")
system("git commit -m 'PTP new'")
system("git pull")
system("git push")
