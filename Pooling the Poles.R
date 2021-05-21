#####Load packages#####
library(tidyverse)
library(patchwork)
library(tidybayes)
library(lubridate)
library(htmltab)
library(brms)
library(here)
library(glue)
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
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.1.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}


theme_changes <- theme(axis.title.y = element_text(family="Roboto Condensed Light"), axis.title.x = element_text(family="Roboto Condensed Light"),
                       axis.text.x = element_text(size=10, family="Roboto Condensed Light"), axis.text.y = element_text(size=10, family="Roboto Condensed Light"),
                       strip.text.x = element_text(size = 10, family="Roboto Condensed Light"), legend.text = element_text(size=9, family="Roboto Condensed Light"), 
                       legend.title = element_text(size=10, family="Roboto Condensed Light"), plot.title = element_text(size=14, family="Roboto Condensed Bold"),
                       plot.subtitle = element_text(size=8, family="Roboto Condensed Light"))

theme_changes_map <- theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                           axis.text.x = element_blank(), axis.text.y = element_blank(),
                           strip.text.x = element_text(size = 10, family="Roboto Condensed Light"), legend.text = element_text(size=9, family="Roboto Condensed Light"), 
                           legend.title = element_text(size=10, family="Roboto Condensed Light"), plot.title = element_text(size=14, family="Roboto Condensed Bold"),
                           plot.subtitle = element_text(size=8, family="Roboto Condensed Light"), aspect.ratio=1, legend.position="none")

set.seed(780045)

#####Read in, adjust and subset data#####
import <- drive_download(as_id("https://drive.google.com/file/d/1ZiaHdyGqkeWaQwpADjBloRSPnfAAhXrC/view?usp=sharing"), overwrite=TRUE)
1
polls <- read_excel('polldata.xlsx')

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + (difftime(endDate, startDate, units="days")/2)),
         midDate_int=as.integer(midDate)) %>%
  #filter(midDate_int > (max(midDate_int)-150)) %>%
  mutate(PiS = 100/((100-DK))*PiS,
         KO = 100/((100-DK))*KO,
         Lewica = 100/((100-DK))*Lewica,
         PSL = 100/((100-DK))*PSL,
         Konfederacja = 100/((100-DK))*Konfederacja,
         `Polska 2050` = 100/((100-DK))*`Polska 2050`,
         Other = 100/((100-DK))*Other,
    PSL = PSL,
         Polska2050 = `Polska 2050`,
    time = as.integer(difftime(midDate, min(midDate), units = "days")),
         pollster = as.integer(factor(org)))

cols <- c("PiS"="blue4", "KO"="orange", "PSL"="darkgreen", "Konfederacja" = "midnightblue", "Lewica" = "red", "MN" = "yellow", "Other"="gray50", "Polska 2050"="darkgoldenrod")
names <- data.frame(as.factor(get_labels(polls$org)))
names <- separate(names, as.factor.get_labels.polls.org.., c("house", "method"), sep="_")
names$house <- as.factor(names$house)
housenames <- fct_recode(names$house, "Kantar" = "Kantar") %>%
  fct_collapse(., Kantar=c("Kantar"))
#names <- paste0(get_labels(housenames), collapse=", ")
names <- glue_collapse(get_labels(housenames), ", ", last = " and ")
polls$org <- str_replace_all(polls$org, "_", ", ")

polls <-
  polls %>%
  mutate(time = interval(min(midDate), midDate)/years(1))

polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska2050")] <-
  polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska2050")] %>%
  mutate_all(function(x) (as.numeric(str_remove(x, "%"))/100)-0.001) #to ensure no zeroes in model

polls <-
  polls %>%
  mutate(Other = 1 - (PiS + KO + Lewica + PSL + Konfederacja + Polska2050))

polls <-
  polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("PiS", "KO", "Lewica", "PSL", "Konfederacja", "Polska2050", "Other")])
  )


#####Run model#####
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
        prior(gamma(1, 0.01), class = "phi"),
      data = polls,
      seed = 780045,
      iter = 5000,
      backend="cmdstanr", chains=6, cores=12, threads = threading(6),
      refresh = 5,
      control =
        list(
          adapt_delta = .95,
          max_treedepth = 15
        )
  )


#####Trend plot#####
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
        levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "Other"),
        labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other")
      )
  )

point_dta <-
  polls[names(polls) %in% c("midDate", "PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL", "Polska2050")] %>%
  pivot_longer(
    cols = -midDate,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "Other"),
        labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other")
      )
  )

plot_trends_parl <-
  ggplot() +
  geom_point(data=point_dta, aes(x = midDate, y = est, colour = party, fill=party), alpha = .5, size = 1, show.legend=FALSE) +
  stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "", x="", title = "Trends", 
       subtitle=str_c("Data from ", names, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_changes
ggsave(plot_trends_parl, file = "plot_trends_parl.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####Latest plot#####
plotdraws <- add_fitted_draws(
  model = m1,
  newdata =
    tibble(time = today),
  re_formula = NA
) %>%
  group_by(.category) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL", "Polska2050"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL","Polska 2050")))

medians <- plotdraws %>%
  summarise(est = median(.value)*100, .groups = "drop")

PiS.KO.diff <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(., PiSKO = PiS-KO,
         PiSKO = sum((PiSKO > 0) / length(PiSKO)),
         PiSKO = round(PiSKO, 2)) %>%
  pull(PiSKO) %>%
  last(.)

KO.P50.diff <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(., KOP50 = `Polska 2050`-KO,
         KOP50 = sum((KOP50 > 0) / length(KOP50)),
         KOP50 = round(KOP50, 2)) %>%
  pull(KOP50) %>%
  last(.)

P50.Lewica.diff <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(., P50Lew = `Polska 2050`-Lewica,
         P50Lew = sum((P50Lew > 0) / length(P50Lew)),
         P50Lew = round(P50Lew, 2)) %>%
  pull(P50Lew) %>%
  last(.)

Lewica_5 <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(., Lewica_5 = Lewica-0.05,
         Lewica_5 = sum((Lewica_5 > 0) / length(Lewica_5)),
         Lewica_5 = round(Lewica_5, 2)) %>%
  pull(Lewica_5) %>%
  last(.)

PSL_5 <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(., PSL_5 = PSL-0.05,
         PSL_5 = sum((PSL_5 > 0) / length(PSL_5)),
         PSL_5 = round(PSL_5, 2)) %>%
  pull(PSL_5) %>%
  last(.)

Konfederacja_5 <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(., Konfederacja_5 = Konfederacja-0.05,
         Konfederacja_5 = sum((Konfederacja_5 > 0) / length(Konfederacja_5)),
         Konfederacja_5 = round(Konfederacja_5, 2)) %>%
  pull(Konfederacja_5) %>%
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
    levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL", "Polska2050"),
    labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL","Polska 2050"))) %>%
  ggplot() +
  geom_vline(aes(xintercept=0.05), colour="gray40", linetype="dotted") +
  stat_slab(aes(y=reorder(.category, dplyr::desc(-.value)), 
                 x=.value, fill=.category), normalize="xy") +
  scale_y_discrete(name="", position="right") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PiS"],0)),
           y="PiS", x=medians$est[medians$.category=="PiS"]/100, size=4, hjust = "center", vjust=-1,
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="KO"],0)),
           y="KO", x=medians$est[medians$.category=="KO"]/100, size=4, hjust = "center", vjust=-1,
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Polska 2050"],0)),
           y="Polska 2050", x=medians$est[medians$.category=="Polska 2050"]/100, size=4, hjust = "center", vjust=-1,
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Lewica"],0)),
           y="Lewica", x=medians$est[medians$.category=="Lewica"]/100, size=4, hjust = "center", vjust=-1,
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Konfederacja"],0)),
           y="Konfederacja", x=medians$est[medians$.category=="Konfederacja"]/100, size=4, hjust = "center", vjust=-1,
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PSL"],0)),
           y="PSL", x=medians$est[medians$.category=="PSL"]/100, size=4, hjust = "center", vjust=-1,
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Other"],0)),
           y="Other", x=medians$est[medians$.category=="Other"]/100, size=4, hjust = "center", vjust=-1,
           family="Roboto Condensed", color="white") +
  annotate(geom = "text", label=paste("Pr(PiS > KO)  = ", PiS.KO.diff), y="PiS",
           x=quantile(plotdraws$.value[plotdraws$.category=="PiS"], 0.005), size=3.5, adj=c(1), vjust=-3, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(Polska 2050 > KO)  = ", KO.P50.diff), y="Polska 2050",
           x=quantile(plotdraws$.value[plotdraws$.category=="Polska 2050"], 0.005), size=3.5, adj=c(1), vjust=-3, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(Lewica > 5%)  = ", Lewica_5), y="Lewica",
           x=quantile(plotdraws$.value[plotdraws$.category=="Lewica"], 0.999), size=3.5, adj=c(0), vjust=-4, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(PSL > 5%)  = ", PSL_5), y="PSL",
           x=quantile(plotdraws$.value[plotdraws$.category=="PSL"], 0.999), size=3.5, adj=c(0), vjust=-4, family="Roboto Condensed Light") +
  annotate(geom = "text", label=paste("Pr(Konfederacja > 5%)  = ", Konfederacja_5), y="Konfederacja",
           x=quantile(plotdraws$.value[plotdraws$.category=="Konfederacja"], 0.999), size=3.5, adj=c(0), vjust=-4, family="Roboto Condensed Light") +
  scale_fill_manual(name=" ", values=cols, guide=FALSE) +
  scale_x_continuous(breaks=c(0, 0.5, 0.8, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "5", "8", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Latest estimates",
       subtitle=str_c("Data from ", names,".")) +
  theme_minimal() +
  theme_ipsum_rc() +
  theme_changes 
ggsave(plot_latest_parl, file = "polls_latest_parl.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

#####Seat maps#####
median_PiS <- ifelse(medians$est[medians$.category=="PiS"] >=5, medians$est[medians$.category=="PiS"], 0)
median_KO <- ifelse(medians$est[medians$.category=="KO"] >=5, medians$est[medians$.category=="KO"], 0)
`median_PSL` <- ifelse(medians$est[medians$.category=="PSL"] >=5, medians$est[medians$.category=="PSL"], 0)
median_Lewica <- ifelse(medians$est[medians$.category=="Lewica"] >=5, medians$est[medians$.category=="Lewica"], 0)
median_Konfederacja <- ifelse(medians$est[medians$.category=="Konfederacja"] >=5, medians$est[medians$.category=="Konfederacja"], 0)
`median_Polska 2050` <- ifelse(medians$est[medians$.category=="Polska 2050"] >=5, medians$est[medians$.category=="Polska 2050"], 0)
PiSpct <- round(weights$PiScoef*median_PiS, digits=2)
KOpct <- round(weights$KOcoef*median_KO, digits=2)
PSLpct <- round(weights$PSLcoef*`median_PSL`, digits=2)
Lewicapct <- round(weights$Lewicacoef*median_Lewica, digits=2)
Konfederacjapct <- round(weights$Konfcoef*median_Konfederacja, digits=2)
`Polska 2050pct` <- round(weights$KOcoef*`median_Polska 2050`, digits=2)
MNpct <- c(0.17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7.90, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
KOest <- (weights$validvotes/100)*KOpct
PiSest <- (weights$validvotes/100)*PiSpct
PSLest <- (weights$validvotes/100)*PSLpct
Lewicaest <- (weights$validvotes/100)*Lewicapct
Konfederacjaest <- (weights$validvotes/100)*Konfederacjapct
`Polska 2050est` <- (weights$validvotes/100)*`Polska 2050pct`
MNest <- (weights$validvotes/100)*MNpct
poldHondt <- data.frame(KO=rep(1,42), Konfederacja=rep(1,42), Lewica=rep(1,42),  MN=rep(1,42), PiS=rep(1,42), `Polska 2050`=rep(1,42), PSL=rep(1,42))

for( i in 1 : 42 ) {
  poldHondt[i,] <- c(giveseats(v = c(KOest[i], Konfederacjaest[i], Lewicaest[i], MNest[i], PiSest[i], 
                                     `Polska 2050est`[i], PSLest[i]), ns = weights$magnitude[i], method="dh", thresh=5))$seats
}

# seats table
seats <- cbind(poldHondt, weights)
row.names(seats) <- weights$name
keep <- c("KO","Konfederacja","Lewica","MN", "PiS", "Polska 2050", "PSL")
colnames(seats) <- c("KO", "Konfederacja","Lewica","MN", "PiS", "Polska 2050", "PSL")
seats <- seats[keep]
seats <- seats[-1,]
seats$id <- 1:41
seats$PiSKO <-abs(seats$PiS-seats$KO)
seats$PiSmKO <- seats$PiS-seats$KO

#regional maps
const=readOGR("/Users/benstanley/Google Drive/Resources/Polish materials/Regional data/GRED_beta2_20170530_Poland/shapefile/GRED_Poland_2011_beta2.shp")
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
  aes(long,lat,group=group,fill=as.integer(`Polska 2050`)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Polska 2050", limits=c(min=0, max=20), low = "white", high = "darkgoldenrod", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=`Polska 2050`, label=`Polska 2050`), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for Polska 2050", subtitle="Seat distribution reflects regional levels of support for Szymon Hołownia at 2020 presidential election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).", family="Roboto Condensed") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_p2050, file = "Polska_2050_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_lewica <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Lewica)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Lewica", limits=c(min=0, max=20), low = "white", high = "red", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Lewica, label=Lewica), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for Lewica", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).", family="Roboto Condensed") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_lewica, file = "Lewica_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_pis <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiS)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PiS", limits=c(min=0, max=20), low = "white", high = "blue4", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiS, label=PiS), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for PiS", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_pis, file = "PiS_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_ko <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(KO)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="KO", limits=c(min=0, max=20), low = "white", high = "orange", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=KO, label=KO), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for Koalicja Obywatelska", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_ko, file = "KO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_psl <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PSL)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="PSL", limits=c(min=0, max=20), low = "white", high = "darkgreen", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PSL, label=PSL), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for PSL", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_psl, file = "PSL_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_konf <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(Konfederacja)) + 
  geom_polygon() +
  geom_path(color="black") +
  theme(aspect.ratio=1) +
  scale_fill_gradient(name="Konfederacja", limits=c(min=0, max=20), low = "white", high = "midnightblue", guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=Konfederacja, label=Konfederacja), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level share of seats for Konfederacja", subtitle="Seat distribution reflects regional levels of support at October 2019 election", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_konf, file = "Konf_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_pis_ko <- ggplot(plotdata) + 
  aes(long,lat,group=group,fill=as.integer(PiSmKO)) + 
  geom_polygon() +
  geom_path(color="black") +
  scale_fill_gradient2(name="PiSKO", limits=c(min=-20, max=20), low = "orange", mid="white", high = "blue4", midpoint=0, guide="colorbar") +
  geom_label(seats, mapping = aes(x=label_point_x, y=label_point_y, group=PiSKO, label=PiSKO), fill="white", family="Roboto Condensed Light") +
  labs(title="Constituency-level differences in share of seats for PiS and Koalicja Obywatelska", subtitle="Constituencies in shades of blue have more PiS MPs; constituencies in orange have more KO MPs", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).", family="Roboto Condensed")+
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0) +
  theme_changes_map
ggsave(p_pis_ko, file = "PiSKO_seats.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)


#####Seats plot#####
weights <- read_excel('~/Google Drive/Resources/Polish materials/Poll data/2019_elec_percentages.xlsx')
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
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL", "Polska2050"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL","Polska 2050")))

plotdraws <- plotdraws %>%
  pivot_wider(names_from = .category, values_from = .value) %>%
  mutate(magnitude=460) %>%
  select(., c(.draw, PiS, KO, Lewica, PSL, Konfederacja, `Polska 2050`, magnitude)) 

plotdraws$MN <- rnorm(1000, mean=0.079, sd=0.00001)

plotdraws <- plotdraws %>%
  mutate(., PSL = ifelse(PSL<0.05, 0, PSL),
         PiS = ifelse(PiS<0.05, 0, PiS),
         KO = ifelse(KO<0.05, 0, KO),
         Konfederacja = ifelse(Konfederacja<0.05, 0, Konfederacja),
         Lewica = ifelse(Lewica<0.05, 0, Lewica),
         `Polska 2050` = ifelse(`Polska 2050`<0.05, 0, `Polska 2050`)
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
  `Polska 2050` = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*`Polska 2050`*weights$KOcoef[weights$okreg==1],
                            okreg==2 ~ (weights$validvotes[weights$okreg==2])*`Polska 2050`*weights$KOcoef[weights$okreg==2],
                            okreg==3 ~ (weights$validvotes[weights$okreg==3])*`Polska 2050`*weights$KOcoef[weights$okreg==3],
                            okreg==4 ~ (weights$validvotes[weights$okreg==4])*`Polska 2050`*weights$KOcoef[weights$okreg==4],
                            okreg==5 ~ (weights$validvotes[weights$okreg==5])*`Polska 2050`*weights$KOcoef[weights$okreg==5],
                            okreg==6 ~ (weights$validvotes[weights$okreg==6])*`Polska 2050`*weights$KOcoef[weights$okreg==6],
                            okreg==7 ~ (weights$validvotes[weights$okreg==7])*`Polska 2050`*weights$KOcoef[weights$okreg==7],
                            okreg==8 ~ (weights$validvotes[weights$okreg==8])*`Polska 2050`*weights$KOcoef[weights$okreg==8],
                            okreg==9 ~ (weights$validvotes[weights$okreg==9])*`Polska 2050`*weights$KOcoef[weights$okreg==9],
                            okreg==10 ~ (weights$validvotes[weights$okreg==10])*`Polska 2050`*weights$KOcoef[weights$okreg==10],
                            okreg==11 ~ (weights$validvotes[weights$okreg==11])*`Polska 2050`*weights$KOcoef[weights$okreg==11],
                            okreg==12 ~ (weights$validvotes[weights$okreg==12])*`Polska 2050`*weights$KOcoef[weights$okreg==12],
                            okreg==13 ~ (weights$validvotes[weights$okreg==13])*`Polska 2050`*weights$KOcoef[weights$okreg==13],
                            okreg==14 ~ (weights$validvotes[weights$okreg==14])*`Polska 2050`*weights$KOcoef[weights$okreg==14],
                            okreg==15 ~ (weights$validvotes[weights$okreg==15])*`Polska 2050`*weights$KOcoef[weights$okreg==15],
                            okreg==16 ~ (weights$validvotes[weights$okreg==16])*`Polska 2050`*weights$KOcoef[weights$okreg==16],
                            okreg==17 ~ (weights$validvotes[weights$okreg==17])*`Polska 2050`*weights$KOcoef[weights$okreg==17],
                            okreg==18 ~ (weights$validvotes[weights$okreg==18])*`Polska 2050`*weights$KOcoef[weights$okreg==18],
                            okreg==19 ~ (weights$validvotes[weights$okreg==19])*`Polska 2050`*weights$KOcoef[weights$okreg==19],
                            okreg==20 ~ (weights$validvotes[weights$okreg==20])*`Polska 2050`*weights$KOcoef[weights$okreg==20],
                            okreg==21 ~ (weights$validvotes[weights$okreg==21])*`Polska 2050`*weights$KOcoef[weights$okreg==21],
                            okreg==22 ~ (weights$validvotes[weights$okreg==22])*`Polska 2050`*weights$KOcoef[weights$okreg==22],
                            okreg==23 ~ (weights$validvotes[weights$okreg==23])*`Polska 2050`*weights$KOcoef[weights$okreg==23],
                            okreg==24 ~ (weights$validvotes[weights$okreg==24])*`Polska 2050`*weights$KOcoef[weights$okreg==24],
                            okreg==25 ~ (weights$validvotes[weights$okreg==25])*`Polska 2050`*weights$KOcoef[weights$okreg==25],
                            okreg==26 ~ (weights$validvotes[weights$okreg==26])*`Polska 2050`*weights$KOcoef[weights$okreg==26],
                            okreg==27 ~ (weights$validvotes[weights$okreg==27])*`Polska 2050`*weights$KOcoef[weights$okreg==27],
                            okreg==28 ~ (weights$validvotes[weights$okreg==28])*`Polska 2050`*weights$KOcoef[weights$okreg==28],
                            okreg==29 ~ (weights$validvotes[weights$okreg==29])*`Polska 2050`*weights$KOcoef[weights$okreg==29],
                            okreg==30 ~ (weights$validvotes[weights$okreg==30])*`Polska 2050`*weights$KOcoef[weights$okreg==30],
                            okreg==31 ~ (weights$validvotes[weights$okreg==31])*`Polska 2050`*weights$KOcoef[weights$okreg==31],
                            okreg==32 ~ (weights$validvotes[weights$okreg==32])*`Polska 2050`*weights$KOcoef[weights$okreg==32],
                            okreg==33 ~ (weights$validvotes[weights$okreg==33])*`Polska 2050`*weights$KOcoef[weights$okreg==33],
                            okreg==34 ~ (weights$validvotes[weights$okreg==34])*`Polska 2050`*weights$KOcoef[weights$okreg==34],
                            okreg==35 ~ (weights$validvotes[weights$okreg==35])*`Polska 2050`*weights$KOcoef[weights$okreg==35],
                            okreg==36 ~ (weights$validvotes[weights$okreg==36])*`Polska 2050`*weights$KOcoef[weights$okreg==36],
                            okreg==37 ~ (weights$validvotes[weights$okreg==37])*`Polska 2050`*weights$KOcoef[weights$okreg==37],
                            okreg==38 ~ (weights$validvotes[weights$okreg==38])*`Polska 2050`*weights$KOcoef[weights$okreg==38],
                            okreg==39 ~ (weights$validvotes[weights$okreg==39])*`Polska 2050`*weights$KOcoef[weights$okreg==39],
                            okreg==40 ~ (weights$validvotes[weights$okreg==40])*`Polska 2050`*weights$KOcoef[weights$okreg==40],
                            okreg==41 ~ (weights$validvotes[weights$okreg==41])*`Polska 2050`*weights$KOcoef[weights$okreg==41]
  ),
  PSL = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*PSL*weights$PSLcoef[weights$okreg==1],
                  okreg==2 ~ (weights$validvotes[weights$okreg==2])*PSL*weights$PSLcoef[weights$okreg==2],
                  okreg==3 ~ (weights$validvotes[weights$okreg==3])*PSL*weights$PSLcoef[weights$okreg==3],
                  okreg==4 ~ (weights$validvotes[weights$okreg==4])*PSL*weights$PSLcoef[weights$okreg==4],
                  okreg==5 ~ (weights$validvotes[weights$okreg==5])*PSL*weights$PSLcoef[weights$okreg==5],
                  okreg==6 ~ (weights$validvotes[weights$okreg==6])*PSL*weights$PSLcoef[weights$okreg==6],
                  okreg==7 ~ (weights$validvotes[weights$okreg==7])*PSL*weights$PSLcoef[weights$okreg==7],
                  okreg==8 ~ (weights$validvotes[weights$okreg==8])*PSL*weights$PSLcoef[weights$okreg==8],
                  okreg==9 ~ (weights$validvotes[weights$okreg==9])*PSL*weights$PSLcoef[weights$okreg==9],
                  okreg==10 ~ (weights$validvotes[weights$okreg==10])*PSL*weights$PSLcoef[weights$okreg==10],
                  okreg==11 ~ (weights$validvotes[weights$okreg==11])*PSL*weights$PSLcoef[weights$okreg==11],
                  okreg==12 ~ (weights$validvotes[weights$okreg==12])*PSL*weights$PSLcoef[weights$okreg==12],
                  okreg==13 ~ (weights$validvotes[weights$okreg==13])*PSL*weights$PSLcoef[weights$okreg==13],
                  okreg==14 ~ (weights$validvotes[weights$okreg==14])*PSL*weights$PSLcoef[weights$okreg==14],
                  okreg==15 ~ (weights$validvotes[weights$okreg==15])*PSL*weights$PSLcoef[weights$okreg==15],
                  okreg==16 ~ (weights$validvotes[weights$okreg==16])*PSL*weights$PSLcoef[weights$okreg==16],
                  okreg==17 ~ (weights$validvotes[weights$okreg==17])*PSL*weights$PSLcoef[weights$okreg==17],
                  okreg==18 ~ (weights$validvotes[weights$okreg==18])*PSL*weights$PSLcoef[weights$okreg==18],
                  okreg==19 ~ (weights$validvotes[weights$okreg==19])*PSL*weights$PSLcoef[weights$okreg==19],
                  okreg==20 ~ (weights$validvotes[weights$okreg==20])*PSL*weights$PSLcoef[weights$okreg==20],
                  okreg==21 ~ (weights$validvotes[weights$okreg==21])*PSL*weights$PSLcoef[weights$okreg==21],
                  okreg==22 ~ (weights$validvotes[weights$okreg==22])*PSL*weights$PSLcoef[weights$okreg==22],
                  okreg==23 ~ (weights$validvotes[weights$okreg==23])*PSL*weights$PSLcoef[weights$okreg==23],
                  okreg==24 ~ (weights$validvotes[weights$okreg==24])*PSL*weights$PSLcoef[weights$okreg==24],
                  okreg==25 ~ (weights$validvotes[weights$okreg==25])*PSL*weights$PSLcoef[weights$okreg==25],
                  okreg==26 ~ (weights$validvotes[weights$okreg==26])*PSL*weights$PSLcoef[weights$okreg==26],
                  okreg==27 ~ (weights$validvotes[weights$okreg==27])*PSL*weights$PSLcoef[weights$okreg==27],
                  okreg==28 ~ (weights$validvotes[weights$okreg==28])*PSL*weights$PSLcoef[weights$okreg==28],
                  okreg==29 ~ (weights$validvotes[weights$okreg==29])*PSL*weights$PSLcoef[weights$okreg==29],
                  okreg==30 ~ (weights$validvotes[weights$okreg==30])*PSL*weights$PSLcoef[weights$okreg==30],
                  okreg==31 ~ (weights$validvotes[weights$okreg==31])*PSL*weights$PSLcoef[weights$okreg==31],
                  okreg==32 ~ (weights$validvotes[weights$okreg==32])*PSL*weights$PSLcoef[weights$okreg==32],
                  okreg==33 ~ (weights$validvotes[weights$okreg==33])*PSL*weights$PSLcoef[weights$okreg==33],
                  okreg==34 ~ (weights$validvotes[weights$okreg==34])*PSL*weights$PSLcoef[weights$okreg==34],
                  okreg==35 ~ (weights$validvotes[weights$okreg==35])*PSL*weights$PSLcoef[weights$okreg==35],
                  okreg==36 ~ (weights$validvotes[weights$okreg==36])*PSL*weights$PSLcoef[weights$okreg==36],
                  okreg==37 ~ (weights$validvotes[weights$okreg==37])*PSL*weights$PSLcoef[weights$okreg==37],
                  okreg==38 ~ (weights$validvotes[weights$okreg==38])*PSL*weights$PSLcoef[weights$okreg==38],
                  okreg==39 ~ (weights$validvotes[weights$okreg==39])*PSL*weights$PSLcoef[weights$okreg==39],
                  okreg==40 ~ (weights$validvotes[weights$okreg==40])*PSL*weights$PSLcoef[weights$okreg==40],
                  okreg==41 ~ (weights$validvotes[weights$okreg==41])*PSL*weights$PSLcoef[weights$okreg==41]
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
                        PiS=rep(1,41000), `Polska 2050`=rep(1,41000), PSL=rep(1,41000))

for(i in 1:41000) { 
  poldHondt[i,] <- giveseats(v = c(consts$KO[i], consts$Konfederacja[i], consts$Lewica[i], consts$MN[i],
                                   consts$PiS[i], consts$`Polska 2050`[i], consts$PSL[i]), ns = consts$magnitude[i], method="dh", thresh=0)$seats
}

poldHondt <- cbind(poldHondt, consts$okreg, consts$.draw)

colnames(poldHondt) <- c("KO", "Konfederacja", "Lewica", "MN", "PiS", "Polska 2050", "PSL", "okreg", "draw")

poldHondt <- poldHondt %>% 
  group_by(draw) %>% 
  summarise(KO = sum(KO),
            PiS = sum(PiS),
            PSL = sum(PSL),
            Konfederacja = sum(Konfederacja),
            `Polska 2050` = sum(`Polska 2050`),
            MN = sum(MN),
            Lewica = sum(Lewica))

poldHondt <- poldHondt %>%
  pivot_longer(., cols=c("KO", "Konfederacja", "Lewica", "MN", "PiS", "Polska 2050", "PSL"), names_to="party", values_to="seats")

PSL_seats <- poldHondt %>%
  filter(., party=="PSL") %>%
  hypothesis(., "seats>0")

Konf_seats <- poldHondt %>%
  filter(., party=="Konfederacja") %>%
  hypothesis(., "seats>0")

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
frame$in2019[frame$party=="PSL"] <- 30
frame$in2019[frame$party=="MN"] <- 1
frame$in2019[frame$party=="Konfederacja"] <- 11
frame$in2019[frame$party=="Polska 2050"] <- 0
frame$party <- factor(frame$party, levels=c("PiS", "KO", "PSL", "Lewica", "Konfederacja", "Polska 2050", "MN"))
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
                            levels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL", "Polska2050"),
                            labels = c("PiS", "KO", "Lewica", "Konfederacja", "Other", "PSL","Polska 2050")))

plotdraws <- plotdraws %>%
  pivot_wider(names_from = .category, values_from = .value) %>%
  mutate(magnitude=460) %>%
  select(., c(.draw, PiS, KO, Lewica, PSL, Konfederacja, `Polska 2050`, magnitude)) 

plotdraws$MN <- rnorm(1000, mean=0.079, sd=0.00001)

plotdraws <- plotdraws %>%
  mutate(., PSL = ifelse(PSL<0.05, 0, PSL),
         PiS = ifelse(PiS<0.05, 0, PiS),
         KO = ifelse(KO<0.05, 0, KO),
         Konfederacja = ifelse(Konfederacja<0.05, 0, Konfederacja),
         Lewica = ifelse(Lewica<0.05, 0, Lewica),
         `Polska 2050` = ifelse(`Polska 2050`<0.05, 0, `Polska 2050`),
         PSL = ifelse(median_PSL<5, 0, PSL),
         PiS = ifelse(median_PiS<5, 0, PiS),
         KO = ifelse(median_KO<5, 0, KO),
         Konfederacja = ifelse(median_Konfederacja<5, 0, Konfederacja),
         Lewica = ifelse(median_Lewica<5, 0, Lewica),
         `Polska 2050` = ifelse(`median_Polska 2050`<5, 0, `Polska 2050`)
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
  `Polska 2050` = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*`Polska 2050`*weights$KOcoef[weights$okreg==1],
                            okreg==2 ~ (weights$validvotes[weights$okreg==2])*`Polska 2050`*weights$KOcoef[weights$okreg==2],
                            okreg==3 ~ (weights$validvotes[weights$okreg==3])*`Polska 2050`*weights$KOcoef[weights$okreg==3],
                            okreg==4 ~ (weights$validvotes[weights$okreg==4])*`Polska 2050`*weights$KOcoef[weights$okreg==4],
                            okreg==5 ~ (weights$validvotes[weights$okreg==5])*`Polska 2050`*weights$KOcoef[weights$okreg==5],
                            okreg==6 ~ (weights$validvotes[weights$okreg==6])*`Polska 2050`*weights$KOcoef[weights$okreg==6],
                            okreg==7 ~ (weights$validvotes[weights$okreg==7])*`Polska 2050`*weights$KOcoef[weights$okreg==7],
                            okreg==8 ~ (weights$validvotes[weights$okreg==8])*`Polska 2050`*weights$KOcoef[weights$okreg==8],
                            okreg==9 ~ (weights$validvotes[weights$okreg==9])*`Polska 2050`*weights$KOcoef[weights$okreg==9],
                            okreg==10 ~ (weights$validvotes[weights$okreg==10])*`Polska 2050`*weights$KOcoef[weights$okreg==10],
                            okreg==11 ~ (weights$validvotes[weights$okreg==11])*`Polska 2050`*weights$KOcoef[weights$okreg==11],
                            okreg==12 ~ (weights$validvotes[weights$okreg==12])*`Polska 2050`*weights$KOcoef[weights$okreg==12],
                            okreg==13 ~ (weights$validvotes[weights$okreg==13])*`Polska 2050`*weights$KOcoef[weights$okreg==13],
                            okreg==14 ~ (weights$validvotes[weights$okreg==14])*`Polska 2050`*weights$KOcoef[weights$okreg==14],
                            okreg==15 ~ (weights$validvotes[weights$okreg==15])*`Polska 2050`*weights$KOcoef[weights$okreg==15],
                            okreg==16 ~ (weights$validvotes[weights$okreg==16])*`Polska 2050`*weights$KOcoef[weights$okreg==16],
                            okreg==17 ~ (weights$validvotes[weights$okreg==17])*`Polska 2050`*weights$KOcoef[weights$okreg==17],
                            okreg==18 ~ (weights$validvotes[weights$okreg==18])*`Polska 2050`*weights$KOcoef[weights$okreg==18],
                            okreg==19 ~ (weights$validvotes[weights$okreg==19])*`Polska 2050`*weights$KOcoef[weights$okreg==19],
                            okreg==20 ~ (weights$validvotes[weights$okreg==20])*`Polska 2050`*weights$KOcoef[weights$okreg==20],
                            okreg==21 ~ (weights$validvotes[weights$okreg==21])*`Polska 2050`*weights$KOcoef[weights$okreg==21],
                            okreg==22 ~ (weights$validvotes[weights$okreg==22])*`Polska 2050`*weights$KOcoef[weights$okreg==22],
                            okreg==23 ~ (weights$validvotes[weights$okreg==23])*`Polska 2050`*weights$KOcoef[weights$okreg==23],
                            okreg==24 ~ (weights$validvotes[weights$okreg==24])*`Polska 2050`*weights$KOcoef[weights$okreg==24],
                            okreg==25 ~ (weights$validvotes[weights$okreg==25])*`Polska 2050`*weights$KOcoef[weights$okreg==25],
                            okreg==26 ~ (weights$validvotes[weights$okreg==26])*`Polska 2050`*weights$KOcoef[weights$okreg==26],
                            okreg==27 ~ (weights$validvotes[weights$okreg==27])*`Polska 2050`*weights$KOcoef[weights$okreg==27],
                            okreg==28 ~ (weights$validvotes[weights$okreg==28])*`Polska 2050`*weights$KOcoef[weights$okreg==28],
                            okreg==29 ~ (weights$validvotes[weights$okreg==29])*`Polska 2050`*weights$KOcoef[weights$okreg==29],
                            okreg==30 ~ (weights$validvotes[weights$okreg==30])*`Polska 2050`*weights$KOcoef[weights$okreg==30],
                            okreg==31 ~ (weights$validvotes[weights$okreg==31])*`Polska 2050`*weights$KOcoef[weights$okreg==31],
                            okreg==32 ~ (weights$validvotes[weights$okreg==32])*`Polska 2050`*weights$KOcoef[weights$okreg==32],
                            okreg==33 ~ (weights$validvotes[weights$okreg==33])*`Polska 2050`*weights$KOcoef[weights$okreg==33],
                            okreg==34 ~ (weights$validvotes[weights$okreg==34])*`Polska 2050`*weights$KOcoef[weights$okreg==34],
                            okreg==35 ~ (weights$validvotes[weights$okreg==35])*`Polska 2050`*weights$KOcoef[weights$okreg==35],
                            okreg==36 ~ (weights$validvotes[weights$okreg==36])*`Polska 2050`*weights$KOcoef[weights$okreg==36],
                            okreg==37 ~ (weights$validvotes[weights$okreg==37])*`Polska 2050`*weights$KOcoef[weights$okreg==37],
                            okreg==38 ~ (weights$validvotes[weights$okreg==38])*`Polska 2050`*weights$KOcoef[weights$okreg==38],
                            okreg==39 ~ (weights$validvotes[weights$okreg==39])*`Polska 2050`*weights$KOcoef[weights$okreg==39],
                            okreg==40 ~ (weights$validvotes[weights$okreg==40])*`Polska 2050`*weights$KOcoef[weights$okreg==40],
                            okreg==41 ~ (weights$validvotes[weights$okreg==41])*`Polska 2050`*weights$KOcoef[weights$okreg==41]
  ),
  PSL = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*PSL*weights$PSLcoef[weights$okreg==1],
                  okreg==2 ~ (weights$validvotes[weights$okreg==2])*PSL*weights$PSLcoef[weights$okreg==2],
                  okreg==3 ~ (weights$validvotes[weights$okreg==3])*PSL*weights$PSLcoef[weights$okreg==3],
                  okreg==4 ~ (weights$validvotes[weights$okreg==4])*PSL*weights$PSLcoef[weights$okreg==4],
                  okreg==5 ~ (weights$validvotes[weights$okreg==5])*PSL*weights$PSLcoef[weights$okreg==5],
                  okreg==6 ~ (weights$validvotes[weights$okreg==6])*PSL*weights$PSLcoef[weights$okreg==6],
                  okreg==7 ~ (weights$validvotes[weights$okreg==7])*PSL*weights$PSLcoef[weights$okreg==7],
                  okreg==8 ~ (weights$validvotes[weights$okreg==8])*PSL*weights$PSLcoef[weights$okreg==8],
                  okreg==9 ~ (weights$validvotes[weights$okreg==9])*PSL*weights$PSLcoef[weights$okreg==9],
                  okreg==10 ~ (weights$validvotes[weights$okreg==10])*PSL*weights$PSLcoef[weights$okreg==10],
                  okreg==11 ~ (weights$validvotes[weights$okreg==11])*PSL*weights$PSLcoef[weights$okreg==11],
                  okreg==12 ~ (weights$validvotes[weights$okreg==12])*PSL*weights$PSLcoef[weights$okreg==12],
                  okreg==13 ~ (weights$validvotes[weights$okreg==13])*PSL*weights$PSLcoef[weights$okreg==13],
                  okreg==14 ~ (weights$validvotes[weights$okreg==14])*PSL*weights$PSLcoef[weights$okreg==14],
                  okreg==15 ~ (weights$validvotes[weights$okreg==15])*PSL*weights$PSLcoef[weights$okreg==15],
                  okreg==16 ~ (weights$validvotes[weights$okreg==16])*PSL*weights$PSLcoef[weights$okreg==16],
                  okreg==17 ~ (weights$validvotes[weights$okreg==17])*PSL*weights$PSLcoef[weights$okreg==17],
                  okreg==18 ~ (weights$validvotes[weights$okreg==18])*PSL*weights$PSLcoef[weights$okreg==18],
                  okreg==19 ~ (weights$validvotes[weights$okreg==19])*PSL*weights$PSLcoef[weights$okreg==19],
                  okreg==20 ~ (weights$validvotes[weights$okreg==20])*PSL*weights$PSLcoef[weights$okreg==20],
                  okreg==21 ~ (weights$validvotes[weights$okreg==21])*PSL*weights$PSLcoef[weights$okreg==21],
                  okreg==22 ~ (weights$validvotes[weights$okreg==22])*PSL*weights$PSLcoef[weights$okreg==22],
                  okreg==23 ~ (weights$validvotes[weights$okreg==23])*PSL*weights$PSLcoef[weights$okreg==23],
                  okreg==24 ~ (weights$validvotes[weights$okreg==24])*PSL*weights$PSLcoef[weights$okreg==24],
                  okreg==25 ~ (weights$validvotes[weights$okreg==25])*PSL*weights$PSLcoef[weights$okreg==25],
                  okreg==26 ~ (weights$validvotes[weights$okreg==26])*PSL*weights$PSLcoef[weights$okreg==26],
                  okreg==27 ~ (weights$validvotes[weights$okreg==27])*PSL*weights$PSLcoef[weights$okreg==27],
                  okreg==28 ~ (weights$validvotes[weights$okreg==28])*PSL*weights$PSLcoef[weights$okreg==28],
                  okreg==29 ~ (weights$validvotes[weights$okreg==29])*PSL*weights$PSLcoef[weights$okreg==29],
                  okreg==30 ~ (weights$validvotes[weights$okreg==30])*PSL*weights$PSLcoef[weights$okreg==30],
                  okreg==31 ~ (weights$validvotes[weights$okreg==31])*PSL*weights$PSLcoef[weights$okreg==31],
                  okreg==32 ~ (weights$validvotes[weights$okreg==32])*PSL*weights$PSLcoef[weights$okreg==32],
                  okreg==33 ~ (weights$validvotes[weights$okreg==33])*PSL*weights$PSLcoef[weights$okreg==33],
                  okreg==34 ~ (weights$validvotes[weights$okreg==34])*PSL*weights$PSLcoef[weights$okreg==34],
                  okreg==35 ~ (weights$validvotes[weights$okreg==35])*PSL*weights$PSLcoef[weights$okreg==35],
                  okreg==36 ~ (weights$validvotes[weights$okreg==36])*PSL*weights$PSLcoef[weights$okreg==36],
                  okreg==37 ~ (weights$validvotes[weights$okreg==37])*PSL*weights$PSLcoef[weights$okreg==37],
                  okreg==38 ~ (weights$validvotes[weights$okreg==38])*PSL*weights$PSLcoef[weights$okreg==38],
                  okreg==39 ~ (weights$validvotes[weights$okreg==39])*PSL*weights$PSLcoef[weights$okreg==39],
                  okreg==40 ~ (weights$validvotes[weights$okreg==40])*PSL*weights$PSLcoef[weights$okreg==40],
                  okreg==41 ~ (weights$validvotes[weights$okreg==41])*PSL*weights$PSLcoef[weights$okreg==41]
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
                        PiS=rep(1,41000), `Polska 2050`=rep(1,41000), PSL=rep(1,41000))

for(i in 1:41000) { 
  poldHondt[i,] <- giveseats(v = c(consts$KO[i], consts$Konfederacja[i], consts$Lewica[i], consts$MN[i],
                                   consts$PiS[i], consts$`Polska 2050`[i], consts$PSL[i]), ns = consts$magnitude[i], method="dh", thresh=0)$seats
}

poldHondt <- cbind(poldHondt, consts$okreg, consts$.draw)

colnames(poldHondt) <- c("KO", "Konfederacja", "Lewica", "MN", "PiS", "Polska 2050", "PSL", "okreg", "draw")

poldHondt <- poldHondt %>% 
  group_by(draw) %>% 
  summarise(KO = sum(KO),
            PiS = sum(PiS),
            PSL = sum(PSL),
            Konfederacja = sum(Konfederacja),
            `Polska 2050` = sum(`Polska 2050`),
            MN = sum(MN),
            Lewica = sum(Lewica))

poldHondt <- poldHondt %>%
  pivot_longer(., cols=c("KO", "Konfederacja", "Lewica", "MN", "PiS", "Polska 2050", "PSL"), names_to="party", values_to="seats")

frame2 <- poldHondt %>%
  group_by(party) %>%
  summarise(mean_qi(seats)) %>%
  mutate(., y = round(y, 0),
         ymin = round(ymin, 0),
         ymax = round(ymax, 0))

frame2$in2019[frame2$party=="KO"] <- 134
frame2$in2019[frame2$party=="PiS"] <- 235
frame2$in2019[frame2$party=="Lewica"] <- 49
frame2$in2019[frame2$party=="PSL"] <- 30
frame2$in2019[frame2$party=="MN"] <- 1
frame2$in2019[frame2$party=="Konfederacja"] <- 11
frame2$in2019[frame2$party=="Polska 2050"] <- 0
frame2$party <- factor(frame2$party, levels=c("PiS", "KO", "PSL", "Lewica", "Konfederacja", "Polska 2050", "MN"))
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
  geom_label(aes(x=2, y=231), label="Legislative majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed Light") +
  geom_label(aes(x=2, y=276), label="Overturn presidential veto", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed Light") +
  geom_label(aes(x=2, y=307), label="Constitutional majority", size=3, adj=c(0), label.size=NA, fill="grey95", family="Roboto Condensed Light") +
  annotate("text", x=frame$party, y=c(frame$y+18), label=frame$y, size=4, family="Roboto Condensed Light")+
  annotate("text", x=frame$party, y=c(frame$y+8), label=paste("(",round(frame$ymin,0), "\u2013",round(frame$ymax,0),")", sep=""), size=3, family="Roboto Condensed Light") +
  labs(x="", y="% of vote", title="Estimated share of seats",
       subtitle="Mean estimated seat share with 95% credible intervals. Sum total may not equal 460.",
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_minimal() +
  theme_ipsum_rc() +
  theme_changes
ggsave(plot_seats_parl, file = "plot_seats_parl.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####House effects#####
houselevels <- get_labels(polls$org)

PiS_house <- as_tibble(as.data.frame(ranef(m1, summary=F)), repair_names("minimal")) %>%
  select(.,contains("PiS")) %>%
  setNames(houselevels) %>%
  mutate(party="PiS")

KO_house <- as_tibble(as.data.frame(ranef(m1, summary=F)), repair_names("minimal")) %>%
  select(.,contains("KO", ignore.case = F)) %>%
  setNames(houselevels) %>%
  mutate(party="KO")

`PSL_house` <- as_tibble(as.data.frame(ranef(m1, summary=F)), repair_names("minimal")) %>%
  select(.,contains("PSL", ignore.case = F)) %>%
  setNames(houselevels) %>%
  mutate(party="PSL")

Lewica_house <- as_tibble(as.data.frame(ranef(m1, summary=F)), repair_names("minimal")) %>%
  select(.,contains("Lewica", ignore.case = F)) %>%
  setNames(houselevels) %>%
  mutate(party="Lewica")

Konfederacja_house <- as_tibble(as.data.frame(ranef(m1, summary=F)), repair_names("minimal")) %>%
  select(.,contains("Konfederacja", ignore.case = F)) %>%
  setNames(houselevels) %>%
  mutate(party="Konfederacja")

`Polska 2050_house` <- as_tibble(as.data.frame(ranef(m1, summary=F)), repair_names("minimal")) %>%
  select(.,contains("Polska2050", ignore.case = F)) %>%
  setNames(houselevels) %>%
  mutate(party="Polska 2050")

p_house <- bind_rows(PiS_house, KO_house, `PSL_house`, Lewica_house, Konfederacja_house, `Polska 2050_house`) %>%
  pivot_longer(., cols=where(is.numeric), names_to="house") %>%
  separate(., house, c("house", "method"), sep=", ") %>%
  ggplot(aes(x = party, y = 10*value, color=house, shape=method)) +
  geom_abline(intercept=0, slope=0, colour="gray10", linetype=3) +
  stat_pointinterval(position = position_dodge(width = .7)) +
  guides(color = guide_legend(override.aes = list(size = 5, shape="|"), keywidth=0.3, keyheight=0.3, default.unit="inch")) +
  guides(shape = guide_legend(override.aes = list(size = 5, linetype=NULL), keywidth=0.3, keyheight=0.3, default.unit="inch")) +
  labs(color="Pollster", shape="Mode", x="", y="Deviation from mean party vote share (percent)", 
       title="House and mode effects", 
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_minimal() +
  theme_ipsum_rc() +
  theme_changes
ggsave(p_house, file = "p_house.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####Run model by pollster#####
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
        levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "Other"),
        labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other")
  )
  )

point_dta <- polls %>%
  select(org, midDate, PiS, KO, Lewica, Konfederacja, Other, PSL, Polska2050) %>%
  pivot_longer(
    cols = c(-midDate, -org),
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "Polska2050", "Lewica", "Konfederacja", "PSL", "Other"),
        labels = c("PiS", "KO", "Polska 2050", "Lewica", "Konfederacja", "PSL", "Other")
      )
  )


plot_trends_pollster <-
  ggplot() +
  geom_point(data=point_dta, aes(x = midDate, y = est, colour=party, fill=party), alpha = .5, size = 1, show.legend=FALSE) +
  stat_lineribbon(data=pred_dta, aes(x = date, y = .value, color=party, fill=party), .width=c(0.5, 0.66, 0.95), alpha=1/4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  facet_wrap(~org, nrow=3) +
  coord_cartesian(xlim = c(min(polls$midDate), max(polls$midDate)),
                  ylim = c(0, .5)) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols, guide=FALSE) +
  labs(y = "% of vote", x="", title = "Trends by pollster",
       subtitle="Only pollsters with at least five polls are included.", color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_changes
ggsave(plot_trends_pollster , file = "plot_trends_pollster.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)


#####INCLUDING DON'T KNOWS#####
polls <- read_excel('polldata.xlsx')

polls <- unite(polls, org, remark, col="org", sep="_") %>%
  filter(., DK!=0)
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)
polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + (difftime(endDate, startDate)/2)),
         midDate_int=as.integer(midDate)) %>%
  #filter(midDate_int > (max(midDate_int)-150)) %>%
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

m1 <-
  brm(formula = bf(outcome ~ 1 + s(time, k = 10) + (1 | pollster)),
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
  labs(y = "% of vote", x="", title = "Trends (including undecided voters)", 
       subtitle=str_c("Data from ", names, "."), color="", caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_minimal() +
  theme_ipsum_rc() +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_changes
ggsave(plot_trends_parl_DK, file = "plot_trends_parl_DK.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

#####Save image out#####
save.image("~/Desktop/PoolingthePoles.RData")