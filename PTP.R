#####Prepare workspace
system("git pull")
pacman::p_load(tidyverse, googledrive, rio, readxl, sf, glue, sjlabelled, 
               lubridate, brms, stringr, tidybayes, ggdist, ggblend, seatdist)

set.seed(780045)

theme_plots <- function(base_size = 11, base_family = "Jost") {
  theme_bw(base_size, base_family) +
    theme(
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      title = element_text(size = rel(1), family = "Jost", face = "bold"),
      plot.subtitle = element_text(size = rel(0.8), family = "Jost", face = "plain"),
      plot.caption = element_text(margin = margin(t = 10), size = rel(0.6), family = "Jost", face = "plain"),
      panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.15),
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(size = rel(0.8), family = "Jost", face = "plain"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.8), vjust = 0.5, family = "Jost", face = "bold"),
      legend.key.size = unit(0.7, "line"),
      legend.key = element_blank(),
      legend.spacing = unit(0.1, "lines"),
      legend.justification = "left",
      legend.margin = margin(t = -5, b = 0, l = 0, r = 0),
      strip.text = element_text(size = rel(0.9), hjust = 0, family = "Jost", face = "plain"),
      strip.background = element_rect(fill = "white", colour = NA),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )
}

theme_plots_map <- function(base_size = 11, base_family = "Jost") {
  theme_minimal(base_size, base_family) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.text.x = element_text(size = 10, family = "Jost", face = "plain"),
      legend.text = element_text(size=9, family = "Jost", face = "plain"),
      title = element_text(size = rel(1), family = "Jost", face = "bold"),
      plot.subtitle = element_text(size = rel(0.8), family = "Jost", face = "plain"),
      plot.caption = element_text(margin = margin(t = 10), size = rel(0.6), family = "Jost", face = "plain"),
      legend.title = element_text(family = "Jost", face = "plain"),
      plot.title = element_text(family = "Jost", face = "bold"),
      aspect.ratio = 1,
      legend.position = "none"
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

#####Read in, adjust and subset data#####
source("poll data scraper.R")

polls <- polls_cleaned

polls <- polls %>%
  select(startDate, endDate, org, PiS, KO, Lewica, Razem, Polska2050, PSL, Konfederacja, Other, DK)

weights <- read_excel('2023_elec_percentages.xlsx')
const <- st_read('GRED_20190215_Poland_2011.shp')

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
         Razem = 100/((100-DK))*Razem,
         Polska2050 = 100/((100-DK))*Polska2050,
         PSL = 100/((100-DK))*PSL,
         Konfederacja = 100/((100-DK))*Konfederacja,
         Other = 100/((100-DK))*Other,
         time = as.integer(difftime(midDate, min(midDate), units = "days")),
         pollster = as.integer(factor(org)))

cols <- c("PiS"="blue", "KO"="orange", "Polska 2050"="goldenrod", "PSL"="darkgreen", 
          "Konfederacja" = "midnightblue", "Lewica" = "red", "Razem" = "purple", 
          "MN" = "yellow", "Other"="gray50")

names <- data.frame(as.factor(get_labels(polls$org)))
names$house <- names$as.factor.get_labels.polls.org
names$house <- as.factor(names$house)
names <- glue_collapse(get_labels(names$house), ", ", last = " and ")

polls <-
  polls %>%
  mutate(time = interval(min(midDate), midDate)/years(1))

polls[names(polls) %in% c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Polska2050", "PSL")] <-
  polls[names(polls) %in% c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Polska2050", "PSL")] %>%
  mutate_all(function(x) (as.numeric(str_remove(x, "%"))/100))

polls <-
  polls %>%
  mutate(Other = 1 - (PiS + KO + Lewica + Razem + Konfederacja + Polska2050 + PSL),
         check = PiS + KO + Lewica + Razem + Konfederacja + Polska2050 + PSL + Other) %>%
  filter(Other>0)

polls <-
  polls %>%
  mutate(
    outcome = as.matrix(polls[names(polls) %in% c("PiS", "KO", "Lewica", "Razem", "Polska2050", "PSL", "Konfederacja", "Other")])
  )

party_cols <- c("PiS", "KO", "Lewica", "Razem", "Polska2050", "PSL", "Konfederacja", "Other")
outcome_matrix <- as.matrix(polls[, party_cols])
cat("Current zeros in outcome_matrix:", sum(outcome_matrix == 0), "\n")

# Apply fix: Add small constant to all values and renormalize
tiny_constant <- 0.0005  # 0.05% - adjust if needed
outcome_matrix_fixed <- outcome_matrix + tiny_constant
outcome_matrix_fixed <- outcome_matrix_fixed / rowSums(outcome_matrix_fixed)

# Update all data structures
outcome_matrix <- outcome_matrix_fixed
polls$outcome <- outcome_matrix_fixed

# Update individual party columns
for(i in 1:length(party_cols)) {
  polls[[party_cols[i]]] <- outcome_matrix_fixed[, i]
}

#####Run model#####
m1 <-
  brm(formula = bf(outcome ~ 1 + s(time, k = 12, bs = "cs", m = 2) + (1 | pollster)),
      family = dirichlet(link = "logit", refcat = "Other"),
      prior =
        prior(normal(0, 1.5), class = "Intercept", dpar = "muPiS") +
        prior(exponential(2), class = "sd", dpar = "muPiS") +
        prior(exponential(2), class = "sds", dpar = "muPiS") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muKO") +
        prior(exponential(2), class = "sd", dpar = "muKO") +
        prior(exponential(2), class = "sds", dpar = "muKO") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muLewica") +
        prior(exponential(2), class = "sd", dpar = "muLewica") +
        prior(exponential(2), class = "sds", dpar = "muLewica") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muRazem") +
        prior(exponential(2), class = "sd", dpar = "muRazem") +
        prior(exponential(2), class = "sds", dpar = "muRazem") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muPolska2050") +
        prior(exponential(2), class = "sd", dpar = "muPolska2050") +
        prior(exponential(2), class = "sds", dpar = "muPolska2050") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muPSL") +
        prior(exponential(2), class = "sd", dpar = "muPSL") +
        prior(exponential(2), class = "sds", dpar = "muPSL") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "muKonfederacja") +
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
        levels = c("PiS", "KO", "Polska2050", "PSL", "Lewica", "Razem", "Konfederacja", "Other"),
        labels = c("PiS", "KO", "Polska 2050", "PSL", "Lewica", "Razem", "Konfederacja", "Other")
      )
  )

point_dta <-
  polls[names(polls) %in% c("midDate", "PiS", "KO", "Lewica", "Razem", "Konfederacja", "Other", "Polska2050", "PSL")] %>%
  pivot_longer(
    cols = -midDate,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("PiS", "KO", "Polska2050", "PSL", "Lewica", "Razem", "Konfederacja", "Other"),
        labels = c("PiS", "KO", "Polska 2050", "PSL", "Lewica", "Razem", "Konfederacja", "Other")
      )
  )

trends_parl <- pred_dta %>%
  ggplot(aes(x = date, color=party, fill=party)) +
  ggdist::stat_lineribbon(
    aes(y = .value, fill_ramp = stat(.width)),
    .width = seq(0, 0.95, 0.01)
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
       subtitle=str_wrap(str_c("Data from ", paste(names, collapse=", "), "."), width = 120), 
       color="", caption = ".") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill=NA))) +
  theme_plots()

ggsave(trends_parl, file = "trends_parl.png",
       width = 7, height = 5, units = "cm", dpi=600, scale = 3, bg="white", device=png(type="cairo"))

#####Latest plot#####
plotdraws <- add_fitted_draws(
  model = m1,
  newdata =
    tibble(time = today),
  re_formula = NA
) %>%
  group_by(.category) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Other", "PSL", "Polska2050"),
                            labels = c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Other", "PSL", "Polska 2050")))

medians <- plotdraws %>%
  summarise(est = median(.value)*100, .groups = "drop")

# Calculate dynamic probability comparison between PiS and KO
comparison_data <- plotdraws %>%
  pivot_wider(names_from=.category, values_from=.value) %>%
  mutate(PiS_leading = PiS > KO,
         KO_leading = KO > PiS)

# Determine which party is leading based on medians
pis_median <- medians$est[medians$.category=="PiS"]/100
ko_median <- medians$est[medians$.category=="KO"]/100

if (pis_median > ko_median) {
  # PiS is leading, show probability PiS > KO
  lead_prob <- mean(comparison_data$PiS_leading)
  lead_text <- paste("Pr(PiS > KO) = ", round(lead_prob, 2))
  lead_party <- "PiS"
} else {
  # KO is leading, show probability KO > PiS  
  lead_prob <- mean(comparison_data$KO_leading)
  lead_text <- paste("Pr(KO > PiS) = ", round(lead_prob, 2))
  lead_party <- "KO"
}

# Calculate 5% threshold probabilities for parties with overlapping credible intervals
threshold_probs <- plotdraws %>%
  group_by(.category) %>%
  summarise(
    median = median(.value),
    lower_95 = quantile(.value, 0.025),
    upper_95 = quantile(.value, 0.975),
    prob_above_5 = mean(.value >= 0.05),
    .groups = "drop"
  ) %>%
  # Only include parties where credible interval overlaps with 5% threshold
  filter((lower_95 < 0.05 & upper_95 > 0.05) | (median < 0.06 & median > 0.04)) %>%
  mutate(threshold_text = paste("Pr(≥5%) = ", round(prob_above_5, 2)))

latest_parl <-
  add_fitted_draws(
    model = m1,
    newdata =
      tibble(time = today),
    re_formula = NA
  ) %>%
  group_by(.category) %>%
  mutate(.category = factor(.category,
                            levels = c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Other", "PSL", "Polska2050"),
                            labels = c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Other", "PSL", "Polska 2050"))) %>%
  ggplot(aes(y=reorder(.category, dplyr::desc(-.value)), 
             x=.value, color=.category)) +
  # Add 5% threshold line first (behind everything else)
  geom_vline(xintercept = 0.05, color = "grey60", linetype = "dashed", linewidth = 0.5) +
  stat_interval(aes(x=.value, color_ramp = stat(.width)), .width = ppoints(100)) %>%
  partition(vars(.category)) +
  scale_fill_manual(values=cols, guide=FALSE) +
  scale_color_manual(name=" ", values=cols, guide=FALSE) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_discrete(name="") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PiS"],0)),
           y="PiS", x=medians$est[medians$.category=="PiS"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Jost") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="KO"],0)),
           y="KO", x=medians$est[medians$.category=="KO"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Jost") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Polska 2050"],0)),
           y="Polska 2050", x=medians$est[medians$.category=="Polska 2050"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Jost") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="PSL"],0)),
           y="PSL", x=medians$est[medians$.category=="PSL"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Jost") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Lewica"],0)),
           y="Lewica", x=medians$est[medians$.category=="Lewica"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Jost") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Razem"],0)),
           y="Razem", x=medians$est[medians$.category=="Razem"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Jost") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Konfederacja"],0)),
           y="Konfederacja", x=medians$est[medians$.category=="Konfederacja"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Jost") +
  annotate(geom = "text", label=paste(round(medians$est[medians$.category=="Other"],0)),
           y="Other", x=medians$est[medians$.category=="Other"]/100, size=3.5, hjust = "center", vjust=-1,
           family="Jost", fontface="plain") +
  # Dynamic probability annotation based on which party is leading (smaller size, red color)
  annotate(geom = "text", label=lead_text, y=lead_party,
           x=quantile(plotdraws$.value[plotdraws$.category==lead_party], 0.005), adj=c(1), family="Jost", fontface="plain", size=3, color="red") +
  # Add threshold probability annotations for parties near 5%
  {if(nrow(threshold_probs) > 0) {
    lapply(1:nrow(threshold_probs), function(i) {
      party_name <- threshold_probs$.category[i]
      threshold_text <- threshold_probs$threshold_text[i]
      x_pos <- quantile(plotdraws$.value[plotdraws$.category==party_name], 0.995)
      
      annotate(geom = "text", label=threshold_text, y=party_name,
               x=x_pos, adj=c(0), family="Jost", fontface="plain", size=3, color="red")
    })
  }} +
  scale_x_continuous(breaks=c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c("0", "5", "10", "20", "30", "40", "50")) +
  expand_limits(x = 0) +
  labs(y = "", x="", title = "Latest estimates",
       subtitle=str_wrap(str_c("Data from ", paste(names, collapse=", "), "."), width = 120), 
       color="", caption = ".") +
  theme_plots()

ggsave(latest_parl, file = "latest_parl.png",
       width = 7, height = 5, units = "cm", dpi=600, scale = 3, bg="white")


#####Seat maps#####
library(seatdist)

median_PiS <- ifelse(medians$est[medians$.category=="PiS"] >=5, medians$est[medians$.category=="PiS"], 0)
median_KO <- ifelse(medians$est[medians$.category=="KO"] >=5, medians$est[medians$.category=="KO"], 0)
median_Lewica <- ifelse(medians$est[medians$.category=="Lewica"] >=5, medians$est[medians$.category=="Lewica"], 0)
median_Razem <- ifelse(medians$est[medians$.category=="Razem"] >=5, medians$est[medians$.category=="Razem"], 0)
median_Konfederacja <- ifelse(medians$est[medians$.category=="Konfederacja"] >=5, medians$est[medians$.category=="Konfederacja"], 0)
median_Polska2050 <- ifelse(medians$est[medians$.category=="Polska 2050"] >=5, medians$est[medians$.category=="Polska 2050"], 0)
median_PSL <- ifelse(medians$est[medians$.category=="PSL"] >=5, medians$est[medians$.category=="PSL"], 0)

PiSpct <- round(weights$PiScoef*median_PiS, digits=2)
KOpct <- round(weights$KOcoef*median_KO, digits=2)
Lewicapct <- round(weights$Lewicacoef*median_Lewica, digits=2)
Razempct <- round(weights$Lewicacoef*median_Razem, digits=2)  # Using same coef as Lewica
Konfederacjapct <- round(weights$Konfcoef*median_Konfederacja, digits=2)
Polska2050pct <- round(weights$TDcoef*median_Polska2050, digits=2)
PSLpct <- round(weights$TDcoef*median_PSL, digits=2)
MNpct <- c(0.12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.37, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

KOest <- (weights$validvotes/100)*KOpct
PiSest <- (weights$validvotes/100)*PiSpct
Lewicaest <- (weights$validvotes/100)*Lewicapct
Razemest <- (weights$validvotes/100)*Razempct
Konfederacjaest <- (weights$validvotes/100)*Konfederacjapct
Polska2050est <- (weights$validvotes/100)*Polska2050pct
PSLest <- (weights$validvotes/100)*PSLpct
MNest <- (weights$validvotes/100)*MNpct

poldHondt <- data.frame(KO=rep(1,42), Konfederacja=rep(1,42), Lewica=rep(1,42), Razem=rep(1,42), 
                        MN=rep(1,42), PiS=rep(1,42), Polska2050=rep(1,42), PSL=rep(1,42))

for( i in 1 : 42 ) {
  poldHondt[i,] <- c(giveseats(v = c(KOest[i], Konfederacjaest[i], Lewicaest[i], Razemest[i], MNest[i], 
                                     PiSest[i], Polska2050est[i], PSLest[i]), ns = weights$magnitude[i], method="dh", thresh=5))$seats
}

#seats table
seats <- cbind(poldHondt, weights)
row.names(seats) <- weights$name
keep <- c("KO","Konfederacja","Lewica","Razem","MN", "PiS", "Polska2050", "PSL")
colnames(seats) <- c("KO", "Konfederacja","Lewica","Razem","MN", "PiS", "Polska2050", "PSL")
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

p_pis <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(PiS)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=PiS, label=PiS), fill="white") +
  scale_fill_gradient(name="PiS", limits=c(min=0, max=20), low = "white", high = "blue", guide="colorbar") +
  labs(title="Constituency-level share of seats for PiS", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "") +
  theme_plots_map()
ggsave(p_pis, file = "PiS_seats.png", 
       width = 7, height = 7, units = "cm", dpi=600, scale = 3, bg="white")

p_ko <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(KO)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=KO, label=KO), fill="white") +
  scale_fill_gradient(name="KO", limits=c(min=0, max=20), low = "white", high = "orange", guide="colorbar") +
  labs(title="Constituency-level share of seats for Koalicja Obywatelska", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "") +
  theme_plots_map()
ggsave(p_ko, file = "KO_seats.png", 
       width = 7, height = 7, units = "cm", dpi=600, scale = 3, bg="white")

p_lewica <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(Lewica)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=Lewica, label=Lewica), fill="white") +
  scale_fill_gradient(name="Lewica", limits=c(min=0, max=20), low = "white", high = "red", guide="colorbar") +
  labs(title="Constituency-level share of seats for Lewica", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "") +
  theme_plots_map()
ggsave(p_lewica, file = "Lewica_seats.png", 
       width = 7, height = 7, units = "cm", dpi=600, scale = 3, bg="white")

p_razem <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(Razem)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=Razem, label=Razem), fill="white") +
  scale_fill_gradient(name="Razem", limits=c(min=0, max=20), low = "white", high = "purple", guide="colorbar") +
  labs(title="Constituency-level share of seats for Razem", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "") +
  theme_plots_map()
ggsave(p_razem, file = "Razem_seats.png", 
       width = 7, height = 7, units = "cm", dpi=600, scale = 3, bg="white")

p_PSL <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(PSL)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=PSL, label=PSL), fill="white") +
  scale_fill_gradient(name="PSL", limits=c(min=0, max=20), low = "white", high = "darkgreen", guide="colorbar") +
  labs(title="Constituency-level share of seats for PSL", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "") +
  theme_plots_map()
ggsave(p_PSL, file = "PSL_seats.png", 
       width = 7, height = 7, units = "cm", dpi=600, scale = 3, bg="white")

p_konfederacja <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(Konfederacja)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=Konfederacja, label=Konfederacja), fill="white") +
  scale_fill_gradient(name="Konfederacja", limits=c(min=0, max=20), low = "white", high = "midnightblue", guide="colorbar") +
  labs(title="Constituency-level share of seats for Konfederacja", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "") +
  theme_plots_map()
ggsave(p_konfederacja, file = "Konfederacja_seats.png", 
       width = 7, height = 7, units = "cm", dpi=600, scale = 3, bg="white")

p_P2050 <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(Polska2050)))+
  theme(aspect.ratio=1) +
  geom_label(aes(x=x, y=y, group=Polska2050, label=Polska2050), fill="white") +
  scale_fill_gradient(name="Polska2050", limits=c(min=0, max=20), low = "white", high = "goldenrod", guide="colorbar") +
  labs(title="Constituency-level share of seats for Polska 2050", subtitle="Seat distribution reflects regional levels of support at October 2023 election", 
       caption = "") +
  theme_plots_map()
ggsave(p_P2050, file = "P2050_seats.png", 
       width = 7, height = 7, units = "cm", dpi=600, scale = 3, bg="white")

p_pis_ko <- ggplot(plotdata)+
  geom_sf(aes(fill=as.integer(PiSmKO)))+
  theme(aspect.ratio=1) +
  scale_fill_gradient2(name="PiSKO", limits=c(min=-20, max=20), low = "orange", mid="white", high = "blue", midpoint=0, guide="colorbar") +
  labs(title="Constituency-level differences in share of seats for PiS and Koalicja Obywatelska", subtitle="Constituencies in shades of blue have more PiS MPs; constituencies in orange have more KO MPs;\nconstituencies in white have equal numbers of PiS and KO MPs", 
       caption = "") +
  theme_plots_map()
ggsave(p_pis_ko, file = "PiSKO_seats.png", 
       width = 7, height = 7, units = "cm", dpi=600, scale = 3, bg="white")


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
                            levels = c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Other", "PSL", "Polska2050"),
                            labels = c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Other", "PSL", "Polska 2050")))

plotdraws <- plotdraws %>%
  pivot_wider(names_from = .category, values_from = .value) %>%
  mutate(magnitude=460) %>%
  select(., c(.draw, PiS, KO, Lewica, Razem, Konfederacja, `Polska 2050`, PSL, magnitude)) 

plotdraws$MN <- rnorm(1000, mean=0.079, sd=0.00001)

party_medians <- plotdraws %>%
  ungroup() %>%
  summarise(
    PiS_med = median(PiS),
    KO_med = median(KO),
    Lewica_med = median(Lewica),
    Razem_med = median(Razem),
    Konfederacja_med = median(Konfederacja),
    Polska2050_med = median(`Polska 2050`),
    PSL_med = median(PSL)
  )

plotdraws <- plotdraws %>%
  mutate(
    PiS = ifelse(party_medians$PiS_med < 0.05, 0, PiS),
    KO = ifelse(party_medians$KO_med < 0.05, 0, KO),
    Lewica = ifelse(party_medians$Lewica_med < 0.05, 0, Lewica),
    Razem = ifelse(party_medians$Razem_med < 0.05, 0, Razem),
    Konfederacja = ifelse(party_medians$Konfederacja_med < 0.05, 0, Konfederacja),
    `Polska 2050` = ifelse(party_medians$Polska2050_med < 0.05, 0, `Polska 2050`),
    PSL = ifelse(party_medians$PSL_med < 0.05, 0, PSL)
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
  `Polska 2050` = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*`Polska 2050`*weights$TDcoef[weights$okreg==1],
                            okreg==2 ~ (weights$validvotes[weights$okreg==2])*`Polska 2050`*weights$TDcoef[weights$okreg==2],
                            okreg==3 ~ (weights$validvotes[weights$okreg==3])*`Polska 2050`*weights$TDcoef[weights$okreg==3],
                            okreg==4 ~ (weights$validvotes[weights$okreg==4])*`Polska 2050`*weights$TDcoef[weights$okreg==4],
                            okreg==5 ~ (weights$validvotes[weights$okreg==5])*`Polska 2050`*weights$TDcoef[weights$okreg==5],
                            okreg==6 ~ (weights$validvotes[weights$okreg==6])*`Polska 2050`*weights$TDcoef[weights$okreg==6],
                            okreg==7 ~ (weights$validvotes[weights$okreg==7])*`Polska 2050`*weights$TDcoef[weights$okreg==7],
                            okreg==8 ~ (weights$validvotes[weights$okreg==8])*`Polska 2050`*weights$TDcoef[weights$okreg==8],
                            okreg==9 ~ (weights$validvotes[weights$okreg==9])*`Polska 2050`*weights$TDcoef[weights$okreg==9],
                            okreg==10 ~ (weights$validvotes[weights$okreg==10])*`Polska 2050`*weights$TDcoef[weights$okreg==10],
                            okreg==11 ~ (weights$validvotes[weights$okreg==11])*`Polska 2050`*weights$TDcoef[weights$okreg==11],
                            okreg==12 ~ (weights$validvotes[weights$okreg==12])*`Polska 2050`*weights$TDcoef[weights$okreg==12],
                            okreg==13 ~ (weights$validvotes[weights$okreg==13])*`Polska 2050`*weights$TDcoef[weights$okreg==13],
                            okreg==14 ~ (weights$validvotes[weights$okreg==14])*`Polska 2050`*weights$TDcoef[weights$okreg==14],
                            okreg==15 ~ (weights$validvotes[weights$okreg==15])*`Polska 2050`*weights$TDcoef[weights$okreg==15],
                            okreg==16 ~ (weights$validvotes[weights$okreg==16])*`Polska 2050`*weights$TDcoef[weights$okreg==16],
                            okreg==17 ~ (weights$validvotes[weights$okreg==17])*`Polska 2050`*weights$TDcoef[weights$okreg==17],
                            okreg==18 ~ (weights$validvotes[weights$okreg==18])*`Polska 2050`*weights$TDcoef[weights$okreg==18],
                            okreg==19 ~ (weights$validvotes[weights$okreg==19])*`Polska 2050`*weights$TDcoef[weights$okreg==19],
                            okreg==20 ~ (weights$validvotes[weights$okreg==20])*`Polska 2050`*weights$TDcoef[weights$okreg==20],
                            okreg==21 ~ (weights$validvotes[weights$okreg==21])*`Polska 2050`*weights$TDcoef[weights$okreg==21],
                            okreg==22 ~ (weights$validvotes[weights$okreg==22])*`Polska 2050`*weights$TDcoef[weights$okreg==22],
                            okreg==23 ~ (weights$validvotes[weights$okreg==23])*`Polska 2050`*weights$TDcoef[weights$okreg==23],
                            okreg==24 ~ (weights$validvotes[weights$okreg==24])*`Polska 2050`*weights$TDcoef[weights$okreg==24],
                            okreg==25 ~ (weights$validvotes[weights$okreg==25])*`Polska 2050`*weights$TDcoef[weights$okreg==25],
                            okreg==26 ~ (weights$validvotes[weights$okreg==26])*`Polska 2050`*weights$TDcoef[weights$okreg==26],
                            okreg==27 ~ (weights$validvotes[weights$okreg==27])*`Polska 2050`*weights$TDcoef[weights$okreg==27],
                            okreg==28 ~ (weights$validvotes[weights$okreg==28])*`Polska 2050`*weights$TDcoef[weights$okreg==28],
                            okreg==29 ~ (weights$validvotes[weights$okreg==29])*`Polska 2050`*weights$TDcoef[weights$okreg==29],
                            okreg==30 ~ (weights$validvotes[weights$okreg==30])*`Polska 2050`*weights$TDcoef[weights$okreg==30],
                            okreg==31 ~ (weights$validvotes[weights$okreg==31])*`Polska 2050`*weights$TDcoef[weights$okreg==31],
                            okreg==32 ~ (weights$validvotes[weights$okreg==32])*`Polska 2050`*weights$TDcoef[weights$okreg==32],
                            okreg==33 ~ (weights$validvotes[weights$okreg==33])*`Polska 2050`*weights$TDcoef[weights$okreg==33],
                            okreg==34 ~ (weights$validvotes[weights$okreg==34])*`Polska 2050`*weights$TDcoef[weights$okreg==34],
                            okreg==35 ~ (weights$validvotes[weights$okreg==35])*`Polska 2050`*weights$TDcoef[weights$okreg==35],
                            okreg==36 ~ (weights$validvotes[weights$okreg==36])*`Polska 2050`*weights$TDcoef[weights$okreg==36],
                            okreg==37 ~ (weights$validvotes[weights$okreg==37])*`Polska 2050`*weights$TDcoef[weights$okreg==37],
                            okreg==38 ~ (weights$validvotes[weights$okreg==38])*`Polska 2050`*weights$TDcoef[weights$okreg==38],
                            okreg==39 ~ (weights$validvotes[weights$okreg==39])*`Polska 2050`*weights$TDcoef[weights$okreg==39],
                            okreg==40 ~ (weights$validvotes[weights$okreg==40])*`Polska 2050`*weights$TDcoef[weights$okreg==40],
                            okreg==41 ~ (weights$validvotes[weights$okreg==41])*`Polska 2050`*weights$TDcoef[weights$okreg==41]
  ),
  PSL = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*PSL*weights$TDcoef[weights$okreg==1],
                  okreg==2 ~ (weights$validvotes[weights$okreg==2])*PSL*weights$TDcoef[weights$okreg==2],
                  okreg==3 ~ (weights$validvotes[weights$okreg==3])*PSL*weights$TDcoef[weights$okreg==3],
                  okreg==4 ~ (weights$validvotes[weights$okreg==4])*PSL*weights$TDcoef[weights$okreg==4],
                  okreg==5 ~ (weights$validvotes[weights$okreg==5])*PSL*weights$TDcoef[weights$okreg==5],
                  okreg==6 ~ (weights$validvotes[weights$okreg==6])*PSL*weights$TDcoef[weights$okreg==6],
                  okreg==7 ~ (weights$validvotes[weights$okreg==7])*PSL*weights$TDcoef[weights$okreg==7],
                  okreg==8 ~ (weights$validvotes[weights$okreg==8])*PSL*weights$TDcoef[weights$okreg==8],
                  okreg==9 ~ (weights$validvotes[weights$okreg==9])*PSL*weights$TDcoef[weights$okreg==9],
                  okreg==10 ~ (weights$validvotes[weights$okreg==10])*PSL*weights$TDcoef[weights$okreg==10],
                  okreg==11 ~ (weights$validvotes[weights$okreg==11])*PSL*weights$TDcoef[weights$okreg==11],
                  okreg==12 ~ (weights$validvotes[weights$okreg==12])*PSL*weights$TDcoef[weights$okreg==12],
                  okreg==13 ~ (weights$validvotes[weights$okreg==13])*PSL*weights$TDcoef[weights$okreg==13],
                  okreg==14 ~ (weights$validvotes[weights$okreg==14])*PSL*weights$TDcoef[weights$okreg==14],
                  okreg==15 ~ (weights$validvotes[weights$okreg==15])*PSL*weights$TDcoef[weights$okreg==15],
                  okreg==16 ~ (weights$validvotes[weights$okreg==16])*PSL*weights$TDcoef[weights$okreg==16],
                  okreg==17 ~ (weights$validvotes[weights$okreg==17])*PSL*weights$TDcoef[weights$okreg==17],
                  okreg==18 ~ (weights$validvotes[weights$okreg==18])*PSL*weights$TDcoef[weights$okreg==18],
                  okreg==19 ~ (weights$validvotes[weights$okreg==19])*PSL*weights$TDcoef[weights$okreg==19],
                  okreg==20 ~ (weights$validvotes[weights$okreg==20])*PSL*weights$TDcoef[weights$okreg==20],
                  okreg==21 ~ (weights$validvotes[weights$okreg==21])*PSL*weights$TDcoef[weights$okreg==21],
                  okreg==22 ~ (weights$validvotes[weights$okreg==22])*PSL*weights$TDcoef[weights$okreg==22],
                  okreg==23 ~ (weights$validvotes[weights$okreg==23])*PSL*weights$TDcoef[weights$okreg==23],
                  okreg==24 ~ (weights$validvotes[weights$okreg==24])*PSL*weights$TDcoef[weights$okreg==24],
                  okreg==25 ~ (weights$validvotes[weights$okreg==25])*PSL*weights$TDcoef[weights$okreg==25],
                  okreg==26 ~ (weights$validvotes[weights$okreg==26])*PSL*weights$TDcoef[weights$okreg==26],
                  okreg==27 ~ (weights$validvotes[weights$okreg==27])*PSL*weights$TDcoef[weights$okreg==27],
                  okreg==28 ~ (weights$validvotes[weights$okreg==28])*PSL*weights$TDcoef[weights$okreg==28],
                  okreg==29 ~ (weights$validvotes[weights$okreg==29])*PSL*weights$TDcoef[weights$okreg==29],
                  okreg==30 ~ (weights$validvotes[weights$okreg==30])*PSL*weights$TDcoef[weights$okreg==30],
                  okreg==31 ~ (weights$validvotes[weights$okreg==31])*PSL*weights$TDcoef[weights$okreg==31],
                  okreg==32 ~ (weights$validvotes[weights$okreg==32])*PSL*weights$TDcoef[weights$okreg==32],
                  okreg==33 ~ (weights$validvotes[weights$okreg==33])*PSL*weights$TDcoef[weights$okreg==33],
                  okreg==34 ~ (weights$validvotes[weights$okreg==34])*PSL*weights$TDcoef[weights$okreg==34],
                  okreg==35 ~ (weights$validvotes[weights$okreg==35])*PSL*weights$TDcoef[weights$okreg==35],
                  okreg==36 ~ (weights$validvotes[weights$okreg==36])*PSL*weights$TDcoef[weights$okreg==36],
                  okreg==37 ~ (weights$validvotes[weights$okreg==37])*PSL*weights$TDcoef[weights$okreg==37],
                  okreg==38 ~ (weights$validvotes[weights$okreg==38])*PSL*weights$TDcoef[weights$okreg==38],
                  okreg==39 ~ (weights$validvotes[weights$okreg==39])*PSL*weights$TDcoef[weights$okreg==39],
                  okreg==40 ~ (weights$validvotes[weights$okreg==40])*PSL*weights$TDcoef[weights$okreg==40],
                  okreg==41 ~ (weights$validvotes[weights$okreg==41])*PSL*weights$TDcoef[weights$okreg==41]
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
  Razem = case_when(okreg==1 ~ (weights$validvotes[weights$okreg==1])*Razem*weights$Lewicacoef[weights$okreg==1],
                    okreg==2 ~ (weights$validvotes[weights$okreg==2])*Razem*weights$Lewicacoef[weights$okreg==2],
                    okreg==3 ~ (weights$validvotes[weights$okreg==3])*Razem*weights$Lewicacoef[weights$okreg==3],
                    okreg==4 ~ (weights$validvotes[weights$okreg==4])*Razem*weights$Lewicacoef[weights$okreg==4],
                    okreg==5 ~ (weights$validvotes[weights$okreg==5])*Razem*weights$Lewicacoef[weights$okreg==5],
                    okreg==6 ~ (weights$validvotes[weights$okreg==6])*Razem*weights$Lewicacoef[weights$okreg==6],
                    okreg==7 ~ (weights$validvotes[weights$okreg==7])*Razem*weights$Lewicacoef[weights$okreg==7],
                    okreg==8 ~ (weights$validvotes[weights$okreg==8])*Razem*weights$Lewicacoef[weights$okreg==8],
                    okreg==9 ~ (weights$validvotes[weights$okreg==9])*Razem*weights$Lewicacoef[weights$okreg==9],
                    okreg==10 ~ (weights$validvotes[weights$okreg==10])*Razem*weights$Lewicacoef[weights$okreg==10],
                    okreg==11 ~ (weights$validvotes[weights$okreg==11])*Razem*weights$Lewicacoef[weights$okreg==11],
                    okreg==12 ~ (weights$validvotes[weights$okreg==12])*Razem*weights$Lewicacoef[weights$okreg==12],
                    okreg==13 ~ (weights$validvotes[weights$okreg==13])*Razem*weights$Lewicacoef[weights$okreg==13],
                    okreg==14 ~ (weights$validvotes[weights$okreg==14])*Razem*weights$Lewicacoef[weights$okreg==14],
                    okreg==15 ~ (weights$validvotes[weights$okreg==15])*Razem*weights$Lewicacoef[weights$okreg==15],
                    okreg==16 ~ (weights$validvotes[weights$okreg==16])*Razem*weights$Lewicacoef[weights$okreg==16],
                    okreg==17 ~ (weights$validvotes[weights$okreg==17])*Razem*weights$Lewicacoef[weights$okreg==17],
                    okreg==18 ~ (weights$validvotes[weights$okreg==18])*Razem*weights$Lewicacoef[weights$okreg==18],
                    okreg==19 ~ (weights$validvotes[weights$okreg==19])*Razem*weights$Lewicacoef[weights$okreg==19],
                    okreg==20 ~ (weights$validvotes[weights$okreg==20])*Razem*weights$Lewicacoef[weights$okreg==20],
                    okreg==21 ~ (weights$validvotes[weights$okreg==21])*Razem*weights$Lewicacoef[weights$okreg==21],
                    okreg==22 ~ (weights$validvotes[weights$okreg==22])*Razem*weights$Lewicacoef[weights$okreg==22],
                    okreg==23 ~ (weights$validvotes[weights$okreg==23])*Razem*weights$Lewicacoef[weights$okreg==23],
                    okreg==24 ~ (weights$validvotes[weights$okreg==24])*Razem*weights$Lewicacoef[weights$okreg==24],
                    okreg==25 ~ (weights$validvotes[weights$okreg==25])*Razem*weights$Lewicacoef[weights$okreg==25],
                    okreg==26 ~ (weights$validvotes[weights$okreg==26])*Razem*weights$Lewicacoef[weights$okreg==26],
                    okreg==27 ~ (weights$validvotes[weights$okreg==27])*Razem*weights$Lewicacoef[weights$okreg==27],
                    okreg==28 ~ (weights$validvotes[weights$okreg==28])*Razem*weights$Lewicacoef[weights$okreg==28],
                    okreg==29 ~ (weights$validvotes[weights$okreg==29])*Razem*weights$Lewicacoef[weights$okreg==29],
                    okreg==30 ~ (weights$validvotes[weights$okreg==30])*Razem*weights$Lewicacoef[weights$okreg==30],
                    okreg==31 ~ (weights$validvotes[weights$okreg==31])*Razem*weights$Lewicacoef[weights$okreg==31],
                    okreg==32 ~ (weights$validvotes[weights$okreg==32])*Razem*weights$Lewicacoef[weights$okreg==32],
                    okreg==33 ~ (weights$validvotes[weights$okreg==33])*Razem*weights$Lewicacoef[weights$okreg==33],
                    okreg==34 ~ (weights$validvotes[weights$okreg==34])*Razem*weights$Lewicacoef[weights$okreg==34],
                    okreg==35 ~ (weights$validvotes[weights$okreg==35])*Razem*weights$Lewicacoef[weights$okreg==35],
                    okreg==36 ~ (weights$validvotes[weights$okreg==36])*Razem*weights$Lewicacoef[weights$okreg==36],
                    okreg==37 ~ (weights$validvotes[weights$okreg==37])*Razem*weights$Lewicacoef[weights$okreg==37],
                    okreg==38 ~ (weights$validvotes[weights$okreg==38])*Razem*weights$Lewicacoef[weights$okreg==38],
                    okreg==39 ~ (weights$validvotes[weights$okreg==39])*Razem*weights$Lewicacoef[weights$okreg==39],
                    okreg==40 ~ (weights$validvotes[weights$okreg==40])*Razem*weights$Lewicacoef[weights$okreg==40],
                    okreg==41 ~ (weights$validvotes[weights$okreg==41])*Razem*weights$Lewicacoef[weights$okreg==41]
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

poldHondt <- data.frame(KO=rep(1,41000), Konfederacja=rep(1,41000), Lewica=rep(1,41000), Razem=rep(1,41000), 
                        MN=rep(1,41000), PiS=rep(1,41000), `Polska 2050`=rep(1,41000), PSL=rep(1,41000))

for(i in 1:41000) { 
  poldHondt[i,] <- giveseats(v = c(consts$KO[i], consts$Konfederacja[i], consts$Lewica[i], consts$Razem[i], consts$MN[i],
                                   consts$PiS[i], consts$`Polska 2050`[i], consts$PSL[i]), ns = consts$magnitude[i], method="dh", thresh=0)$seats
}

poldHondt <- cbind(poldHondt, consts$okreg, consts$.draw)

colnames(poldHondt) <- c("KO", "Konfederacja", "Lewica", "Razem", "MN", "PiS", "Polska 2050", "PSL", "okreg", "draw")

poldHondt <- poldHondt %>% 
  group_by(draw) %>% 
  summarise(KO = sum(KO),
            PiS = sum(PiS),
            Konfederacja = sum(Konfederacja),
            `Polska 2050` = sum(`Polska 2050`),
            PSL = sum(PSL),
            MN = sum(MN),
            Lewica = sum(Lewica),
            Razem = sum(Razem))

poldHondt <- poldHondt %>%
  pivot_longer(., cols=c("KO", "Konfederacja", "Lewica", "Razem", "MN", "PiS", "Polska 2050", "PSL"), names_to="party", values_to="seats")

PiS_seats <- poldHondt %>%
  filter(., party=="PiS") %>%
  hypothesis(., "seats>230")

frame <- poldHondt %>%
  group_by(party) %>%
  summarise(median_qi(seats, .width=0.8)) %>%
  mutate(., y = round(y, 0),
         ymin = round(ymin, 0),
         ymax = round(ymax, 0))

frame$in2023[frame$party=="KO"] <- 157
frame$in2023[frame$party=="PiS"] <- 194
frame$in2023[frame$party=="Lewica"] <- 19  
frame$in2023[frame$party=="Razem"] <- 7  
frame$in2023[frame$party=="MN"] <- 0
frame$in2023[frame$party=="Konfederacja"] <- 18
frame$in2023[frame$party=="Polska 2050"] <- 33  
frame$in2023[frame$party=="PSL"] <- 32          
frame$party <- factor(frame$party, levels=c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Polska 2050", "PSL", "MN"))
frame$diffPres <- sprintf("%+d", (frame$y - frame$in2023))
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
                            levels = c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Other", "Polska2050", "PSL"),
                            labels = c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Other", "Polska2050", "PSL")))

plotdraws <- plotdraws %>%
  pivot_wider(names_from = .category, values_from = .value) %>%
  mutate(magnitude=460) %>%
  select(., c(.draw, PiS, KO, Lewica, Razem, Konfederacja, Polska2050, PSL, magnitude)) 

plotdraws$MN <- rnorm(1000, mean=0.079, sd=0.00001)

plotdraws <- plotdraws %>%
  mutate(., PiS = ifelse(PiS<0.05, 0, PiS),
         KO = ifelse(KO<0.05, 0, KO),
         Konfederacja = ifelse(Konfederacja<0.05, 0, Konfederacja),
         Lewica = ifelse(Lewica<0.05, 0, Lewica),
         Razem = ifelse(Razem<0.05, 0, Razem),
         Polska2050 = ifelse(Polska2050<0.05, 0, Polska2050),
         PSL = ifelse(PSL<0.05, 0, PSL),
         KO = ifelse(median_KO<5, 0, KO),
         Konfederacja = ifelse(median_Konfederacja<5, 0, Konfederacja),
         Lewica = ifelse(median_Lewica<5, 0, Lewica),
         Razem = ifelse(median_Razem<5, 0, Razem),
         Polska2050 = ifelse(median_Polska2050<5, 0, Polska2050),
         PSL = ifelse(median_PSL<5, 0, PSL)
  )

coalition_pis_konf <- frame$y[frame$party=="PiS"] + frame$y[frame$party=="Konfederacja"]
coalition_opposition <- frame$y[frame$party=="KO"] + frame$y[frame$party=="Lewica"] + 
  frame$y[frame$party=="Polska 2050"] + frame$y[frame$party=="PSL"]

pis_konf_status <- ifelse(coalition_pis_konf >= 231, "MAJORITY", "NO MAJORITY")
opposition_status <- ifelse(coalition_opposition >= 231, "MAJORITY", "NO MAJORITY")

pis_konf_text <- paste0("PiS + Konfederacja: ", coalition_pis_konf, " seats\n", pis_konf_status)
opposition_text <- paste0("KO + Lewica + Polska 2050 + PSL: ", coalition_opposition, " seats\n", opposition_status)


seats_parl <- ggplot(data=frame, mapping=aes(x=party, y=y, fill=party)) +
  geom_bar(stat="identity", width=.75, show.legend = F) +
  geom_abline(intercept=231, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=276, slope=0, colour="gray10", linetype=3) +
  geom_abline(intercept=307, slope=0, colour="gray10", linetype=3) +
  scale_y_continuous('Number of seats', limits=c(0,320), breaks=c(0, 50, 100, 150, 200, 231, 276, 307)) +
  scale_fill_manual(name="Party", values = cols)+
  annotate("label", x=2, y=231, label="Legislative majority", size=2.5, hjust=0, label.size=NA, fill="grey95", family="Jost") +
  annotate("label", x=2, y=276, label="Overturn presidential veto", size=2.5, hjust=0, label.size=NA, fill="grey95", family="Jost") +
  annotate("label", x=2, y=307, label="Constitutional majority", size=2.5, hjust=0, label.size=NA, fill="grey95", family="Jost") +
  # Coalition annotations
  annotate("label", x=5, y=280, label=pis_konf_text, size=2.5, hjust=0, 
           label.size=0.5, fill=ifelse(coalition_pis_konf >= 231, "lightgreen", "lightcoral"), 
           family="Jost") +
  annotate("label", x=5, y=250, label=opposition_text, size=2.5, hjust=0, 
           label.size=0.5, fill=ifelse(coalition_opposition >= 231, "lightgreen", "lightcoral"), 
           family="Jost") +
  annotate("text", x=frame$party, y=c(frame$y+18), label=frame$y, size=3, family="Jost", hjust=0.5)+
  annotate("text", x=as.numeric(frame$party)+0.1, y=c(frame$y+18), label=frame$diffPres, size=2.5, family="Jost", fontface="italic", hjust=0) +
  annotate("text", x=frame$party, y=c(frame$y+8), label=paste("(",round(frame$ymin,0), "\u2013",round(frame$ymax,0),")", sep=""), size=2, family="Jost") +
  labs(x="", y="Number of seats", title="Estimated share of seats",
       subtitle="Median estimated seat share with 80% credible intervals. Sum total may not equal 460.",
       caption = "Figures in brackets show change from 2023 share of seats.") +
  theme_plots()
ggsave(seats_parl, file = "seats_parl.png",
       width = 7, height = 5, units = "cm", dpi=600, scale = 3, bg="white")


#####Save to Github#####
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
system("rsync -av --include='*.png' --exclude='*' '/Users/benstanley/R scripts/Pooling the Poles/' '/Users/benstanley/R scripts/Website/docs/images/'")
