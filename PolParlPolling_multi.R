rm(list=ls())
library("rstan")
library("tidyverse")
library("lubridate")
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
library("tidybayes")

options(mc.cores = parallel::detectCores())
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

import <- drive_download(as_id("https://drive.google.com/file/d/1rt9ZSJfKHSvuOZAuBxaRlv3xBHDY4Mbo/view?usp=sharing"), overwrite=TRUE)
1
polls <- read_excel('pooledpolls_parl.xlsx')

polls <- unite(polls, org, remark, col="org", sep="_")
polls$org <-as.factor(polls$org)

polls$startDate <- as.Date(polls$startDate)
polls$endDate <- as.Date(polls$endDate)

polls <-
  polls %>%
  mutate(midDate = as.Date(startDate + difftime(endDate, startDate)),
         midDate_int=as.integer(midDate)) %>%
  filter(midDate_int > (max(midDate_int)-150)) %>%
  mutate(PiS = 100/((100-DK))*PiS,
         KO = 100/((100-DK))*KO,
         Lewica = 100/((100-DK))*Lewica,
         `PSL-Kukiz` = 100/((100-DK))*`PSL-Kukiz`,
         Konfederacja = 100/((100-DK))*Konfederacja,
         Other = 100/((100-DK))*Other,
         time = as.integer(difftime(midDate, min(midDate)-1, units = "days")) + 1L,
         pollster = as.integer(factor(org)),
         pollno = length(PiS)) %>%
         pivot_longer(c(PiS, KO, Lewica, `PSL-Kukiz`, Konfederacja, Other), names_to="party", values_to="percent")

START_DATE <- min(polls$midDate)-1
END_DATE <- max(polls$midDate)

data <- within(list(), {
  n_days <- as.integer(difftime(Sys.Date(), START_DATE, units = "days")) + 1
  n_polls <- max(polls$pollno)
  n_houses <- max(polls$pollster)
  n_parties <- 6L
  pseudoSampleSize <- 600
  
  startingPoint = c(0.40, 0.30, 0.10, 0.10, 0.09, 0.01)
  startingPointCertainty = 10
  transmissionStrength = 80000
  
  y <- as.integer(polls$percent)
  poll_day <- polls$time
  house <- as.integer(polls$pollster)
})

write("data {
    // data size
    int<lower=1> n_polls;
    int<lower=1> n_days;
    int<lower=1> n_houses;
    int<lower=1> n_parties;
    
    // key variables
    int<lower=1> pseudoSampleSize; // maximum sample size for y
    real<lower=1> transmissionStrength;
    
    // give a rough idea of a staring point ...
    simplex[n_parties] startingPoint; // rough guess at series starting point
    int<lower=1> startingPointCertainty; // strength of guess - small number is vague
    
    // poll data
    int<lower=0,upper=pseudoSampleSize> y[n_polls, n_parties]; // a multinomial
    int<lower=1,upper=n_houses> house[n_polls]; // polling house
    int<lower=1,upper=n_days> poll_day[n_polls]; // day polling occured
}

parameters {
    simplex[n_parties] hidden_voting_intention[n_days];
    matrix[n_houses, n_parties] houseAdjustment;
}

transformed parameters {
    matrix[n_houses, n_parties] aHouseAdjustment;
    matrix[n_houses, n_parties] tHouseAdjustment;
     for(p in 1:n_parties) // included parties sum to zero 
        aHouseAdjustment[,p] = houseAdjustment[,p] - mean(houseAdjustment[,p]);
     for(h in 1:n_houses) // included parties sum to zero 
        tHouseAdjustment[h,] = aHouseAdjustment[h,] - mean(aHouseAdjustment[h,]);
}

model{
    // -- house effects model
    for(h in 1:n_houses)
        houseAdjustment[h] ~ normal(0, 0.05); 
    
    // -- temporal model
    hidden_voting_intention[1] ~ dirichlet(startingPoint * startingPointCertainty);
    for (day in 2:n_days)
        hidden_voting_intention[day] ~ 
            dirichlet(hidden_voting_intention[day-1] * transmissionStrength);
    
    // -- observed data model
    for(poll in 1:n_polls)
        y[poll] ~ multinomial(hidden_voting_intention[poll_day[poll]] + 
            tHouseAdjustment'[,house[poll]]);
}",
"polls_mult.stan")

model <- "polls_mult.stan"



fit <- stan(model, data=data, chains = 4, control = list(adapt_delta=0.99), iter=4000)
