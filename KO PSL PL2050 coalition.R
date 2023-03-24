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

cols <- c("PiS"="blue", "KO/Polska 2050/PSL"="orange", "Lewica"="red", "Konfederacja" = "midnightblue", "MN" = "yellow")

#####Read in, adjust and subset data#####
library(googledrive)
library(rio)
library(readxl)

import <- drive_download(as_id("https://docs.google.com/spreadsheets/d/1cOC1mY4xf0iXgPavA1CNQeSFwqi_0U_m/edit?usp=sharing&ouid=111487015973215379663&rtpof=true&sd=true"), overwrite=TRUE)
1
weights <- read_excel('2019_elec_percentages.xlsx')

import <- drive_download(as_id("https://drive.google.com/file/d/1JmF3bjRA_sTaJZ4rqPd1WAQyGm-XM-l7/view?usp=sharing"), overwrite=TRUE)
1
const <- readRDS('constituencies')


#####Enter party support and weight by constituency#####
PiS_raw <- 33.0
KO_raw <- 27.6
Konf_raw <- 12.8
Lewica_raw <- 8.4
Other_raw <- 10.3
Undecided_raw <- 7.8

PiS <- rnorm(1000, (PiS_raw/(100-Other_raw-Undecided_raw))*100, sd=1)
KO <- rnorm(1000, (KO_raw/(100-Other_raw-Undecided_raw))*100, sd=1)
Konfederacja <- rnorm(1000, (Konf_raw/(100-Other_raw-Undecided_raw))*100, sd=1)
Lewica <- rnorm(1000, (Lewica_raw/(100-Other_raw-Undecided_raw))*100, sd=1)
MN <- rnorm(1000, mean=7.9, sd=0.00001)

plotdraws <- tibble(PiS, KO, Konfederacja, Lewica, MN)
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

consts$.draw <- rep(1:1000, each=41)


#####Calculate seat shares#####
library(seatdist)
library(ggdist)

poldHondt <- data.frame(KO=rep(1,41000), Konfederacja=rep(1,41000), Lewica=rep(1,41000), MN=rep(1,41000), PiS=rep(1,41000))

for(i in 1:41000) { 
  poldHondt[i,] <- giveseats(v = c(consts$KO[i], consts$Konfederacja[i], consts$Lewica[i],consts$MN[i], consts$PiS[i]), 
                             ns = consts$magnitude[i], method="dh", thresh=0)$seats
}

poldHondt <- cbind(poldHondt, consts$okreg, consts$.draw)

colnames(poldHondt) <- c("KO", "Konfederacja", "Lewica", "MN", "PiS", "okreg", "draw")

poldHondt <- poldHondt %>% 
  group_by(draw) %>% 
  summarise(KO = sum(KO),
            PiS = sum(PiS),
            Konfederacja = sum(Konfederacja),
            Lewica=sum(Lewica),
            MN = sum(MN))

poldHondt <- poldHondt %>%
  pivot_longer(., cols=c("KO", "Konfederacja", "Lewica", "MN", "PiS"), names_to="party", values_to="seats")

frame <- poldHondt %>%
  group_by(party) %>%
  summarise(mean_qi(seats)) %>%
  mutate(., y = round(y, 0),
         ymin = round(ymin, 0),
         ymax = round(ymax, 0))

frame$party <- factor(frame$party, levels=c("PiS", "KO", "Konfederacja", "Lewica", "MN"))
frame$party <- reorder(frame$party, -frame$y)
levels(frame$party)[levels(frame$party)=="KO"] <- "KO/Polska 2050/PSL"


#####Plots#####
plot_seats_ko_p2050_psl <- ggplot(data=frame, mapping=aes(x=party, y=y, fill=party)) +
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
  labs(x="", y="", title="Rozkład mandatów w Sejmie - wspólny start KO, Polski 2050 i PSL",
       subtitle="(95%-owy przedział wiarygodności)",
       caption = "Ben Stanley (@BDStanley; benstanley.pl).") +
  theme_plots()
ggsave(plot_seats_ko_p2050_psl, file = "plot_seats_ko_p2050_psl.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")


library(tidybayes)
library(ggblend)
names(plotdraws)[names(plotdraws)=="KO"] <- "KO/Polska 2050/PSL"

KO.PiS.diff <- plotdraws %>%
  mutate(., KOPiS = `KO/Polska 2050/PSL`-PiS,
         KOPiS = sum((KOPiS > 0) / length(KOPiS)),
         KOPiS = round(KOPiS, 2)) %>%
  pull(KOPiS) %>%
  last(.)

plot_latest_ko_p2050_psl <- plotdraws %>%
  pivot_longer(., cols=c("PiS", "KO/Polska 2050/PSL", "Lewica", "Konfederacja")) %>%
  select(., !MN) %>%
  ggplot(aes(y=reorder(name, dplyr::desc(-value)), 
             x=value, color=name)) +
  geom_vline(aes(xintercept=5), colour="gray40", linetype="dotted") +
  stat_interval(aes(x=value, color_ramp = stat(.width)), .width = ppoints(100)) %>%
  partition(vars(name)) +
  scale_color_manual(values=cols, guide="none") +
  scale_fill_manual(values=cols, guide="none") +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide=FALSE) +
  scale_y_discrete(name="", position="right") +
  annotate(geom = "text", label=paste(round(median(plotdraws$PiS),0)),
           y="PiS", x=median(plotdraws$PiS), size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(mean(plotdraws$`KO/Polska 2050/PSL`),0)),
           y="KO/Polska 2050/PSL", x=mean(plotdraws$`KO/Polska 2050/PSL`), size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(mean(plotdraws$`Konfederacja`),0)),
           y="Konfederacja", x=mean(plotdraws$`Konfederacja`), size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  annotate(geom = "text", label=paste(round(mean(plotdraws$`Lewica`),0)),
           y="Lewica", x=mean(plotdraws$`Lewica`), size=4, hjust = "center", vjust=-1,
           family="IBM Plex Sans Condensed Light", color="black") +
  scale_fill_manual(name=" ", values=cols, guide="none") +
  expand_limits(x = 0) +
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="", title="Poparcie dla partii politycznych oraz koalicji KO-Polska 2050-PSL", subtitle="(95%-owy przedział wiarygodności)", color="") +
  theme_plots()
ggsave(plot_latest_ko_p2050_psl, file = "plot_latest_ko_p2050_psl.png",
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")


#####Upload to Github#####
system("git add -A")
system("git commit -m 'KO PSL PL2050 coalition new'")
system("git pull")
system("git push")