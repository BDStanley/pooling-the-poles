rm(list=ls())
library("tidyverse")
library("lubridate")
library("stringr")
library("googledrive")
library("rio")
library("readxl")
library("hrbrthemes")
library("sjlabelled")
library("rgdal")
library("maptools") 
library("rgeos") 
library("gpclib")
library("xtable")
library("stringi")
library("ggrepel")


results <- read.delim2("wyniki_gl_na_kand_po_powiatach_proc_utf8.csv", sep=";")
results <- results %>%
  filter(., Powiat!="statki") %>%
  filter(., Powiat!="zagranica") %>%
  mutate(Kod.TERYT = replace(Kod.TERYT, Kod.TERYT==146501, 146500))

const <- readOGR("Powiaty.shp")
const@data$JPT_NAZWA_ <- str_remove(const@data$JPT_NAZWA_, "powiat ")
const@data <- rename(const@data, Kod.TERYT = JPT_KOD_JE)
const@data$Kod.TERYT <- stri_pad_right(const@data$Kod.TERYT, 6, "0")
results$Kod.TERYT <- stri_pad_left(results$Kod.TERYT, 6, "0")
results$diff <- results$Andrzej.Sebastian.DUDA-results$Rafał.Kazimierz.TRZASKOWSKI
label_points <- coordinates(const)
colnames(label_points) <- c("x_point","y_point")
label_points <- data.frame(label_points)

const@data$id = rownames(const@data)
label_points$id = rownames(label_points)
const@data = full_join(const@data, label_points)
const@data <- full_join(const@data, results, by = 'Kod.TERYT')

const.points = fortify(const, region="id")
const.df = full_join(const.points, const@data, by="id")

const_woj <- readOGR("Wojewodztwa.shp")
const_woj_points <- fortify(const_woj)


p_duda <- ggplot()+ 
  geom_polygon(data=const.df, aes(long,lat,group=group,fill=as.integer(Andrzej.Sebastian.DUDA))) +
  theme(aspect.ratio=1) +
  geom_path(data=const_woj_points, aes(long,lat, group=group), color="black") +
  scale_fill_gradient(name="", low = "white", high = "blue", guide="colorbar") +
  geom_label_repel(data=const@data %>%
                     slice_max(., Andrzej.Sebastian.DUDA, n=10), 
                   mapping = aes(x=x_point, y=y_point,
                                 label=paste(Powiat, Andrzej.Sebastian.DUDA, sep=" - ")), 
                   fill="white", family="Roboto Condensed Light", size=3, nudge_x = -0.2, nudge_y = 0.2) +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0, plot_title_family = "Roboto Condensed Light") + theme(aspect.ratio=1,
                                                                                  axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title="Powiat-level share of vote for Andrzej Duda" , 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")
ggsave(p_duda, file = "Duda_votes.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_trza <- ggplot() + 
  geom_polygon(data=const.df, aes(long,lat,group=group,fill=as.integer(Rafał.Kazimierz.TRZASKOWSKI))) +
  theme(aspect.ratio=1) +
  geom_path(data=const_woj_points, aes(long,lat, group=group), color="black") +
  scale_fill_gradient(name="", low = "white", high = "orange", guide="colorbar") +
  geom_label_repel(data=const@data %>%
                     slice_max(., Rafał.Kazimierz.TRZASKOWSKI, n=10), 
                   mapping = aes(x=x_point, y=y_point,
                                 label=paste(Powiat, Rafał.Kazimierz.TRZASKOWSKI, sep=" - ")), 
                   fill="white", family="Roboto Condensed Light", size=3, nudge_y=0.3, nudge_x=0.4) +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0, plot_title_family = "Roboto Condensed Light") + 
  theme(aspect.ratio=1, axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title="Powiat-level share of vote for Rafał Trzaskowski" , 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")
ggsave(p_trza, file = "Trzaskowski_votes.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)

p_diff <- ggplot() + 
  geom_polygon(data=const.df, aes(long,lat,group=group,fill=as.integer(diff))) +
  theme(aspect.ratio=1) +
  geom_path(data=const_woj_points, aes(long,lat, group=group), color="black") +
  scale_fill_gradient2(name="Duda advantage\nover Trzaskowski", low = "orange", high = "blue", mid = "white", guide="colorbar") +
  theme_ipsum_rc(grid=FALSE, axis=FALSE, ticks=FALSE, axis_text_size = 0, plot_title_family = "Roboto Condensed Light") + theme(aspect.ratio=1,
                                                                                                                                axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title="Powiat-level difference in vote share" , 
       caption = "@BDStanley; benstanley.org", family="Roboto Condensed")
ggsave(p_diff, file = "diff_votes.png", 
       width = 7, height = 7, units = "cm", dpi = 320, scale = 4)
