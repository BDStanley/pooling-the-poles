const <- readOGR("/Users/benstanley/Google Drive/Resources/Polish materials/Regional data/gminy/gminy.shp")
const@data$id <- gsub('.{1}$', '', as.character(const@data$jpt_kod_je))
const.points = fortify(const, region="id")
const.df = join(const.points, const@data, by="id")

gminy <- read.csv2("/Users/benstanley/Google Drive/Resources/Polish materials/Regional data/gminy.csv", sep=",")
gminy <- add_row(gminy, Jednostka.terytorialna="Warszawa", TERYT=146501, turnout=62.53, KE=50.04, PiS=27.84, Wiosna=10.70, Konfederacja=4.44, Kukiz=3.24, Razem=1.98)
gminy$id <- str_pad(as.character(gminy$TERYT), width=6, side="left", pad="0")
gminy$KEcoef <- ((100/38.47)*gminy$KE)/100
  
plotdata <- merge(const.df,gminy,by="id")

const <- readOGR("/Users/benstanley/Google Drive/Resources/Polish materials/Regional data/powiaty/powiaty.shp")
const@data$id <- const@data$jpt_kod_je
const.points = fortify(const, region="id")
const.df = join(const.points, const@data, by="id")

powiaty <- read.csv2("/Users/benstanley/Google Drive/Resources/Polish materials/Regional data/powiaty.csv", sep=",")
powiaty$id <- str_pad(as.character(powiaty$TERYT), width=6, side="left", pad="0")
powiaty$id <- gsub('.{2}$', '', powiaty$id)

powiaty2015 <- read.csv("/Users/benstanley/Google Drive/Resources/Polish materials/Regional data/powiaty2015.csv")
powiaty2015$id <- str_pad(as.character(powiaty2015$TERYT), width=4, side="left", pad="0")

regdata <- merge(powiaty, powiaty2015, by="id")
regdata$POcoef <- ((100/38.47)*powiaty$KE)/100
regdata$POcoef <- ((100/38.47)*gminy$KE)/100

plotdata <- merge(const.df,regdata,by="id")






lmPiS <- lm(PiS ~ PiS2015 + I(PiS2015^2), data=regdata)
pred <- ggemmeans(lmPiS, terms="PiS2015[15,20,30,40,50,60,70]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=PiS2015, y=PiS), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for PiS at 2015 national elections", y = "% vote for PiS at 2019 EP elections", 
       title = "Support for PiS at the 2019 EP election and 2015 general election, powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "PiS.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmKE <- lm(KE ~ PO2015, data=regdata)
pred <- ggemmeans(lmKE, terms="PO2015[5,10,20,30,40,45]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=PO2015, y=KE), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for PO at 2015 general election", y = "% vote for KE at 2019 EP election", 
       title = "Support for KE (2019 EP election) and PO (2015 general election), powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "KEPO.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmKE <- lm(KE ~ Nowoczesna2015, data=regdata)
pred <- ggemmeans(lmKE, terms="Nowoczesna2015[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=Nowoczesna2015, y=KE), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for Nowoczesna in 2015 general election", y = "% vote for KE in 2019 EP election", 
       title = "Support for KE (2019 EP election) and Nowoczesna (2015 general election), powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "KEN.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmKE <- lm(KE ~ PSL2015, data=regdata)
pred <- ggemmeans(lmKE, terms="PSL2015[0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=PSL2015, y=KE), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for PSL in 2015 general election", y = "% vote for KE in 2019 EP election", 
       title = "Support for KE (2019 EP election) and PSL (2015 general election), powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "KEPSL.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmPiS <- lm(PiS ~ PSL2015, data=regdata)
pred <- ggemmeans(lmPiS, terms="PSL2015[0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=PSL2015, y=PiS), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for PSL in 2015 general election", y = "% vote for PiS in 2019 EP election", 
       title = "Support for PiS (2019 EP election) and PSL (2015 general election), powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "PiSPSL.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmKE <- lm(KE ~ ZL2015, data=regdata)
pred <- ggemmeans(lmKE, terms="ZL2015[0,2,4,6,8,10,12,14,16,18,20]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=ZL2015, y=KE), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for ZL in 2015 general election", y = "% vote for KE in 2019 EP election", 
       title = "Support for KE (2019 EP election) and ZL (2015 general election), powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "KEZL.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmW <- lm(Wiosna ~ ZL2015, data=regdata)
pred <- ggemmeans(lmW, terms="ZL2015[0,2,4,6,8,10,12,14,16,18,20]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=ZL2015, y=Wiosna), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for ZL in 2015 general election", y = "% vote for Wiosna in 2019 EP elections", 
       title = "Support for Wiosna (2019 EP election) and ZL (2015 general election), powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "KEW.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmW <- lm(Wiosna ~ Nowoczesna2015, data=regdata)
pred <- ggemmeans(lmW, terms="Nowoczesna2015[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=Nowoczesna2015, y=Wiosna), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for Nowoczesna in 2015 general election", y = "% vote for Wiosna in 2019 EP elections", 
       title = "Support for Wiosna (2019 EP elections) and N (2015 general election), powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "WiosnaN.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmRaz <- lm(Razem ~ Razem2015, data=regdata)
pred <- ggemmeans(lmRaz, terms="Razem2015[0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=Razem2015, y=Razem), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for Razem in 2015 general election", y = "% vote for Razem in 2019 EP election", 
       title = "Support for Razem at 2019 EP election and 2015 general election, powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "Razem.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmKonf <- lm(Konfederacja ~ KORWiN2015, data=regdata)
pred <- ggemmeans(lmKonf, terms="KORWiN2015[0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=KORWiN2015, y=Konfederacja), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for KORWiN at 2015 general election", y = "% vote for Konfederacja at 2019 EP election", 
       title = "Support for Konf. at 2019 EP election and KORWiN at 2015 general election, powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "KonfKOR.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmKukiz <- lm(Kukiz ~ Kukiz2015, data=regdata)
pred <- ggemmeans(lmKukiz, terms="Kukiz2015[5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=Kukiz2015, y=Kukiz), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "% vote for Kukiz'15 at 2015 general election", y = "% vote for Kukiz'15 at 2019 EP election", 
       title = "Support for Kukiz'15 at 2019 EP election and 2015 general election, powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "Kukiz.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmTurn <- lm(turnout ~ turnout2015 + I(turnout2015^2), data=regdata)
pred <- ggemmeans(lmTurn, terms="turnout2015[30,40,50,60,70]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=turnout2015, y=turnout), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "Turnout at 2015 general election", y = "Turnout at 2019 EP election", 
       title = "Turnout at 2019 EP election and 2015 general election, powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "Turnout.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmTurnPiS <- lm(PiS ~ turnout, data=regdata)
pred <- ggemmeans(lmTurnPiS, terms="turnout[30,40,50,60,70]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=turnout, y=PiS), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "Turnout", y = "% of vote for PiS", 
       title = "Turnout and support for PiS at 2019 EP election, powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "TurnoutPiS.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)

lmTurnKE <- lm(KE ~ turnout, data=regdata)
pred <- ggemmeans(lmTurnKE, terms="turnout[30,40,50,60,70]")
plot <- data.frame(pred)
p <- ggplot(plot) +
  geom_point(aes(x=turnout, y=KE), data=regdata) +
  geom_ribbon(aes(x=x, ymin = conf.low, ymax = conf.high), alpha=0.3, linetype=0, show.legend = F) +
  geom_line(aes(x=x, y=predicted)) +
  labs(x = "Turnout", y = "% of vote for KE", 
       title = "Turnout and support for KE at 2019 EP election, powiat level", caption = "@BDStanley") +
  theme_minimal() +
  theme_ipsum_rc()
ggsave(p, file = "TurnoutKE.png", width = 7, height = 5, units = "cm", dpi = 320, scale = 4)
