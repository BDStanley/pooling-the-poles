const <- readOGR("/Users/benstanley/Desktop/Personal/Dropbox/Resources/Polish materials/Regional data/gminy/gminy.shp")
const@data$id <- gsub('.{1}$', '', as.character(const@data$jpt_kod_je))
const.points = fortify(const, region="id")
const.df = join(const.points, const@data, by="id")

gminy <- read.csv2("/Users/benstanley/Desktop/Personal/Dropbox/Resources/Polish materials/Regional data/gminy.csv", sep=",")
gminy <- add_row(gminy, Jednostka.terytorialna="Warszawa", TERYT=146501, turnout=62.53, KE=50.04, PiS=27.84, Wiosna=10.70, Konfederacja=4.44, Kukiz=3.24, Razem=1.98)
gminy$id <- str_pad(as.character(gminy$TERYT), width=6, side="left", pad="0")
gminy$KEcoef <- ((100/38.47)*gminy$KE)/100
  
plotdata <- merge(const.df,gminy,by="id")

const <- readOGR("/Users/benstanley/Desktop/Personal/Dropbox/Resources/Polish materials/Regional data/powiaty/powiaty.shp")
const@data$id <- const@data$jpt_kod_je
const.points = fortify(const, region="id")
const.df = join(const.points, const@data, by="id")

powiaty <- read.csv2("/Users/benstanley/Desktop/Personal/Dropbox/Resources/Polish materials/Regional data/powiaty.csv", sep=",")
powiaty$id <- str_pad(as.character(powiaty$TERYT), width=6, side="left", pad="0")
powiaty$id <- gsub('.{2}$', '', powiaty$id)

plotdata <- merge(const.df,powiaty,by="id")

