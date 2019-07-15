library(tidyverse); library(hrbrthemes); library(reshape2); library(ggrepel)

year <- c(2001, 2005, 2007, 2011, 2015)
PO <- c(12.65, 24.14, 41.51, 39.18, 24.09)
PiS <- c(9.50, 26.99, 32.11, 29.89, 37.58)
frame <- tibble(year, PO, PiS)
data <- melt(frame, id=c("year"))

p <- ggplot(aes(year, value, colour=variable, label=value, group=variable), data=data) +
  geom_line(aes(linetype=variable))+
  theme_minimal() +
  scale_x_continuous(breaks=c(2001, 2005, 2007, 2011, 2015), 
                     labels=c("2001", "2005", "2007", "2011", "2015"))+
  scale_colour_manual(name="Party", values=c("black", "black"))+
  scale_linetype_manual(name="Party", values=c("solid", "dashed"))+
  geom_label_repel(show.legend = F, family="Roboto Condensed", 
                   box.padding=0.5, segment.color="gray60", segment.size=0.3)+
  labs(x="Year of election", y="Share of vote (%)") +
  theme_ipsum_rc()

ggsave(p, file = '~/Google Drive/Resources/Polish materials/Party votes in elections.png', 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 3)