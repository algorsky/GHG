library(tidyverse)
library(patchwork)
library(scales)

loadLTERtemp <- function() {
  # Package ID: knb-lter-ntl.29.27 Cataloging System:https://pasta.edirepository.org.
  # Data set title: North Temperate Lakes LTER:
  # Physical Limnology of Primary Study Lakes 1981 - current
  inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/27/03e232a1b362900e0f059859abe8eb97"
  infile3 <- tempfile()
  download.file(inUrl3,infile3,method="curl")
  
  LTERtemp <-read_csv(infile3, skip=1, quote ='"',guess_max = 100000, col_names=c(
    "lakeid" ,"year4" ,"daynum" ,"sampledate" ,"depth" ,"rep" ,"sta" ,"event" ,"wtemp" ,"o2" ,"o2sat" ,"deck" ,
    "light" ,"frlight" ,"flagdepth" ,"flagwtemp" ,"flago2" ,"flago2sat" ,"flagdeck" ,"flaglight" ,"flagfrlight"))
}

library(scales)
LTERphysical = loadLTERtemp()
temp<- LTERphysical %>%
  filter(lakeid == "TB")%>%
  select(year4, daynum, sampledate, depth, rep, wtemp, flagwtemp)%>%
  filter(!is.na(wtemp))%>%
  filter(is.na(flagwtemp))%>%
  filter(wtemp >= 0)%>%
  mutate(icecovered = ifelse(month(sampledate)%in% 1:3,"yes",
                             "no"))
ice<-ggplot(dplyr::filter(temp, icecovered == 'yes' & sampledate != "2004-01-21" & sampledate != "1991-02-20")) + 
  geom_point(aes(x = wtemp, y = depth, color = daynum), size = 2) +
  geom_path(aes(x = wtemp, y = depth, group = sampledate, color = daynum))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(expression("Temperature " ( degree*C)))+
  scale_colour_viridis_c(name = "Day of Year")+
  labs(title = "Ice Covered")+
  theme_bw(base_size = 8)+
  theme(legend.position = "none")


open<-ggplot(dplyr::filter(temp, icecovered == 'no' )) + 
  geom_point(aes(x = wtemp, y = depth, color = daynum), size = 2) +
  geom_path(aes(x = wtemp, y = depth, group = sampledate, color = daynum))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(expression("Temperature " ( degree*C)))+
  scale_colour_viridis_c(name = "Day of Year")+
  labs(title = "Open Water")+
  theme_bw(base_size = 8)
temphist<-ice + open+ plot_layout(ncol = 2, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("figures/Historical/temphist.png", width = 8, height = 6, units = 'in', temphist)


tempsum<- temp%>%
  filter(icecovered == "no")%>%
  mutate(month = month(sampledate))
