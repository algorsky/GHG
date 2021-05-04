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

oxy<- LTERphysical %>%
  filter(lakeid == "TB")%>%
  select(year4, daynum, sampledate, depth, rep, o2)%>%
  filter(!is.na(o2))

temp<- LTERphysical %>%
  filter(lakeid == "TB")%>%
  select(year4, daynum, sampledate, depth, rep, wtemp, flagwtemp)%>%
  filter(!is.na(wtemp))%>%
  filter(is.na(flagwtemp))

surfo2<-ggplot(dplyr::filter(oxy, depth == 0))+
  geom_line(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = o2, group = factor(year4), color = (year4)))+
  geom_point(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = o2, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_x_date(labels = date_format("%b"))+
  ylab('Surface DO (mg/L)') + xlab('Date') +
  theme_bw(base_size = 8)
ggsave("figures/Historical/surfaceo2.png", width = 15, height = 10, units = 'in', surfo2)

bottomo2<-ggplot(dplyr::filter(oxy, depth == 7))+
  geom_line(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = o2, group = factor(year4), color = (year4)))+
  geom_point(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = o2, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_x_date(labels = date_format("%b"))+
  ylab('Bottom DO (mg/L)') + xlab('Date') +
  theme_bw(base_size = 8)
ggsave("figures/Historical/bottomo2.png", width = 15, height = 10, units = 'in', bottomo2)

#Water
surftemp<-ggplot(dplyr::filter(temp, depth == 0))+
  geom_line(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = wtemp, group = factor(year4), color = (year4)))+
  geom_point(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = wtemp, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_x_date(labels = date_format("%b"))+
  ylab('Surface Water Temperature (C)') + xlab('Date') +
  theme_bw(base_size = 8)
ggsave("figures/Historical/surfacetemp.png", width = 15, height = 10, units = 'in', surftemp)

bottomtemp<-ggplot(dplyr::filter(temp, depth == 7))+
  geom_line(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = wtemp, group = factor(year4), color = (year4)))+
  geom_point(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = wtemp, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_x_date(labels = date_format("%b"))+
  ylab('Bottom Water Temperature (C)') + xlab('Date') +
  theme_bw(base_size = 8)
ggsave("figures/Historical/bottomtemp.png", width = 15, height = 10, units = 'in', bottomtemp)
  
