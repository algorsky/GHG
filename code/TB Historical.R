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
ice<-read_csv("data/ChemTempDO/ice_duration.csv")
ice<- ice%>%
  filter(lakeid == "TB")%>%
  select(year, datelastice, datefirstopen, datelastopen, datefirstice)%>%
  mutate(year4 = year)
oxy<- LTERphysical %>%
  filter(lakeid == "TB")%>%
  select(year4, daynum, sampledate, depth, rep, o2)%>%
  filter(!is.na(o2))
oxyice<- left_join(oxy, ice, by = "year4")
oxyice$datelastopen<- as.Date(oxyice$datelastopen, format =  "%m/%d/%y")

winteroxy<- oxyice %>%
  filter(daynum > 85 & daynum < 140)
winteroxy$datelastice <- as.Date(winteroxy$datelastice, format =  "%m/%d/%y")

falloxy<- oxyice %>%
  filter(daynum >274 & daynum < 334)
falloxy$datelastopen <- as.Date(falloxy$datelastopen, format =  "%m/%d/%y")
spring<- winteroxy %>%
  group_by(year4)%>%
  mutate(icecovered = ifelse(sampledate > datelastice,"Ice Covered",
                             "Open Water"))

ggplot(spring)+
  geom_line(aes(x = o2, y = depth, group = sampledate, color = (year4)))+
  geom_point(aes(x = o2, y = depth, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Oxygen (mg/L)")+
  facet_wrap(~icecovered)+
  theme_bw(base_size = 8)

beforefall<- oxyice %>%
  mutate(difference = (yday(sampledate) - yday(datelastopen)))%>%
  group_by(year4, depth)%>%
  filter(difference > 0)%>%
  slice(which.min(difference))
afterfall<- oxyice %>%
  filter(year4>1981 & (month(sampledate) == 10 | month(sampledate)==11))%>%
  mutate(difference = (yday(sampledate) - yday(datelastopen)))%>%
  filter(difference < 0)%>%
  group_by(year4, depth)%>%
  slice(which.max(difference))

beforefall<-ggplot(beforefall)+
  geom_line(aes(x = o2, y = depth, group = sampledate, color = (year4)))+
  geom_point(aes(x = o2, y = depth, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = ((expression(paste(O[2], " (mg " , L^-1,")")))))+
  labs(title = "Ice Covered")+
  theme_bw(base_size = 8)+
  theme(legend.position = "none")
turnfall<-ggplot(afterfall)+
  geom_line(aes(x = o2, y = depth, group = sampledate, color = (year4)))+
  geom_point(aes(x = o2, y = depth, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Oxygen (mg/L)")+
  theme_bw(base_size = 8)

ggsave("figures/lastfallo2.png", width = 8, height = 6, units = 'in', turnfall)

springice<- wintertemp %>%
  mutate(difference = (yday(sampledate) - yday(datelastice)))%>%
  filter(difference < 0)%>%
  group_by(year4, depth)%>%
  slice(which.max(difference))

springopen<- wintertemp %>%
  mutate(difference = (yday(sampledate) - yday(datelastice)))%>%
  filter(difference > 0)%>%
  group_by(year4, depth)%>%
  slice(which.min(difference))

ice<-ggplot(springice)+
  geom_line(aes(x = wtemp, y = depth, group = sampledate, color = (year4)))+
  geom_point(aes(x = wtemp, y = depth, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  labs(title = "Ice Covered")+
  theme_bw(base_size = 8)+
  theme(legend.position = "none")
open<-ggplot(springopen)+
  geom_line(aes(x = wtemp, y = depth, group = sampledate, color = (year4)))+
  geom_point(aes(x = wtemp, y = depth, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  labs(title = "Open Water")+
  theme_bw(base_size = 8)

lastfirstice<-ice + open+ plot_layout(ncol = 2, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("figures/lastfirsticeo2.png", width = 8, height = 6, units = 'in', lastfirstice)


ggplot(autumn)+
  geom_line(aes(x = o2, y = depth, group = sampledate, color = (year4)))+
  geom_point(aes(x = o2, y = depth, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Oxygen (mg/L)")+
  theme_bw(base_size = 8)

ggplot(dplyr::filter(oxy, month(sampledate) == 10))+
  geom_line(aes(x = o2, y = depth, group = sampledate, color = (day(sampledate))))+
  geom_point(aes(x = o2, y = depth, color = (day(sampledate))))+
  scale_colour_viridis_c(name = "Year")+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Oxygen (mg/L)")+
  theme_bw(base_size = 8)


temp<- LTERphysical %>%
  filter(lakeid == "TB")%>%
  select(year4, daynum, sampledate, depth, rep, wtemp, flagwtemp)%>%
  filter(!is.na(wtemp))%>%
  filter(is.na(flagwtemp))%>%
  filter(wtemp >= 0)%>%
  mutate(icecovered = ifelse(month(sampledate)%in% 1:3,"yes",
                             "no"))

tempice<- left_join(temp, ice, by = "year4")
tempice$datelastopen<- as.Date(tempice$datelastopen, format =  "%m/%d/%y")

wintertemp<- tempice %>%
  filter(daynum > 85 & daynum < 140)
wintertemp$datelastice <- as.Date(wintertemp$datelastice, format =  "%m/%d/%y")

falltemp<- tempice %>%
  filter(daynum >274 & daynum < 334)
falltemp$datelastopen <- as.Date(falltemp$datelastopen, format =  "%m/%d/%y")

surfo2<-ggplot(dplyr::filter(oxy, depth == 0))+
  geom_line(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = o2, group = factor(year4), color = (year4)))+
  geom_point(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = o2, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_x_date(labels = date_format("%b"))+
  ylab(expression(paste("Surface " , O[2], " (mg " , L^-1,")")))+
  xlab('Date') +
  theme_bw(base_size = 8)
ggsave("figures/Historical/surfaceo2.png", width = 15, height = 10, units = 'in', surfo2)

bottomo2<-ggplot(dplyr::filter(oxy, depth == 7))+
  geom_line(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = o2, group = factor(year4), color = (year4)))+
  geom_point(aes(x = as.Date(daynum, origin = as.Date('1981-01-01')), y = o2, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_x_date(labels = date_format("%b"))+
  ylab(expression(paste("Bottom " , O[2], " (mg " , L^-1,")")))+
  xlab('Date') +
  theme_bw(base_size = 8)

bottom<- oxy %>%
  filter(depth == 7 & (daynum > 274 | daynum < 343))%>%
  summarize(numb = sum(o2 > 2))

histo2<-surfo2/bottomo2 + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("figures/histo2.png", width = 8, height = 6, units = 'in', histo2)


ggplot(dplyr::filter(oxy, month(sampledate) == 4 | month(sampledate) == 5))+
  geom_line(aes(x = o2, y = depth, group = sampledate, color = (year4)))+
  geom_point(aes(x = o2, y = depth, color = (year4)))+
  scale_colour_viridis_c(name = "Year")+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Oxygen (mg/L)")+
  theme_bw(base_size = 8)
ggsave("figures/Historical/bottomo2.png", width = 15, height = 10, units = 'in', bottomo2)
ggplot(dplyr::filter(tempdowinter, lake == 'TB')) + 
  geom_point(aes(x = waterTemp_C, y = water_depth_m, color = factor(Year)), size = 2) +
  geom_path(aes(x = waterTemp_C, y = water_depth_m, color = factor(Year), group = sampledate))+
  scale_color_manual(name = 'Year',values = c('gray','lightblue4','gold')) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  labs(title = "Trout Bog")+
  theme_bw(base_size = 8)+
  theme(legend.title = element_blank())
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
  ylab('Bottom Water Temperature (C)') + 
  xlab('Date') +
  theme_bw(base_size = 8)

ggsave("figures/Historical/bottomtemp.png", width = 15, height = 10, units = 'in', bottomtemp)

wintertemp<- temp%>%
  filter(month(sampledate) == 2)
ggplot(dplyr::filter(temp, icecovered == 'yes' & sampledate != "2004-01-21" & sampledate != "1991-02-20"))+
  geom_point(aes(x = wtemp, y = depth, color = factor(month(sampledate))))+
  geom_path(aes(x = wtemp, y = depth, color = factor(month(sampledate)), group = sampledate))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Water Temperature")+
  theme_bw()+
  theme(legend.position = "none")

ggplot(dplyr::filter(temp, month(sampledate) == 10))+
  geom_point(aes(x = wtemp, y = depth, color = factor(month(sampledate))))+
  geom_path(aes(x = wtemp, y = depth, color = factor(month(sampledate)), group = sampledate))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Water Temperature")+
  theme_bw()+
  theme(legend.position = "none")
  
