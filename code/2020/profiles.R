library(tidyverse)
library(devtools)
library(lubridate)
library(neonDissGas)
library(gridExtra)
library(patchwork)
library(scales)

## Load the data from Github
# Read in data 
dat = read_csv('data/2020/data.2020.csv')
dat$date = as.Date(dat$date, format =  "%m/%d/%y")

tidy.dat.out2020 <- def.calc.sdg.conc(as.data.frame(dat)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,dissolvedCO2,dissolvedCH4)
tidy.dat.out2020$date<- as.Date(tidy.dat.out2020$date, format =  "%m/%d/%y")

sat.dat.out2020 <- def.calc.sdg.conc(as.data.frame(dat)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,barometricPressure,waterTemp, headspaceTemp, dissolvedCO2, concentrationCO2Air,dissolvedCH4, concentrationCH4Air, dissolvedN2O, concentrationN2OAir)

#Gas Saturation
dat.out.sat <- def.calc.sdg.sat(as.data.frame(sat.dat.out2020)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,waterTemp,satConcCO2,satConcCH4, CO2PercSat, CH4PercSat)
dat.out.sat$date = as.Date(dat.out.sat$date, format =  "%m/%d/%y")
gas.sat <- dat.out.sat %>% 
  group_by(lake, date, depth) %>% 
  summarise(
    CO2persat = mean(CO2PercSat),
    CH4persat = mean(CH4PercSat))

gd <- tidy.dat.out2020 %>% 
  group_by(lake, date, depth) %>% 
  summarise(
    CO2 = mean(dissolvedCO2),
    CH4 = mean(dissolvedCH4)
  )%>%
  mutate(icecovered = ifelse(month(date)%in% 1:4,"yes",
                             "no"))
# CO2
CO2<-ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CO2*1000000, y = depth, color = month(date), shape = icecovered),size = 3) +
  geom_path(aes(x = CO2*1000000, y = depth, group = date, color = month(date))) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CO2 gas (umol/L)", limits = c(0, 2000))+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

# CH4
CH4<-ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CH4*1000000, y = depth, color = month(date), shape = icecovered),size = 3) +
  geom_path(aes(x = CH4*1000000, y = depth, group = date, color = month(date))) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CH4 gas (umol/L)")+
  scale_colour_viridis_c(name = "Month")+
  scale_shape_discrete(name = "Ice Covered")+
  theme_bw()+
  theme()

# Read in data 
tempdo = read_csv('data/ChemTempDO/tempdo.csv')
tempdo$date = as.Date(tempdo$Date, format =  "%m/%d/%y")
tempdo<- tempdo%>%
  filter(year(date) == "2020")%>%
  mutate(icecovered = ifelse(month(date)%in% 1:4,"yes",
                             "no"))


#Facet_Wrap by Date for Temperature and DO for Trout Bog
Temp<-ggplot(dplyr::filter(tempdo, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = waterTemp, y = Depth, color = month(date), shape = icecovered), size = 1.5) +
  geom_path(aes(x = waterTemp, y = Depth, color = month(date), group = date))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

O2<-ggplot(dplyr::filter(tempdo, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = DO, y = Depth, color = month(date), shape = icecovered), size = 1.5) +
  geom_path(aes(x = DO, y = Depth, color = month(date), group = date))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Dissolved Oxygen (mg/L)")+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

figure2020<-(CO2 + CH4 + Temp + O2) + plot_layout(guides = "collect", ncol = 2)

ggsave("figures/2020/Profiles.png", width = 10, height = 6, units = 'in', figure2020)

# Heat map 
heatmap = read_csv('data/GC2021/fullheat.csv')
heatmap$date = as.Date(heatmap$date, format =  "%m/%d/%y")

heatmap<- heatmap%>%
  mutate(sampledate = date)%>%
  mutate(ch4unit = CH4*1000000)%>%
  mutate(co2unit = CO2*1000000)
# Contour Map... you can see the problems 
heatCH4<-ggplot(heatmap) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = ch4unit)) +
  geom_point(aes(x = sampledate, y = depth), size = 0.25, color = "white") +
  scale_y_reverse() +
  xlab("")+
  facet_wrap(~lake)+
  labs(fill = ("CH4(umol/L)"))+
  scale_color_distiller() +
  theme_bw(base_size = 12)
heatCO2<-ggplot(heatmap) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = co2unit)) +
  geom_point(aes(x = sampledate, y = depth), size = 0.25, color = "white") +
  scale_y_reverse() +
  xlab("")+
  facet_wrap(~lake)+
  labs(fill = ("CO2(umol/L)"))+
  scale_color_distiller() +
  theme_bw(base_size = 12)

heatmaps<-(heatCH4 + heatCO2) + plot_layout(guides = "collect", nrow = 2)

ggsave("figures/HeatMap.png", width = 15, height = 10, units = 'in', heatmaps)


#Gas Saturation
gas.saturation = read_csv('data/GC2021/gas.sat.csv')
gas.saturation$date <- as.Date(gas.saturation$date, format =  "%m/%d/%y")

gas.sat.surface<- gas.saturation%>%
  filter(depth == 0)

co2sat<-ggplot(dplyr::filter(gas.saturation, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = date, y = CO2persat, color = depth), size = 3) +
  facet_wrap(~lake) +
  ylab("CO2 Percent Saturation")+
  xlab("")+
  theme_bw()+
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b %y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2021,'-01-15'))))
ch4sat<-ggplot(dplyr::filter(gas.saturation, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = date, y = CH4persat, color = depth), size = 3) +
  facet_wrap(~lake) +
  ylab("CH4 Percent Saturation")+
  xlab("")+
  theme_bw()+
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b %y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2021,'-01-15'))))

#Surface Saturation
surCO2<-ggplot(dplyr::filter(gas.sat.surface, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = date, y = CO2persat), size = 3) +
  facet_wrap(~lake) +
  xlab("")+
  ylab("Surface CO2 Percent Saturation")+
  theme_bw()+
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b %y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2021,'-01-15'))))
surCH4<-ggplot(dplyr::filter(gas.sat.surface, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = date, y = CH4persat), size = 3) +
  facet_wrap(~lake) +
  xlab("")+
  ylab("Surface CH4 Percent Saturation")+
  theme_bw()+
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b %y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2021,'-01-15'))))

saturation<-(co2sat + ch4sat + surCO2 + surCH4) + plot_layout(guides = "collect", ncol = 2)
ggsave("figures/saturation.png", width = 15, height = 10, units = 'in', saturation)
