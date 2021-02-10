library(tidyverse)
library(devtools)
library(lubridate)
library(neonDissGas)
library(gridExtra)
library(patchwork)
library(scales)

## Load the data from Github
# Read in data 
prac2021 = read_csv('data/GC2021/tidy.dat.out2021.csv')
prac2020 = read_csv('data/tidy.dat.out.csv')
gas<- rbind(prac2021, prac2020)
gas$date = as.Date(gas$date, format =  "%m/%d/%y")
gas$doy = yday(gas$date)



gd <- gas %>% 
  group_by(lake, date, depth, doy) %>% 
  summarise(
    CO2 = mean(dissolvedCO2),
    CH4 = mean(dissolvedCH4)
  )%>%
  mutate(icecovered = ifelse(month(date)%in% 1:4,"yes",
                             "no"))


# CO2
CO2<-ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CO2*1000000, y = depth, color = doy, shape = icecovered),size = 3) +
  geom_path(aes(x = CO2*1000000, y = depth, group = date, color = doy)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CO2 gas (umol/L)", limits = c(0, 2000))+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

# CH4
CH4<-ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CH4*1000000, y = depth, color = doy, shape = icecovered),size = 3) +
  geom_path(aes(x = CH4*1000000, y = depth, group = date, color = doy)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CH4 gas (umol/L)")+
  scale_colour_viridis_c(name = "Day of Year")+
  scale_shape_discrete(name = "Ice Covered")+
  theme_bw()+
  theme()

# Read in data 
tempdo = read_csv('data/ChemTempDO/tempdo.csv')
tempdo$date = as.Date(tempdo$Date, format =  "%m/%d/%y")
tempdo<- tempdo%>%
  mutate(icecovered = ifelse(month(date)%in% 1:4,"yes",
                             "no"))


#Facet_Wrap by Date for Temperature and DO for Trout Bog
Temp<-ggplot(dplyr::filter(tempdo, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = waterTemp, y = Depth, color = date, shape = icecovered), size = 1.5) +
  geom_path(aes(x = waterTemp, y = Depth, color = date, group = date))+
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

figure<-(CO2 + CH4 + Temp + O2) + plot_layout(guides = "collect", ncol = 2)

ggsave("figure.png", width = 10, height = 6, units = 'in', figure)

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

ggsave("HeatMap.png", width = 10, height = 6, units = 'in', heatmaps)



