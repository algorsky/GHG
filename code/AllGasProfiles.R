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

gd.2020<- gd%>%
  filter(date <= as.POSIXct('2021-01-01'))

summary2020<- gd.2020 %>%
  group_by(lake, depth, icecovered)%>%
  summarize(minCO2 = min(CO2)*1000000,
            maxCO2 = max(CO2)*1000000,
            minCH4 = min(CH4)*1000000,
            maxCH4 = max(CH4)*1000000,
            meanCO2 = mean(CO2)*1000000,
            meanCH4 = mean(CH4)*1000000) 

ssb.gas = read_csv('data/GC2021/SSB_GHG.csv')
ssb.gas$date = as.Date(ssb.gas$sampledate, format =  "%m/%d/%y")
ggplot(dplyr::filter(ssb.gas, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CO2, y = depth, color = sampledate), size = 3) +
  geom_path(aes(x = CO2, y = depth, group = sampledate, color = date)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CO2 gas (umol/L)", limits = c(0, 2000))+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")
# CO2
CO2<-ggplot(dplyr::filter(gd.2020, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CO2*1000000, y = depth, color = doy, shape = icecovered),size = 3) +
  geom_path(aes(x = CO2*1000000, y = depth, group = date, color = doy)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CO2 gas (umol/L)", limits = c(0, 2000))+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

# CH4
CH4<-ggplot(dplyr::filter(gd.2020, lake == 'TB'| lake == 'SSB')) + 
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

tempdowinter<- tempdo%>%
  filter(month(date)%in% 1:4)%>%
  mutate(Year = year(date))

ssbwinter<-ggplot(dplyr::filter(tempdowinter, Bog == 'SSB')) + 
  geom_point(aes(x = DO, y = Depth, color = factor(Year)), size = 5) +
  geom_path(aes(x = DO, y = Depth, color = factor(Year), group = date))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "DO (mg/L)")+
  theme_bw(base_size = 20)+
  theme(legend.title = element_blank())

tbwinter<-ggplot(dplyr::filter(tempdowinter, Bog == 'TB')) + 
  geom_point(aes(x = DO, y = Depth, color = factor(Year)), size = 5) +
  geom_path(aes(x = DO, y = Depth, color = factor(Year), group = date))+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "DO (mg/L)")+
  theme_bw(base_size = 20)+
  theme(legend.title = element_blank())


  
tempdo2020 <- tempdo %>%
  filter(date <= as.POSIXct('2021-01-01'))

#Facet_Wrap by Date for Temperature and DO for Trout Bog
Temp<-ggplot(dplyr::filter(tempdo, Bog == 'SSB')) + 
  geom_point(aes(x = waterTemp, y = Depth, color = date, shape = icecovered), size = 2) +
  geom_path(aes(x = waterTemp, y = Depth, color = date, group = date))+
  facet_wrap(~year(tempdo$date)) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

O2<-ggplot(dplyr::filter(tempdo2020, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = DO, y = Depth, color = month(date), shape = icecovered), size = 2) +
  geom_path(aes(x = DO, y = Depth, color = month(date), group = date))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Dissolved Oxygen (mg/L)")+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

figure<-(CO2 + CH4 + Temp + O2) + plot_layout(guides = "collect", ncol = 2)

ggsave("figures/Profiles2020color.png", width = 10, height = 6, units = 'in', figure)

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
