library(tidyverse)
library(devtools)
library(lubridate)
library(neonDissGas)
library(gridExtra)

## Load the data from Github
# Read in data 
prac = read_csv('data/tidy.dat.out2021.csv')
prac$date = as.Date(prac$date, format =  "%m/%d/%y")

gd <- prac %>% 
  group_by(lake, date, depth) %>% 
  summarise(
    CO2 = mean(dissolvedCO2),
    CH4 = mean(dissolvedCH4)
  )

# CO2
ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CO2*1000000, y = depth, color = date),size = 3) +
  geom_path(aes(x = CO2*1000000, y = depth, color = date)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CO2 gas (umol/L)", limits = c(0, 2000))+
  theme_bw()+
  theme(legend.position = "none")

# CH4
ggplot(dplyr::filter(gd, lake == 'SSB')) + 
  geom_point(aes(x = CH4*1000000, y = depth, color = date),size = 3) +
  geom_path(aes(x = CH4*1000000, y = depth, color = date)) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CH4 gas (umol/L)")+
  theme_bw()+
  theme(legend.position = "none")

#Facet_Wrap by Date for Temperature and DO for Trout Bog
Temp<-ggplot(dplyr::filter(dat, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = waterTemp, y = Depth)) +
  geom_path(aes(x = waterTemp, y = Depth))+
  facet_wrap(~date) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  theme_bw()+
  theme(legend.position = "none")
#Facet_Wrap by Date for Temperature and DO
DO<-ggplot(dplyr::filter(dat, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = waterTemp, y = Depth)) +
  geom_path(aes(x = waterTemp, y = Depth))+
  facet_wrap(~date) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  theme_bw()+
  theme(legend.position = "none")

O2<-ggplot(dplyr::filter(dat, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = DO, y = Depth, color = col, size = 1.5)) +
  geom_path(aes(x = DO, y = Depth, color = col))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Dissolved Oxygen (mg/L)")+
  theme_bw()+
  theme(legend.position = "none")

grid.arrange(CO2,CH4,Temp,O2, ncol = 2, nrow =2)

ggsave("figure.png", width = 6.5, height = 6, units = 'in', figure)
