library(tidyverse)
library(devtools)
library(lubridate)
library(neonDissGas)
library(gridExtra)
library(patchwork)
library(scales)

## Load the data from Github
# Read in data 
prac2020all = read_csv('data/GC2021/tidy.dat.out.all.csv')
prac2020all$date = as.Date(prac2020all$date, format =  "%m/%d/%y")
prac2020 = read_csv('data/tidy.dat.out.csv')
gas<- rbind(prac2021, prac2020)
gas$date = as.Date(gas$date, format =  "%m/%d/%y")
gas$doy = yday(gas$date)
gas$year = format(gas$date, format = "%Y")

gasTB<- prac2020all%>%
  filter(year(date) == 2020)%>%
  filter(lake == "TB")%>%
  mutate(ch4 = dissolvedCH4*1000000)%>%
  mutate(co2 = dissolvedCO2*1000000)%>%
  select(date, depth, ch4, co2)

TBgasAvg <- gasTB%>%
  group_by(date, depth) %>% 
  summarise(CH4 = mean(ch4),
            CO2 = mean(co2),
            CH4sd = sd(ch4),
            CO2sd = sd(co2))
write.csv(TBgasAvg,"data/gasTBmeansd.csv")
getwd()
gd <- gas %>% 
  group_by(lake, date, depth, doy, year) %>% 
  summarise(
    CO2 = mean(dissolvedCO2*1000000),
    CH4 = mean(dissolvedCH4*1000000)
  ) %>%
  mutate(Year = factor(year))


summary2021<- gd %>%
  group_by(lake, depth, icecovered)%>%
  summarize(minCO2 = min(CO2)*1000000,
            maxCO2 = max(CO2)*1000000,
            minCH4 = min(CH4)*1000000,
            maxCH4 = max(CH4)*1000000,
            meanCO2 = mean(CO2)*1000000,
            meanCH4 = mean(CH4)*1000000) 

# CO2
CO2<-ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CO2, y = depth, shape = Year, color = Year),size = 5) +
  geom_path(aes(x = CO2, y = depth, group = date, color = Year)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = ((expression(paste("C", O[2], " (", mu,"mol ", L^-1,")")))), limits = c(0, 2000))+
  scale_color_manual(values = c('lightblue4','gold')) +
  theme_bw(base_size = 20)+
  theme(legend.position = "none")

# CH4
CH4<-ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CH4, y = depth, color = Year, shape = Year),size = 5) +
  geom_path(aes(x = CH4, y = depth, group = date, color = Year)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = ((expression(paste("C", H[4], " (", mu,"mol ", L^-1,")")))))+
  scale_shape_discrete(name = "")+
  scale_color_manual(values = c('lightblue4','gold')) +
  theme_bw(base_size = 20)

# Read in data 
tempdo = read_csv('data/ChemTempDO/tempdo.csv')
tempdo$date = as.Date(tempdo$sampledate, format =  "%m/%d/%y")
tempdo<- tempdo%>%
  mutate(icecovered = ifelse(month(date)%in% 1:4,"yes",
                             "no"))
tempdo2021 <- tempdo %>%
  filter(date > as.POSIXct('2021-01-01'))

#Facet_Wrap by Date for Temperature and DO for Trout Bog
Temp<-ggplot(dplyr::filter(tempdo2021, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = waterTemp, y = Depth, color = date, shape = icecovered), size = 2) +
  geom_path(aes(x = waterTemp, y = Depth, color = date, group = date))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

O2<-ggplot(dplyr::filter(tempdo2021, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = DO, y = Depth, color = month(date), shape = icecovered), size = 2) +
  geom_path(aes(x = DO, y = Depth, color = month(date), group = date))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Dissolved Oxygen (mg/L)")+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

gas<-(CO2 + CH4) + plot_layout(guides = "collect", ncol = 2)

ggsave("figures/ProfilesCompare.png", width = 14, height = 6, units = 'in', gas)


