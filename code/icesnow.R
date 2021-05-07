library(tidyverse)
library(devtools)
library(lubridate)
library(bigleaf)
library(patchwork)
# Read in data for SSB
icesnow = read_csv('data/ChemTempDO/barplotice.csv')
icesnow$Date <- as.Date(icesnow$Date, format =  "%m/%d/%y")
snow = read_csv('data/ChemTempDO/snowMinocqua.csv')
ice = read_csv('data/ChemTempDO/icesnow.csv')
ice$Date <- as.Date(ice$Date, format =  "%m/%d/%y")
summary<-ice%>%
  mutate(month = month(Date))%>%
  mutate(year = year(Date))%>%
  filter(month == 3)%>%
  group_by(Lake,year)%>%
  summarize(max = max(totice))

snow$Month = factor(snow$Month, levels = c("November", "December", "January", "February", "March", "April", "Annual Total"))

snow<-ggplot(data = snow, aes(x = Month, AvgSnow_cm, fill = factor(Year)))+
  geom_bar(stat = "identity",position = 'dodge')+
  scale_fill_manual(values = c("gray",'lightblue4','gold'), name = "Year") +
  xlab("Month")+
  ylab("Total Accumulation (cm)")+
  theme_bw(base_size = 8)
ggsave("figures/snowfall.png", width = 8, height = 6, units = 'in', snow)

ice_snow<- icesnow %>%
  mutate(Year = year(Date))%>%
  filter(Variable != "total_ice")%>%
  group_by(factor(Year))%>%
  mutate(doy = yday(Date))%>%
  mutate(Month = month(Date))

ice_snow$Variable = factor(ice_snow$Variable , levels = c("snow", "white", "black"))
SSBice<-ggplot(dplyr::filter(ice_snow, Lake == "SSB"), aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = Value, fill = Variable, color = "gray"))+
  geom_bar(stat = "identity", width = 5)+
  scale_x_date(labels = date_format("%b"))+
  ylab("Thickness(cm)")+
  xlab("Sampling Month")+
  labs(title = "South Sparkling Bog")+
  scale_fill_manual(values = c("white",'gray88','gray4'), name = "") +
  guides(color = FALSE)+
  scale_color_manual(values = c("black",'black','black'), name = "") +
  facet_wrap(~Year)+
  theme_bw(base_size = 8)

TBice<-ggplot(dplyr::filter(ice_snow, Lake == "TB"), aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = Value, fill = Variable, color = "black"))+
  geom_bar(stat = "identity", width = 5)+
  scale_x_date(labels = date_format("%b"))+
  ylab("Thickness(cm)")+
  xlab("Sampling Month")+
  labs(title = "Trout Bog")+
  scale_fill_manual(values = c("white",'gray88','gray4'), name = "") +
  guides(color = FALSE)+
  scale_color_manual(values = c("black",'black','black'), name = "") +
  facet_wrap(~Year)+
  theme_bw(base_size = 8)

ice<-SSBice/TBice
ggsave("figures/ice.png", width = 8, height = 6, units = 'in', ice)
