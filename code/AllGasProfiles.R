library(tidyverse)
library(devtools)
library(lubridate)
library(neonDissGas)
library(gridExtra)
library(patchwork)
library(scales)

## Load the data from Github
# Read in data 
prac2021 = read_csv('data/GC2021/tidy.all2021.csv')
prac2020 = read_csv('data/tidy.dat.out2020.csv')
gas<- rbind(prac2021, prac2020)
gas$date = as.Date(gas$date, format =  "%m/%d/%y")
gas$doy = yday(gas$date)



gd <- gas %>% 
  group_by(lake, date, depth, doy) %>% 
  summarise(
    CO2 = mean(dissolvedCO2)*1000000,
    CH4 = mean(dissolvedCH4)*1000000
  )%>%
  mutate(icecovered = ifelse(month(date)%in% 1:3,"yes",
                             "no"))%>%
  mutate(year = factor(year(date)))

#write_csv(gd,'data/dataALL.csv')
#write.table(gd, 'data/dataALL.csv', sep="\t")

onoff<- gd%>%
  filter(date == as.POSIXct('2020-03-06') |date == as.POSIXct('2020-05-04')|date == as.POSIXct('2020-03-07')|date == as.POSIXct('2021-03-15')|date == as.POSIXct('2021-04-13'))

co2ice<-ggplot(dplyr::filter(onoff, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CO2, y = depth, color = year, shape = icecovered), size = 3) +
  geom_path(aes(x = CO2, y = depth, group = date, color = year)) +
  facet_wrap(~lake) +
  guides(size = FALSE)+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = ((expression(paste("C", O[2], " (", mu,"mol ", L^-1,")")))), limits = c(0, 2000))+
  scale_color_manual(values = c('lightblue4','gold'), name = "Year") +
  scale_shape(name = "Ice Covered?")+
  theme_bw(base_size = 8)+
  theme()
ch4ice<-ggplot(dplyr::filter(onoff, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CH4, y = depth, color = year, shape = icecovered), size = 3) +
  geom_path(aes(x = CH4, y = depth, group = date, color = year)) +
  facet_wrap(~lake) +
  guides(size = FALSE)+
  guides(shape = FALSE)+
  guides(color = FALSE)+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = ((expression(paste("C", H[4], " (", mu,"mol ", L^-1,")")))))+
  scale_color_manual(values = c('lightblue4','gold')) +
  theme_bw(base_size = 8)+
  theme()

diffgas<- co2ice/ch4ice + plot_layout(guides = "collect")
ggsave("figures/diffgas.png", width = 8, height = 6, units = 'in', diffgas)

#On and Off
co2compare<-ggplot(dplyr::filter(gd, (lake == 'TB'& icecovered == "yes")| (lake == 'SSB' & icecovered == "yes"))) + 
  geom_point(aes(x = CO2, y = depth, color = year), size = 3) +
  geom_path(aes(x = CO2, y = depth, group = date, color = year)) +
  facet_wrap(~lake) +
  guides(size = FALSE)+
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = ((expression(paste("C", O[2], " (", mu,"mol ", L^-1,")")))), limits = c(0, 2000))+
  scale_color_manual(values = c('lightblue4','gold'), name = "Year") +
  theme_bw(base_size = 8)

ch4compare<-ggplot(dplyr::filter(gd, (lake == 'TB'& icecovered == "yes")|( lake == 'SSB'& icecovered == "yes"))) + 
  geom_point(aes(x = CH4, y = depth, color = year), size = 3) +
  geom_path(aes(x = CH4, y = depth, group = date, color = year)) +
  guides(size = FALSE)+
  guides(shape = FALSE)+
  guides(color = FALSE)+
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = ((expression(paste("C", H[4], " (", mu,"mol ", L^-1,")")))))+
  scale_color_manual(values = c('lightblue4','gold'), name = "Year") +
  scale_shape_discrete(name = "Ice Covered")+
  theme_bw(base_size = 8)+
  theme()

compare<- gd %>%
  filter(icecovered == "yes")%>%
  group_by(lake, depth, year(date))%>%
  summarise(minCO2 = min(CO2),
            maxCO2 = max(CO2),
            minCH4 = min(CH4),
            maxCH4 = max(CH4))

compare<-co2compare/ch4compare + plot_layout(guides = "collect")
ggsave("figures/19-20compare.png", width = 8, height = 6, units = 'in', compare)

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
CO2<-ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CO2*1000000, y = depth, color = doy, shape = factor(year),size = 3)) +
  geom_path(aes(x = CO2*1000000, y = depth, group = date, color = doy)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CO2 gas (umol/L)", limits = c(0, 2000))+
  scale_colour_viridis_c()+
  theme_bw()+
  #theme(legend.position = "none")

# CH4
CH4<-ggplot(dplyr::filter(gd, lake == 'TB' & year == 2020| lake == 'SSB' & year == 2020)) + 
  geom_point(aes(x = CH4*1000000, y = depth, color = doy, shape = factor(year)),size = 3) +
  geom_path(aes(x = CH4*1000000, y = depth, group = date, color = doy)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CH4 gas (umol/L)")+
  scale_colour_viridis_c(name = "Day of Year")+
  scale_shape_discrete(name = "Ice Covered")+
  theme_bw()+
  theme()

# Read in data 
tempdo = read_csv('data/ChemTempDO/tempdoSSB.csv')
tempdo$sampledate = as.Date(tempdo$sampledate, format =  "%m/%d/%y")
tempdo<- tempdo%>%
  mutate(icecovered = ifelse(month(sampledate)%in% 1:3,"yes",
                             "no"))

tempdowinter<- tempdo%>%
  filter(month(sampledate)%in% 1:3)%>%
  mutate(Year = year(sampledate))

ssbwinter<-ggplot(dplyr::filter(tempdowinter, lake == 'SSB')) + 
  geom_point(aes(x = waterTemp_C, y = water_depth_m, color = factor(Year)), size = 2) +
  geom_path(aes(x = waterTemp_C, y = water_depth_m, color = factor(Year), group = sampledate))+
  scale_color_manual(name = 'Year',values = c('gray','lightblue4','gold')) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  labs(title = "South Sparkling Bog")+
  theme_bw(base_size = 8)+
  theme(legend.title = element_blank())

tbwinter<-ggplot(dplyr::filter(tempdowinter, lake == 'TB')) + 
  geom_point(aes(x = waterTemp_C, y = water_depth_m, color = factor(Year)), size = 2) +
  geom_path(aes(x = waterTemp_C, y = water_depth_m, color = factor(Year), group = sampledate))+
  scale_color_manual(name = 'Year',values = c('gray','lightblue4','gold')) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  labs(title = "Trout Bog")+
  theme_bw(base_size = 8)+
  theme(legend.title = element_blank())

temp<- ssbwinter + tbwinter
ggsave("figures/tempSSBTB.png", width = 8, height = 6, units = 'in', temp)
  
tempdo2020 <- tempdo %>%
  filter(date <= as.POSIXct('2021-01-01'))

#Facet_Wrap by Date for Temperature and DO for Trout Bog
Temp<-ggplot(dplyr::filter(tempdo, lake == 'SSB')) + 
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
