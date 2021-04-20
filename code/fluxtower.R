library(tidyverse)
library(devtools)
library(lubridate)
library(bigleaf)
library(patchwork)
# Read in data for SSB
dataSSB = read_csv('data/FluxTower/eddypro_SSB_full_output.csv')
dataTB = read_csv('data/FluxTower/eddypro_TB_full_output_new.csv')

qcTBCH4<- dataTB%>%
  filter(ch4_flux > -9999)%>%
  filter(qc_ch4_flux <= 1)
fluxTBCH4<-qcTBCH4%>%
  filter(ch4_flux > -0.05 & ch4_flux < 0.05)%>%
  mutate(airtemp = (air_temperature - 273.15))
fluxTBCH4$date<- as.Date(fluxTBCH4$date, format = "%m/%d/%y")

qcTBCO2<- dataTB%>%
  filter(co2_flux > -9999)%>%
  filter(qc_co2_flux <= 1)%>%
  filter(co2_flux > -10 & co2_flux < 10)
fluxTBCO2<-qcTBCO2%>%
  filter(co2_flux > -10 & co2_flux < 10)
fluxTBCO2$date<- as.Date(fluxTBCO2$date, format = "%m/%d/%y")

TBch4mean<- fluxTBCH4%>%
  group_by(date)%>%
  summarise(mean = mean(ch4_flux))%>%
  filter(mean*1000 > -10)

iceoffch4<- TBch4mean %>%
  filter(date > as.POSIXct('2020-04-27')& date < as.POSIXct('2020-05-16'))%>%
  mutate(day = mean * 86400)
sum(iceoffch4$day) * 17 * 0.001


TBco2mean<- fluxTBCO2%>%
  group_by(date)%>%
  summarise(mean = mean(co2_flux))%>%
  filter(mean > -2.5)

iceoffco2<- TBco2mean %>%
  filter(date > as.POSIXct('2020-04-26')& date < as.POSIXct('2020-05-07'))%>%
  mutate(day = mean * 86400)
sum(iceoffco2$day) * 10 * 0.001

iceoffch4<- TBch4mean %>%
  filter(date > as.POSIXct('2020-04-26')& date < as.POSIXct('2020-05-16'))
sum(iceoffch4$mean) * 86400 * 0.001

fallco2<- TBco2mean %>%
  filter(date > as.POSIXct('2020-09-30')& date < as.POSIXct('2020-10-15'))%>%
  mutate(day = mean * 86400)
sum(fallco2$day) * 14 * 0.001

fallch4<- TBch4mean %>%
  filter(date > as.POSIXct('2020-09-30')& date < as.POSIXct('2020-10-10'))
sum(fallch4$mean) * 86400 * 0.001

#CH4 Flux (µmol m-2 s-1)
ch4<-ggplot()+
  geom_point(data = fluxTBCH4, aes(x = date, y = ch4_flux *1000), size = 0.05, color = "gray", alpha = 0.5)+
  geom_point(data = TBch4mean, aes( x = date, y = mean*1000), size = 0.9, color = "red2")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2020-10-01")),  linetype = "dashed", color = "brown")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, 10))+
  scale_x_date(date_labels="%b", date_breaks  ="1 month")+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))+
  theme_bw(base_size = 20)

co2<- ggplot()+
  geom_point(data = fluxTBCO2, aes(x = date, y = co2_flux), size = 0.05, color = "gray", alpha = 0.5)+
  geom_point(data = TBco2mean, aes(x = date, y = mean), color = "turquoise4", size = 0.75)+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2020-10-01")),  linetype = "dashed", color = "brown")+
  geom_vline(xintercept = as.numeric(as.Date("2020-03-07")),  linetype = "dashed", color = "yellow")+
  geom_vline(xintercept = as.numeric(as.Date("2020-05-04")),  linetype = "dashed", color = "yellow")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 2))+
  xlab("")+
  ylab("CO2 Flux (µmol m-2 s-1)")+
  ylab(expression(paste("C", O[2], " flux (", µ,"mol ", m^-2, s^-1,")")))+
  scale_x_date(date_labels="%b", date_breaks  ="1 month")+
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom", legend.title = element_blank())

gas2<-ch4/co2 + plot_layout(ncol = 1)  
ggsave("figures/fluxshade.png", width = 10, height = 8, units = 'in', gas2)
