library(tidyverse)
library(devtools)
library(lubridate)
library(bigleaf)
library(patchwork)
# Read in data for SSB
dataSSB = read_csv('data/FluxTower/eddypro_SSB_data.csv')
dataTB = read_csv('data/FluxTower/eddypro_TB_data.csv')

#QC CH4 for SSB
#qcSSBCH4<- dataSSB%>%
 # filter(ch4_flux > -9999)
#ggplot(qcSSBCH4, aes(ch4_flux))+
 # geom_histogram(binwidth = 0.01)

qcSSBCH4<- dataSSB%>%
  filter(ch4_flux > -9999)%>%
  filter(qc_ch4_flux <= 1)
fluxSSBCH4<-qcSSBCH4%>%
  filter(ch4_flux > -0.15 & ch4_flux < 0.15)
fluxSSBCH4$date<- as.Date(fluxSSBCH4$date, format = "%m/%d/%y")

#QC CH4 for TB
#qcTBCH4<- dataTB%>%
 #filter(ch4_flux > -9999)
#ggplot(qcTBCH4, aes(ch4_flux))+
  #geom_histogram(binwidth = 0.01)

qcTBCH4<- dataTB%>%
  filter(ch4_flux > -9999)%>%
  filter(qc_ch4_flux <= 1)
fluxTBCH4<-qcTBCH4%>%
  filter(ch4_flux > -0.05 & ch4_flux < 0.05)
fluxTBCH4$date<- as.Date(fluxTBCH4$date, format = "%m/%d/%y")

#CH4 Flux (µmol m-2 s-1)
ch4SSB<-ggplot()+
  geom_point(data = fluxSSBCH4, aes(x = date, y = ch4_flux), size = 0.05, color = "blue")+
  #geom_point(data = fluxTBCH4, aes(x = date, y = ch4_flux), size = 0.05, color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()
  

tempplot<- ggplot(fluxSSBCH4)+
  geom_path(aes(x = date, y = (air_temperature-273.15)))+
  ylab("Air Temperature (C)")+
  xlab("")+
  theme_bw()
temp<- fluxSSBCH4 %>%
  group_by(date) %>%
  summarize(mean_temperature = mean(air_temperature))
  
tempgas<-tempplot/ch4SSB + plot_layout(ncol = 1) + plot_annotation(
  caption = 'Figure: Timeseries of a) air temperature and b) methane flux (umol/m2/s) from South Sparkling Bog with the dashed line representing ice-off.',
  theme = theme(plot.caption = element_text( hjust = 0, size = 10)))

ggsave("tempCH4SSB.png", width = 10, height = 6, units = 'in', tempgas)

ggplot()+
  geom_point(data = fluxSSBCH4, aes(x = date, y = ch4_flux), size = 0.05, color = "blue")+
  #geom_point(data = fluxTBCH4, aes(x = date, y = ch4_flux), size = 0.05, color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()

#CH4 and Temp SSB
ggplot()+
  geom_point(data = fluxTBCH4, aes(x = (air_temperature), y = (ch4_flux)))+
  geom_point(data = fluxSSBCH4, aes(x = (air_temperature), y = (ch4_flux)))+
  geom_smooth(aes(x = (air_temperature-273.15), y = (ch4_flux)), method = "lm")+
  xlab("T (C)")+
  ylab("CH4 Flux (nmol m-2 s-1)")+
  theme_bw()
SSBch4.lm = lm((air_temperature-273.15) ~ (ch4_flux*1000), data = fluxSSBCH4)
summary(SSBch4.lm)

#Month of May
ggplot()+
  geom_point(data = fluxSSBCH4, aes(x = date, y = ch4_flux), size = 0.05, color = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  xlim(as.Date('2020-05-01'), as.Date('2020-05-31'))+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()

#Day variation
may <- fluxSSBCH4%>% 
  filter(date %in% as.Date('2020-05-02'))
may2<-ggplot(may)+
  geom_point(aes(x = time, y = ch4_flux), size = 1.5, color = "blue")+
  #geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  xlab("Time (2020-05-02)")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()
ggsave("figure.png", width = 10, height = 6, units = 'in', may2)

#QC CO2
#qcCO2<- data%>%
 # filter(co2_flux > -9999)
#ggplot(qcCO2, aes(co2_flux))+
 # geom_histogram(binwidth = 1)

qcSSBCO2<- dataSSB%>%
  filter(co2_flux > -9999)%>%
  filter(qc_co2_flux <= 1)
fluxSSBCO2<-qcSSBCO2%>%
  filter(co2_flux > -10 & co2_flux < 10)
fluxSSBCO2$date<- as.Date(fluxSSBCO2$date, format = "%m/%d/%y")

#convert flux from (µmol m-2 s-1) to (gC m-2 d-1)
fluxunitsCO2<- fluxCO2%>%
  select(date, DOY, co2_flux)%>%
  mutate(gC = umolCO2.to.gC(co2_flux, constants = bigleaf.constants()))%>%
  mutate(co2 = gC.to.umolCO2(gC))

#CO2 Flux (µmol m-2 s-1)
ggplot(fluxCO2)+
  geom_point(aes(x = date, y = co2_flux), size = 0.05, color = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  ylab("CO2 Flux (µmol m-2 s-1)")+
  theme_bw()

#CO2 Flux (gC m-2 d-1)
ggplot(fluxunitsCO2)+
  geom_point(aes(x = date, y = gC), size = 0.05, color = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  ylab("CO2 Flux (gC m-2 d-1)")+
  theme_bw()


#mgC/m2/day
day<- flux %>%
  group_by(date) %>%
  summarize(mean_flux = mean(ch4_flux))

ggplot(day)+
  geom_line(aes(x = date, y = mean_flux))+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "blue")+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 d-1)")+
  theme_bw()


