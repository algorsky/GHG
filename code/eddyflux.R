library(tidyverse)
library(devtools)
library(lubridate)
library(bigleaf)
library(patchwork)
# Read in data for SSB
dataSSB = read_csv('data/FluxTower/eddypro_SSB_full_output.csv')
dataTB = read_csv('data/FluxTower/eddypro_TB_full_output_new.csv')

#QC CH4 for SSB
#qcSSBCH4<- dataSSB%>%
 #filter(ch4_flux > -9999)
 #ggplot(qcSSBCH4, aes(ch4_flux))+
  #geom_histogram(binwidth = 0.01)

qcSSBCH4<- dataSSB%>%
  filter(ch4_flux > -9999)%>%
  filter(qc_ch4_flux <= 1)
fluxSSBCH4<-qcSSBCH4%>%
  filter(ch4_flux > -0.15 & ch4_flux < 0.15)
fluxSSBCH4$date<- as.Date(fluxSSBCH4$date, format = "%m/%d/%y")

qcSSBCO2<- dataSSB%>%
  filter(co2_flux > -9999)%>%
  filter(qc_co2_flux <= 1)
fluxSSBCO2<-qcSSBCO2%>%
  filter(co2_flux > -10 & co2_flux < 10)
fluxSSBCO2$date<- as.Date(fluxSSBCO2$date, format = "%m/%d/%y")

#QC CH4 for TB
#qcTBCH4<- dataTB%>%
 #filter(ch4_flux > -9999 & ch4_flux > -0.1 & ch4_flux < 0.1)
#ggplot(qcTBCH4, aes(ch4_flux))+
  #geom_histogram(binwidth = 0.01)

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


#CH4 Flux (µmol m-2 s-1)
ch4<-ggplot()+
  geom_point(data = fluxSSBCH4, aes(x = date, y = ch4_flux), size = 0.05, color = "blue")+
  geom_point(data = fluxTBCH4, aes(x = date, y = ch4_flux), size = 0.05, color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()


colours <- c("South Sparkling Bog" = "blue", "Trout Bog" = "red")
theme_set(theme_bw())
co2<- ggplot()+
  geom_point(data = fluxSSBCO2, aes(x = date, y = co2_flux, color = "South Sparkling Bog"), size = 0.05)+
  geom_point(data = fluxTBCO2, aes(x = date, y = co2_flux, color = "Trout Bog"), size = 0.05)+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  ylab("CO2 Flux (µmol m-2 s-1)")+
  scale_color_manual(values = colours)+
  theme(legend.position = "bottom", legend.title = element_blank())

buoy = read_csv('data/Buoy/temp.buoy2020.csv')
buoy$Date.GMT<- as.Date(buoy$Date.GMT, format = "%m/%d/%y")
buoysurf <- buoy %>%
  filter(depth == 1)%>%
  filter(as.Date(Date.GMT) > as.Date('2020-03-15')) %>%
  filter(as.Date(Date.GMT) < as.Date('2020-10-25'))%>%
  mutate(date = as.Date(Date.GMT))

buoybottom<- buoy %>%
  filter(depth == 7)%>%
  filter(as.Date(Date.GMT) > as.Date('2020-03-15')) %>%
  filter(as.Date(Date.GMT) < as.Date('2020-10-25'))%>%
  mutate(date = as.Date(Date.GMT))

colors <- c("Air Temperature" = "black", "Surface Temperature" = "green", "Bottom Temperature" = "yellow")
theme_set(theme_bw())
tempgap<- ggplot()+
  geom_path(data = dplyr::filter(fluxTBCH4, DOY < 110), aes(x = date, y = (airtemp), color = "Air Temperature"))+
  geom_path(data = dplyr::filter(fluxTBCH4, DOY > 112), aes(x = date, y = (airtemp), color = "Air Temperature"))+
  geom_path(data = buoysurf, aes(x = date, y = Temp.C, color = "Surface Temperature"))+
  geom_path(data = buoybottom, aes(x = date, y = Temp.C, color = "Bottom Temperature"))+
  ylab("Temperature (C)")+
  xlab("")+
  scale_color_manual(values = colors)+
  theme(legend.position = "top", legend.title = element_blank())
  
  
tempgas<-tempgap/ch4/co2 + plot_layout(ncol = 1)  
  
#plot_annotation(
  #caption = 'Figure: Timeseries of a) air/water temperature, b) methane flux (umol/m2/s) and c) carbon dioxide flux (umol/m2/s) from South Sparkling Bog (blue) and Trout Bog (red) with the dashed line representing ice-off.',
 # theme = theme(plot.caption = element_text( hjust = 0, size = 10)))

ggsave("figures/eddyco.png", width = 10, height = 6, units = 'in', tempgas)


#CH4, CO2 and Temp SSB
templinear<-ggplot(data = fluxTBCH4)+
  geom_point(aes(x = (air_temperature-273.15), y = (ch4_flux)))+
  geom_smooth(aes(x = (air_temperature-273.15), y = (ch4_flux)), method = "lm")+
  xlab("Temperature (C)")+
  ylab("CH4 Flux (nmol m-2 s-1)")+
  theme_bw()

ggsave("templinear.png", width = 10, height = 6, units = 'in', templinear)
TBch4.lm = lm((air_temperature-273.15) ~ (ch4_flux), data = fluxTBCH4)
summary(TBch4.lm)

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


