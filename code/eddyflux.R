library(tidyverse)
library(devtools)
library(lubridate)
library(bigleaf)
# Read in data for SSB
data = read_csv('data/FluxTower/eddypro_SSB_data.csv')

#QC CH4
qcCH4<- data%>%
  filter(ch4_flux > -9999)
ggplot(qcCH4, aes(ch4_flux))+
  geom_histogram(binwidth = 0.01)

qcCH4<- data%>%
  filter(ch4_flux > -9999)%>%
  filter(qc_ch4_flux <= 1)
fluxCH4<-qcCH4%>%
  filter(ch4_flux > -0.15 & ch4_flux < 0.15)
fluxCH4$date<- as.Date(fluxCH4$date, format = "%m/%d/%y")

#QC CO2
qcCO2<- data%>%
  filter(co2_flux > -9999)
ggplot(qcCO2, aes(co2_flux))+
  geom_histogram(binwidth = 1)

qcCO2<- data%>%
  filter(co2_flux > -9999)%>%
  filter(qc_co2_flux <= 1)
fluxCO2<-qcCO2%>%
  filter(co2_flux > -10 & co2_flux < 10)
fluxCO2$date<- as.Date(fluxCO2$date, format = "%m/%d/%y")

#convert flux from (µmol m-2 s-1) to (gC m-2 d-1)
fluxunitsCO2<- fluxCO2%>%
  select(date, DOY, co2_flux)%>%
  mutate(gC = umolCO2.to.gC(co2_flux, constants = bigleaf.constants()))%>%
  mutate(co2 = gC.to.umolCO2(gC))

#CH4 Flux (µmol m-2 s-1)
ggplot(fluxCH4)+
  geom_point(aes(x = date, y = ch4_flux), size = 0.05, color = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()

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


#Month of May
ggplot(flux)+
  geom_point(aes(x = daytime, y = ch4_flux), size = 0.05, color = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  xlim(as.Date('2020-05-01'), as.Date('2020-05-31'))+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()

#Day variation
may <- flux%>% 
  filter(date %in% as.Date('2020-05-02'))
ggplot(may)+
  geom_point(aes(x = time, y = ch4_flux), size = 1.5, color = "blue")+
  #geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  xlab("Time (2020-05-02)")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
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

# Read in data for TB
dataTB = read_csv('data/eddypro_TB_data.csv')
fluxTB<- dataTB%>%
  filter(ch4_flux > -9999)
fluxTB$date<- as.Date(fluxTB$date, format = "%m/%d/%y")

ggplot(fluxTB)+
  geom_line(aes(x = date, y = ch4_flux))+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "blue")+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()

#mgC/m2/day
dayTB<- fluxTB %>%
  group_by(date) %>%
  summarize(mean_flux = mean(ch4_flux))

ggplot(dayTB)+
  geom_line(aes(x = date, y = mean_flux))+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "blue")+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 d-1)")+
  theme_bw()

