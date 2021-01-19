library(tidyverse)
library(devtools)
library(lubridate)

# Read in data for SSB
data = read_csv('data/eddypro_SSB_data.csv')

#QC
qc<- data%>%
  filter(ch4_flux > -9999)
ggplot(qc, aes(ch4_flux))+
  geom_histogram(binwidth = 0.01)

qc<- data%>%
  filter(ch4_flux > -9999)%>%
  filter(qc_ch4_flux <= 1)
flux<-qc%>%
  filter(ch4_flux > -0.15 & ch4_flux < 0.15)
flux$date<- as.Date(flux$date, format = "%m/%d/%y")

ggplot(flux)+
  geom_point(aes(x = date, y = ch4_flux), size = 0.05, color = "blue")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")),  linetype = "dashed", color = "black")+
  xlab("")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()

#Month


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

