library(tidyverse)
library(lubridate)

tb = read_csv('data/Data/QAQCFlux.csv') %>% 
  mutate(datetime = ymd_hms(paste(date,time))) %>% 
  select(date, time, datetime, airtemp, wind_speed, co2_flux, ch4_flux, sonic_temperature) 

tbspring<- tb%>%
  filter(date > "2020-04-24" & date < "2020-05-14") #4/25-5/13 = 19 day period

calc<- tbspring%>%
  group_by(date)%>% #19 day total
  summarise(meanflux  = mean(ch4_flux*1000)) #nmol/m2/s

sumCH4<- sum(calc$meanflux)

nmol.m2 <- sumCH4 * 86400 * 19 #seconds per day and days per study

mmol.m2 <- nmol.m2 * 1e-6


(196.128 *86400*19/1000 )* 0.001

ggplot(tb)+
  geom_point(aes(x = date, y = ch4_flux))+
  geom_vline(aes(xintercept = as.Date('2020-09-06')), linetype = 2, size = 0.2)
