library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
data = read_csv('data/eddypro_SSB_data.csv')
flux<- data%>%
  filter(ch4_flux >= 0)


ggplot(flux)+
  geom_line(aes(x = DOY, y = ch4_flux))+
  geom_vline(xintercept = 119,  linetype = "dashed", color = "blue")+
  xlab("Day of Year")+
  ylab("CH4 Flux (µmol m-2 s-1)")+
  theme_bw()
