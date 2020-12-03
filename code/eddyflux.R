library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
data = read_csv('data/eddypro_SSB_data.csv')
flux<- data%>%
  filter(ch4_flux >= 0)

ggplot(flux)+
  geom_point(aes(x = date, y = ch4_flux))
