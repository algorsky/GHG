library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)


################## Temp/Light Sensor ##################
buoy.2020 = read_csv('data/Buoy/2020Trout/TB_2019_underice_chla.csv')


ggplot(data = buoy.2020)+
  geom_line(aes(x = date_time_UTC, y = tempC_93_cm), color = "red")+
  geom_line(aes(x = date_time_UTC, y = tempC_768_cm), color = "blue")+
  theme_bw()
