library(tidyverse)
library(devtools)
library(lubridate)
library(gridExtra)
library(ggplot2)

dat = read.csv('data/tempdoSummer2020.csv')
str(dat)

summer <- dat%>%
  drop_na

ggplot(dplyr::filter(summer, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = waterTemp, y = Depth, color = Date, size = 1.5)) +
  geom_path(aes(x = waterTemp, color = Date, y = Depth))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Water Temperature (C)")+
  theme_bw()

ggplot(dplyr::filter(summer, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = DO, y = Depth, color = Date)) +
  geom_path(aes(x = DO, color = Date, y = Depth))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Water Temperature (C)")+
  theme_bw()
