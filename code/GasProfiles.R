library(tidyverse)
library(devtools)
library(lubridate)
library(neonDissGas)
library(gridExtra)

## Load the data from Github
# Read in data 
prac = read_csv('data/tidy.dat.out.csv')
prac$date = mdy(prac$date)

gd <- prac %>% 
  group_by(lake, date, lakedepth) %>% 
  summarise(
    CO2 = mean(dissolvedCO2),
    CH4 = mean(dissolvedCH4)
  )
gd$col <- "Jan. 30-31"
gd$col <- ifelse(gd$date < as.POSIXct("2020-01-30"), "Jan. 9-10", gd$col)
gd$col <- ifelse(gd$date > as.POSIXct("2020-01-31"), "Feb. 14-15", gd$col)
gd$col <- ifelse(gd$date > as.POSIXct("2020-02-15"), "March 6-7", gd$col)
gd$col <- factor(gd$col, c("Jan. 9-10", "Jan. 30-31", "Feb. 14-15", "March 6-7"), ordered = TRUE)


dat = read.csv('data/tempdo.csv')
dat$date = mdy(dat$Date)

dat$col <- "Jan. 30-31"
dat$col <- ifelse(dat$date < as.POSIXct("2020-01-30"), "Jan. 9-10", dat$col)
dat$col <- ifelse(dat$date > as.POSIXct("2020-01-31"), "Feb. 14-15", dat$col)
dat$col <- ifelse(dat$date > as.POSIXct("2020-02-15"), "March 6-7", dat$col)
dat$col <- factor(dat$col, c("Jan. 9-10", "Jan. 30-31", "Feb. 14-15", "March 6-7"), ordered = TRUE)


# CO2
CO2<-ggplot(dplyr::filter(gd, lake == 'TB' | lake == 'SSB')) + 
  geom_point(aes(x = CO2*1000000, y = lakedepth, color = col),size = 3) +
  geom_path(aes(x = CO2*1000000, y = lakedepth, color = col)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CO2 gas (umol/L)", limits = c(0, 2000))+
  theme_bw()+
  theme(legend.position = "none")

# CH4
CH4<-ggplot(dplyr::filter(gd, lake == 'TB' | lake == 'SSB')) + 
  geom_point(aes(x = CH4*1000000, y = lakedepth, color = col),size = 3) +
  geom_path(aes(x = CH4*1000000, y = lakedepth, color = col)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CH4 gas (umol/L)", limits = c(0, 1600))+
  theme_bw()+
  theme(legend.position = "none")


Temp<-ggplot(dplyr::filter(dat, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = waterTemp, y = Depth, color = col, size = 1.5)) +
  geom_path(aes(x = waterTemp, y = Depth, color = col))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Water Temperature (C)")+
  theme_bw()+
  theme(legend.position = "none")

O2<-ggplot(dplyr::filter(dat, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = DO, y = Depth, color = col, size = 1.5)) +
  geom_path(aes(x = DO, y = Depth, color = col))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Dissolved Oxygen (mg/L)")+
  theme_bw()+
  theme(legend.position = "none")

grid.arrange(CO2,CH4,Temp,O2, ncol = 2, nrow =2)

ggsave("figure.png", width = 6.5, height = 6, units = 'in', figure)
