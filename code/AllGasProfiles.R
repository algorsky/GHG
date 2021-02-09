library(tidyverse)
library(devtools)
library(lubridate)
library(neonDissGas)
library(gridExtra)
library(patchwork)

## Load the data from Github
# Read in data 
prac2021 = read_csv('data/GC2021/tidy.dat.out2021.csv')
prac2020 = read_csv('data/tidy.dat.out.csv')
gas<- rbind(prac2021, prac2020)
gas$date = as.Date(gas$date, format =  "%m/%d/%y")
gas$doy = yday(gas$date)

gd <- gas %>% 
  group_by(lake, date, depth, doy) %>% 
  summarise(
    CO2 = mean(dissolvedCO2),
    CH4 = mean(dissolvedCH4)
  )%>%
  mutate(icecovered = ifelse(month(date)%in% 1:4,"yes",
                             "no"))
  

# CO2
CO2<-ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CO2*1000000, y = depth, color = doy, shape = icecovered),size = 3) +
  geom_path(aes(x = CO2*1000000, y = depth, group = date, color = doy)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CO2 gas (umol/L)", limits = c(0, 2000))+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

# CH4
CH4<-ggplot(dplyr::filter(gd, lake == 'TB'| lake == 'SSB')) + 
  geom_point(aes(x = CH4*1000000, y = depth, color = doy, shape = icecovered),size = 3) +
  geom_path(aes(x = CH4*1000000, y = depth, group = date, color = doy)) +
  facet_wrap(~lake) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "dissolved CH4 gas (umol/L)")+
  scale_colour_viridis_c(name = "Day of Year")+
  scale_shape_discrete(name = "Ice Covered")+
  theme_bw()+
  theme()

# Read in data 
tempdo = read_csv('data/ChemTempDO/tempdo.csv')
tempdo$date = as.Date(tempdo$Date, format =  "%m/%d/%y")
tempdo<- tempdo%>%
  mutate(icecovered = ifelse(month(date)%in% 1:4,"yes",
                             "no"))


#Facet_Wrap by Date for Temperature and DO for Trout Bog
Temp<-ggplot(dplyr::filter(tempdo, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = waterTemp, y = Depth, color = date, shape = icecovered), size = 1.5) +
  geom_path(aes(x = waterTemp, y = Depth, color = date, group = date))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Temperature (C)")+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

O2<-ggplot(dplyr::filter(tempdo, Bog == 'TB' | Bog == 'SSB')) + 
  geom_point(aes(x = DO, y = Depth, color = month(date), shape = icecovered), size = 1.5) +
  geom_path(aes(x = DO, y = Depth, color = month(date), group = date))+
  facet_wrap(~Bog) +
  scale_y_reverse(name = "Depth (m)") +
  scale_x_continuous(name = "Dissolved Oxygen (mg/L)")+
  scale_colour_viridis_c()+
  theme_bw()+
  theme(legend.position = "none")

figure<-(CO2 + CH4 + Temp + O2) + plot_layout(guides = "collect", ncol = 2)

ggsave("figure.png", width = 10, height = 6, units = 'in', figure)

# Heat map 
heatmap<- gd%>%
  mutate(sampledate = date)
# Normal scatter plot colorded by depth
ggplot(dplyr::filter(heatmap, lake == 'TB' | lake == 'SSB')) +
  geom_point(aes(x = date, y = depth, color = CH4*1000000))+
    facet_wrap(~lake)

# Vertical linear interpolation of water column concentrations 
interpData <- function(observationDF, date, maxdepth) {
  a = observationDF %>% filter(sampledate == date)
  if (sum(!is.na(a$ca)) == 0) {
    print('nothing')
    return(NULL)
  }
  
  b = a %>% filter(!is.na(ca))
  if (max(b$depth) < (maxdepth/2)) {
    print('too shallow')
    return(NULL)
  }
  
  yout = approx(x = a$depth, y = a$ca, xout = c(0:maxdepth), rule = 2)
  return(yout$y)
}

maxdepth = 18 # Should be depth of lowest sample, not necessarily depth of lake 
usedates = ntl2 %>%
  dplyr::distinct(sampledate) 

f <- lapply(X = usedates$sampledate, FUN = interpData, observationDF = ntl2,
            maxdepth = maxdepth)

f = as.data.frame(do.call(cbind, f))
names(f) = usedates$sampledate

# Bind list into dataframe
f2 = bind_cols(depth = 0:maxdepth,f) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))

# Heat map 
ggplot(f2) +
  guides(fill = guide_colorsteps(barheight = unit(4, "cm"))) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = var)) +
  geom_point(data = ntl2, aes(x = sampledate, y = depth), size = 0.25, color = 'white') +
  scale_y_reverse()  +
  scale_color_viridis_c(name = var) +
  ylab('depth') + xlab('2018') +
  xlim(as.Date(paste0(2018,'-01-01')), as.Date(paste0(2018,'-12-31'))) +
  theme_bw(base_size = 8) +
  scale_x_date(breaks = "4 month", minor_breaks = "1 month", labels=date_format("%b"),
               limits = c(as.Date(paste0(2018,'-01-01')), as.Date(paste0(2018,'-12-31'))))