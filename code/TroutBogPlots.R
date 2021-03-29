#Heat Map CO2/CH4/Temperature
library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)
library(metR)
library(RColorBrewer)

## Load the data from Github
# Read in data 
heatmap = read_csv('data/GC2021/fullheat.csv')
heatmap$date = as.Date(heatmap$date, format =  "%m/%d/%y")

heatmapTB<-heatmap%>%
  filter(lake == "TB")%>%
  mutate(sampledate = date)%>%
  mutate(ch4unit = CH4*1000000)%>%
  mutate(co2unit = CO2*1000000)%>%
  ungroup()%>%
  select(sampledate, depth, ch4unit, co2unit)

#Function CH4
interpDataCH4 <- function(observationDF, date, maxdepth) {
  a = observationDF %>% filter(sampledate == date)
  if (sum(!is.na(a$ch4unit)) == 0) {
    print('nothing')
    return(NULL)
  }
  
  b = a %>% filter(!is.na(ch4unit))
  if (max(b$depth) < (maxdepth/2)) {
    print('too shallow')
    return(NULL)
  }
  
  yout = approx(x = a$depth, y = a$ch4unit, xout = c(0:maxdepth), rule = 2)
  return(yout$y)
}

#Function CO2
interpDataCO2 <- function(observationDF, date, maxdepth) {
  a = observationDF %>% filter(sampledate == date)
  if (sum(!is.na(a$co2unit)) == 0) {
    print('nothing')
    return(NULL)
  }
  
  b = a %>% filter(!is.na(co2unit))
  if (max(b$depth) < (maxdepth/2)) {
    print('too shallow')
    return(NULL)
  }
  
  yout = approx(x = a$depth, y = a$co2unit, xout = c(0:maxdepth), rule = 2)
  return(yout$y)
}

maxdepth = 7 # Should be depth of lowest sample, not necessarily depth of lake 
usedatesTB = heatmapTB %>%
  dplyr::distinct(sampledate)

fTBch4 <- lapply(X = usedatesTB$sampledate, FUN = interpDataCH4, observationDF = heatmapTB,
              maxdepth = maxdepth)
fTBch4 = as.data.frame(do.call(cbind, fTBch4))
names(fTBch4) = usedatesTB$sampledate

fTBco2 <- lapply(X = usedatesTB$sampledate, FUN = interpDataCO2, observationDF = heatmapTB,
                 maxdepth = maxdepth)
fTBco2 = as.data.frame(do.call(cbind, fTBco2))
names(fTBch4) = usedatesTB$sampledate
names(fTBco2) = usedatesTB$sampledate
# Bind list into dataframe
f2TBch4 = bind_cols(depth = 0:maxdepth,fTBch4) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))
f2TBco2 = bind_cols(depth = 0:maxdepth,fTBco2) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))

#Heat Map CH4
CH4<-ggplot(f2TBch4) +
  guides(fill = guide_colorsteps(barheight = unit(4, "cm"))) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = var)) +
  geom_point(data = heatmapTB, aes(x = sampledate, y = depth), size = 0.25, color = 'white') +
  scale_y_reverse()  +
  scale_color_viridis_c(name = var) +
  ylab('Depth (m)') + xlab('') +
  labs(fill = ((expression(paste("C", H[4], " (", mu,"mol ", L^-1,")")))))+
  theme_bw(base_size = 8) +
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b, %Y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2021,'-1-15'))))

#Heat Map CO2
CO2<-ggplot(f2TBco2) +
  guides(fill = guide_colorsteps(barheight = unit(4, "cm"))) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = var)) +
  geom_point(data = heatmapTB, aes(x = sampledate, y = depth), size = 0.25, color = 'white') +
  scale_y_reverse()  +
  scale_color_viridis_c(name = var) +
  ylab('Depth (m)') + xlab('') +
  labs(fill = ((expression(paste("C", O[2], " (", mu,"mol ", L^-1,")")))))+
  theme_bw(base_size = 8) +
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b, %Y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2021,'-1-15'))))

gas<- (CH4 + CO2) + plot_layout(guides = "collect", ncol = 1)
ggsave("figures/HeatMap_TB.png", width = 10, height = 6, units = 'in', gas)

#Heat Map Temperature
buoy.2020 = read_csv('data/Buoy/temp.buoy2020.csv')
buoy.2020$Date.GMT <- as.Date(buoy.2020$Date.GMT)
buoy.2020<- buoy.2020%>%
  mutate(sampledate = as.POSIXct(Date.GMT, format="%Y-%m-%dT%H:%M"))

interpDataTemp <- function(observationDF, date, maxdepth) {
  a = observationDF %>% filter(sampledate == date)
  if (sum(!is.na(a$Temp.C)) == 0) {
    print('nothing')
    return(NULL)
  }
  
  b = a %>% filter(!is.na(Temp.C))
  if (max(b$depth) < (maxdepth/2)) {
    print('too shallow')
    return(NULL)
  }
  
  yout = approx(x = a$depth, y = a$Temp.C, xout = c(0:maxdepth), rule = 2)
  return(yout$y)
}

maxdepth = 7 # Should be depth of lowest sample, not necessarily depth of lake 
usedates = buoy.2020 %>%
  dplyr::distinct(sampledate) 

f <- lapply(X = usedates$sampledate, FUN = interpData, observationDF = buoy.2020,
            maxdepth = maxdepth)

f = as.data.frame(do.call(cbind, f))
names(f) = usedates$sampledate

# Bind list into dataframe
f2 = bind_cols(depth = 0:maxdepth,f) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))

# Heat map 
theme_set(theme_bw())
tempheat<-ggplot(f2) +
  guides(fill = guide_colorsteps(barheight = unit(4, "cm")), name = "Temp (degC)") +
  geom_contour_filled(aes(x = sampledate, y = depth, z = var)) +
  scale_y_reverse()  +
  scale_color_viridis_c(name = var) +
  ylab('Depth (m)') + xlab('') +
  #xlim(as.Date(paste0(2018,'-01-01')), as.Date(paste0(2018,'-12-31'))) +
  theme_bw(base_size = 8)+
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b %y"),
               limits = c(as.Date(paste0(2019,'-12-15')), as.Date(paste0(2020,'-10-26'))))