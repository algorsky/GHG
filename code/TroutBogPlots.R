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
  select(sampledate, depth, ch4unit, co2unit)%>%
  filter(sampledate <= as.POSIXct('2021-01-01'))

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
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2020,'-10-31'))))

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
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2020,'-10-31'))))

gas<- (CH4 + CO2) + plot_layout(ncol = 1)
ggsave("figures/HeatMap_TB.png", width = 10, height = 6, units = 'in', gas)

#Heat Map Temperature
tempdo = read_csv('data/ChemTempDO/TBtempdo.csv')
tempdo$date = as.Date(tempdo$date, format =  "%m/%d/%y")
tempdoTB <- tempdo%>%
  mutate(sampledate = date)%>%
  select(sampledate, depth, waterTemp)

tempAnnual <- tempdoTB %>%
  filter(sampledate <= as.POSIXct('2021-01-01'))

interpDataTemp <- function(observationDF, date, maxdepth) {
  a = observationDF %>% filter(sampledate == date)
  if (sum(!is.na(a$waterTemp)) == 0) {
    print('nothing')
    return(NULL)
  }
  
  b = a %>% filter(!is.na(waterTemp))
  if (max(b$depth) < (maxdepth/2)) {
    print('too shallow')
    return(NULL)
  }
  
  yout = approx(x = a$depth, y = a$waterTemp, xout = c(0:maxdepth), rule = 2)
  return(yout$y)
}

maxdepth = 7 # Should be depth of lowest sample, not necessarily depth of lake 
usedatesTemp = tempAnnual %>%
  dplyr::distinct(sampledate)

fTBTemp <- lapply(X = usedatesTemp$sampledate, FUN = interpDataTemp, observationDF = tempAnnual,
                 maxdepth = maxdepth)
fTBTemp = as.data.frame(do.call(cbind, fTBTemp))
names(fTBTemp) = usedatesTemp$sampledate

# Bind list into dataframe
f2TBTemp = bind_cols(depth = 0:maxdepth,fTBTemp) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))

# Heat map 
temp<-ggplot(f2TBTemp) +
  guides(fill = guide_colorsteps(barheight = unit(4, "cm"))) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = var)) +
  geom_point(data = tempAnnual, aes(x = sampledate, y = depth), size = 0.25, color = 'white') +
  scale_y_reverse()  +
  scale_color_viridis_c(name = var) +
  ylab('Depth (m)') + xlab('') +
  labs(fill = ((expression("Temperature " ( degree*C)))))+
  theme_bw(base_size = 8) +
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b, %Y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2020,'-10-31'))))

#DO
DO_TB <- tempdo%>%
  mutate(sampledate = date)%>%
  select(sampledate, depth, DO)%>%
  filter(!is.na(DO))

DO_TB_depths <- tempdo%>%
  mutate(sampledate = date)%>%
  select(sampledate, depth, DO)%>%
  filter(!is.na(DO))%>%
  filter(depth != 0)

DOAnnual <- DO_TB %>%
  filter(sampledate <= as.POSIXct('2021-01-01'))

interpDataDO <- function(observationDF, date, maxdepth) {
  a = observationDF %>% filter(sampledate == date)
  if (sum(!is.na(a$DO)) == 0) {
    print('nothing')
    return(NULL)
  }
  
  b = a %>% filter(!is.na(DO))
  if (max(b$depth) < (maxdepth/2)) {
    print('too shallow')
    return(NULL)
  }
  
  yout = approx(x = a$depth, y = a$DO, xout = c(0:maxdepth), rule = 2)
  return(yout$y)
}

maxdepth = 7 # Should be depth of lowest sample, not necessarily depth of lake 
usedatesDO = DOAnnual %>%
  dplyr::distinct(sampledate)

fTBDO <- lapply(X = usedatesDO$sampledate, FUN = interpDataDO, observationDF = DOAnnual,
                  maxdepth = maxdepth)
fTBDO = as.data.frame(do.call(cbind, fTBDO))
names(fTBDO) = usedatesDO$sampledate

# Bind list into dataframe
f2TBDO = bind_cols(depth = 0:maxdepth,fTBDO) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))

# Heat map 
O2<-ggplot(f2TBDO) +
  guides(fill = guide_colorsteps(barheight = unit(4, "cm"))) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = var)) +
  geom_point(data = DOAnnual, aes(x = sampledate, y = depth), size = 0.25, color = 'white') +
  scale_y_reverse()  +
  scale_color_viridis_c(name = var) +
  ylab('Depth (m)') + xlab('') +
  labs(fill = ((expression(paste(O[2], " (mg " , L^-1,")")))))+
  theme_bw(base_size = 8) +
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b, %Y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2020,'-10-31'))))

heatmaps<- (temp+ O2+ CH4 + CO2) + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")
ggsave("figures/Full_HeatMap_TB.png", width = 7, height = 8, units = 'in', heatmaps)
