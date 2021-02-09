library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

## Load the data from Github
# Read in data 
heat = read_csv('data/heatmap.csv')
heat$date = as.Date(heat$date, format =  "%m/%d/%y")
heatmap = read_csv('data/GC2021/fullheat.csv')
heatmap$date = as.Date(heatmap$date, format =  "%m/%d/%y")

heatmapSSB<- heat%>%
  filter(lake == "SSB")%>%
  mutate(sampledate = date)%>%
  mutate(ch4unit = CH4*1000000)%>%
  ungroup()%>%
  select(sampledate, depth, ch4unit)

heatmapTB<-heatmap%>%
  filter(lake == "TB")%>%
  mutate(sampledate = date)%>%
  mutate(ch4unit = CH4*1000000)%>%
  ungroup()%>%
  select(sampledate, depth, ch4unit)

#Function
# Vertical linear interpolation of water column concentrations 
interpData <- function(observationDF, date, maxdepth) {
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

maxdepth = 7 # Should be depth of lowest sample, not necessarily depth of lake 
usedatesSSB = heatmapSSB %>%
  dplyr::distinct(sampledate) 
usedatesTB = heatmapTB %>%
  dplyr::distinct(sampledate)

fSSB <- lapply(X = usedatesSSB$sampledate, FUN = interpData, observationDF = heatmapSSB,
            maxdepth = maxdepth)
fSSB = as.data.frame(do.call(cbind, fSSB))
names(fSSB) = usedatesSSB$sampledate

fTB <- lapply(X = usedatesTB$sampledate, FUN = interpData, observationDF = heatmapTB,
               maxdepth = maxdepth)
fTB = as.data.frame(do.call(cbind, fTB))
names(fTB) = usedatesTB$sampledate

# Bind list into dataframe
f2SSB = bind_cols(depth = 0:maxdepth,fSSB) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))
f2TB = bind_cols(depth = 0:maxdepth,fTB) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))

# Heat map 
CH4SSB<-ggplot(f2SSB) +
  guides(fill = guide_colorsteps(barheight = unit(4, "cm"))) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = var)) +
  geom_point(data = heatmapSSB, aes(x = sampledate, y = depth), size = 0.25, color = 'white') +
  scale_y_reverse()  +
  scale_color_viridis_c(name = var) +
  ylab('depth') + xlab('') +
  labs(fill = ("CH4(umol/L)"))+
  theme_bw(base_size = 8) +
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b, %Y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2021,'-1-15'))))
# Heat map 
CH4TB<-ggplot(f2TB) +
  guides(fill = guide_colorsteps(barheight = unit(4, "cm"))) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = var)) +
  geom_point(data = heatmapTB, aes(x = sampledate, y = depth), size = 0.25, color = 'white') +
  scale_y_reverse()  +
  scale_color_viridis_c(name = var) +
  ylab('depth') + xlab('') +
  labs(fill = ("CH4(umol/L)"))+
  theme_bw(base_size = 8) +
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b, %Y"),
               limits = c(as.Date(paste0(2020,'-01-01')), as.Date(paste0(2021,'-1-15'))))

