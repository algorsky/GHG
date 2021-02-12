library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)


################## Temp/Light Sensor ##################
buoy.2020 = read_csv('data/Buoy/temp.buoy2020.csv')
buoy.2020<- buoy.2020%>%
  mutate(sampledate = as.Date(Date.GMT))

#Plotting Temperature
buoy.temp2020<-ggplot(buoy.2020) +
  geom_line(aes(x = Date.GMT, y = Temp.C, color = depth, group = depth)) +
  scale_colour_viridis_c() +
  theme_bw() +
  # facet_wrap(vars(depth)) +
  labs(title = 'Sparkling Bog 2020') + ylab('Temp (degC)') + xlab('Date') +
  NULL
ggsave("figures/buoytemp2020.png", width = 7, height = 3, units = 'in', buoy.temp2020)

#Plotting Light
buoy.light2020 = ggplot(buoy.2020) +
  geom_line(aes(x = Date.GMT, y = Intensity.lum.ft2, color = depth, group = depth)) +
  scale_colour_viridis_c() +
  theme_bw() +
  # facet_wrap(vars(depth)) +
  labs(title = 'Sparkling Bog 2020') + ylab('Light(lum/ft2)') + xlab('Date') +
  NULL

#Surface Plot
ggplot(filter(buoy.2020, depth == 1)) +
  geom_line(aes(x = Date.GMT, y = Intensity.lum.ft2, color = depth), alpha = 0.7) +
  theme_bw() +
  labs(title = 'Sparkling Bog 2019-2020') + ylab('Light(lum/ft2)') + xlab('Date')
#ggsave('SparklingBogLightString_Surf.png',width = 7, height = 3)

#Heat Map
interpData <- function(observationDF, date, maxdepth) {
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

