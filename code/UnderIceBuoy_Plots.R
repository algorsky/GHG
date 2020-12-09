library(tidyverse)
library(lubridate)
library(patchwork)

################## SOUTH SPARKLING BOG ##################
################## Chlorophyll Sensor ##################
# 2019
chl.2019 = read_csv('data/SB_2018-2019_underice_chla_import.csv')
names(chl.2019) = c('Time.s','UTC.Date','CT.Date', 'Bat.V','T.degC','Sensor.ppb','Gain')

chl1 = ggplot(chl.2019) + geom_line(aes(x = CT.Date, y = Sensor.ppb), col = 'lightblue4') +
  theme_bw() +
  labs(title = 'Sparkling Bog 2019') + ylab('Sensor (ppb)') + xlab('Date')

# 2020
chl.2020 <- list.files(path='CHLA/', full.names = TRUE) %>%
  lapply(read_csv, skip = 2) %>%
  bind_rows %>%
  mutate(CT.Date = as.POSIXct(`Time (sec)`, origin = '1970-01-01'))
names(chl.2020) = c('Time.s', 'Bat.V','T.degC','Sensor.ppb','Gain','CT.Date')

chl2 = ggplot(chl.2020) + geom_line(aes(x = CT.Date, y = Sensor.ppb), col = 'gold') +
  theme_bw() +
  labs(title = 'Sparkling Bog 2020') + ylab('Sensor (ppb)') + xlab('Date')

# Combo plot
chl1/chl2
ggsave('SparklingBogChl.png',width = 7, height = 5)

################## DO Sensor ##################
do.2019 = read_csv('SB_2018-2019_underice_DO_import.csv')
doHour.2019 = do.2019 %>% group_by(CT.Date = floor_date(`Central Standard Time`, "1 hour")) %>%
  summarize(DO.mgL = mean(`Dissolved Oxygen`), DO.Sat = mean(`Dissolved Oxygen Saturation`), T.degC = mean(Temperature))


do.2020 = list.files(path='DO/', full.names = TRUE) %>%
  lapply(read_csv, skip = 2) %>%
  bind_rows %>%
  mutate(CT.Date = as.POSIXct(`Time (sec)`, origin = '1970-01-01'))
names(do.2020) = c('Time.s', 'Bat.V','T.degC','DO.mgL','Gain','CT.Date')
doHour.2020 = do.2020 %>% group_by(CT.Date = floor_date(CT.Date, "1 hour")) %>%
  summarize(DO.mgL = mean(DO.mgL), T.degC = mean(T.degC))


p.do = ggplot(doHour.2019) + geom_line(aes(x = CT.Date, y = DO.mgL, color = '2019')) +
  geom_line(data = doHour.2020, aes(x = CT.Date - 365*24*60*60, y = DO.mgL, color = '2020')) +
  theme_bw() +
  scale_color_manual(name = 'Year', values = c('lightblue4','gold')) +
  labs(title = 'Sparkling Bog 2019-2020') + ylab('DO (mg/L)') + xlab('Date') +
  # ylim(c(0,1)) +
  NULL

p.temp = ggplot(doHour.2019) + geom_line(aes(x = CT.Date, y = T.degC, color = '2019')) +
  geom_line(data = doHour.2020, aes(x = CT.Date - 365*24*60*60, y = T.degC, color = '2020')) +
  theme_bw() +
  scale_color_manual(name = 'Year',values = c('lightblue4','gold')) +
  labs(title = 'Sparkling Bog 2019-2020') + ylab('Temp (degC)') + xlab('Date') +
  ylim(c(0,10))

p.do/p.temp
ggsave('SparklingBogTemp.png',width = 7, height = 5)


################## Temp/Light Sensor ##################
# 2019
light.list = list()
for(i in 1:7) {
  light.list[[i]] = read_csv(list.files(path='2018_2019_underice/Temp/', full.names = TRUE)[i], skip = 1) %>%
  # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
  mutate(depth = i)
}
light.2019 = light.list %>%  bind_rows() %>%
  mutate(Temp.C = 5/9 * (`Temp, (*F)` - 32))
names(light.2019) = c('Date.GMT','Temp.F','Intensity.lum.ft2','ButtonDown','ButtonUp','HostConnect','EOF','depth','Temp.C')

#QA/QC
light.2019 = light.2019 %>%
  filter(as.Date(Date.GMT) > as.Date('2018-12-10')) %>%
  filter(as.Date(Date.GMT) < as.Date('2019-06-06 ')) %>%
  arrange(Date.GMT, depth)

# 2020
light.list1 = list()
light.list2 = list()
for(i in 1:7) {
  light.list1[[i]] = read_csv(list.files(path='2019_2020_underice/Temp/2MAR2020/' , full.names = TRUE)[i], skip = 1) %>%
    # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
    mutate(depth = i)

  light.list2[[i]] = read_csv(list.files(path='2019_2020_underice/Temp/19MAY2020/' , full.names = TRUE)[i], skip = 1) %>%
    # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
    mutate(depth = i)
}
light.2020 = light.list1 %>%  bind_rows() %>%
  bind_rows(light.list2 %>%  bind_rows()) %>%
  mutate(Temp.C = 5/9 * (`Temp, (*F)` - 32))
names(light.2020) = c('Date.GMT','Temp.F','Intensity.lum.ft2','ButtonDown','ButtonUp','HostConnect','EOF','depth','Temp.C')

#QA/QC
light.2020 = light.2020 %>% filter(as.Date(Date.GMT) != as.Date('2020-03-02')) %>%
  filter(as.Date(Date.GMT) > as.Date('2019-11-15')) %>%
  filter(as.Date(Date.GMT) < as.Date('2020-05-19')) %>%
  arrange(Date.GMT, depth)

#Plotting Temperature
p.t1 = ggplot(light.2019) +
  geom_line(aes(x = Date.GMT, y = Temp.C, color = depth, group = depth)) +
  scale_colour_viridis_c() +
  theme_bw() +
  # facet_wrap(vars(depth)) +
  labs(title = 'Sparkling Bog 2019') + ylab('Temp (degC)') + xlab('Date') +
  NULL

p.t2 = ggplot(light.2020) +
  geom_line(aes(x = Date.GMT, y = Temp.C, color = depth, group = depth)) +
  scale_colour_viridis_c() +
  theme_bw() +
  # facet_wrap(vars(depth)) +
  labs(title = 'Sparkling Bog 2020') + ylab('Temp (degC)') + xlab('Date') +
  NULL

#Combo Plot
p.t1/p.t2 & ylim(c(0,15))
ggsave('SparklingBogTempString.png',width = 7, height = 5)

#Surface Plot
ggplot(filter(light.2019, depth == 1)) + geom_line(aes(x = Date.GMT, y = Temp.C, color = '2019')) +
  geom_line(data = filter(light.2020, depth == 1), aes(x = Date.GMT - 365*24*60*60, y = Temp.C, color = '2020')) +
  theme_bw() +
  scale_color_manual(name = 'Year',values = c('lightblue4','gold')) +
  labs(title = 'Sparkling Bog 2019-2020') + ylab('Temp (degC)') + xlab('Date') +
  ylim(c(0,10))
ggsave('SparklingBogTempString_Surf.png',width = 7, height = 3)


#Plotting Light
p.l1 = ggplot(light.2019) +
  geom_line(aes(x = Date.GMT, y = Intensity.lum.ft2, color = depth, group = depth)) +
  scale_colour_viridis_c() +
  theme_bw() +
  # facet_wrap(vars(depth)) +
  labs(title = 'Sparkling Bog 2019') + ylab('Light(lum/ft2)') + xlab('Date') +
  NULL

p.l2 = ggplot(light.2020) +
  geom_line(aes(x = Date.GMT, y = Intensity.lum.ft2, color = depth, group = depth)) +
  scale_colour_viridis_c() +
  theme_bw() +
  # facet_wrap(vars(depth)) +
  labs(title = 'Sparkling Bog 2020') + ylab('Light(lum/ft2)') + xlab('Date') +
  NULL

#Combo Plot
p.l1/p.l2
ggsave('SparklingBogLightString.png',width = 7, height = 5) & ylim(0,420)

#Surface Plot
ggplot(filter(light.2019, depth == 1)) +
  geom_line(aes(x = Date.GMT, y = Intensity.lum.ft2, color = '2019'), alpha = 0.7) +
  geom_line(data = filter(light.2020, depth == 1), aes(x = Date.GMT - 365*24*60*60, y = Intensity.lum.ft2, color = '2020'), alpha = 0.7) +
  theme_bw() +
  scale_color_manual(name = 'Year',values = c('lightblue4','gold')) +
  labs(title = 'Sparkling Bog 2019-2020') + ylab('Light(lum/ft2)') + xlab('Date') +
  ylim(0,50)

ggsave('SparklingBogLightString_Surf.png',width = 7, height = 3)

