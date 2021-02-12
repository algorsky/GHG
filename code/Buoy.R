library(tidyverse)
library(lubridate)
library(patchwork)


################## Temp/Light Sensor ##################
# 2020 Summer
light.list = list()
for(i in 1:7) {
  light.list[[i]] = read_csv(list.files(path='data/Buoy/2020/Temp', full.names = TRUE)[i], skip = 1) %>%
    # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
    mutate(depth = i)
}
light.summer.2020 = light.list %>%  bind_rows() %>%
  mutate(Temp.C = 5/9 * (`Temp, (*F)` - 32))
names(light.summer.2020) = c('Date.GMT','Temp.F','Intensity.lum.ft2','ButtonDown','ButtonUp','HostConnect','EOF','depth','Temp.C')

#QA/QC
light.summer.2020 = light.summer.2020 %>%
  filter(as.Date(Date.GMT) > as.Date('2020-05-22')) %>%
  filter(as.Date(Date.GMT) < as.Date('2020-10-25 ')) %>%
  arrange(Date.GMT, depth)

# 2020 Winter
light.list1 = list()
light.list2 = list()
for(i in 1:7) {
  light.list1[[i]] = read_csv(list.files(path='data/Buoy/Temp/2MAR2020' , full.names = TRUE)[i], skip = 1) %>%
    # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
    mutate(depth = i)
  
  light.list2[[i]] = read_csv(list.files(path='data/Buoy/Temp/19MAY2020' , full.names = TRUE)[i], skip = 1) %>%
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

#Combine two buoys
buoy.2020<-rbind(light.2020, light.summer.2020)

#Plotting Temperature
buoy.temp2020<-ggplot(buoy.2020) +
  geom_line(aes(x = Date.GMT, y = Temp.C, color = depth, group = depth)) +
  scale_colour_viridis_c() +
  theme_bw() +
  # facet_wrap(vars(depth)) +
  labs(title = 'Sparkling Bog 2020') + ylab('Temp (degC)') + xlab('Date') +
  NULL

ggsave("figures/buoytemp2020.png", width = 7, height = 3, units = 'in', buoy.temp2020)

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

