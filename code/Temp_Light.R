library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

################## Temp/Light Sensor ##################
# 2019
light.list = list()
for(i in 1:7) {
  light.list[[i]] = read_csv(list.files(path='data/Buoy/Temp 2/', full.names = TRUE)[i], skip = 1) %>%
    # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
    mutate(depth = i)
}
light.2019 = light.list %>%  bind_rows() %>%
  mutate(Temp.C = 5/9 * (`Temp, (*F)` - 32))
names(light.2019) = c('Date.GMT','Temp.F','Intensity.lum.ft2','ButtonDown','ButtonUp','HostConnect','EOF','depth','Temp.C')

#QA/QC
light.2019 = light.2019 %>%
  filter(as.Date(Date.GMT) > as.Date('2018-12-10')) %>%
  filter(as.Date(Date.GMT) < as.Date('2019-05-15')) %>%
  arrange(Date.GMT, depth)%>%
  mutate(metric = Intensity.lum.ft2 * 10.7639104)

# 2021
light.list = list()
for(i in 1:7) {
  light.list[[i]] = read_csv(list.files(path='data/Buoy/2021/Temp/', full.names = TRUE)[i], skip = 1) %>%
    # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
    mutate(depth = i)
}
light.2021 = light.list %>%  bind_rows() 
names(light.2021) = c('Date.GMT','Temp.C','Intensity.lum.ft2','ButtonDown','ButtonUp','HostConnect','EOF','depth')

#QA/QC
light.2021 = light.2021 %>%
  filter(as.Date(Date.GMT) > as.Date('2020-12-10')) %>%
  filter(as.Date(Date.GMT) < as.Date('2021-04-12')) %>%
  arrange(Date.GMT, depth)%>%
  mutate(metric = Intensity.lum.ft2 * 10.7639104)%>%
  mutate(Temp.F = Temp.C*1.8 + 32)

# 2020
light.list1 = list()
light.list2 = list()
for(i in 1:7) {
  light.list1[[i]] = read_csv(list.files(path='data/Buoy/Temp/2MAR2020/' , full.names = TRUE)[i], skip = 1) %>%
    # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
    mutate(depth = i)
  
  light.list2[[i]] = read_csv(list.files(path='data/Buoy/Temp/19MAY2020/' , full.names = TRUE)[i], skip = 1) %>%
    # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
    mutate(depth = i)
}
light.2020 = light.list1 %>%  bind_rows() %>%
  bind_rows(light.list2 %>%  bind_rows()) %>%
  mutate(Temp.C = 5/9 * (`Temp, (*F)` - 32))
names(light.2020) = c('Date.GMT','Temp.F','Intensity.lum.ft2','ButtonDown','ButtonUp','HostConnect','EOF','depth','Temp.C')

#write.csv(light.2020, "data/tempSSBbuoy.csv", row.names = FALSE)

#QA/QC
light.2020 = light.2020 %>% filter(as.Date(Date.GMT) != as.Date('2020-03-02')) %>%
  filter(as.Date(Date.GMT) > as.Date('2019-11-15')) %>%
  filter(as.Date(Date.GMT) < as.Date('2020-05-10')) %>%
  arrange(Date.GMT, depth)%>%
  mutate(metric = Intensity.lum.ft2 * 10.7639104)

lighttemp19<- light.2019 %>%
  select(Date.GMT, Temp.C, depth, Intensity.lum.ft2)%>%
  mutate(doy = yday(as.Date(Date.GMT)))%>%
  mutate(metric = Intensity.lum.ft2 * 10.7639104)

max(lighttemp19$Intensity.lum.ft2) 
lighttemp20<- light.2020%>%
  select(Date.GMT, Temp.C, depth, Intensity.lum.ft2)%>%
  mutate(doy = yday(as.Date(Date.GMT)))

max(lighttemp20$Intensity.lum.ft2) 
lighttemp21<- light.2021 %>%
  select(Date.GMT, Temp.C, depth, Intensity.lum.ft2)%>%
  mutate(doy = yday(as.Date(Date.GMT)))
max(lighttemp21$Intensity.lum.ft2) 

light <- rbind(lighttemp19, lighttemp20, lighttemp21)%>%
  mutate(year = year(as.Date(Date.GMT)))%>%
  filter(doy > 0 & doy < 130) %>%
  filter(depth == 1)%>%
  mutate(metric = Intensity.lum.ft2 * 10.7639104)%>%
  mutate(date = `year<-`(Date.GMT, 2014))

ggplot(light)+
  geom_line(aes(x = date, y =  metric, group = year(Date.GMT), color = factor(year(Date.GMT)))) +
  scale_x_datetime(labels = date_format("%b-%d"))+
  ylab(expression("Surface Temperature " (degree *C)))+
  geom_vline(xintercept = as.POSIXct("2014-04-13 00:15:00"),  linetype = "dashed", color = "gray")+
  geom_vline(xintercept = as.POSIXct("2014-04-26 00:15:00"),  linetype = "dashed", color = "lightblue4")+
  geom_vline(xintercept = as.POSIXct("2014-04-06 00:15:00"),  linetype = "dashed", color = "gold")+
  scale_color_manual(breaks = c("2019", "2020", "2021"),values = c("gray", "lightblue4", "gold"))+
  xlab("Date")+
  theme_bw()+
  theme(legend.title = element_blank())

ggsave('figures/surfacewatertemp.png',width = 7, height = 5)

library(scales)
surfacetemp<-light %>%
  ggplot() +
  geom_line(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = Temp.C, color = factor(year))) +
  scale_color_manual(name = 'Year',values = c('gray','lightblue4','gold')) +
  scale_x_date(labels = date_format("%b"))+
  ylab('Temp (degC)') + xlab('Date') +
  geom_vline(xintercept = as.numeric(as.Date("2019-04-13")),  linetype = "dashed", color = "gray")+
  geom_vline(xintercept = as.numeric(as.Date("2019-04-06")),  linetype = "dashed", color = "gold")+
  geom_vline(xintercept = as.numeric(as.Date("2019-04-26")),  linetype = "dashed", color = "lightblue4")+
  theme_bw(base_size = 8)

ggsave('figures/surfacewatertemp.png',width = 7, height = 5)



#Summer
# 2021
light.list = list()
for(i in 1:7) {
  light.list[[i]] = read_csv(list.files(path='data/Buoy/summer/', full.names = TRUE)[i], skip = 1) %>%
    # read_csv('2018_2019_underice/Temp/SB_1.csv', skip = 1) %>%
    mutate(depth = i)%>%
    mutate(Temp.C = 5/9 * (`Temp, (*F)` - 32))
}
summer = light.list %>%  bind_rows() 
names(summer) = c('Date.GMT','Temp.F','Intensity.lum.ft2','HostConnect', 'ButtonDown','ButtonUp','EOF','depth', 'Temp.C')

autumnturnover<- summer %>%
  filter(Date.GMT > '2020-09-30' & Date.GMT < '2020-10-26')
ggplot(autumnturnover) +
  geom_line(aes(x = Date.GMT, y = Temp.C, color = depth, group = depth)) +
  scale_colour_viridis_c() +
  theme_bw() +
  # facet_wrap(vars(depth)) +
  labs(title = 'Sparkling Bog 2020') + ylab('Temp (degC)') + xlab('Date') +
  NULL
write.csv(summer, "data/summertempSSBbuoy.csv", row.names = FALSE)


#Plotting Temperature
p.t1 = ggplot(light.2020) +
  geom_line(aes(x = Date.GMT, y = Temp.C, color = depth, group = depth)) +
  scale_colour_viridis_c(name = "Depth (m)") +
  scale_x_datetime(labels = date_format("%b-%d"), breaks = "1 month", limits = as.POSIXct(c('2019-12-01 00:00:05','2020-05-01 00:00:05')))+
  theme_bw(base_size = 8) +
  geom_vline(xintercept = as.POSIXct(("2020-04-26 00:00:05")),  linetype = "dashed", color = "black")+
  # facet_wrap(vars(depth)) +
  labs(title = 'South Sparkling Bog 2020') + 
  ylab(expression("Temperature " ( degree*C)))+ 
  xlab('Date') +
  NULL

p.t2 = ggplot(light.2021) +
  geom_line(aes(x = Date.GMT, y = Temp.C, color = depth, group = depth)) +
  scale_colour_viridis_c(name = "Depth (m)") +
  scale_x_datetime(labels = date_format("%b-%d"), breaks = "1 month", limits = as.POSIXct(c('2020-12-01 00:00:05','2021-05-01 00:00:05')))+
  ylab(expression("Temperature " ( degree*C)))+
  theme_bw(base_size = 8) +
  geom_vline(xintercept = as.POSIXct(("2021-04-06 00:00:05")),  linetype = "dashed", color = "black")+
  # facet_wrap(vars(depth)) +
  labs(title = 'South Sparkling Bog 2021') + xlab('Date') +
  NULL

p.t3 = ggplot(light.2019) +
  geom_line(aes(x = Date.GMT, y = Temp.C, color = depth, group = depth)) +
  scale_colour_viridis_c(name = "Depth (m)") +
  scale_x_datetime(labels = date_format("%b-%d"), breaks = "1 month", limits = as.POSIXct(c('2018-12-01 00:00:05','2019-05-01 00:00:05')))+
  theme_bw(base_size = 8) +
  geom_vline(xintercept = as.POSIXct(("2019-04-13 00:00:05")),  linetype = "dashed", color = "black")+
  # facet_wrap(vars(depth)) +
  labs(title = 'South Sparkling Bog 2019') + 
  ylab(expression("Temperature " ( degree*C)))+ 
  xlab('Date') +
  NULL

#Combo Plot
p.t3/p.t1/p.t2 & ylim(c(0,15)) 
ggsave('figures/SparklingBogTempString.png',width = 7, height = 5)


#Surface Plot
ggplot(filter(light.2021, depth == 1)) + geom_line(aes(x = Date.GMT, y = Temp.C, color = '2021')) +
  geom_line(data = filter(light.2020, depth == 1), aes(x = Date.GMT, y = Temp.C, color = '2020')) +
  geom_line(data = filter(light.2019, depth == 1), aes(x = Date.GMT, y = Temp.C, color = '2019')) +
  theme_bw() +
  scale_x_datetime(labels = date_format("%b-%d"))+
  scale_color_manual(name = 'Year',values = c('gray','lightblue4','gold')) +
  labs(title = 'Sparkling Bog Surface Temperature') + ylab('Temp (degC)') + xlab('Date') +
  ylim(c(0,10))
ggsave('figures/SparklingBogTempString_Surf.png',width = 7, height = 3)


#Plotting Light
p.l1 = ggplot(dplyr::filter(light.2020, depth != 1)) +
  geom_line(aes(x = as.Date(Date.GMT), y = metric, color = depth, group = depth)) +
  scale_colour_viridis_c(name = "Depth (m)") +
  theme_bw(base_size = 8) +
  scale_x_date(labels = date_format("%b"), breaks = "1 month", limits = as.Date(c('2019-12-01','2020-05-01')))+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "black")+
  # facet_wrap(vars(depth)) +
  ylim(0, 1500)+
  ylab(expression(paste("Light (lum ", m^-2,")")))+
  labs(title = 'South Sparkling Bog 2020') + 
  xlab('Date') +
  NULL

p.l2 = ggplot(dplyr::filter(light.2021, depth != 1)) +
  geom_line(aes(x = as.Date(Date.GMT), y = metric, color = depth, group = depth)) +
  scale_colour_viridis_c(name = "Depth (m)") +
  scale_x_date(labels = date_format("%b"), breaks = "1 month", limits = as.Date(c('2020-12-01','2021-05-01')))+
  theme_bw(base_size = 8) +
  geom_vline(xintercept = as.numeric(as.Date("2021-04-06")),  linetype = "dashed", color = "black")+
  ylim(0, 1500)+
  ylab(expression(paste("Light (lum ", m^-2,")")))+
  labs(title = 'South Sparkling Bog 2021') + 
  xlab('Date') +
  NULL

p.l3 = ggplot(dplyr::filter(light.2019, depth != 1)) +
  geom_line(aes(x = as.Date(Date.GMT), y = metric, color = depth, group = depth)) +
  scale_colour_viridis_c(name = "Depth (m)") +
  theme_bw(base_size = 8) +
  scale_x_date(labels = date_format("%b"), breaks = "1 month", limits = as.Date(c('2018-12-01','2019-05-01')))+
  geom_vline(xintercept = as.numeric(as.Date("2019-04-13")),  linetype = "dashed", color = "black")+
  ylim(0,1500)+
  ylab(expression(paste("Light (lum ", m^-2,")")))+
  labs(title = 'South Sparkling Bog 2019') + 
  xlab('Date') +
  NULL


#Combo Plot
p.l3/p.l1/p.l2
ggsave('figures/SSBNoSurfLightString.png',width = 7, height = 5)
library(scales)
#Surface Plot
surfaceDO<-light %>%
  ggplot() +
  geom_line(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = metric, color = factor(year))) +
  scale_x_date(labels = date_format("%b"), breaks = "1 month")+
  scale_color_manual(name = 'Year', values = c('gray','lightblue4','gold')) +
  theme_bw(base_size = 8) +
  ylab('Light') + xlab('Date')
ggsave("figures/surfacebuoyDO.png", width = 15, height = 10, units = 'in', surfaceDO)

ggplot(filter(light.2020, depth == 1)) +
  geom_line(aes(x = as.Date(Date.GMT), y = metric, color = '2020'), alpha = 0.7) +
  geom_line(data = filter(light.2021, depth == 1), aes(x = as.Date(Date.GMT), y = metric, color = '2021'), alpha = 0.7) +
  geom_line(data = filter(light.2019, depth == 1), aes(x = as.Date(Date.GMT), y = metric, color = '2019'), alpha = 0.7) +
  theme_bw() +
  scale_color_manual(name = 'Year',values = c('gray','lightblue4','gold')) +
  ylab(expression(paste("Surface Light Intensity (lum ", m^-2,")")))+
 xlab('Date')+
  geom_vline(xintercept = as.numeric(as.Date("2019-04-13")),  linetype = "dashed", color = "gray")+
  geom_vline(xintercept = as.numeric(as.Date("2021-04-06")),  linetype = "dashed", color = "gold")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "lightblue4")

ggsave('figures/SparklingBogLightString_Surf.png',width = 7, height = 4)

