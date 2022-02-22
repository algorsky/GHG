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

#Surface Light Plot
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
