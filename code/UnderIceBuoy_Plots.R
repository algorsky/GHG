library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

################## SOUTH SPARKLING BOG ##################
################## DO Sensor ##################
do.2019 = read_csv('data/Buoy/DO/SB_2018-2019_underice_DO_import.csv')
doHour.2019 = do.2019 %>% group_by(CT.Date = floor_date(`Central Standard Time`, "1 hour")) %>%
  summarize(DO.mgL = mean(`Dissolved Oxygen`), DO.Sat = mean(`Dissolved Oxygen Saturation`), T.degC = mean(Temperature))

do.2020 = list.files(path='data/Buoy/DO/', full.names = TRUE) %>%
  lapply(read_csv, skip = 2) %>%
  bind_rows %>%
  mutate(CT.Date = as.POSIXct(`Time (sec)`, origin = '1970-01-01'))
do.2020<- do.2020[,c(1:5, 22)]
names(do.2020) = c('Time.s', 'Bat.V','T.degC','DO.mgL','Gain','CT.Date')
doHour.2020 = do.2020 %>% group_by(CT.Date = floor_date(CT.Date, "1 hour")) %>%
  summarize(DO.mgL = mean(DO.mgL), T.degC = mean(T.degC))

do.2021 = list.files(path='data/Buoy/2021/DO/', full.names = TRUE) %>%
  lapply(read_csv, skip = 2) %>%
  bind_rows %>%
  mutate(CT.Date = as.POSIXct(`Time (sec)`, origin = '1970-01-01'))
do.2021<- do.2021[,c(1:4, 7)]
names(do.2021) = c('Time.s', 'Bat.V','T.degC','DO.mgL','CT.Date')
doHour.2021 = do.2021 %>% group_by(CT.Date = floor_date(CT.Date, "1 hour")) %>%
  summarize(DO.mgL = mean(DO.mgL), T.degC = mean(T.degC))


doHour.2019 = doHour.2019 %>%
  filter(as.Date(CT.Date) > as.Date('2018-12-09')) %>%
  filter(as.Date(CT.Date) < as.Date('2019-04-01')) %>%
  mutate(doy = yday(as.Date(CT.Date)))%>%
  select(CT.Date, DO.mgL, T.degC, doy)


doHour.2021 = doHour.2021 %>%
  filter(as.Date(CT.Date) > as.Date('2020-12-10')) %>%
  filter(as.Date(CT.Date) < as.Date('2021-04-01')) %>%
  mutate(doy = yday(as.Date(CT.Date)))

doHour.2020 = doHour.2020 %>% 
  filter(as.Date(CT.Date) != as.Date('2020-03-02')) %>%
  filter(as.Date(CT.Date) > as.Date('2019-11-15')) %>%
  filter(as.Date(CT.Date) < as.Date('2020-04-01'))%>%
  mutate(doy = yday(as.Date(CT.Date)))

doHour<- rbind(doHour.2019, doHour.2020, doHour.2021)%>%
  mutate(year = year(as.Date(CT.Date)))%>%
  filter(doy > 0 & doy < 60)

surfaceDO<-doHour %>%
  ggplot() +
  geom_line(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = DO.mgL, color = factor(year))) +
  scale_x_date(labels = date_format("%b"), breaks = "1 month")+
  scale_color_manual(name = 'Year', values = c('gray','lightblue4','gold')) +
  theme_bw(base_size = 8) +
  labs(title = 'Sparkling Bog 2019-2021') + ylab('Surface DO (mg/L)') + xlab('Date')
ggsave("figures/surfacebuoyDO.png", width = 15, height = 10, units = 'in', surfaceDO)

p.do1 = ggplot(doHour.2020) +
  geom_line(aes(x = CT.Date, y = DO.mgL, color = '2020')) +
  theme_bw() +
  theme(legend.position = "none")+
  geom_vline(xintercept = as.Date('2020-04-26'), linetype = "dotted", color = "blue")+
  labs(title = 'Sparkling Bog 2020') + ylab('DO (mg/L)') + xlab('Date') +
  NULL

p.do2 = ggplot(doHour.2021) +
  geom_line(aes(x = CT.Date, y = DO.mgL, color = '2021')) +
  theme_bw() +
  theme(legend.position = "none")+
  # facet_wrap(vars(depth)) +
  labs(title = 'Sparkling Bog 2021') + ylab('DO (mg/L)') + xlab('Date') +
  NULL

p.do = ggplot(doHour.2020) + geom_line(aes(x = as.Date(doy, origin = as.Date('2019-01-01'))), y = DO.mgL, color = '2020') +
  geom_line(data = doHour.2021, aes(x = as.Date(doy, origin = as.Date('2019-01-01'))), y = DO.mgL, color = '2021') +
  geom_line(data = doHour.2019, aes(x = as.Date(doy, origin = as.Date('2019-01-01'))), y = DO.mgL, color = '2019') +
  theme_bw() +
  scale_color_manual(name = 'Year', values = c('gray','lightblue4','gold')) +
  scale_x_date(labels = date_format("%b"))+
  labs(title = 'Sparkling Bog 2019-2021') + ylab('DO (mg/L)') + xlab('Date') +
  # ylim(c(0,1)) +
  NULL

p.temp = ggplot(doHour.2020) + geom_line(aes(x = CT.Date, y = T.degC, color = '2020')) +
  geom_line(data = doHour.2021, aes(x = CT.Date, y = T.degC, color = '2021')) +
  theme_bw() +
  scale_color_manual(name = 'Year',values = c('lightblue4','gold')) +
  labs(title = 'Sparkling Bog 2019-2021') + ylab('Temp (degC)') + xlab('Date') +
  ylim(c(0,10))

p.do1/p.do2
ggsave('figures/SparklingBogDOSurf.png',width = 7, height = 5)

