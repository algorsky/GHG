library(tidyverse)
library(lubridate)
library(patchwork)

################## SOUTH SPARKLING BOG ##################
################## DO Sensor ##################
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

doHour.2021 = doHour.2021 %>%
  filter(as.Date(CT.Date) > as.Date('2020-12-10')) %>%
  filter(as.Date(CT.Date) < as.Date('2021-04-06')) 

doHour.2020 = doHour.2020 %>% 
  filter(as.Date(CT.Date) != as.Date('2020-03-02')) %>%
  filter(as.Date(CT.Date) > as.Date('2019-11-15')) %>%
  filter(as.Date(CT.Date) < as.Date('2020-05-19'))

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

p.do = ggplot(doHour.2020) + geom_line(aes(x = CT.Date, y = DO.mgL, color = '2020')) +
  geom_line(data = doHour.2021, aes(x = CT.Date, y = DO.mgL, color = '2021')) +
  theme_bw() +
  scale_color_manual(name = 'Year', values = c('lightblue4','gold')) +
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

