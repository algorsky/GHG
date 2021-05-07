library(tidyverse)
library(lubridate)
library(scales)

# Read in data 
dat = read_csv('data/EDI/SSB_GHG.csv')
dat$depth <- dat$water_depth_m
dat$CH4<-dat$CH4_micromolL
dat$CO2<-dat$CO2_micromolL
dat$sampledate = as.Date(dat$sampledate, format =  "%m/%d/%y")
bath = read_csv('data/tbssbbath.csv')

bathSSB<- bath%>%
  filter(lake == "SSB")%>%
  mutate(lakevolume = sum(volume))

#Join datasets
bathjoin <- left_join(dat, bath, by = c("depth" = "depth"))

#Calculate Hypsometrically Weighted gas concentrations
volTB<- bathjoin%>%
  mutate(doy = yday(sampledate))%>%
  filter(lake.x == "TB" & lake.y =="TB")%>%
  mutate(multipliedCH4Vol = volume * (CH4)) %>%
  mutate(multipliedCO2Vol = volume * (CO2))
gasweightedTB<- volTB%>%
  group_by(sampledate) %>%
  mutate(lakevolume = sum(volume))%>%
  mutate(CH4Mass = sum(multipliedCH4Vol/(sum(volume)))) %>% 
  mutate(CO2Mass = sum(multipliedCO2Vol/(sum(volume)))) %>%
  ungroup() 

storageTB = gasweightedTB %>% 
  group_by(sampledate) %>%
  summarise(ch4mass = mean(CH4Mass), 
            co2mass = mean(CO2Mass), 
            doy = mean(doy))%>%
  mutate(year = year(sampledate))%>%
  filter(month(sampledate) < 6)%>%
  mutate(ch4SA = (ch4mass * 61693.5)/10983)%>%
  mutate(co2SA = (co2mass * 61693.5)/10983)

storageTB<-ggplot(storageTB)+
  geom_point(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = ch4mass, shape = factor(year)), size = 3, color = "red2")+
  geom_point(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = co2mass, shape = factor(year)), size = 3, color = "blue")+
  geom_path(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = ch4mass, group = factor(year)), color = "red2")+
  geom_path(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = co2mass, group = factor(year)), color = "blue")+
  scale_x_date(labels = date_format("%b"))+
  labs(color = "Gas")+
  xlab("")+
  ylab(expression(paste("Volume Weighted Gas Storage (",  mu,"mol ", L^-1,")")))+
  theme_bw(base_size = 8)
# plot_annotation(
# caption = 'Volumetric Weight of Gas Storage: Red = Methane and Blue = Carbon dioxide',
# theme = theme(plot.caption = element_text( hjust = 0, size = 18)))

ggsave("figures/storage.png", width = 8, height = 8, units = 'in', storage)
ggsave("figures/methodweight.png", width = 10, height = 6, units = 'in', estimate)
#Calculate Hypsometrically Weighted gas concentrations
volSSB<- bathjoin%>%
  mutate(doy = yday(sampledate))%>%
  filter(lake.x == "SSB" & lake.y =="SSB")%>%
  mutate(multipliedCH4Vol = volume * (CH4)) %>%
  mutate(multipliedCO2Vol = volume * (CO2))
gasweightedSSB<- volSSB%>%
  group_by(sampledate) %>%
  mutate(lakevolume = sum(volume))%>%
  mutate(CH4Mass = sum(multipliedCH4Vol/(sum(volume)))) %>% 
  mutate(CO2Mass = sum(multipliedCO2Vol/(sum(volume)))) %>%
  ungroup() 

storageSSB <- gasweightedSSB %>% 
  group_by(sampledate) %>%
  summarise(ch4mass = mean(CH4Mass), 
            co2mass = mean(CO2Mass), 
            doy = mean(doy))%>%
  mutate(year = year(sampledate))%>%
  filter(month(sampledate) < 6)%>%
  mutate(ch4SA = (ch4mass * 30981.9)/4783.56)%>%
  mutate(co2SA = (co2mass * 30981.9)/4783.56)

storageplotSSB<-ggplot(storageSSB)+
  geom_point(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = ch4mass, color = factor(year)), size = 3, color = "red2")+
  geom_point(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = co2mass, color = factor(year)), size = 3, color = "blue")+
  geom_path(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = ch4mass, group = factor(year)))+
  geom_path(aes(x = as.Date(doy, origin = as.Date('2019-01-01')), y = co2mass, group = factor(year)))+
  scale_x_date(labels = date_format("%b"))+
  labs(color = "Gas")+
  xlab("")+
  ylab(expression(paste("Volume Weighted Gas Storage (",  mu,"mol ", L^-1,")")))+
  theme_bw(base_size = 8)
