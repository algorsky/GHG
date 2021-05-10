library(tidyverse)
library(lubridate)

# Read in data 
dat = read_csv('data/GC2021/fullheat.csv')
dat$date = as.Date(dat$date, format =  "%m/%d/%y")
dat<- dat %>%
  filter(lake == "TB")
bath = read_csv('data/tbbath.csv')

#Join datasets
bathjoin <- left_join(dat, bath, by = c("depth" = "depth"))

#Calculate Hypsometrically Weighted gas concentrations
gasweighted<- bathjoin%>%
  mutate(multipliedCH4Vol = volume * (CH4*1000000)) %>%
  mutate(multipliedCO2Vol = volume * (CO2*1000000))%>%
  group_by(date) %>%
  mutate(lakevolume = sum(volume))%>%
  mutate(CH4Mass = sum(multipliedCH4Vol/(sum(volume)))) %>% 
  mutate(CO2Mass = sum(multipliedCO2Vol/(sum(volume)))) %>%
  ungroup() 

storageTB = gasweighted %>% 
  group_by(date) %>%
  summarise(ch4mass = mean(CH4Mass), 
            co2mass = mean(CO2Mass), 
            doy = mean(doy))

spring<- storageTB %>%
  filter(date == as.POSIXct("2020-03-07")|date == as.POSIXct("2020-05-04"))%>%
  mutate(ch4SA = (ch4mass * 61693.5)/10983)%>%
  mutate(co2SA = (co2mass * 61693.5)/10983)

ch4vol_iceoff = 277.1684 - 214.5911
co2vol_iceoff = 3677.656 - 2359.088

fall<- storageTB %>%
  filter(date == as.POSIXct("2020-10-05")|date == as.POSIXct("2020-10-30"))%>%
  mutate(ch4SA = (ch4mass * 61693.5)/10983)%>%
  mutate(co2SA = (co2mass * 61693.5)/10983)

ch4vol_fall = 481.3385 - 264.8943
co2vol_fall = 3067.311 - 2400.751


#Higher estimate
gashigh<- bathjoin%>%
  mutate(multipliedCH4Vol = highvolume * (CH4*1000000)) %>%
  mutate(multipliedCO2Vol = highvolume * (CO2*1000000))%>%
  group_by(date) %>%
  mutate(lakevolume = sum(highvolume))%>%
  mutate(CH4Mass = sum(multipliedCH4Vol/(sum(highvolume)))) %>% 
  mutate(CO2Mass = sum(multipliedCO2Vol/(sum(highvolume)))) %>%
  ungroup()  

storageHigh = gashigh %>% 
  group_by(date) %>%
  summarise(ch4mass = mean(CH4Mass), 
            co2mass = mean(CO2Mass), 
            doy = mean(doy))
estimate<-ggplot()+
  geom_point(data = storageTB, aes(x = date, y = ch4mass), color = "red2")+
  geom_point(data = storageTB, aes(x = date, y = co2mass), color = "turquoise4")+
  geom_point(data = storageHigh, aes(x = date, y = ch4mass))+
  geom_point(data = storageHigh, aes(x = date, y = co2mass))+
  xlab("")+
  ylab(expression(paste("Volume Weighted Gas Storage (",  mu,"mol ", L^-1,")")))+
  theme_bw(base_size = 20)+
  annotate("rect", xmin = as.Date(paste0(2020,'-03-07')), xmax = as.Date(paste0(2020,'-05-04')), ymin = -1, ymax = 900, alpha = 0.1)+
  annotate("rect", xmin = as.Date(paste0(2020,'-10-05')), xmax = as.Date(paste0(2020,'-10-30')), ymin = -1, ymax = 900, alpha = 0.1)+
  plot_annotation(
    caption = 'Figure: Method comparison for calculating volumetric weight of gas. Red represents methane and blue represents carbon dioxide.',
    theme = theme(plot.caption = element_text( hjust = 0, size = 10)))

storage<-ggplot(dplyr::filter(storageTB, date < as.POSIXct('2021-01-01')))+
  geom_point(aes(x = date, y = ch4mass), size = 3, color = "red2")+
  geom_point(aes(x = date, y = co2mass), size = 3, color = "turquoise4")+
  labs(color = "Gas")+
  xlab("")+
  ylab(expression(paste("Volume Weighted Gas Storage (",  mu,"mol ", L^-1,")")))+
  theme_bw(base_size = 20)+
  annotate("rect", xmin = as.Date(paste0(2020,'-03-07')), xmax = as.Date(paste0(2020,'-05-04')), ymin = -1, ymax = 900, alpha = 0.1)+
  annotate("rect", xmin = as.Date(paste0(2020,'-10-05')), xmax = as.Date(paste0(2020,'-10-30')), ymin = -1, ymax = 900, alpha = 0.1)
 # plot_annotation(
   # caption = 'Volumetric Weight of Gas Storage: Red = Methane and Blue = Carbon dioxide',
   # theme = theme(plot.caption = element_text( hjust = 0, size = 18)))

ggsave("figures/storage.png", width = 8, height = 8, units = 'in', storage)
ggsave("figures/methodweight.png", width = 10, height = 6, units = 'in', estimate)

annual<- gasweighted %>%
  filter(date <= as.POSIXct('2021-01-01'))%>%
  mutate(Depth = as.factor(depth))%>%
  mutate(ydoy = yday(as.Date(date)))%>%
  mutate(ch4 = CH4 *1000000)%>%
  mutate(co2 = CO2 *1000000)
storageCH4<-ggplot(annual)+
  geom_point(aes(x = as.Date(doy, origin = as.Date('2020-01-01')), y = ch4, shape = Depth), size = 3)+
  geom_path(aes(x = as.Date(doy, origin = as.Date('2020-01-01')), y = ch4, group = Depth), color = "black")+
  geom_line(aes(x = as.Date(doy, origin = as.Date('2020-01-01')), y = CH4Mass, color = "Volumetric Weighted Average"), size = 1.2)+
  scale_x_date(labels = date_format("%b"))+
  scale_shape(name = "Depth (m)")+
  scale_color_manual(name = "",values = "red2")+
  xlab("")+
  ylab(expression(paste("[C", H[4 (aq)], "] (",   mu,"mol ", L^-1,")")))+
  theme_bw(base_size = 8)+
  annotate("rect", xmin = as.Date('2020-01-01'), xmax = as.Date('2020-04-26'), ymin = -1, ymax = 600, alpha = 0.1)

storageCO2<-ggplot(annual)+
  geom_point(aes(x = as.Date(doy, origin = as.Date('2020-01-01')), y = co2, shape = Depth), size = 3)+
  geom_path(aes(x = as.Date(doy, origin = as.Date('2020-01-01')), y = co2, group = Depth), color = "black")+
  geom_line(aes(x = as.Date(doy, origin = as.Date('2020-01-01')), y = CO2Mass, color = "Volumetric Weighted Average"), size = 1.2)+
  scale_x_date(labels = date_format("%b"))+
  scale_shape(name = "Depth (m)")+
  scale_color_manual(name = "",values = "turquoise4")+
  xlab("")+
  scale_y_continuous(breaks=seq(0,1200,200))+
  ylab(expression(paste("[C", O[2 (aq)], "] (",   mu,"mol ", L^-1,")")))+
  theme_bw(base_size = 8)+
  annotate("rect", xmin = as.Date('2020-01-01'), xmax = as.Date('2020-04-26'), ymin = -1, ymax = 1200, alpha = 0.1)

#storageCO2<-ggplot(data = annual)+
 # geom_point(aes(x = doy, y = CO2 *1000000,  shape = Depth), size = 3)+
  #geom_line(aes(x = doy, y = CO2 *1000000,  group = depth))+
  #geom_path(aes(x = doy, y = CO2Mass), color = "turquoise4", size = 1.2)+
  #xlab("Day Number")+
  #scale_y_continuous(breaks=seq(0,1500,200))+
  #scale_shape_manual(values = c(1, 16, 0, 15))+
  #ylab(expression(paste("[C", O[2 (aq)], "] (", mu,"M) ")))+
  #theme_bw(base_size = 30)+
  #annotate("rect", xmin = 0, xmax = 118, ymin = -1, ymax = 1200, alpha = 0.1)

figure<-(storageCH4 +storageCO2) + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")
ggsave("figures/TBstorage.png", width = 8, height = 6, units = 'in', figure)
