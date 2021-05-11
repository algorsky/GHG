library(tidyverse)
library(devtools)
library(lubridate)
library(bigleaf)
library(patchwork)
# Read in data for SSB
dataSSB = read_csv('data/FluxTower/eddypro_SSB_full_output.csv')
dataTB = read_csv('data/FluxTower/eddypro_TB_full_output_new.csv')

qcTBCH4<- dataTB%>%
  filter(ch4_flux > -9999)%>%
  filter(qc_ch4_flux <= 1)
fluxTBCH4<-qcTBCH4%>%
  filter(ch4_flux > -0.05 & ch4_flux < 0.05)%>%
  mutate(airtemp = (air_temperature - 273.15))
fluxTBCH4$date<- as.Date(fluxTBCH4$date, format = "%m/%d/%y")

qcTBCO2<- dataTB%>%
  filter(co2_flux > -9999)%>%
  filter(qc_co2_flux <= 1)%>%
  filter(co2_flux > -10 & co2_flux < 10)
fluxTBCO2<-qcTBCO2%>%
  filter(co2_flux > -10 & co2_flux < 10)
fluxTBCO2$date<- as.Date(fluxTBCO2$date, format = "%m/%d/%y")

TBch4mean<- fluxTBCH4%>%
  group_by(date)%>%
  summarise(mean = mean(ch4_flux))%>%
  filter(mean*1000 > -10)

iceoffch4<- TBch4mean %>%
  filter(date > as.POSIXct('2020-04-27')& date < as.POSIXct('2020-05-16'))%>%
  mutate(day = mean * 86400)
sum(iceoffch4$day) * 17 * 0.001


TBco2mean<- fluxTBCO2%>%
  group_by(date)%>%
  summarise(mean = mean(co2_flux))%>%
  filter(mean > -2.5)

iceoffco2<- TBco2mean %>%
  filter(date > as.POSIXct('2020-04-26')& date < as.POSIXct('2020-05-07'))%>%
  mutate(day = mean * 86400)
sum(iceoffco2$day) * 10 * 0.001

iceoffch4<- TBch4mean %>%
  filter(date > as.POSIXct('2020-04-26')& date < as.POSIXct('2020-05-16'))
sum(iceoffch4$mean) * 86400 * 0.001

fallco2<- TBco2mean %>%
  filter(date > as.POSIXct('2020-09-30')& date < as.POSIXct('2020-10-15'))%>%
  mutate(day = mean * 86400)
sum(fallco2$day) * 14 * 0.001

fallch4<- TBch4mean %>%
  filter(date > as.POSIXct('2020-09-30')& date < as.POSIXct('2020-10-10'))
sum(fallch4$mean) * 86400 * 0.001

#CH4 Flux (µmol m-2 s-1)
ch4<-ggplot()+
  geom_point(data = fluxTBCH4, aes(x = date, y = ch4_flux *1000), size = 0.05, color = "gray", alpha = 0.5)+
  geom_point(data = TBch4mean, aes( x = date, y = mean*1000), size = 0.9, color = "red2")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "black")+
  geom_vline(xintercept = as.numeric(as.Date("2020-10-01")),  linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, 10))+
  scale_x_date(date_labels="%b", date_breaks  ="1 month")+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))+
  theme_bw(base_size = 8)

co2<- ggplot()+
  geom_point(data = fluxTBCO2, aes(x = date, y = co2_flux), size = 0.05, color = "gray", alpha = 0.5)+
  geom_point(data = TBco2mean, aes(x = date, y = mean), color = "turquoise4", size = 0.75)+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "black")+
  geom_vline(xintercept = as.numeric(as.Date("2020-10-01")),  linetype = "dashed", color = "black")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 2))+
  xlab("")+
  ylab("CO2 Flux (µmol m-2 s-1)")+
  ylab(expression(paste("C", O[2], " flux (", µ,"mol ", m^-2, s^-1,")")))+
  scale_x_date(date_labels="%b", date_breaks  ="1 month")+
  theme_bw(base_size = 8)+
  theme(legend.position = "bottom", legend.title = element_blank())

gas2<-ch4/co2 + plot_layout(ncol = 1)+plot_annotation(tag_levels = "A")
ggsave("figures/fluxshade.png", width = 8, height = 6, units = 'in', gas2)

hourCH4<- fluxTBCH4%>%
  group_by(time)%>%
  summarize(mean= mean(ch4_flux*1000),
            se=sd(ch4_flux*1000)/sqrt(length(ch4_flux)))%>%
  mutate(hour = 1:20)
hourwholeCH4<-hourCH4%>%
  filter(hour %% 2 == 0)%>%
  mutate(Hour = hour(time))

hourCO2<- fluxTBCO2%>%
  group_by(time)%>%
  summarize(mean= mean(co2_flux),
            se=sd(co2_flux)/sqrt(length(co2_flux)))%>%
  mutate(hour = 1:20)
hourwholeCO2<-hourCO2%>%
  filter(hour %% 2 == 1)%>%
  mutate(Hour = hour(time))

ggplot(hourwholeCH4)+
  geom_point(aes(x = Hour, y = mean))+
  geom_errorbar(aes(x = Hour, y = mean, ymin = (mean - se), ymax = (mean + se)), width = .2)+
  scale_y_continuous(limits = c(0,7),breaks = seq(0,7,1))+
  scale_x_continuous(limits = c(7,18),breaks = seq(8,18,2))+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))+
  theme_bw(base_size = 8)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(hourwholeCO2)+
  geom_point(aes(x = Hour, y = mean))+
  geom_errorbar(aes(x = Hour, y = mean, ymin = (mean - se), ymax = (mean + se)), width = .2)+
  scale_y_continuous(limits = c(-0.1,7),breaks = seq(0,7,1))+
  scale_x_continuous(limits = c(7,18),breaks = seq(8,18,2))+
  ylab(expression(paste("C", O[2], " flux (", µ,"mol ", m^-2, s^-1,")")))+
  theme_bw(base_size = 8)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

iceoff<- fluxTBCH4 %>%
  filter(date > '2020-03-15'& date < '2020-05-19' )%>%
  mutate(doy = yday(date))
#Ice-Off
ggplot()+
  geom_point(data = iceoff, aes(x = as.Date(DOY, origin = as.Date('2020-01-01')), y = ch4_flux *1000), size = 1, color = "red2")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "black")+
 geom_line(data = fluxTBCH4, aes(x= as.Date(DOY, origin = as.Date('2020-01-01')), y = (max_wind_speed*2), alpha = 0.4, fill= "gray2"))+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  guides(color = FALSE)+
  guides(alpha = FALSE)+
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, 10))+
  scale_y_continuous(sec.axis = sec_axis(~./2, name = expression("Air Temperature " ( degree*C))))+ 
  scale_x_date(date_labels="%b %d", date_breaks  ="5 day", limits = c(as.Date("2020-04-19"), as.Date("2020-05-16")))+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))+
  theme_bw(base_size = 8)
ggplot()+
geom_line(data = fluxTBCH4, aes(x= as.Date(DOY, origin = as.Date('2020-01-01')), y = (max_wind_speed), alpha = 0.4, fill= "gray2"))
  #~./5))
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

#QA/QC
light.2020 = light.2020 %>% filter(as.Date(Date.GMT) != as.Date('2020-03-02')) %>%
  filter(as.Date(Date.GMT) > as.Date('2020-03-15')) %>%
  filter(as.Date(Date.GMT) < as.Date('2020-05-20')) %>%
  arrange(Date.GMT, depth)%>%
  mutate(metric = Intensity.lum.ft2 * 10.7639104)

buoysurf <- light.2020 %>%
  filter(depth == 1)%>%
  filter(as.Date(Date.GMT) > as.Date('2020-03-15')) %>%
  filter(as.Date(Date.GMT) < as.Date('2020-05-19'))%>%
  mutate(date = as.Date(Date.GMT))

buoybottom<- light.2020 %>%
  filter(depth == 7)%>%
  filter(as.Date(Date.GMT) > as.Date('2020-03-15')) %>%
  filter(as.Date(Date.GMT) < as.Date('2020-05-19'))%>%
  mutate(date = as.Date(Date.GMT))

colors <- c("Air Temperature" = "gray69","1 m Water Temperature" = "navy", "7 m Water Temperature" = "chocolate4")
theme_set(theme_bw(base_size = 8))
iceoff<-ggplot()+
  geom_path(data = dplyr::filter(fluxTBCH4, DOY < 110), aes(x = date, y = (airtemp*2), color = "Air Temperature"))+
  geom_path(data = dplyr::filter(fluxTBCH4, DOY > 112 & DOY <140), aes(x = date, y = (airtemp*2), color = "Air Temperature"))+
  geom_path(data = buoysurf, aes(x = as.Date(Date.GMT), y = Temp.C*2, color = "1 m Water Temperature"))+
  geom_path(data = buoybottom, aes(x = as.Date(Date.GMT), y = Temp.C*2, color = "7 m Water Temperature"))+
  geom_point(data = iceoff, aes(x = as.Date(DOY, origin = as.Date('2020-01-01')), y = ch4_flux *1000), size = 1, color = "red2", alpha = 0.2)+
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, 10))+
  scale_y_continuous(sec.axis = sec_axis(~./2, name = expression("Temperature " ( degree*C))))+ 
  xlab("")+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "black")+
  scale_color_manual(values = colors)+
  theme(legend.position = "top", legend.title = element_blank())

ggsave("figures/fluxiceoff.png", width = 8, height = 6, units = 'in', iceoff)

#Ice-Off
ggplot()+
  geom_point(data = iceoff, aes(x = as.Date(DOY, origin = as.Date('2020-01-01')), y = ch4_flux *1000), size = 1, color = "red2", alpha = 0.2)+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "black")+
  geom_line(data = fluxTBCH4, aes(x= as.Date(DOY, origin = as.Date('2020-01-01')), y = ((air_temperature-273.15)*2), alpha = 0.4, fill= "gray2"))+
  geom_path(data = buoysurf, aes(x = date, y = Temp.C*2, color = "Surface Temperature"))+
  geom_path(data = buoybottom, aes(x = date, y = Temp.C*2, color = "Bottom Temperature"))+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  guides(color = FALSE)+
  guides(alpha = FALSE)+
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, 10))+
  scale_y_continuous(sec.axis = sec_axis(~./2, name = expression("Air Temperature " ( degree*C))))+ 
  scale_x_date(date_labels="%b %d", date_breaks  ="5 day", limits = c(as.Date("2020-04-19"), as.Date("2020-05-16")))+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))+
  theme_bw(base_size = 8)
ggplot()+
  geom_line(data = fluxTBCH4, aes(x= as.Date(DOY, origin = as.Date('2020-01-01')), y = (max_wind_speed), alpha = 0.4, fill= "gray2"))

ggplot(buoybottom)+
  geom_line(aes(x = Date.GMT, y = Temp.C))
