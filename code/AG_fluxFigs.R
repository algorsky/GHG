library(tidyverse)
library(lubridate)
library(patchwork)

# Load meteorological data
min = read_csv('data/Data/GHCND_USC00475516_Minocqua.csv') %>% 
  mutate(datetime = ymd_hms(paste(DATE,'12:00:00')))

pressure = read_csv('data/Data/NOAA_airpressure.csv')%>% 
  mutate(datetime = ymd_hms(paste(year, month, day, '12:00:00')))

dateseq = data.frame(datetime = seq.POSIXt(from = as.POSIXct('2020-03-23 12:00:00'), 
                                                     to = as.POSIXct('2020-10-28 12:00:00'),
                                          by = '1 hour'))
# Load flux data and merge with timeseries dataframe
dataTB= read_csv('data/FluxTower/eddypro_TB_full_output_new.csv') %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))%>%
  mutate(datetime = ymd_hms(paste(date,time))) %>% 
  select(datetime, wind_speed, co2_flux, ch4_flux, qc_ch4_flux, qc_co2_flux, sonic_temperature, date, air_pressure) %>% 
  right_join(dateseq) %>% 
  left_join(min) %>% 
  left_join(pressure)%>%
  arrange(datetime)
names(tb)

tbch4<- dataTB%>%
  filter(ch4_flux > -9999)%>%
  filter(qc_ch4_flux <= 1)%>%
  filter(ch4_flux > -0.05 & ch4_flux < 0.05)

unique(ymd(tbch4$date))

tbco2<- dataTB%>%
  filter(co2_flux > -9999)%>%
  filter(qc_co2_flux <= 1)%>%
  filter(co2_flux > -10 & co2_flux < 10)



tb2 = read_csv('data/Data/QAQCFlux.csv') %>% 
  mutate(datetime = ymd_hms(paste(date,time))) %>% 
  select(datetime, airtemp, wind_speed, co2_flux, ch4_flux, sonic_temperature) %>% 
  right_join(dateseq) %>% 
  left_join(min) %>% 
  arrange(datetime)

# Load buoy data
tb1.hour = read_csv('data/Data/TB_2019_underice_chla.csv') %>% 
  mutate(date = as.Date(date_time_UTC), hour = hour(date_time_UTC)) %>% 
  group_by(date, hour) %>% 
  summarise_all(mean, na.rm = TRUE)

tb2.hour = read_csv('data/Data/TB_2020.csv') %>% 
  mutate(date = as.Date(date_time_UTC), hour = hour(date_time_UTC)) %>% 
  group_by(date, hour) %>% 
  summarise_all(mean, na.rm = TRUE)

TBco2mean<- tbco2%>%
  group_by(date)%>%
  summarise(mean = mean(co2_flux))%>%
  filter(mean > -2.6)%>%
  add_column(time = "12:00:00")%>%
  mutate(datetime = ymd_hms(paste(date,time)))

TBch4mean<- tbch4%>%
  group_by(date)%>%
  summarise(mean = mean(ch4_flux))%>%
  filter(mean*1000 > -10)%>%
  add_column(time = "12:00:00")%>%
  mutate(datetime = ymd_hms(paste(date,time)))
  


# Baseplot for plotting whole year 
basePlot<-ggplot(tbch4) +
  geom_rect(data = tb[1,], aes(xmin =  as.POSIXct(-Inf, origin="1970-01-01"),
                               xmax = as.POSIXct('2020-04-25 12:00:00'), 
                               ymin = -Inf, ymax = Inf), fill = 'grey80', alpha = 0.7) +
  scale_x_datetime(limits = c(as.POSIXct('2020-03-16 12:00:00'), 
                              as.POSIXct('2020-10-28 12:00:00')), breaks = breaks_width("month"), date_labels = "%b") +
  geom_vline(aes(xintercept = as.POSIXct('2020-04-25 12:00:00')), linetype = 2, size = 0.2) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank())
basePlotCH4 <- ggplot(tbch4) +
  geom_rect(data = tb[1,], aes(xmin =  as.POSIXct(-Inf, origin="1970-01-01"),
                               xmax = as.POSIXct('2020-04-25 12:00:00'), 
                               ymin = -Inf, ymax = Inf), fill = 'grey80', alpha = 0.7) +
  scale_x_datetime(limits = c(as.POSIXct('2020-03-16 12:00:00'), 
                              as.POSIXct('2020-10-28 12:00:00')), breaks = breaks_width("month"), date_labels = "%b") +
  geom_vline(aes(xintercept = as.POSIXct('2020-04-25 12:00:00')), linetype = 2, size = 0.2) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank())
library(scales)
basePlotCO2 <- ggplot(tbco2) +
  geom_rect(data = tb[1,], aes(xmin =  as.POSIXct(-Inf, origin="1970-01-01"),
                               xmax = as.POSIXct('2020-04-25 12:00:00'), 
                               ymin = -Inf, ymax = Inf), fill = 'grey80', alpha = 0.7) +
  scale_x_datetime(limits = c(as.POSIXct('2020-03-16 12:00:00'), 
                              as.POSIXct('2020-10-28 12:00:00')), breaks = breaks_width("month"), date_labels = "%b") +
  geom_vline(aes(xintercept = as.POSIXct('2020-04-25 12:00:00')), linetype = 2, size = 0.2) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank())

p1 = basePlot + 
  geom_ribbon(data = min, aes(x = datetime, ymax = TMAX, ymin = TMIN), fill = 'lightblue3', alpha = 0.8) +
  geom_line(aes(x = datetime, y = sonic_temperature-273.15), size = 0.2) +
  geom_line(data = tb1.hour, aes(x = date_time_UTC, y = tempC_93_cm), size = 0.2, col = 'blue1') + #
  geom_line(data = tb2.hour, aes(x = date_time_UTC, y = temp_minidot_c), size = 0.2, col = 'blue1') +
  scale_y_continuous(limits = c(-20, 33), expand = c(0,0))+
  ylab(expression("Temperature " ( degree*C)))

p1 = basePlot + 
  geom_ribbon(data = min, aes(x = datetime, ymax = TMAX, ymin = TMIN), fill = 'lightblue3', alpha = 0.8) +
  geom_line(aes(x = datetime, y = sonic_temperature-273.15), size = 0.2) +
  geom_line(data = tb1.hour, aes(x = date_time_UTC, y = tempC_93_cm), size = 0.2, col = 'lightblue4') + #
  geom_line(data = tb2.hour, aes(x = date_time_UTC, y = temp_minidot_c), size = 0.2, col = 'lightblue4') +
  scale_y_continuous(limits = c(-20, 33), expand = c(0,0))+
  ylab(expression("Temperature " ( degree*C)))

p10<-basePlot + 
  geom_ribbon(data = pressure, aes(x = datetime, ymax = Max.Pressure, ymin = Min.Pressure), fill = 'goldenrod', alpha = 0.8) +
  geom_line(aes(x = datetime, y = air_pressure/3386), size = 0.2) +
  scale_y_continuous(limits = c(27.5, 29), expand = c(0,0))+
  ylab(expression("Pressure (Hg)" ))

p2 = basePlot + 
  geom_col(aes(x = datetime, y = wind_speed), col = 'lightblue4', size = 0.2)+
  ylab(expression(paste("Wind speed (m",s^-1,")")))

p4 = basePlot + 
  geom_line(aes(x = datetime, y = air_pressure/1000))+
  ylab(expression(paste("Air Pressure (kPa)")))

p3 = basePlotCH4 + 
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.2) +
  geom_point(aes(x = datetime, y = ch4_flux*1000), fill = 'gray', size = 0.5, alpha = 0.7, shape = 21, stroke = 0.1)+
  geom_point(data = TBch4mean, aes(x = datetime, y = mean*1000), fill = 'red4', size = 1, shape = 21, stroke = 0.1)+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))

p8 = basePlotCO2 + 
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.2) +
  geom_point(aes(x = datetime, y = co2_flux), fill = 'gray', size = 0.5, alpha = 0.7, shape = 21, stroke = 0.1)+
  geom_point(data = TBco2mean, aes(x = datetime, y = mean), fill = 'green4', size = 1, shape = 21, stroke = 0.1)+
  ylab(expression(paste("C", O[2], " flux (", µ,"mol ", m^-2, s^-1,")")))+
  scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 2))

p1/p10/p2/p3/p8 + plot_annotation(tag_levels = "A")

ggsave('figures/AG_flux_test_total.pdf', width = 6.5, height = 5, units = 'in', dpi = 500)

# Baseplot for plotting spring

tb = read_csv('data/Data/QAQCFlux.csv') %>% 
  mutate(datetime = ymd_hms(paste(date,time))) %>% 
  select(datetime, airtemp, wind_speed, co2_flux, ch4_flux, sonic_temperature, air_pressure) %>% 
  right_join(dateseq) %>% 
  left_join(min) %>%
  left_join(pressure)%>%
  arrange(datetime)

pulseTB<- tb%>%
  filter(datetime > '2020-04-23 12:00:00' & datetime < '2020-05-14 12:00:00')
basePlot <- ggplot(tb) +
  geom_rect(data = tb[1,], aes(xmin =  as.POSIXct(-Inf, origin="1970-01-01"),
                               xmax = as.POSIXct('2020-04-25 12:00:00'), 
                               ymin = -Inf, ymax = Inf), fill = 'grey90', alpha = 0.7) +
  geom_vline(aes(xintercept = as.POSIXct('2020-04-25 12:00:00')), linetype = 2, size = 0.2) +
  scale_x_datetime(limits = c(as.POSIXct('2020-03-16 12:00:00'), 
                              as.POSIXct('2020-05-20 12:00:00'))) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(hjust = 0.5))

p1 = basePlot + 
  geom_ribbon(data = min, aes(x = datetime, ymax = TMAX, ymin = TMIN), fill = 'lightblue3', alpha = 0.8) +
  geom_line(aes(x = datetime, y = sonic_temperature-273.15), size = 0.2) +
  geom_line(data = tb1.hour, aes(x = date_time_UTC, y = tempC_93_cm), size = 0.2, col = 'blue1') + #
  geom_line(data = tb2.hour, aes(x = date_time_UTC, y = temp_minidot_c), size = 0.2, col = 'blue1') +
  scale_y_continuous(limits = c(-20, 23), expand = c(0,0))+
  ylab(expression("Temperature\n"( degree*C)))

p10<-basePlot + 
  geom_ribbon(data = pressure, aes(x = datetime, ymax = Max.Pressure, ymin = Min.Pressure), fill = 'goldenrod', alpha = 0.8) +
  geom_line(aes(x = datetime, y = air_pressure/3386), size = 0.2) +
  scale_y_continuous(limits = c(27.5, 29), expand = c(0,0))+
  ylab(expression("Pressure \n (Hg)" ))

p2 = basePlot + 
  geom_col(aes(x = datetime, y = wind_speed), col = 'lightblue4', size = 0.1)+
  ylab(expression(paste("Wind speed \n (m",s^-1,")")))

p3 = basePlot + 
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.2) +
  geom_point(aes(x = datetime, y = ch4_flux*1000), fill = 'red4', size = 1, alpha = 0.7, shape = 21, stroke = 0.1)+
  geom_point(data = pulseTB, aes(x = datetime, y = ch4_flux *1000), fill = "gold", size = 1, alpha = 0.7, shape = 21, stroke = 0.1)+
  ylab(expression(paste("C", H[4], " flux \n (", n,"mol ", m^-2, s^-1,")")))


p4 = ggplot(tb) +
  geom_ribbon(data = min, aes(x = datetime, ymax = TMAX, ymin = TMIN), fill = 'lightblue3', alpha = 0.8) +
  geom_line(aes(x = datetime, y = sonic_temperature-273.15), size = 0.2) +
  geom_line(data = tb1.hour, aes(x = date_time_UTC, y = tempC_93_cm), size = 0.2, col = 'blue1') + #
  geom_line(data = tb2.hour, aes(x = date_time_UTC, y = temp_minidot_c), size = 0.2, col = 'blue1') +
  theme_bw(base_size = 8) +
  scale_x_datetime(limits = c(as.POSIXct('2020-09-28 12:00:00'), as.POSIXct('2020-10-28 12:00:00'))) +
  scale_y_continuous(limits = c(-20, 23), expand = c(0,0)) +
  ylab(expression("Temperature \n" ( degree*C)))+
  theme(axis.title.x = element_blank())

p11 = ggplot(tb) +
  geom_ribbon(data = pressure, aes(x = datetime, ymax = Max.Pressure, ymin = Min.Pressure), fill = 'goldenrod', alpha = 0.8) +
  geom_line(aes(x = datetime, y = air_pressure/3386), size = 0.2) +
  scale_y_continuous(limits = c(27.5, 29), expand = c(0,0))+
  ylab(expression("Pressure \n (Hg)" ))+
  theme_bw(base_size = 8) +
  scale_x_datetime(limits = c(as.POSIXct('2020-09-28 12:00:00'), as.POSIXct('2020-10-28 12:00:00'))) +
  theme(axis.title.x = element_blank())

p5 = ggplot(tb) +
  geom_col(aes(x = datetime, y = wind_speed), col = 'lightblue4', size = 0.1) +
  theme_bw(base_size = 8) +
  scale_x_datetime(limits = c(as.POSIXct('2020-09-28 12:00:00'), as.POSIXct('2020-10-28 12:00:00'))) +
  ylab(expression(paste("Wind speed \n (m",s^-1,")")))+
  theme(axis.title.x = element_blank())

p6 = ggplot(tb) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.2) +
  geom_point(aes(x = datetime, y = ch4_flux*1000), fill = 'red4', size = 1, alpha = 0.7, shape = 21, stroke = 0.1) +
  theme_bw(base_size = 8) +
  scale_x_datetime(limits = c(as.POSIXct('2020-09-28 12:00:00'), as.POSIXct('2020-10-28 12:00:00'))) +
  ylab(expression(paste("C", H[4], " flux \n (", n,"mol ", m^-2, s^-1,")")))+
  theme(axis.title.x = element_blank())

layout <- "
AB
CD
EF
GH
"

p1 + p4 + p10 + p11 + p2 + p5 + p3 + p6 + plot_layout(design = layout) + plot_annotation(tag_levels = "A")
ggsave('figures/AG_flux_test.pdf', width = 6.5, height = 4, units = 'in', dpi = 500)
