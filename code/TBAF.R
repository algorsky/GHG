library(tidyverse)
library(devtools)
library(lubridate)
library(bigleaf)
library(patchwork)
library(scales)
# Read in data for TB from Ameriflux
tbAF = read_csv('data/FluxTower/US-TrB_HH.csv')
tbAF<- tbAF%>%
  mutate(datetime = as.POSIXct(x = as.character(TIMESTAMP_START),format = "%Y%m%d%H%M"))%>%
  mutate(date = as.Date(datetime))%>%
  mutate(time = format(datetime, format = "%H:%M"))

TBch4mean<- tbAF%>%
  filter(FCH4 != -9999)%>%
  group_by(date)%>%
  summarise(mean = mean(FCH4))

ggplot()+
  geom_point(data = tbAF, aes(x = as.Date(datetime), y = FCH4), size = 0.05, color = "gray", alpha = 0.5)+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-26")),  linetype = "dashed", color = "black")+
  geom_point(data = TBch4mean, aes( x = date, y = mean), size = 0.9, color = "red2")+
  geom_hline(yintercept = 0, color = "black", alpha = 0.4)+
  xlab("")+
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, 10))+
  scale_x_date(date_labels="%b", date_breaks  ="1 month")+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))+
  theme_bw()

diurnalCH4<- tbAF%>%
  filter(FCH4 != -9999)%>%
  group_by(time)%>%
  summarize(mean = mean(FCH4),
            se = sd(FCH4/sqrt(length(FCH4))))%>%
  mutate(hour = 1:20)
hourdiurnalCH4<- diurnalCH4%>%
  filter(hour %% 2 == 0)

ggplot(hourdiurnalCH4)+
  geom_point(aes(x = time, y = mean))+
  geom_errorbar(aes(x = time, y = mean, ymin = (mean - se), ymax = (mean + se)), width = .2)+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))+
  theme_bw(base_size = 8)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

diurnalCO2<- tbAF%>%
  filter(FC != -9999)%>%
  group_by(time)%>%
  summarize(mean = mean(FC),
            se = sd(FC/sqrt(length(FC))))%>%
  mutate(hour = 1:20)
hourdiurnalCO2<- diurnalCO2%>%
  filter(hour %% 2 == 0)

ggplot(hourdiurnalCO2)+
  geom_point(aes(x = time, y = mean))+
  geom_errorbar(aes(x = time, y = mean, ymin = (mean - se), ymax = (mean + se)), width = .2)+
  ylab(expression(paste("C", H[4], " flux (", n,"mol ", m^-2, s^-1,")")))+
  theme_bw(base_size = 8)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
