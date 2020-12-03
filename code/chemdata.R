library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
data = read_csv('data/chemcarbon.csv')

average <- data %>%
  group_by(lakeid, Date)%>%
  summarise(DOC = mean(DOC, na.rm = T),
          DIC = mean(DIC, na.rm = T),
          TOC = mean(TOC, na.rm = T),
          TIC = mean(TIC, na.rm = T))

#DOC
ggplot(average)+
  stat_summary(aes(x = lakeid, y = DOC, fill = lakeid), fun = mean, geom = "bar")+
  stat_summary(aes(x = lakeid, y = DOC), fun.data = mean_se, geom = "errorbar", width = 0.5)+
  xlab("")+
  ylab("Dissolved Organic Carbon (mg/L)")+
  theme_bw()

#DIC
ggplot(average)+
  stat_summary(aes(x = lakeid, y = DIC, fill = lakeid), fun = mean, geom = "bar")+
  stat_summary(aes(x = lakeid, y = DIC), fun.data = mean_se, geom = "errorbar", width = 0.5)+
  xlab("")+
  ylab("Dissolved Organic Carbon (mg/L)")+
  theme_bw()

#TOC
ggplot(average)+
  stat_summary(aes(x = lakeid, y = TOC, fill = lakeid), fun = mean, geom = "bar")+
  stat_summary(aes(x = lakeid, y = TOC), fun.data = mean_se, geom = "errorbar", width = 0.5)+
  xlab("")+
  ylab("Dissolved Organic Carbon (mg/L)")+
  theme_bw()

#TOC
ggplot(average)+
  stat_summary(aes(x = lakeid, y = TIC, fill = lakeid), fun = mean, geom = "bar")+
  stat_summary(aes(x = lakeid, y = TIC), fun.data = mean_se, geom = "errorbar", width = 0.5)+
  xlab("")+
  ylab("Dissolved Organic Carbon (mg/L)")+
  theme_bw()


  