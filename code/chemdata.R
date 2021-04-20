library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
data = read_csv('data/ChemTempDO/chemcarbon.csv')
data$Date = as.Date(data$Date, format =  "%m/%d/%y")

winter<- data %>%
  filter()

#DOC
ggplot(dplyr::filter(data, lakeid == 'TB' | lakeid == 'SSB'))+
  geom_point(aes(x = Date, y = DOC, color = Depth))+
  xlab("")+
  ylab("Dissolved Organic Carbon (mg/L)")+
  facet_wrap(~lakeid)

doc<-ggplot(dplyr::filter(data, lakeid == 'TB'))+
  geom_point(aes(x = Date, y = DOC, color = Depth), size = 2)+
  xlab("")+
  ylab("Dissolved Organic Carbon (mg/L)")+
  theme_bw(base_size = 12)

average <- data %>%
  group_by(lakeid, Date)%>%
  summarise(DOC = mean(DOC, na.rm = T),
            DIC = mean(DIC, na.rm = T),
            TOC = mean(TOC, na.rm = T),
            TIC = mean(TIC, na.rm = T))

average<-ggplot(dplyr::filter(average, lakeid == 'TB'))+
  geom_point(aes(x = Date, y = DOC), size = 2)+
  xlab("")+
  ylab("Mean DOC (mg/L)")+
  theme_bw(base_size = 12)

doc<-(doc +average) + plot_layout(guides = "collect", ncol = 1)
ggsave("figures/doc.png", width = 8, height = 10, units = 'in', doc)


#DIC
dic<-ggplot(dplyr::filter(data, lakeid == 'TB'))+
  geom_point(aes(x = Date, y = DIC, color = Depth), size = 2)+
  xlab("")+
  ylab("Dissolved Inorganic Carbon (mg/L)")+
  theme_bw(base_size = 12)
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


average <- data %>%
  group_by(lakeid, Date)%>%
  summarise(DOC = mean(DOC, na.rm = T),
            DIC = mean(DIC, na.rm = T),
            TOC = mean(TOC, na.rm = T),
            TIC = mean(TIC, na.rm = T))

  