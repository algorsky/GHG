library(tidyverse)
library(lubridate)
library(patchwork)

tbGas = read_csv("data/gasTBmeansd.csv")
tbGas$date<- as.Date(tbGas$date, format =  "%m/%d/%y")

tbGas<- tbGas%>%
  mutate(Depth = factor(depth))

basePlot <- ggplot(tbGas) +
  geom_rect(data = tbGas, aes(xmin =  as.Date(-Inf, origin="1970-01-01"),
                               xmax = as.Date('2020-04-26'), 
                               ymin = -Inf, ymax = Inf), fill = 'grey80', alpha = 0.7) +
  scale_x_date(limits = c(as.Date('2020-01-01'), 
                              as.Date('2020-10-31'))) +
  geom_vline(aes(xintercept = as.Date('2020-04-26')), linetype = 2, size = 0.2) +
  theme_bw(base_size = 8) +
  theme(axis.title.x = element_blank())
pd <- position_dodge(width = 0.5)
col<- c("lightpink", "red2","red3", "red4")
col2<- c("lightgreen", "springgreen3", "springgreen4", "darkgreen")
ch4plot<-basePlot + 
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.2) +
  geom_point(aes(x = date, y = CH4, shape = Depth, color = Depth), fill = "red4",size = 2,stroke = 0.1, position = pd)+
  geom_path(aes(x = date, y = CH4, group = depth, color = Depth), size = .2)+
  geom_errorbar(aes(x = date, y = CH4, ymin = CH4 - CH4sd, ymax = CH4 + CH4sd, color = Depth), size = 0.5, position = pd)+
  geom_line(data = annual, aes(x = date, y = CH4Mass),linetype = "dashed")+
  scale_shape_manual(values = c(17,16,15,23))+
  scale_color_manual(values = col)+
  scale_linetype_manual(name = "Volumetric Weighted Average")+
  ylab(expression(paste("C", H[4], " (", µ,"mol ", L^-1,")")))+
  theme(axis.title.x = element_blank())
co2plot<-basePlot + 
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.2) +
  geom_point(aes(x = date, y = CO2, shape = Depth, color = Depth), size = 2,stroke = 0.1, fill = "darkgreen")+
  geom_path(aes(x = date, y = CO2, group = depth, color = Depth), size = .2)+
  geom_errorbar(aes(x = date, y = CO2, ymin = CO2 - CO2sd, ymax = CO2 + CO2sd, color = Depth), size = 0.5, width  = 1, position = pd)+
  geom_line(data = annual, aes(x = date, y = CO2Mass),linetype = "dashed")+
  scale_shape_manual(values = c(17,16,15,23))+
  scale_linetype_manual(name = "Volumetric Weighted Average")+
  scale_color_manual(values = col2)+
  ylab(expression(paste("C", O[2], " (", µ,"mol ", L^-1,")")))+
  theme(axis.title.x = element_blank())
ch4plot/co2plot + plot_annotation(tag_levels = "A")

ggsave('figures/storageTBAnnual.pdf', width = 6.5, height = 5, units = 'in', dpi = 500)
