library(tidyverse)
library(devtools)
library(lubridate)
library(patchwork)

# Read in data 
ssb.chloro = read_csv('data/ChemTempDO/ssb_chloro.csv')
ssb.chloro$sampledate = as.Date(ssb.chloro$sampledate, format =  "%m/%d/%y")
ssb.chloro = ssb.chloro %>%
  filter(sampledate > as.POSIXct('2019-12-31'))%>%
  filter(month(sampledate) < 5)
tb.chloro = read_csv('data/ChemTempDO/tb_chlor.csv')
tb.chloro$sampledate = as.Date(tb.chloro$sampledate, format =  "%m/%d/%y")
tb.chloro = tb.chloro  %>% 
  filter(rep == 1)%>%
  select(lakeid, sampledate, depth, chlor)


chloro = rbind(ssb.chloro,tb.chloro)


ggplot(dplyr::filter(chloro, lakeid == 'SSB'))+
  geom_boxplot(aes(x = factor(sampledate), y = chlor))+
  geom_jitter(aes(x = factor(sampledate), y =chlor, color = depth), alpha = 0.05)+
  facet_wrap(~lakeid)

chlsurf<-ggplot(dplyr::filter(ssb.chloro, depth == 0))+
  geom_col(aes(x = factor(sampledate), y = chlor, fill = year(sampledate)))+
  xlab("")+
  ylab(expression(paste("SSB Surface Chl a (",  mu,"g ", L^-1,")")))+
  theme_bw(base_size = 20)+
  theme(legend.position = "none")

tbchlsurf<-ggplot(dplyr::filter(tb.chloro, depth == 0))+
  geom_col(aes(x = factor(sampledate), y = chlor, fill = year(sampledate)))+
  xlab("")+
  ylab(expression(paste("TB Surface Chl a (",  mu,"g ", L^-1,")")))+
  theme_bw(base_size = 20)+
  theme(legend.position = "none")

ggsave("figures/chlsurf.png", width = 15, height = 6, units = 'in', chlsurf)

chl<-ggplot(dplyr::filter(ssb.chloro, depth != 0), aes(x = factor(sampledate), y = chlor, group = factor(sampledate), color = depth))+
  geom_boxplot()+ #might help better display the median and range of your data.
  geom_jitter(alpha = 0.5, size = 3)+ #alpha plays around with the transparency of the points
  #jitter makes it so the points aren't on top of each other
  scale_colour_viridis_c()+
  xlab("Sample Date")+
  ylab(expression(paste("SSB Chl a (",  mu,"g ", L^-1,")")))+
  theme_bw(base_size = 20)
  
ggsave("figures/chl.png", width = 15, height = 6, units = 'in', chl)

tbchl<-ggplot(dplyr::filter(tb.chloro, depth != 0), aes(x = factor(sampledate), y = chlor, group = factor(sampledate), color = depth))+
  geom_boxplot()+ #might help better display the median and range of your data.
  geom_jitter(alpha = 0.5, size = 3)+ #alpha plays around with the transparency of the points
  #jitter makes it so the points aren't on top of each other
  scale_colour_viridis_c()+
  xlab("Sample Date")+
  ylab(expression(paste("TB Chl a (",  mu,"g ", L^-1,")")))+
  theme_bw(base_size = 20)

figure<-(chl +tbchl) + plot_layout(guides = "collect", ncol = 1)
ggsave("figures/TBstorage.png", width = 8, height = 10, units = 'in', figure)

  xlab('Month')+
  ylab("pH")+
  scale_colour_viridis_c()+
  theme_bw(base_size = 12)+
  plot_annotation(
    caption = 'Figure: Seasonal variation of pH in Lake Geneva, WI from 1997-2020.',
    theme = theme(plot.caption = element_text( hjust = 0, size = 10)))

ggplot(dplyr::filter(chloro, lakeid == 'TB'| lakeid == 'SSB' & depth == 0)) + 
  geom_point(aes(x = sampledate, y = chlor), size = 3) +
  facet_wrap(~lakeid) +
  xlab("")+
  ylab("Surface CH4 Percent Saturation")+
  theme_bw()
ggplot(dplyr::filter(chloro, lakeid == 'SSB')) + 
  geom_point(aes(x = sampledate, y = chlor, color = depth)) +
  facet_wrap(~lake) +
  xlab("")+
  ylab("Surface CH4 Percent Saturation")+
  theme_bw()
