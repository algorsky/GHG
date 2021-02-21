library(tidyverse)
library(devtools)
library(lubridate)
library(patchwork)

ssb.gas = read_csv('data/GC2021/SSB_GHG.csv')
ssb.gas = ssb.gas%>% 
  select(c(lake, depth, co2, ch4))
hypo <- read_csv('data/bogshypo.csv')

hypsoGas<- left_join(ssb.gas, hypo, by = c("lake" = "lake", "depth" = "depth"))
