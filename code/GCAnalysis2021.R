library(tidyverse)
library(devtools)
library(lubridate)
library(neonDissGas)

#QA/QC
# Read in data 
df = read_csv('data/GC2021/20210415_GC_Export.csv')  %>% 
  filter(batch_id == 'SSB')%>%
  mutate(sample_id = str_replace_all(sample_id, pattern = "2020_01_", replacement = "2020-01-")) %>%  #because you switch from underscores to dashes
  separate(col = sample_id, into = c('lake','date','depth','replicate'), sep = "_", remove = FALSE) %>% 
  mutate(date = as.Date(date)) %>% 
  #mutate(analysis_date = mdy_hms(analysis_date)) %>% 
  filter(lake == 'TB' | lake == 'SSB') %>% 
  select(-batch_id) %>% 
  pivot_longer(cols = CO2_FID:N2O_ECD,names_to = "parameter",values_to = "value") %>% 
  filter(!(parameter == "CO2_FID" & value >1000)) %>% #subset data so that we are looking keeping FID samples for < 1000 and TCD samples for 1000+
  filter(!(parameter == "CH4_FID" & value >1000)) %>% 
  filter(!(parameter == "CH4_TCD" & value <=1000)) %>% 
  filter(!(parameter == "CO2_TCD" & value <=1000)) %>% 
  mutate(value = round(value,digits=0))

#write.table(df, 'data/qaqcspring2021.csv', sep="\t")


# Read in data 
df.spring.2021 = read_csv('data/GC2021/20210415_GC_Export.csv')  %>% 
  filter(batch_id == 'SSB')%>%
  mutate(sample_id = str_replace_all(sample_id, pattern = "2020_01_", replacement = "2020-01-")) %>%  #because you switch from underscores to dashes
  separate(col = sample_id, into = c('lake','date','depth','replicate'), sep = "_", remove = FALSE) %>% 
  mutate(date = as.Date(date)) 

#write.table(df.2021, 'data/df.new.2021.csv', sep="\t")
air_samples <- df.spring.2021 %>% filter(grepl("Air",depth)) %>% 
  select(-analysis_date,-batch_id,-sample_id,-depth) %>% 
  group_by(lake,date) %>% 
  summarize_all(list(median = median)) %>% 
  select(-CO2_TCD_median,-CH4_TCD_median) %>%  #levels are all to low to use TCD
  ungroup()

dat.spring.2021 <- df.spring.2021 %>% 
  filter(!grepl("Air",depth)) %>% 
  select(analysis_date, batch_id,sample_id, lake, date,depth,CO2_FID,CO2_TCD,CH4_FID,CH4_TCD,N2O_ECD) %>% 
  mutate(N2O_ECD = replace(N2O_ECD,N2O_ECD < 0, 0)) %>% 
  left_join(air_samples) %>% 
  rename(concentrationCO2Gas = CO2_TCD) %>% 
  rename(concentrationCH4Gas = CH4_TCD) %>%
  rename(concentrationN2OGas = N2O_ECD) %>% 
  rename(concentrationCO2Air = CO2_FID_median) %>%
  rename(concentrationCH4Air = CH4_FID_median) %>%
  rename(concentrationN2OAir = N2O_ECD_median) %>% 
  mutate(gasVolume = 60) %>% #mL
  mutate(waterVolume = 940) %>% #mL
  mutate(waterTemp = 0.2) %>% #degC
  mutate(headspaceTemp = 0.2) %>% #degC Assume it is the same was water temp
  mutate(barometricPressure = 103.991) #kpa

write.table(dat.spring.2021, 'data/GC2021/dat.spring.2021.csv', sep="\t")

dat = read_csv('data/GC2021/dat.spring.2021.csv')
dat.out <- def.calc.sdg.conc(as.data.frame(dat)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,dissolvedCO2,dissolvedCH4) %>% 
  gather(value="value",key="parameter",-lake,-date,-depth)

tidy.dat.out.spring2021 <- def.calc.sdg.conc(as.data.frame(dat)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,dissolvedCO2,dissolvedCH4)

dat = read_csv('data/dat.2020.csv')
tidy.dat.out.2020 <- def.calc.sdg.conc(as.data.frame(dat)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,dissolvedCO2,dissolvedCH4)

write.table(tidy.dat.out.spring2021, 'data/GC2021/tidy.spring2021.csv', sep="\t")

sat.dat.out2021 <- def.calc.sdg.conc(as.data.frame(dat)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,barometricPressure,waterTemp, headspaceTemp, dissolvedCO2, concentrationCO2Air,dissolvedCH4, concentrationCH4Air, dissolvedN2O, concentrationN2OAir)

#Gas Saturation
tidy.dat.out.all = read_csv('data/GC2021/tidy.dat.out.all.csv')
dat.out.sat <- def.calc.sdg.sat(as.data.frame(tidy.dat.out.all)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,waterTemp,satConcCO2,satConcCH4, CO2PercSat, CH4PercSat)
dat.out.sat$date = as.Date(dat.out.sat$date, format =  "%m/%d/%y")
gas.sat <- dat.out.sat %>% 
  group_by(lake, date, depth) %>% 
  summarise(
    CO2persat = mean(CO2PercSat),
    CH4persat = mean(CH4PercSat))

#write.table(gas.sat, 'data/GC2021/gas.sat.csv', sep="\t")

