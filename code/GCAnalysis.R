library(tidyverse)
library(devtools)
library(lubridate)
library(neonDissGas)

# Read in data 
df.2021 = read_csv('data/2021Run1/rawdata.csv')  %>% 
  filter(batch_id == 'SSB')%>%
  mutate(sample_id = str_replace_all(sample_id, pattern = "2020_01_", replacement = "2020-01-")) %>%  #because you switch from underscores to dashes
  separate(col = sample_id, into = c('lake','date','depth','replicate'), sep = "_", remove = FALSE) %>% 
  mutate(date = as.Date(date)) 

#write.table(df.2021, 'data/df.2021.csv', sep="\t")

air_samples <- df %>% filter(grepl("Air",depth)) %>% 
  select(-analysis_date,-batch_id,-sample_id,-depth) %>% 
  group_by(lake,date) %>% 
  summarize_all(list(median = median)) %>% 
  select(-CO2_TCD_median,-CH4_TCD_median) %>%  #levels are all to low to use TCD
  ungroup()

dat <- df %>% 
  filter(!grepl("Air",depth)) %>% 
  select(batch_id,lake, sample_id, date,depth,CO2_FID,CO2_TCD,CH4_FID,CH4_TCD,N2O_ECD) %>% 
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

dat.out <- def.calc.sdg.conc(as.data.frame(dat)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,dissolvedCO2,dissolvedCH4) %>% 
  gather(value="value",key="parameter",-lake,-date,-depth)

tidy.dat.out2021 <- def.calc.sdg.conc(as.data.frame(dat)) %>%
  filter(lake == 'TB' | lake == 'SSB')%>%
  select(lake,date,depth,dissolvedCO2,dissolvedCH4)

write.table(tidy.dat.out2021, 'data/tidy.dat.out2021.csv', sep="\t")


