library(skimr)
library(rstatix)
library(ggpubr)
library(glue)
library(ggsignif)
library(magrittr)
library(tidyverse)
sd_raw <- readxl::read_xls("EEG Patients.xls")
colnames(sd_raw) <- make.names(colnames(sd_raw))
sd_raw
# Get Phenotype
sdPhenotype <- sd_raw %>% 
  select(-c(R.f.d:NR.o.g)) %>% 
  mutate(Grouping = fct_recode(as.factor(Grouping), "Snoring only" = "1", "Mild/Moderate OSA" = "2", "Severe OSA" = "3")) 

# Tidy Data
sdProcessed <- sd_raw %>% 
  pivot_longer(cols = 94:129, names_to = "config", values_to = "absPower") %>% 
  separate(col = config, sep = "[.]", into = c("SleepStage", "Location", "Band")) %>%
  mutate(SleepStage = factor(fct_recode(SleepStage, REM = "R", NREM = "NR"), levels = c("REM", "NREM")),
         Location = factor(fct_recode(Location, Frontal = "f", Central = "c", Occipital = "o"), levels = c("Frontal", "Central", "Occipital")),
         Band = factor(fct_recode(Band, alpha = "a", theta = "t", beta = "b",  sigma = "s", delta = "d", gamma = "g"), levels = c("delta", "theta", "alpha", "sigma", "beta", "gamma")),
         Grouping = fct_recode(as.factor(Grouping), "Snoring only" = "1", "Mild-Moderate OSA" = "2", "Severe OSA" = "3")) %>% 
  # filter(Band != "gamma") %>% 
  group_by(id, SleepStage, Location) %>% 
  mutate(powerSum = sum(absPower), num = n(),
         slowWave = sum(absPower[Band %in% c("delta", "theta")]),
         fastWave = sum(absPower[Band %in% c("alpha", "sigma", "beta")]),
         slowingRatio = slowWave / fastWave * 100,
         DAR = absPower[Band == "delta"] / absPower[Band == "alpha"]
         ) %>% 
  ungroup() %>% 
  arrange(id, SleepStage, Location) %>% 
  mutate(relPower = absPower / powerSum * 100)


sdProcessed %>% glimpse
sdLong <- sdProcessed %>% dplyr::select(id, Grouping, SleepStage, Location, Band, relPower, absPower, bmi, age, tst, sleepEff, sleepLatency, N1P, N2P, N3P, REMP, REMLatency, ODI, AHI, ArI, ArI.normalized, epworth, education, mmse, pisq, moca, slowWave, fastWave, slowingRatio, DAR)
sd <- sdLong %>% dplyr::select(id, Grouping, SleepStage, Location, Band, relPower, absPower, slowWave, fastWave, slowingRatio, DAR, AHI)
