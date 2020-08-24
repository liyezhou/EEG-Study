library(skimr)
library(rstatix)
library(ggpubr)
library(glue)
library(tidyverse)
sd_raw <- readxl::read_xls("EEG Patients.xls")
colnames(sd_raw) <- make.names(colnames(sd_raw))

# Tidy Data
sdProcessed <- sd_raw %>% 
  pivot_longer(cols = 94:129, names_to = "config", values_to = "absPower") %>% 
  separate(col = config, sep = "[.]", into = c("SleepStage", "Location", "Band")) %>%
  mutate(SleepStage = factor(fct_recode(SleepStage, REM = "R", NREM = "NR"), levels = c("REM", "NREM")),
         Location = factor(fct_recode(Location, Frontal = "f", Central = "c", Occipital = "o"), levels = c("Frontal", "Central", "Occipital")),
         Band = factor(fct_recode(Band, alpha = "a", theta = "t", beta = "b",  sigma = "s", delta = "d", gamma = "g"), levels = c("delta", "theta", "alpha", "sigma", "beta", "gamma")),
         Grouping = fct_recode(as.factor(Grouping), "Snoring only" = "1", "Mild/Moderate OSA" = "2", "Severe OSA" = "3")) %>% 
  group_by(id, SleepStage, Location) %>% 
  mutate(powerSum = sum(absPower), num = n()) %>% 
  ungroup() %>% 
  arrange(id, SleepStage, Location) %>% 
  group_by(SleepStage, Location, Band) %>% 
  mutate(relPower = absPower / powerSum * 100) %>% 
  ungroup()

sdLong <- sdProcessed %>% dplyr::select(id, Grouping, SleepStage, Location, Band, relPower, bmi, age, tst, sleepEff, sleepLatency, N1P, N2P, N3P, REMP, REMLatency, ODI, AHI, ArI1, ArI2, epworth, education, mmse, pisq, moca)
sd <- sdLong %>% dplyr::select(id, Grouping, SleepStage, Location, Band, relPower, AHI)


