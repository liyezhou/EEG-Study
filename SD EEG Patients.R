library(tidyverse)
library(skimr)
sd_raw <- readxl::read_xls("EEG Patients.xls")
colnames(sd_raw) <- make.names(colnames(sd_raw))

# Tidy Data
sd <- sd_raw %>% 
  pivot_longer(cols = 94:129, names_to = "config", values_to = "absPower") %>% 
  separate(col = config, sep = "[.]", into = c("SleepStage", "Location", "Band")) %>%
  mutate(SleepStage = fct_recode(SleepStage, REM = "R", NREM = "NR"),
         Location = fct_recode(Location, Frontal = "f", Central = "c", Occipital = "o"),
         Band = factor(fct_recode(Band, alpha = "a", theta = "t", beta = "b",  sigma = "s", delta = "d", gamma = "g"), levels = c("delta", "theta", "alpha", "sigma", "beta", "gamma")),
         group = as.factor(group)) %>% 
  group_by(id, SleepStage, Location) %>% 
  mutate(powerSum = sum(absPower), num = n()) %>% 
  ungroup() %>% 
  arrange(id, SleepStage, Location) %>% 
  group_by(SleepStage, Location, Band) %>% 
  mutate(relPower = absPower / powerSum) %>% 
  ungroup()

