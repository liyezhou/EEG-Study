write.csv(, "EEG Patients.csv")

sdWider <-  sd %>% select(-Grouping) %>% 
  unite("configuration", c(SleepStage, Location, Band), sep = ".") %>% 
  mutate(configuration = map_chr(configuration, ~paste(.x, ".relPower", sep = "")))   %>% 
  pivot_wider(names_from = configuration, values_from = relPower) 

sdNew <- sd_raw %>% inner_join(sdWider, by = "id")

write_excel_csv(sdNew, "EEG Patients with Relative Power.csv")

write_excel_csv(sd.test, "T Tests.csv")
data(example_twins)
library(DCL)
