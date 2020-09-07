#### Comorbidities ####
# Normality Test
sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, Band, snore, apnea, pnd, nocturia, plm) %>% 
  group_by(Band) %>% 
  group_map(
    ~{
      shapiro_test(.x, relPower) %>% 
        {bind_cols(.y, .)} %>% add_significance("p")
    }
  ) %>% bind_rows() %>% knitr::kable(digits = 4)

# Wilcox test
comorbidity.wilcox.test <- sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, Band, snore:liver.disease) %>%
  pivot_longer(cols = c(snore:liver.disease), names_to = "Comorbidity", values_to = "value") %>%
  # select(relPower, Band, snore:liver.disease) %>% 
  # pivot_longer(cols = c(snore, apnea, cad, memory.impairment, plm), names_to = "Comorbidity", values_to = "value") %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  group_by(Comorbidity, Band) %>%
  group_map(
    ~{
      if (sum(.x$value == 1) > 1) {
        # t_test(.x, relPower ~ value) %>% 
        wilcox_test(.x, relPower ~ value) %>% 
          {bind_cols(.y, .)}
      }
    }
  ) %>% bind_rows() %>% 
  mutate(Y = "relPower" , X = Comorbidity, N.healthy = n1, N.diseased = n2) %>% 
  select(Y, X, Band, N.healthy, N.diseased, p) %>% 
  mutate(p.adj = p.adjust(p, method = "BH")) %>% 
  # filter(p <= 0.05) %>% 
  add_significance("p") %T>%
  {knitr::kable(., digits = 4) %>% print}


sink("Correlation between relPower and Comorbidities.txt")
cat("---- Correlation between relPower and Comorbidities ---- ")
comorbidity.wilcox.test %>% {knitr::kable(., digits = 4) %>% print}
sink()

# Binomial Model accounting for age, bmi etc
sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM", Band == "delta") %>% 
  mutate(memory.impairment = ifelse(is.na(memory.impairment), 0, memory.impairment)) %>%
  glm(memory.impairment ~ relPower + age, data =., family = "binomial") %>% 
  summary
as.factor(sdProcessed$gender)

comorbidity.lm <- sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, Band, snore:liver.disease, age) %>% 
  pivot_longer(cols = c(snore:liver.disease), names_to = "Comorbidity", values_to = "value") %>% 
  # pivot_longer(cols = c(snore, apnea, cad, memory.impairment, plm), names_to = "Comorbidity", values_to = "value") %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  group_by(Comorbidity, Band) %>%
  group_map(
    ~{
      if (sum(.x$value == 1) > 1) {
        glm(value ~ relPower + age, data = .x, family = "binomial") %>% {bind_cols(.y, tidy(., conf.int = T))} %>% filter(term == "relPower")
      }
    }
  ) %>% 
  bind_rows() %>%
  mutate(Y = Comorbidity , X = "relPower") %>% select(Band, Y, X, term, estimate, p.value) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  # filter(p.value <= 0.05) %>%
  add_significance("p.value") %T>%
  {knitr::kable(., digits = 4) %>% print}

sdProcessed %>% group_by(memory.impairment) %>% summarise(n_unique(id))

# Normality Test
sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, Band, snore, apnea, pnd, nocturia, plm) %>% 
  mutate(snore = ifelse(is.na(snore), 0, snore)) %>% 
  group_by(Band) %>% 
  group_map(
    ~{
      wilcox_test(.x, relPower ~ snore)
    }
  )


# Binomial Model predicting using Grouping instead of relPower
comorbidity.grouping.lm <- sdPhenotype %>% 
  select(snore:liver.disease, age, Grouping) %>% 
  pivot_longer(cols = c(snore:liver.disease), names_to = "Comorbidity", values_to = "value") %>% 
  # pivot_longer(cols = c(snore, apnea, cad, memory.impairment, plm), names_to = "Comorbidity", values_to = "value") %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  group_by(Comorbidity) %>%
  group_map(
    ~{
      if (sum(.x$value == 1) > 1) {
        glm(value ~ Grouping + age, data = .x, family = "binomial") %>% {bind_cols(.y, tidy(., conf.int = T))} %>% filter(term %!in% c("(Intercept)"))
      }
    }
  ) %>% 
  bind_rows() %>%
  mutate(Y = Comorbidity , X = "Grouping") %>% select(Y, X, term, estimate, p.value) %>% 
  # mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  # filter(p.value <= 0.05) %>%
  add_significance("p.value") %T>%
  {knitr::kable(., digits = 4) %>% print}

