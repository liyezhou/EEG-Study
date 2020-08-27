# library(epicalc)


glimpse(sd)

#### T-Test between specific grouops ####
# sd %>% group_by(Grouping) %>% summarise(n_distinct(id))

# sd.t.test <- sd %>% filter(SleepStage == "REM", Location == "Frontal", Band == "delta") %>%
#   group_by(Grouping) %>%
#   nest() %>%
#   mutate(data = map(data, ~unlist(.$relPower))) %>% 
#   pivot_wider(names_from = Grouping, values_from = data) %>% 
#   map(unlist)
# 
# 
# t.test(sd.t.test$`Snoring only`, sd.t.test$`Mild OSA`)

#### Plotting relPower by Band, in each location and each sleep stage ####
# Bar plots
bxp <- ggbarplot(
  sd, x = "Band", y = "relPower", fill = "Grouping", color = "gray12", add = "mean_ci",
  palette = "jco", facet.by = c("Location","SleepStage"), 
  xlab = "Frequency Bands", ylab = "Relative Power (%)",
  position = position_dodge(0.8)
) %>% ggpar(ylim = c(9, 24))
bxp


# Box plots
bxp <- ggboxplot(
  sd, x = "Band", y = "relPower", color = "Grouping",
  palette = "jco", outlier.shape = NA, facet.by = c("Location","SleepStage"), 
  xlab = "Frequency Bands", ylab = "Relative Power (%)", 
)
bxp




# T Tests
sd.test <- sd %>%
  group_by(Location, SleepStage, Band) %>%
  t_test(relPower ~ Grouping)
write.table(sd.test, "T Test Results.txt")
sd.test$.y. <- NULL

sd.test <- sd.test %>%
  add_xy_position(x = "Band", dodge = 0.8, step.increase = 0.05, fun = "mean_ci")
# pdf("relPowerVSband_All.pdf", 10, 13)
(bxp.complex <- bxp + 
  stat_pvalue_manual(
    sd.test, label = "p.adj.signif", tip.length = 0.00,
    step.increase = 0, step.group.by = c("Location","SleepStage"),
    bracket.nudge.y = 0, hide.ns = TRUE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))))
# dev.off()

ggexport(bxp.complex, width = 9, height = 12, pointsize = 30, filename = "relPowerVSband_All_Bar.pdf")
gc()


####  Post Hoc Analysis to determine if the change in relPower is significantly different between locations ####
library(nlme)
sink("Changes in Relative EEG Power in Severe OSA moderated by Location.txt")
sd.lme <- sd %>% 
  filter(SleepStage == "REM") %>%
  filter(Location != "Central") %>%
  # filter(Grouping != "Mild/Moderate OSA") %>%
  group_by(Band) %>% 
  nest() %>% 
  mutate(
    lme.model = map(data, ~ nlme::lme(relPower ~ Location * Grouping, random=~1|id, data=.x)),
    # lme.model = map(data, ~ lm(relPower ~ Location * Grouping, data=.x)),
    lme.anova = map(lme.model, anova),
    lme.summary = map(lme.model, summary)
  )
sd.lme$lme.summary %T>% {names(.) <- sd.lme$Band}
sink()
sd.lme$lme.anova
plot(sd.lme$lme.model[[6]])
anova(sd.lme$lme.model[[2]])

ann_text <- data.frame(Band = "theta", lab = "Text", relPower = 11.5, Location = "Frontal", Grouping = factor("Severe OSA"))



sd %>% 
  filter(SleepStage == "REM") %>%
  filter(Location != "Central") %>%
  ggplot() + 
  aes(x = Grouping, y = relPower, color = Location, group = Location) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  facet_wrap(~Band, scales = "free_y") 
# geom_text(data = ann_text, label = "HAHA")
geom_text(data = ~(print(.x)))

#### Correlation between relPower and clinical variables ####

## Correlation Matrix
sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  group_by(Band) %>% 
  select(relPower, ArI, TS90, MinSaO2, ODI, epworth, N1P, N2P, REMP, sleepEff, AHI, Band, moca) %>% 
  # nest() %>% 
  group_walk(
    ~PerformanceAnalytics::chart.Correlation(.x, method = "pearson") %>% 
      plotExport(glue("export/0{.y} {.y[[1]]}.pdf"))
  )

## Linear Model: RelPower & Specific Items
lm.metrics <- sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, ArI, TS90, MinSaO2, ODI, N2P, sleepEff, Band) %>% 
  pivot_longer(cols = c(ArI, TS90, MinSaO2, ODI, N2P, sleepEff), names_to = "Metric", values_to = "value") %>% 
  group_by(Metric, Band) %>%
  group_map(
    ~{
      lm(value ~ scale(relPower), data = .x) %>% {bind_cols(.y, tidy(., conf.int = T)[2,] %>% add_significance("p.value"))}
    }
    # ~broom::tidy(lm(relPower ~ ArI, data = .x))
  ) %>% bind_rows() %>% mutate(Y = Metric , X = "relPower") %>% select(Band, Y, X, estimate, p.value, p.value.signif)

lm.metrics %>% knitr::kable(caption = "Frontal, REM Sleep: predictive power of Relative Power (scaled to mean=0, SD=1)")

# sdProcessed  %>% filter(Band == "delta", Location == "Frontal", SleepStage == "REM") %>% Hmisc::rcorr(relPower ~ AHI, data = .) %>% summary

## Correlation Test: RelPower & Specific Items
corr.metrics <- sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, ArI, TS90, MinSaO2, ODI, N2P, sleepEff, N1P, N2P, N3P, REMP, moca, mmse, epworth, pisq, Band) %>% 
  pivot_longer(cols = c(ArI, TS90, MinSaO2, ODI, N2P, sleepEff, N1P, N2P, N3P, REMP, moca, mmse, epworth, pisq), names_to = "Metric", values_to = "value") %>% 
  group_by(Metric, Band) %>%
  group_map(
    ~{
      Hmisc::rcorr(.x$relPower, .x$value) %>% 
      {bind_cols(.y, tidy(.) %>% add_significance("p.value"))}
    }
    # ~broom::tidy(lm(relPower ~ ArI, data = .x))
  ) %>% bind_rows() %>% mutate(Y = "relPower" , X = Metric, R = estimate, R.squared = estimate ^ 2) %>% 
  select(Band, Y, X, n, R, R.squared, p.value, p.value.signif)

sink("Correlation between relPower and Clinical Variables.txt")
cat("---- Correlation between relPower and Clinical Variables ---- ")
corr.metrics %>%  knitr::kable(caption = "Frontal, REM Sleep: correlation with Relative Power")
sink()

# sdProcessed  %>% filter(Band == "delta", Location == "Frontal", SleepStage == "REM") %$% cor.test(relPower, ArI)

## Correlation Plot: RelPower & Specific Items
sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, ArI, TS90, MinSaO2, ODI, N2P, sleepEff, Band) %>% 
  pivot_longer(cols = c(ArI, MinSaO2, TS90), names_to = "Metric", values_to = "value") %>% 
  filter(Band %!in% c("alpha")) %>% 
  # group_by(Band, Metric) %>%
  # nest() %>% 
  # {.$data[[1]]} %>% 
  # ggscatter(x = "relPower", y = "value",
  #           # palette = "jco",
  #           add = "reg.line", conf.int = TRUE)
  ggplot(palette = "jco") + aes(value, relPower) + geom_point(size = 0.6, color = "#888888") +
  geom_smooth(method=lm, formula = y ~ x) + 
  xlab("Value of ArI, MinSaO2, TS90") +
  ylab("Relative Power (%)") +
  stat_cor(aes(label = paste(..r.label.., ..p.label.., cut(..p.., 
                                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                          labels = c("'***'", "'**'", "'*'", "''")), 
                        sep = "~")),
           method = "pearson",
           label.x.npc = "right", hjust = "right"
           ) +
  facet_wrap(~Band + Metric, scales = "free", ncol = 3)
ggsave(filename = "Correlation Plots - ArI, MinSaO2, TS90.pdf", width = 9, height = 12)

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










