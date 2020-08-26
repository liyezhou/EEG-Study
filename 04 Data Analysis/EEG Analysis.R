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
    lme.model = map(data, ~ lme(relPower ~ Location * Grouping, random=~1|id, data=.x)),
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
  select(relPower, ArI, TS90, MinSaO2, ODI, epworth, N1P, N2P, REMP, sleepEff, AHI, Band) %>% 
  # nest() %>% 
  group_walk(
    ~PerformanceAnalytics::chart.Correlation(.x, method = "pearson") %>% 
      plotExport(glue("export/0{.y} {.y[[1]]}.pdf"))
  )

## Linear Model: RelPower & Specific Items
lm.metrics <- sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, ArI, TS90, MinSaO2, ODI, N2P, sleepEff, Band) %>% 
  pivot_longer(cols = c(ArI, TS90, MinSaO2, ODI, N2P, sleepEff), names_to = "Metric", values_to = "value") %>% 
  group_by(Band, Metric) %>%
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
  select(relPower, ArI, TS90, MinSaO2, ODI, N2P, sleepEff, Band) %>% 
  pivot_longer(cols = c(ArI, TS90, MinSaO2, ODI, N2P, sleepEff), names_to = "Metric", values_to = "value") %>% 
  group_by(Band, Metric) %>%
  group_map(
    ~{
      Hmisc::rcorr(.x$relPower, .x$value) %>% 
      {bind_cols(.y, tidy(.) %>% add_significance("p.value"))}
    }
    # ~broom::tidy(lm(relPower ~ ArI, data = .x))
  ) %>% bind_rows() %>% mutate(Y = "relPower" , X = Metric, R = estimate, R.squared = estimate ^ 2) %>% 
  select(Band, Y, X, n, R, R.squared, p.value, p.value.signif)

corr.metrics %>%  knitr::kable(caption = "Frontal, REM Sleep: correlation with Relative Power")

# sdProcessed  %>% filter(Band == "delta", Location == "Frontal", SleepStage == "REM") %$% cor.test(relPower, ArI)

## Correlation Plot: RelPower & Specific Items
sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, ArI, TS90, MinSaO2, ODI, N2P, sleepEff, Band) %>% 
  pivot_longer(cols = c(ArI, ODI, N2P), names_to = "Metric", values_to = "value") %>% 
  filter(Band %!in% c("sigma", "alpha")) %>% 
  # group_by(Band, Metric) %>%
  # nest() %>% 
  # {.$data[[1]]} %>% 
  # ggscatter(x = "relPower", y = "value",
  #           # palette = "jco",
  #           add = "reg.line", conf.int = TRUE)
  ggplot(palette = "jco") + aes(value, relPower) + geom_point(size = 0.6, color = "#888888") +
  geom_smooth(method=lm, formula = y ~ x) + 
  xlab("Value of ArI, N2P or ODI") +
  ylab("Relative Power (%)") +
  stat_cor(aes(label = paste(..r.label.., ..p.label.., cut(..p.., 
                                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                          labels = c("'***'", "'**'", "'*'", "''")), 
                        sep = "~")),
           label.x.npc = "right", hjust = "right"
           ) +
  facet_wrap(~Band + Metric, scales = "free", ncol = 3)
ggsave(filename = "Correlation Plots - ODI, ArI, N2P.pdf", width = 9, height = 12)
  
## Comorbidities
comorbidity.t.test <- sdProcessed %>% filter(Location == "Frontal", SleepStage == "REM") %>% 
  select(relPower, ArI, TS90, MinSaO2, ODI, N2P, sleepEff, Band) %>% 
  pivot_longer(cols = c(ArI, TS90, MinSaO2, ODI, N2P, sleepEff), names_to = "Metric", values_to = "value") %>% 
  group_by(Band, Metric) %>%
  group_map(
    ~{
      Hmisc::rcorr(.x$relPower, .x$value) %>% 
        {bind_cols(.y, tidy(.) %>% add_significance("p.value"))}
    }
    # ~broom::tidy(lm(relPower ~ ArI, data = .x))
  ) %>% bind_rows() %>% mutate(Y = "relPower" , X = Metric, R = estimate, R.squared = estimate ^ 2) %>% 
  select(Band, Y, X, n, R, R.squared, p.value, p.value.signif)
















