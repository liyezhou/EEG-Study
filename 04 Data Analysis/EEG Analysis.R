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
sd.test$.y. <- NULL
write.table(sd.test, "T Test Results.txt")

sd.test <- sd.test %>%
  add_xy_position(x = "Band", dodge = 0.8, step.increase = 0.05, fun = "mean_ci")
# pdf("relPowerVSband_All.pdf", 10, 13)
(bxp.complex <- bxp + 
  stat_pvalue_manual(
    sd.test, label = "p.adj.signif", tip.length = 0.00,
    step.increase = 0, step.group.by = c("Location","SleepStage"),
    bracket.nudge.y = -0.2, hide.ns = TRUE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))))
# dev.off()

ggexport(bxp.complex, width = 9, height = 12, pointsize = 30, filename = "relPowerVSband_All_Bar.pdf")
gc()

#### Using absPower ####
# Bar plots
bxp <- ggbarplot(
  sd, x = "Band", y = "absPower", fill = "Grouping", color = "gray12", add = "mean_ci",
  palette = "jco", facet.by = c("Location","SleepStage"), 
  xlab = "Frequency Bands", ylab = "Absolute Power (dB)",
  position = position_dodge(0.8)
) %>% ggpar(ylim = c(25, 68))
bxp


# Box plots
bxp <- ggboxplot(
  sd, x = "Band", y = "absPower", color = "Grouping",
  palette = "jco", outlier.shape = NA, facet.by = c("Location","SleepStage"), 
  xlab = "Frequency Bands", ylab = "Absolute Power (dB)", 
)
bxp




# T Tests
sd.test <- sd %>%
  group_by(Location, SleepStage, Band) %>%
  t_test(absPower ~ Grouping)
sd.test$.y. <- NULL
write.table(sd.test, "T Test Results.txt")

sd.test <- sd.test %>%
  add_xy_position(x = "Band", dodge = 0.8, step.increase = 0.05, fun = "mean_ci")
# pdf("absPowerVSband_All.pdf", 10, 13)
(bxp.complex <- bxp + 
    stat_pvalue_manual(
      sd.test, label = "p.adj.signif", tip.length = 0.00,
      step.increase = 0, step.group.by = c("Location","SleepStage"),
      bracket.nudge.y = -1, hide.ns = TRUE
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))))
# dev.off()

ggexport(bxp.complex, width = 9, height = 12, pointsize = 30, filename = "absPowerVSband_All_Bar.pdf")
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





