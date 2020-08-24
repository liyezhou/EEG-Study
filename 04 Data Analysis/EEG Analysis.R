library(epicalc)
library(ggsignif)


### Plotting
sd %>% group_by(Grouping) %>% summarise(n_distinct(id))

sd.t.test <- sd %>% filter(SleepStage == "REM", Location == "Frontal", Band == "delta") %>%
  group_by(Grouping) %>%
  nest() %>%
  mutate(data = map(data, ~unlist(.$relPower))) %>% 
  pivot_wider(names_from = Grouping, values_from = data)

t.test(unlist(sd.t.test$`Snore only`), unlist(sd.t.test$`Mild OSA`))

# Bar plots

bxp <- ggbarplot(
  sd, x = "Band", y = "relPower", fill = "Grouping", color = "gray12", add = "mean_ci",
  palette = "jco", facet.by = c("Location","SleepStage"), 
  xlab = "Frequency Bands", ylab = "Relative Power (%)",
  position = position_dodge(0.8)
) %>% ggpar(ylim = c(9, 24))
bxp


# 
# bxp <- bxp + 
#   stat_pvalue_manual(
#     sd.test, label = "p.adj.signif", tip.length = 0.00,
#     bracket.nudge.y = -0.02
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0)))
# Box plots
bxp <- ggviolin(
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

# 
# ggplot(pat, aes(x = Band, y = meanRelPower, fill = Grouping)) +
#   geom_col(position=position_dodge(.9)) +
#   coord_cartesian(ylim=c(0.1,0.21)) +
#   xlab("Bands") +
#   ylab("Relative Power") +
#   geom_errorbar(aes(ymin=meanRelPower-error, ymax=meanRelPower+error),
#   # geom_errorbar(aes(ymin=lowerCI, ymax=upperCI),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   facet_wrap(~SleepStage + Location)
# ggsave("Relative Power in bands.pdf", width = 12, height = 8)

