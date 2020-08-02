library(epicalc)
library(ggsignif)
# (pat <- sd %>% 
#   group_by(SleepStage, Location, Band, Grouping) %>% 
#   summarise(meanRelPower = mean(relPower),
#             sampleSize = n(),
#             error = qnorm(0.975) * sd(relPower) / sqrt(sampleSize), 
#             lowerCI = (t.test(relPower)$conf.int[[1]]),
#             upperCI = (t.test(relPower)$conf.int[[2]])) )

# Bar plots
bp <- ggbarplot(
  sd, x = "Band", y = "relPower", color = "Grouping", add = "mean.ci",
  palette = "jco", facet.by = c("Location","SleepStage"), 
  xlab = "Frequency Bands", ylab = "Relative Power (%)",
  position = position_dodge(0.8)
)
bp


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

sd.test <- sd %>%
  group_by(Location, SleepStage, Band) %>%
  t_test(relPower ~ Grouping)
sd.test 

sd.test <- sd.test %>%
  add_xy_position(x = "Band", dodge = 0.8, step.increase = 0.05, fun = "mean_ci")
pdf("relPowerVSband_All.pdf", 10, 13)
(bxp.complex <- bxp + 
  stat_pvalue_manual(
    sd.test, label = "p.adj.signif", tip.length = 0.00,
    step.increase = 0, step.group.by = c("Location","SleepStage"),
    bracket.nudge.y = 2, hide.ns = TRUE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))))
dev.off()

ggexport(bxp.complex, width = 9, height = 12, pointsize = 30, filename = "relPowerVSband_All_Violin.pdf")
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

