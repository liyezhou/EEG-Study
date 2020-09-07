
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

