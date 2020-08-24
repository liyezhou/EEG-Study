#### Non-Faceted ####
df <- ToothGrowth
df$dose <- as.factor(df$dose)
skim(df)

stat.test <- df %>%
  group_by(dose) %>%
  t_test(len ~ supp) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test

# Create a box plot
bxp <- ggboxplot(
  df, x = "dose", y = "len", 
  color = "supp", palette = c("#00AFBB", "#E7B800")
)
bxp

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "dose", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)

# Add 10% spaces between the p-value labels and the plot border
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Use adjusted p-values as labels
# Remove brackets
bxp + stat_pvalue_manual(
  stat.test,  label = "p.adj", tip.length = 0,
  remove.bracket = TRUE
)

# Show adjusted p-values and significance levels
# Hide ns (non-significant)
bxp + stat_pvalue_manual(
  stat.test,  label = "{p.adj}{p.adj.signif}", 
  tip.length = 0, hide.ns = TRUE
)

##### Adding inter-group comparisons
# Additional statistical test
stat.test2 <- df %>%
  t_test(len ~ dose, p.adjust.method = "bonferroni")
stat.test2

# Add p-values of `stat.test` and `stat.test2`
# 1. Add stat.test
stat.test <- stat.test %>%
  add_xy_position(x = "dose", dodge = 0.8)
bxp.complex <- bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)
bxp.complex

# 2. Add stat.test2
# Add more space between brackets using `step.increase` 
stat.test2 <- stat.test2 %>% add_xy_position(x = "dose")
bxp.complex <- bxp.complex + 
  stat_pvalue_manual(
    stat.test2,  label = "p", tip.length = 0.02,
    step.increase = 0.05,
    bracket.nudge.y = 2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

# 3. Display the plot
bxp.complex 


### Grouped bar plots
# Create a bar plot with error bars (mean +/- sd)
bp <- ggbarplot(
  df, x = "dose", y = "len", add = "mean_sd", 
  color= "supp", palette = c("#00AFBB", "#E7B800"),
  position = position_dodge(0.8)
)
bp
# Add p-values onto the bar plots
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "dose", dodge = 0.8) 
bp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0.01
)

# Move down the brackets using `bracket.nudge.y`
bp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0,
  bracket.nudge.y = -2
)

## Stacked bar plots
# Create a bar plot with error bars (mean +/- sd)
bp2 <- ggbarplot(
  df, x = "dose", y = "len", add = "mean_sd", 
  color = "supp", palette = c("#00AFBB", "#E7B800"),
  position = position_stack()
)
bp2
# Add p-values onto the bar plots
# Specify the p-value y position manually
bp2 + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0.01,
  x = "dose", y.position = c(30, 45, 60)
)

# Auto-compute the p-value y position
# Adjust vertically label positions using vjust
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "dose", stack = TRUE) 
bp2 + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  remove.bracket = TRUE, vjust = -3
)


## Grouped line plots
# Create a line plot with error bars (mean +/- sd)
lp <- ggline(
  df, x = "dose", y = "len", add = "mean_sd", 
  color = "supp", palette = c("#00AFBB", "#E7B800")
)

# Add p-values onto the line plots
# Remove brackets using linetype = "blank"
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "dose") 
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  tip.length = 0, linetype  = "blank"
)

# Move down the significance levels using vjust
lp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", 
  linetype  = "blank", vjust = 2
)

## Pairwise comparisons
pwc <- df %>%
  group_by(supp) %>%
  t_test(len ~ dose, p.adjust.method = "bonferroni")
pwc

# Box plot
pwc <- pwc %>% add_xy_position(x = "dose")

bxp +
  stat_pvalue_manual(
    pwc, color = "supp", step.group.by = "supp",
    tip.length = 0, step.increase = 0.1
  )

# Bar plots
pwc <- pwc %>% add_xy_position(x = "dose", fun = "mean_sd", dodge = 0.8)
bp + stat_pvalue_manual(
  pwc, color = "supp", step.group.by = "supp",
  tip.length = 0, step.increase = 0.1
)

# Line plots
pwc <- pwc %>% add_xy_position(x = "dose", fun = "mean_sd")
lp + stat_pvalue_manual(
  pwc, color = "supp", step.group.by = "supp",
  tip.length = 0, step.increase = 0.1
)

# Bar plots (dodged)
# Take a subset of the pairwise comparisons
pwc.filtered <- pwc %>% 
  add_xy_position(x = "dose", fun = "mean_sd", dodge = 0.8) %>%
  filter(supp == "VC")
bp +
  stat_pvalue_manual(
    pwc.filtered, color = "supp", step.group.by = "supp",
    tip.length = 0, step.increase = 0
  )


### 3 groups by x position
# Box plots
bxp <- ggboxplot(
  df, x = "supp", y = "len", fill = "dose",
  palette = "npg"
)
bxp

# Bar plots
bp <- ggbarplot(
  df, x = "supp", y = "len", fill = "dose",
  palette = "npg", add = "mean_ci",
  position = position_dodge(0.8)
)
bp

stat.test <- df %>%
  group_by(supp) %>%
  t_test(len ~ dose)
stat.test 

# Box plots with p-values
stat.test <- stat.test %>%
  add_xy_position(x = "supp", dodge = 0.8)
bxp + 
  stat_pvalue_manual(
    stat.test, label = "p.adj", tip.length = 0.01,
    bracket.nudge.y = -2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Bar plots with p-values
stat.test <- stat.test %>%
  add_xy_position(x = "supp", fun = "mean_ci", dodge = 0.8)
bp + 
  stat_pvalue_manual(
    stat.test, label = "p.adj", tip.length = 0.01,
    bracket.nudge.y = -2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


# pairwise comparison against reference group
stat.test <- df %>%
  group_by(supp) %>%
  t_test(len ~ dose, ref.group = "0.5") 
stat.test

# Box plots with p-values
stat.test <- stat.test %>%
  add_xy_position(x = "supp", dodge = 0.8)
bxp + 
  stat_pvalue_manual(
    stat.test, label = "p.adj", tip.length = 0.01,
    bracket.nudge.y = -2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Show only significance levels
# Move down significance symbols using vjust
bxp + stat_pvalue_manual(
  stat.test, x = "supp",  label = "p.adj.signif", 
  tip.length = 0, vjust = 2
)

# Bar plots with p-values
stat.test <- stat.test %>%
  add_xy_position(x = "supp", fun = "mean_sd", dodge = 0.8)
bp + 
  stat_pvalue_manual(
    stat.test, label = "p.adj", tip.length = 0.01,
    bracket.nudge.y = -2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#### END ####

#### Faceted ####
library(ggpubr)
library(rstatix)

# Transform `dose` into factor variable
df <- ToothGrowth
df$dose <- as.factor(df$dose)
# Add a random grouping variable
df$group <- factor(rep(c("grp1", "grp2"), 30))
head(df, 20)

stat.test <- df %>%
  group_by(dose) %>%
  t_test(len ~ supp) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test 

# Create a box plot
bxp <- ggboxplot(
  df, x = "supp", y = "len", fill = "#00AFBB", 
  facet.by = "dose"
)
bxp

# Make facet and add p-values
stat.test <- stat.test %>% add_xy_position(x = "supp")
bxp + stat_pvalue_manual(stat.test)

# Make the facet scale free and add jitter points
# Move down the bracket using `bracket.nudge.y`
# Hide ns (non-significant)
# Show adjusted p-values and significance levels
# Add 10% spaces between the p-value labels and the plot border
bxp <- ggboxplot(
  df, x = "supp", y = "len", fill = "#00AFBB", 
  facet.by = "dose", scales = "free", add = "jitter"
)
bxp

bxp +  
  stat_pvalue_manual(
    stat.test, bracket.nudge.y = -2, hide.ns = TRUE,
    label = "{p.adj}{p.adj.signif}"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

# Create a bar plot with error bars (mean +/- sd)
bp <- ggbarplot(
  df, x = "supp", y = "len", add = "mean_sd", 
  fill = "#00AFBB", facet.by = "dose"
)
bp
# Add p-values onto the bar plots
stat.test <- stat.test %>% add_xy_position(fun = "mean_sd", x = "supp")
bp + stat_pvalue_manual(stat.test)

# Create a bar plot with error bars (mean +/- sd)
bp <- ggbarplot(
  df, x = "supp", y = "len", add = c("mean_sd"), 
  fill = "#00AFBB", facet.by = "dose"
)

# Add p-values onto the bar plots
stat.test <- stat.test %>% add_xy_position(fun = "max", x = "supp")
bp + stat_pvalue_manual(stat.test)
