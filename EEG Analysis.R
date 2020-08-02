library(epicalc)
library(ggsignif)
(pat <- sd %>% dplyr::select(id, group, AHI, SleepStage, Location, Band, absPower, relPower, powerSum) %>% 
  group_by(SleepStage, Location, Band, group) %>% 
  summarise(meanRelPower = mean(relPower),
            sampleSize = n(),
            error = qnorm(0.975) * sd(relPower) / sqrt(sampleSize), 
            lowerCI = (t.test(relPower)$conf.int[[1]]),
            upperCI = (t.test(relPower)$conf.int[[2]])) )




a <- mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

list(t.test(sd$age), t.test(sd$weight)) %>% map_dbl("estimate")


tibble(vec_col = 1:10) %>%
  mutate(vec_sum = sum(vec_col))


pat

ggplot(pat, aes(x = Band, y = meanRelPower, fill = group)) + 
  geom_col(position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(0.1,0.21)) + 
  xlab("Bands") +
  ylab("Relative Power") +
  geom_errorbar(aes(ymin=meanRelPower-error, ymax=meanRelPower+error),
  # geom_errorbar(aes(ymin=lowerCI, ymax=upperCI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~SleepStage + Location) 
ggsave("Relative Power in bands.pdf", width = 12, height = 8)

data("PlantGrowth")
PlantGrowth$subgroup = ifelse(PlantGrowth$weight > 5, "good", "bad")

t.test

library(ggpubr)
ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_se",
          color = "supp", palette = "jco", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = supp), label = "p.signif", label.y = 29)

gapminder_nested <- gapminder_nested %>% 
  mutate(lm_obj = map(data, ~lm(lifeExp ~ pop + gdpPercap + year, data = .x)))
gapminder_nested <- gapminder_nested %>% 
  mutate(pred = map2(lm_obj, data, function(.lm, .data) predict(.lm, .data)))
gapminder_nested <- gapminder_nested %>% 
  mutate(cor = map2_dbl(pred, data, function(.pred, .data) cor(.pred, .data$lifeExp)))
gapminder_nested$pred
gapminder_nested <- gapminder_nested %>% mutate(lifeExpr = map(data, ~.x$lifeExp))
(gapminder_nested$data) %>% map(~.$lifeExp)

y <- rnorm(100) + 1:100 
x <- 1:100
mod <- lm(y ~ x)
newData <- data.frame(x = 1:5)
predict(mod, newData)

foo <- list( str='R', vec=c(1,2,3), bool=TRUE )
bar <- list( mat=matrix(0,nrow=2,ncol=2), rand=rnorm(1) )
foo[ 2:3 ] <- bar
foo
