gapminder_orig <- read.csv("/Users/yezhouli/Downloads/gapminder-FiveYearData.csv")
# define a copy of the original dataset that we will clean and play with 
gapminder <- gapminder_orig
continent_year <- gapminder %>% distinct(continent, year)
continents <- continent_year %>% pull(continent) %>% as.character
years <- continent_year %>% pull(year)

gapminder %>% 
  filter(continent == .x,
         year == .y)


gapminder_nested <- gapminder %>% 
  group_by(continent) %>% 
  nest()
gapminder_nested$data[[1]]

gapminder_nested %>% unnest(cols = everything())

tidy(mod)

mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

.x <- gapminder_nested %>% pluck("data", 1)
b <- gapminder_nested %>% 
  mutate(avg_lifeExp = map(data, ~{mean(.x$lifeExp)}))

a %>% mutate(test = map(lm.model, ~ pluck(summary(.)) ))

a %>% pluck("lm.model") %>% map(summary) %>%  map_dbl("r.squared")


(
  a <- gapminder %>% 
    group_by(continent) %>% 
    nest() %>% 
    mutate(lm.model = map(data, ~lm(lifeExp ~ pop + year + gdpPercap, data = .))) %>% 
    mutate(summary = map(lm.model, ~tidy(.))) %>% 
    unnest(col=summary) %>% 
    dplyr::select(-data, -lm.model) %>% 
    ungroup()
)

gapminder_list <- gapminder %>% split(gapminder$continent)

gapminder_list %>% map_dbl(~{mean(.$lifeExp) > 50 })
gapminder %>% split(gapminder$continent)
gapminder_list %>%
  reduce(rbind)

a %>% pluck("lm.model", 1)
gapminder %>% pluck(2)
gapminder %>% 
  group_by(continent) %>% 
  nest() %>% pluck("data", 1, "pop")
