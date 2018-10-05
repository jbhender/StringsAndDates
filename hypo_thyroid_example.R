## An example of period vs duration arithmetic
## using "hypothyroidism" diagnoses

# libraries: ------------------------------------------------------------------
library(tidyverse); library(lubridate); library(magrittr)

# Sample n random dates that someone has a diagnosis of hypothyroidism: -------
n = 1e3
start = as.numeric(ymd('2008-01-01'))
end = as.numeric(ymd('2016-12-31'))

hypo_thy = tibble(
   id = sample(LETTERS, 1e3, replace = TRUE),
   date = as_date(sample(start:end, n, replace = TRUE))
)

# Find each second instance within a years: -----------------------------------
hypo_thy %<>%
  arrange(id, date) %>%
  group_by(id) %>%
  mutate(delta = c(Inf, diff(date, unit = 'days')))

hypo_thy %<>% mutate(episode = cumsum(delta > 365))

hypo_thy %>% group_by(episode) %>% summarize(n=n())

hypo_thy %>%
  group_by(id, episode) %>%
  summarize(elg_date = date[2],
            end_date = max(date) + duration(1, 'year'),
            end_date2 = max(date) %m+% period(12, 'months') # months(12)
  ) %>% filter(end_date!=end_date2)

# Exercise - within ids, group dates into episodes if they appear within one
# calendar year of each other.  Then find the third episode within each month,
# and the date 6 months after this. 

