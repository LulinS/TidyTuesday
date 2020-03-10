# Tidy Tuesday: 2020-03-10, college tuition, diversity, and pay
# Lulin Song

library(tidyverse)
library(usmap)
library(ggplot2)

# import data---------------------------------------------------------
tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

# explore data--------------------------------------------------------
tuition_cost %>% head()
tuition_income %>% head()
salary_potential %>% head()
diversity_school %>% head()

# tidy data-----------------------------------------------------------
wide_diversity_school <- diversity_school %>% 
  filter(!is.na(name)) %>% 
  pivot_wider(names_from = category, 
              values_from = enrollment)

# Clean data----------------------------------------------------------
glimpse(wide_diversity_school)
percentage <- wide_diversity_school %>% mutate(Native = 
                                   (`American Indian / Alaska Native` +`Native Hawaiian / Pacific Islander`) / total_enrollment,
                                 Asian = (Asian / total_enrollment),
                                 Black = (Black / total_enrollment),
                                 Hispanic = (Hispanic / total_enrollment),
                                 White = (White / total_enrollment),
                                 Other = (`Two Or More Races` + Unknown + `Non-Resident Foreign`) / total_enrollment,
                                 sum = White + Asian + Black + Hispanic + Other + Native)
percentage$sum %>% summary()  # make sure all the sum is 1
hhi <- percentage %>% select(name, state, White, Black, Asian, Hispanic, Native, Other) %>% 
  mutate(hhi = White^2 + Black^2 + Asian^2 + Hispanic^2 + Native^2 + Other^2) %>% 
  group_by(state) %>% 
  summarise(mean_hhi = mean(hhi)) # compute the average hhi of each state

# graph-----------------------------------------------------------------------
plot_usmap(data = hhi, values = "mean_hhi") +
  scale_fill_continuous(type = "viridis", name = "Avg. Uni. Diversity") +
  theme(legend.position = "right")

ggsave("Uni_diver.png")
