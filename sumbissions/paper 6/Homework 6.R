# I load the various packages for the assignment.
library(tidyverse)
library(moderndive)
library(infer)
library(haven)
library(skimr)

#Question 1. Finding the mean income using the two tail t-test.
t.test(freelancers$income, mu = 50000, alternative="two.sided")

#Question 2 Performing hypothesis test using the re sampling of 1000 samples.
# I set my seed at 4534
set.seed(4534)
resample_mean <- freelancers %>% 
  specify(response = income) %>%
  hypothesize(null = "point", mu = 50000) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")

observe_meanincome <- freelancers %>% 
  specify(response = income) %>%
  calculate(stat = "mean")

PvalofResamplemean <- resample_mean %>%
  get_p_value(obs_stat = observe_meanincome,
              direction = "two-sided")
#Question 3. Finding the difference income using the two tail t-test.
t.test(freelancers$income ~ freelancers$gender, alternative= 'two.sided')

#Question 4 I load the fastDummies packages
install.packages("fastDummies")
library(fastDummies)
#I create the sector data
freelancers <- dummy_cols(freelancers, select_columns = "sector")
#Question 4a. Comparing the income of information technology workers with all other workers.
t.test(freelancers$income ~ freelancers$sector_4, alternative= 'two.sided')
#Question 4b.  Comparing the income of finance workers with all other workers.
t.test(freelancers$income ~ freelancers$sector_2, alternative= 'two.sided')