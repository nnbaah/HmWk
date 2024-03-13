# I load the weuropevdem data for the assignment
#Load necessary libraries.
library(tidyverse)
library(moderndive)
library(infer)
library(haven)
library(skimr)

#Question 1: I create a new data set that contains only country names and level of democracy
LofDem <- weuropevdem%>%
  select(country_name, v2x_polyarchy)
#Finding the sample mean using the data.table function.
install.packages("data.table")
library(data.table)
#I load the data.table package
set.seed(4534)
MeanLofDem <- setDT(LofDem)[, lapply(.SD, mean, na.rm=TRUE), by=country_name]

#Question 2: I create new data for each country and their level of democracy
Portugal <- weuropevdem %>%
  filter(country_text_id %in% c("PRT"))
Netherlands <- weuropevdem %>%
  filter(country_text_id %in% c("NLD"))
France <- weuropevdem %>%
  filter(country_text_id %in% c("FRA"))
Ireland <- weuropevdem %>%
  filter(country_text_id %in% c("IRL"))
Spain <- weuropevdem %>%
  filter(country_text_id %in% c("ESP"))
United_Kingdom <- weuropevdem %>%
  filter(country_text_id %in% c("GBR"))
Belgium <- weuropevdem %>%
  filter(country_text_id %in% c("BEL"))

#Question Creating the t test at 90 percent confidence interval for each ocuntry
t.test(Portugal$v2x_polyarchy, conf.level = 0.90)
t.test(France$v2x_polyarchy, conf.level = 0.90)
t.test(Ireland$v2x_polyarchy, conf.level = 0.90)
t.test(Netherlands$v2x_polyarchy, conf.level = 0.90)
t.test(Spain$v2x_polyarchy, conf.level = 0.90)
t.test(United_Kingdom$v2x_polyarchy, conf.level = 0.90)
t.test(Belgium$v2x_polyarchy, conf.level = 0.90)

#Question 3:  Sampling 1000 times using the seed 4534
#Bootstrap for Netherlands
set.seed(4534)
Nedbootstrap <- Netherlands %>%
  specify(response = v2x_polyarchy) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
#Finding the 95 percentile 
NedPercentile <- Nedbootstrap%>% 
  get_confidence_interval(level = 0.95, type = "percentile")


#Bootstrap for Ireland
set.seed(4534)
Irebootstrap <- Ireland %>%
  specify(response = v2x_polyarchy) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
#Finding the 95 percentile 
IrePercentile <- Irebootstrap%>% 
  get_confidence_interval(level = 0.95, type = "percentile")

#Bootstrap for Unite_kingdom
set.seed(4534)
UKbootstrap <- United_Kingdom %>%
  specify(response = v2x_polyarchy) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
#Finding the 95 percentile 
UKPercentile <- UKbootstrap%>% 
  get_confidence_interval(level = 0.95, type = "percentile")

#Bootstrap for France
set.seed(4534)
FRAbootstrap <- France %>%
  specify(response = v2x_polyarchy) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
#Finding the 95 percentile 
FRAPercentile <- FRAbootstrap%>% 
  get_confidence_interval(level = 0.95, type = "percentile")

#Bootstrap for Belgium
set.seed(4534)
BELbootstrap <- Belgium %>%
  specify(response = v2x_polyarchy) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
#Finding the 95 percentile 
BELPercentile <- BELbootstrap%>% 
  get_confidence_interval(level = 0.95, type = "percentile")

#Bootstrap for Portugal 
set.seed(4534)
PRTbootstrap <- Portugal %>%
  specify(response = v2x_polyarchy) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
#Finding the 95 percentile 
PRTPercentile <- PRTbootstrap%>% 
  get_confidence_interval(level = 0.95, type = "percentile")

#Bootstrap for Spain
set.seed(4534)
Spainbootstrap <- Spain %>%
  specify(response = v2x_polyarchy) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
#Finding the 95 percentile 
SpainPercentile <- Spainbootstrap%>% 
  get_confidence_interval(level = 0.95, type = "percentile")

#Question 4 Calculating the confidence interval for Portugal and United kingdom using the t -test at 95 percent confidence interval.

PRT95ConfI <- t.test(Portugal$v2x_polyarchy, conf.level = 0.95)
UK9595ConfI <- t.test(United_Kingdom$v2x_polyarchy, conf.level = 0.95)

UKConfIn <- United_Kingdom %>%
  specify(response = v2x_polyarchy) %>%
  calculate(stat = "mean")

PRTConfIn <- Portugal %>%
  specify(response = v2x_polyarchy) %>%
  calculate(stat = "mean")

#Question 4b
PRTbootstrap <- Portugal %>%
  specify(response = v2x_polyarchy) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
#Finding the 95 percentile 
PRTPercentile <- PRTbootstrap%>% 
  get_confidence_interval(level = 0.95, type = "percentile")
#Finding 90 percent confidence interval for United Kingdom/Great Britain
ConfUK2 <- United_Kingdom %>%
  specify(response = v2x_polyarchy) %>%
  assume(distribution = "t")
ConfUK3 <- get_confidence_interval(x =ConfUK2, level = .95, point_estimate = UKConfIn)

#visualizing Portugal's 95 percent confidence interval with bootstrap
visualize(PRTbootstrap) +
  shade_confidence_interval(endpoints = PRTPercentile) +
  labs(x = "sample mean (level of democracy)", y = "counts", title = "Portugal's sample mean (level of democracy) at 95 percent confidence level")
  
#Visualization United Kingdom's 95 percent confidence interval with the sample mean.
visualize(ConfUK2) +
  shade_confidence_interval(ConfUK3, color = "red") +
  labs(x = "sample mean (level of democracy mean)", y = "counts", title = "Great Britain's sample mean (level of democracy) at 95 percent confidence level")

#Combining both means in one graphs
visualize(PRTbootstrap) +
  shade_confidence_interval(endpoints = PRTPercentile) +
  shade_confidence_interval(ConfUK3, color = "red") +
  labs(x = "sample mean (level of democracy)", y = "counts", title = "Portugal & Great Britian's mean(level of democracy) at 95% confidence level")

#Question 5 Running the bivariate regression using the lm function
BiReg <- lm(weuropevdem, formula = v2x_polyarchy ~ v2x_gender)
get_regression_table(BiReg, conf.level = 0.99)







