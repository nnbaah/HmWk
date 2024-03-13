library( tidyverse)
library(moderndive)
library(haven)
library(skimr)
library(infer)
library(sjPlot)
library(stargazer)

install.packages("sjplot")
install.packages("stargazer")
#Question 1a: Finding the correlation between Democratic governors and Hilary Clinton's vote.
cor(USfederalelectioncontext.2018$clinton16, USfederalelectioncontext.2018$demgov14, use = "complete.obs")
#Correlation between Republican governors and Trump's vote 
cor(USfederalelectioncontext.2018$trump16, USfederalelectioncontext.2018$repgov14, use = "complete.obs")

#Question 1b: Finding the bivariate regression between governors vote and presidential candidate's votes
#bivariate correlation between Clinton and Democratic governors' vote
DemoBivCor <- lm(clinton16 ~ demgov14, data = USfederalelectioncontext.2018)
get_regression_table(DemoBivCor)
##bivariate correlation between Trump and Republican governors' vote
RepBivCor <- lm(trump16 ~ repgov14, data = USfederalelectioncontext.2018)
get_regression_table(RepBivCor)

#Question 2 Converting votes to percentages using the mutate command
Votepercent <- USfederalelectioncontext.2018 %>%
  mutate(Trump_percent = trump16*100/(trump16 + clinton16 + otherpres16))

#Running a regression of percentage vote using the lm command
#Multiple regression of trump percentage vote with percent black, percent Hispanic, percent 65 and older, median household income, less than college education and precent rural.
TrumpMultiReg <- lm(Trump_percent ~ black_pct + hispanic_pct + age65andolder_pct + median_hh_inc + lesscollege_pct + rural_pct, data = Votepercent)
get_regression_table(TrumpMultiReg)
tab_model(TrumpMultiReg, show.se = TRUE, p.style = "stars", p.threshold = c(.10, .05, .01), show.ci = .99)

#Question 4
lm(Trump_percent ~black_pct, data = Votepercent)
lm(Trump_percent ~ hispanic_pct, data = Votepercent)

#Question 5
plot_model(TrumpMultiReg, type = "pred", show.data = FALSE, ci.lvl = .95, terms = c("hispanic_pct", "median_hh_inc"))

#Question 6 Running the regression by interacting Hispanic percent and rural percent.
TrumpInterReg <- lm(Trump_percent ~ black_pct + hispanic_pct*rural_pct + age65andolder_pct + median_hh_inc + lesscollege_pct, data = Votepercent)
get_regression_table(TrumpInterReg)
tab_model(TrumpInterReg, show.se = TRUE, p.style = "stars", p.threshold = c(.10, .05, .01), show.ci = .99)

#Question 7
plot_model(TrumpInterReg, type = "pred", show.data = FALSE, ci.lvl = .95, terms = c("hispanic_pct", "median_hh_inc[35336, 60300]", "rural_pct"))


