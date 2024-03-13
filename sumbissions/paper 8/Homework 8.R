#Load packages for assignment

library(tidyverse)
library(moderndive)
library(infer)
library(haven)
library(skimr)
library(sjPlot)
view(Pol_Instability)
#Question 1: Regression between the dependent and independent variable.
PolInstaReg <- lm(instab ~ democracy + gdplag + coldwar, data = Pol_Instability )
get_regression_table(PolInstaReg)
tab_model(PolInstaReg, show.p = TRUE, p.style = "stars", p.threshold = c(.10, .05, .01), show.ci = .95)

#Question 1 b: Democracy prediction
plot_model(PolInstaReg, type = "pred", terms = c("democracy"))
#Question 1c: GDP predictions
plot_model(PolInstaReg, type = "pred", terms = c("gdplag"))



#Question 2a: Plotting the GDP graphic
GDPgrapg <- ggplot(data = Pol_Instability, mapping = aes(x = gdplag)) +
  geom_histogram(color = "black", fill = "blue")
#Question 2b: I find the natural of lagged GDP by using the mutate function.
NatlogGDP <- Pol_Instability %>%
  mutate(LogGDP = log(gdplag))
#Graphing the natural log of GDP
HIst_NatLogGDP <- ggplot(data = NatlogGDP, mapping = aes(x =LogGDP)) +
  geom_histogram(bins = 100, color = "black", fill = "blue")
  
#Question 3: Finding the regression of logged GDP with other variables using the lm function.
RegLoggedGDP <- lm(instab ~ democracy + log(gdplag) + coldwar, data = Pol_Instability)
get_regression_table(RegLoggedGDP)
tab_model(RegLoggedGDP, show.p = TRUE, p.style = "stars", p.threshold = c(.10, .05, .01), show.ci = .95)

tab_model(TrumpInterReg, show.se = TRUE, p.style = "stars", p.threshold = c(.10, .05, .01), show.ci = .99)

#Question 4: Plotting the regression of the logged model.
plot_model(RegLoggedGDP, type = "pred", show.data = FALSE, ci.lvl = .95, terms = c("gdplag"))

#Question 5: Non-linear regression
Non_LinQuad <- lm(instab ~ democracy + I(democracy^2) + gdplag + coldwar, data = Pol_Instability)
get_regression_table(Non_LinQuad)
tab_model(Non_LinQuad, show.p = TRUE, p.style = "stars", p.threshold = c(.10, .05, .01),show.ci = .95)
plot_model(Non_LinQuad, type = "pred", terms = c("democracy"))



