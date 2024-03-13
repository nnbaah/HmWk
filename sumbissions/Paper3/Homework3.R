#I begin the work by checking on my packages.
library(tidyverse)
library(moderndive)
library(skimr)
library(gapminder)
library(ggplot2)

#I move on to load the Guj3_6 data set.

#Question 1: To find the correlation between compensation and productivity in the business sector, I used the correlation function.
cor(Guj3_6$compensationbusiness, Guj3_6$productivitybusiness)

#Question 2: To find the regression for the business sector, I used the linear regression function. And in the formula input compensation as dependent variable (since it is being predicted) and productivity as independent variable.
RegBus <- lm(compensationbusiness ~ productivitybusiness, data = Guj3_6)
summary(RegBus)

#Question 3a: I plot the regression between compensation and productivity using ggplot. With compensation on x-axis and productivity on y -axis.
ggplot(Guj3_6, aes(x = productivitybusiness, y = compensationbusiness)) +
  geom_point() +
  labs(x = "productivity", y = "compensation",
       title = "Relationship between compensation and real GDP output") +
  geom_smooth(method = "lm", se = FALSE)


#Question 3b: To plot the residuals against the compensation, first I create a new data BusResidual to calculate the residual.
RegBus <- lm(compensationbusiness ~ productivitybusiness, Guj3_6)
BusResidual <- get_regression_points(RegBus)

# With a new data BusResidual I have the actual compensation and residual that I can plot.
#Q3b I plot the graph for residual and compensationbusiness_hat graph using ggplot. With the compensationbusiness_hat as on the y-axis and residuals on the x-axis.
ggplot(BusResidual, aes(x = compensationbusiness_hat, y = residual)) +
  geom_point() +
  labs(x = "predicted compensation", y = "residual ",
       title = "Predicted compensation and residual graph") 

#Question 4: I export the residual data from my BusResidual data to examine the poor predictions of the model.
write.table(BusResidual, file= "Residualsdatabusines.csv", row.names=F, sep=",")

#Question 5: TO find the percent of variance I run the summary command of the regression for Business to find the Multiple R-Squared.
summary(RegBus)

#Question 6: To find the regression of a line for nonfarm I used the lm function.
RegNonfarm <- lm(Guj3_6$compensationnonfarm ~ Guj3_6$productivitynonfarm)













  







