 #I load my libraries for the assignment.
library(tidyverse)
library(moderndive)
library(infer)
library(haven)
library(skimr)
library(sjPlot)
library(prediction)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(margins)

#Question 1a. I run the logit model for poverty_mean, regext,old,pubhous_mean, Hispanic_mean, and balck_mean which I name as logitM1
Lmodel <- glm(rodent2 ~ poverty_mean + regext + old + pubhous_mean + hispanic_mean + black_mean, data = rodent, family = binomial(link = "logit"))
tab_model(Lmodel, show.se = TRUE, p.style = "stars", transform = NULL)


#Question 1a: I run the probit model for poverty_mean, regext,old,pubhous_mean, Hispanic_mean, and black_mean variables which I name as ProbitM1
Pmodel <- glm(rodent2 ~ poverty_mean + regext + old + pubhous_mean + hispanic_mean + black_mean, data = rodent, family = binomial(link = "probit"))
tab_model(Pmodel, show.se = TRUE, p.style = "stars", transform = NULL)

tab_model(Lmodel,Pmodel,  
          show.se = TRUE, p.style = "stars", p.threshold = c(.10, .05, .01), show.ci = .95,transform = NULL)
          
#Question 1b
tab_model(Lmodel,Pmodel,  
          show.se = TRUE, p.style = "stars", p.threshold = c(.10, .05, .01), show.ci = .99, transform = NULL)

#Question 1c.

plot_model(Lmodel, type = "pred", terms= ("poverty_mean[all]")) +
             labs(x= "poverty mean", y= "Having rodents",
                title = "Logit graph predicting having rodents with poverty")
           
plot_model(Pmodel, type = "pred", terms="poverty_mean[all]") +
  labs(x= "poverty mean", y= "Having rodents",
       title= "Probit graph predicting having rodents with poverty")

#Question 2a
#i. I predict old building, poverty neighborhood using the prediction function.
prediction(Lmodel, data = rodent, at = list(old = 1, poverty_mean = 0.4))
#ii.I predict black neighborhood with regular exterminator in an old house using the prediction function.
PredPob2 <- prediction(Lmodel, data = rodent, at = list(black_mean = 0.28, regext = 1, old = 0))
#iii. I predict rich neighborhood and Hispanic percentage using the prediction function.
PredPob3 <- prediction(Lmodel, data = rodent, at = list(poverty_mean = .08, hispanic_mean = .35))

#Question 2b.
prediction(Lmodel, data = rodent, at = list(pubhous_mean = c(0, .05, .1, .15, .20, .25, .30, .35,.38)))
#Question 2c.

MarEffect <- margins(Lmodel, data = rodent, at = list(poverty_mean = .11, old = 0, regext = 1, black_mean = .17, pubhous_mean = .09, hispanic_mean = .11))

       
 
