Fe_model <- lm(CC ~USAID, data = comms_data_set)

tab_model(Fe_model, show.se = TRUE, p.style = "stars", p.threshold = c(.10, .05,.01), show.ci = .99)

install.packages("sjPlot")

Fe_model <- lm(CC ~USAID + factor(Country), data = comms_data_set)
tab_model(Fe_model, show.se = TRUE, p.style = "stars", p.threshold = c(.10, .05,.01), show.ci = .99)

Fe_model <- lm(CC ~USAID + factor(Year), data = comms_data_set)
tab_model(Fe_model, show.se = TRUE, p.style = "stars", p.threshold = c(.10, .05,.01), show.ci = .99)


Reg <- plm(CC ~ USAID + factor(Country), comms_data_set, index=c("Country", "Year"), model="random")
install.packages("plm")

ing-opportunities/support-for-graduate-students-sgs/
  Alena Gross to Everyone (Apr 3, 2023, 2:28 PM)
tab_model(Reg1h4linke, show.se = TRUE, p.style = "stars", 
          p.threshold = c(.10, .05,.01), show.ci = .99)
sjPlot
Alena Gross to Everyone (Apr 3, 2023, 2:39 PM)
Reg <- plm(plh0007 ~ factor(pgemplst) + d11101 +d11102ll+
             d11104 + d11107+ d11108+ l11101_ew + log(i11110)+
             log(i11102), regdata, index=c("pid", "syear"), model="within", effect = "individual")



ExPanD(filterdata, cs_id = "pid", ts_id = "syear")
library(ExPanDaR)