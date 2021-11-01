#https://bookdown.org/ccolonescu/RPoE4/panel-data-models.html#the-fixed-effects-model
#https://www.econometrics-with-r.org/10-5-tferaaseffer.html
#https://bookdown.org/mike/data_analysis/panel-data.html
library(tidyverse)
income_18780 = life_income_20210403 %>% filter(job_income_12 >= 18780 | job_income_14 >= 18780 
                                            | job_income_16 >= 18780)
library(openxlsx)
write.xlsx(income_18780, file = "income_18780.xlsx", firstActiveCol=2, overwrite = T)
names(income_18780)
income_12_14_16 = income_18780 %>% select(sex_12, education_12, age_12, age_14, age_16, job_income_12, 
                                          job_income_14,job_income_16, job_sat_12, job_sat_14, job_sat_16, 
                                          job_hours_12, job_hours_14, job_hours_16)

#balanced data
income_12_14_16_f = na.omit(income_12_14_16)
income_12_14_16_f$id = c(1:nrow(income_12_14_16_f))
income_12_14_16_f$age12 = income_12_14_16_f$age_12
income_12_14_16_f = income_12_14_16_f %>% select(id, sex_12, age12, education_12, age_12, age_14, age_16, job_income_12, 
                                                 job_income_14,job_income_16, job_sat_12, job_sat_14, job_sat_16, 
                                                 job_hours_12, job_hours_14, job_hours_16)

income_12_14_16_f$job_hours_12_log = log(income_12_14_16_f$job_hours_12)
income_12_14_16_f$job_hours_14_log = log(income_12_14_16_f$job_hours_14)
income_12_14_16_f$job_hours_16_log = log(income_12_14_16_f$job_hours_16)
income_12_14_16_f$job_income_12_log = log(income_12_14_16_f$job_income_12)
income_12_14_16_f$job_income_14_log = log(income_12_14_16_f$job_income_14)
income_12_14_16_f$job_income_16_log = log(income_12_14_16_f$job_income_16)

write.xlsx(income_12_14_16_f, file = "income_12_14_16_f.xlsx", firstActiveCol=2, overwrite = T)
income_12_14_16_f = na.omit(income_12_14_16_f)
income_12_14_16_f_1 = income_12_14_16_f %>% filter(job_income_12 >= 18780 & job_income_14 >= 18780 
                                               & job_income_16 >= 18780)
library(reshape)
income_long <-  reshape(income_12_14_16_f,
                    idvar = c("id", "sex_12", "age12", "education_12"),  
                    varying = list(c("age_12", "age_14", "age_16"),
                                   c("job_income_12", "job_income_14", "job_income_16"), 
                                   c("job_sat_12", "job_sat_14", "job_sat_16"),
                                   c("job_hours_12", "job_hours_14", "job_hours_16")),  
                    timevar = "year", 
                    times = c(2012, 2014, 2016),
                    v.names = c("age", "income", "sat", "hours"), 
                    direction = "long")


library(reshape)
income_long_1 <-  reshape(income_12_14_16_f_1,
                        idvar = c("id", "sex_12", "age12", "education_12"),  
                        varying = list(c("age_12", "age_14", "age_16"),
                                       c("job_income_12", "job_income_14", "job_income_16"), 
                                       c("job_sat_12", "job_sat_14", "job_sat_16"),
                                       c("job_hours_12", "job_hours_14", "job_hours_16")),  
                        timevar = "year", 
                        times = c(2012, 2014, 2016),
                        v.names = c("age", "income", "sat", "hours"), 
                        direction = "long")

library(openxlsx)
write.xlsx(income_long_1, file = "income_long_1.xlsx", firstActiveCol=2, overwrite = T)

##################################################################################################
#################################################################################################
#http://karthur.org/2019/implementing-fixed-effects-panel-models-in-r.html
#https://cran.r-project.org/web/packages/plm/vignettes/A_plmPackage.html
#https://bookdown.org/ccolonescu/RPoE4/panel-data-models.html

library(psych)
des_s = describe(income_long)
des_s
des_s = des_s[ ,c(2, 3, 4, 8, 9)]

des_s_1 = describe(income_long_1)
des_s_1 = des_s_1[ ,c(2, 3, 4, 8, 9)]
des_s_1
write.csv(des_s_1, file = "des_s_1.csv")
table(income_long_1$sex_12)
library(tidyverse)
library(plm)
pool_reg <- plm(sat ~ log(income) + log(hours) + age + sex_12 + education_12, 
               data = income_long_1, index = c("id", "year"), 
               model = "pooling")
pool_s = summary(pool_reg)
pool_s
write.csv(pool_s$coefficients, file = "pool_s.csv")

fe_reg <- plm(sat ~ log(income) + log(hours) + age + sex_12 + education_12, 
              data = income_long_1, index = c("id", "year"), effect = "individual",
              model = "within")
fe_s = summary(fe_reg)
fe_s
write.csv(fe_s$coefficients, file = "fe_s.csv")
### Testing for fixed effects, null: OLS better than fixed 
#If the p-value is < 0.05 then the fixed effects model is a better choice
pFtest(fe_reg, pool_reg)

ra_reg <- plm(sat ~ log(income) + log(hours) + age + sex_12 + education_12, 
              data = income_long_1, index = c("id", "year"), 
              model = "random")
ra_s = summary(ra_reg)
ra_s

write.csv(ra_s$coefficients, file = "ra_s.csv")

#test which one is better for fixed or random
#Run a fixed effects model and save the estimates, then run a random model and save the estimates, 
#then perform the test. If the p-value is significant (for example <0.05) then use fixed effects, 
#if not use random effects.
phtest(fe_reg, ra_reg)


#The LM test helps you decide between a random effects regression and a simple OLS regression.
#The null hypothesis in the LM test is that variances across entities is zero. 
#This is, no significant difference across units (i.e. no panel effect). 
#(http://dss.princeton.edu/training/Panel101.pdf)
# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
plmtest(ra_reg, type=c("bp"))

#LSDV model

fixed.dum <-lm(sat ~ income + age_12 + sex_12 + education_12 + factor(id) - 1, data = income_long)
summary(fixed.dum)

fixef(fe_reg, type="dmean")
summary(fixef(fe_reg, type="dmean"))

#Ramsey's RESET test for functional form
library(lmtest)
income_long_1$income_log = log(income_long_1$income)
income_long_1$hours_log = log(income_long_1$hours)

resettest(sat ~ income_log + I(income_log^2), power = 2:3, 
          data = income_long_1, type = "regressor")

lin = lm(sat ~ income_log, data = income_long_1)

library(ggfortify)
autoplot(lin, which = 1, ncol = 1, label.size = 3)


#This could indicate that something is taking place across these particular 
#weeks to cause internet usage to be higher and lower than usual. 
#We see that using fixef has allowed us to isolate the effect of time in this regard.



#Random Intercept, Partial Pooling Model

#################################


#According to Baltagi, cross-sectional dependence is a problem in macro panels with long time series. 
#This is not much of a problem in micro panels (few years and large number of cases).
#Cross-sectional dependence can lead to bias in tests results (also called contemporaneous correlation).
#Macro panels are characterized by having a relatively large T and a relatively small N
#Source: Hoechle, Daniel, ¡§Robust Standard Errors for Panel Regressions with Cross-Sectional Dependence¡¨, 
#http://fmwww.bc.edu/repec/bocode/x/xtscc_paper.pdf
#pcdtest(fe_reg, test = c("lm"))
#pcdtest(fe_reg, test = c("cd"))

#Serial correlation tests apply to macro panels with long time series. 
#Not a problem in micro panels (with very few years). 
#The null is that there is not serial correlation.
#pbgtest(fixed)


#The Dickey-Fuller test to check for stochastic trends. 
#The null hypothesis is that the series has a unit root (i.e. non-stationary). 
#If unit root is present you can take the first difference of the variable.
#If p-value < 0.05 then no unit roots present.
Panel.set<-plm.data(income_long, index = c("id", "year"))
library(tseries)
adf.test(Panel.set$y, k=2)

#Testing for heteroskedasticity
#The null hypothesis for the Breusch-Pagan test is homoskedasticity
library(lmtest)
bptest(sat ~ log(income) + log(hours) + age + sex_12 + education_12 + factor(id), 
       data = income_long_1, studentize = F)

#If hetersokedaticityis detected you can use robust covariance matrix to account for it. 

#http://cran.r-project.org/web/packages/plm/vignettes/plm.pdf
#http://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf
#Stock and Watson 2006.
#Kleiberand Zeileis, 2008.
library(lmtest)
rob.se = coeftest(ra_reg, vcovHC(ra_reg, type = "HC3"))
rob.se = as.list.data.frame(rob.se)
rob.se
write.csv(rob.se, file = "rob.csv")
# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fe_reg, type = x)))))

#resource for reference
#UCLA Resourcestolearnand use R http://www.ats.ucla.edu/stat/R/
#UCLA Resources to learn and use Stata http://www.ats.ucla.edu/stat/stata/
#DSS -Stata http://dss/online_help/stats_packages/stata/
#DSS -R http://dss.princeton.edu/online_help/stats_packages/r
#Panel Data Econometricsin R: theplmpackagehttp://cran.r-project.org/web/packages/plm/vignettes/plm.pdf
#Econometric Computing withHC and HAC CovarianceMatrixEstimators
#http://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf


##Diagnostics and Inference in R
#Assessing Multicollinearity in Fixed Effects Regression Models
# Calculate VIF scores
# Assuming we've already fit our plm() model...
design.matrix <- as.data.frame(model.matrix(ra_reg))
# Get the time-demeaned response variable, lifeExp
design.matrix$sat <- plm::Within(
  plm::pdata.frame(income_long_1, index = 'id')$sat)
design.matrix
# Fit the OLS model on the demeaned dataset
m3.ols <- lm(sat ~ log(income) + log(hours) + age + sex_12 + education_12, data = design.matrix)
# Calculate VIF scores
library(car)
library(carData)
car::vif(m3.ols)


cor(income_long_1[ , c(2, 4, 6, 7, 8, 9)])
#Here, the VIF scores are all very low, so multicollinearity is not an issue.

##
fixed.dum <-lm(sat ~ income + age + sex_12 + education_12 + factor(id) - 1, data = income_long)
summary(fixed.dum)
fixed.dum

library(ggfortify)
autoplot(fixed.dum, which = 1, ncol = 1, label.size = 3)

library(ggplot2)
res = data.frame(cbind(fixed.dum$fitted.values, fixed.dum$residuals))
colnames(res) = c("¾A°t­È", "´Ý®t")
ggplot(res, aes(x = ¾A°t­È, y = ´Ý®t)) +
  geom_point() +
  geom_smooth(method = "lm", color = "purple", se = T) +
  geom_smooth(se = F, color = "red") +
  labs(title = "´Ý®t vs ¾A°t­È") +
  theme(plot.title = element_text(hjust = 0.5))

library(panelr)
income_long_a = long_panel(income_12_14_16_reshape, prefix = "_W", begin = 12, end = 16, 
                           label_location = "end")

income_long_b = income_long_a %>% filter(wave == 12 | wave == 14 | wave == 16)

library(dplyr)
income_long = arrange(income_long, id, year)
income_long$income_2 = income_long$income*income_long$income
income_long$income_m = income_long$income - mean(income_long$income, na.rm = T)
income_long$income_log = log(income_long$income)
library(plm)
income.pooled <- plm(sat ~ income, 
                   model = "within", data = income_long)
income_FE_a <- plm(sat ~ log(income), 
                 data = income_long, model='within', index=c("id", "year"))
summary(income_FE_a)

#################################################################################################
ols <-lm(sat ~ log(income), data = income_long)
summary(ols)

fixed <- plm(sat ~ log(income), 
          data = income_long, model = 'within', index=c("id", "year"))
summary(fixed)

#Fixed effects using Least squares dummy variable model
fixed.dum <-lm(sat ~ log(income) + factor(id) - 1, data = income_long)
summary(fixed.dum)

pFtest(fixed, ols)

plmtest(rice.p)

random <- plm(sat ~ log(income), data = income_long, index=c("id", "year"), model = "random")
summary(random)

pbltest(fm, Rice, alternative = "onesided")

phtest(fixed, random)

pcdtest(fixed, test = c("lm"))

pcdtest(fixed, test = c("cd")) 

pbgtest(fixed)

library(lmtest)
bptest(sat ~ log(income) + factor(id), data = income_long, studentize = F)

coeftest(random, vcovHC(random, type = "HC3"))
coeftest(fixed, vcovHC(fixed, method = "arellano"))
coeftest(fixed, vcovHC(fixed, type = "HC3"))

fixed <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")
summary(fixed)

random <- plm(formula, data=pdata, model= "random")
summary(random)

library(plm)
data("EmplUK", package="plm")
data("Produc", package="plm")
data("Grunfeld", package="plm")
data("Wages", package="plm")
#Poolability

library(plm)
plm::pooltest(sat ~ income + I(income^2), data = income_long, model = "within", index=c('year'))



