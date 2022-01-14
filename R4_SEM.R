
library(lavaan) # for SEM fit and model functions	
library(semPlot) # for semPaths()	
library(semptools) # for set_sem_layout	
library(tidyverse) # for tidy code	
library(CompQuadForm) # for mvnorm.kur.test and mvnorm.skew.test (prerequisite)	
library(ICS) # for mvnorm.kur.test and mvnorm.skew.test	



#Task 1: Specify a structural equation model matching the 
#theoretical factor structure 
#described above, then fit the model to the dataset. We will call this Model A. 

# Check the assumption of multivariate normality and address it if there is a 
# violation of the assumptions.  
#- Report which estimator you used to fit the model and why. 
#- Report the following fit statistics: model chi-squared, together with degrees of 
#freedom and p-value, TLI, CFI, and RMSEA (with confidence interval). 
#- Based on these fit statistics, is the model fit acceptable?

install.packages("psychTools")

library(psychTools)
my_data = holzinger.swineford 


# Factor: Visual perception -> 
# t01_visperc, t02_cubes, t03_frmbord, and t04_lozenges
# Verbal ability -> 
# t06_paracomp, t07_sentcomp, and t09_wordmean.
# Processing speed -> 
# t10_addition t12_countdot, and t13_sccaps.


model_a <- '	
    VP =~ t01_visperc +t02_cubes + t03_frmbord + t04_lozenges
    VA =~ t06_paracomp + t07_sentcomp + t09_wordmean
    PS =~ t10_addition + t12_countdot + t13_sccaps
    
'	
model_a_fit <- sem(model_a, data = my_data)	

semPaths(model_a_fit)
summary(model_a_fit)

#Check the  assumption of multivariate normality and address it if there is a violation of the assumptions.  
# - Report which estimator you used to fit the model and why. 
# Mult nor mtest data
mvnorm.kur.test(my_data[,c("t01_visperc","t02_cubes","t03_frmbord", "t04_lozenges",
                           "t06_paracomp", "t07_sentcomp", "t09_wordmean",
                           "t10_addition", "t12_countdot", "t13_sccaps")])

###  P value <.05

mvnorm.skew.test(my_data[,c("t01_visperc","t02_cubes","t03_frmbord", "t04_lozenges",
                            "t06_paracomp", "t07_sentcomp", "t09_wordmean",
                            "t10_addition", "t12_countdot", "t13_sccaps")])

### P value <.05


# If the p-value is lower than 0.05, it indicates the 
# violation of the multivariate normality assumption.	
#Since both p-values are < .05, the multivariate normality assumption is violated. 
# Hence, the default ML Estimator is not a preferred solution. 
# Instead we try the solution of the ML estimator with robust SE solution

# Robust estimator ML
model_a_MLM <- sem(model_a_fit, data = my_data, estimator = "MLM")

# unstandardized estimates
summary(model_a_MLM, fit.measures = T)


# - Report the following fit statistics: model chi-squared, together wit

### Roboust Comparative Fit Index (CFI) 
0.934
### Robust Tucker-Lewis Index (TLI)#
0.907
### p-value (chisq)
0.00
### RMSEA   
0.073
## 90 Percent confidence interval - lower         
0.055
## 90 Percent confidence interval - upper
0.092
## P Value RMSEA
0.020


# - Based on these fit statistics, is the model fit acceptable? 
# To be identified the model needs to have 0 or higher degrees of freedom.
### Degrees of freedom
32 

# We conclude the model fit is acceptible since degrees of freedom > 0. 


#####
#Represented by t10_addition and t12_countdot have a very similar structure 
#(that is different from t13_sccaps), so you think that after accounting for the effect of 
#Processing speed (latent factor), there would still be some correlation left between 
#t10_addition and t12_countdot that is not caused by Processing speed. Include this path into 
#the model, lets call this new model Model B. Compare the model fit of Model A and Model 
#B.  

model_b_fit <- '	
    VP =~ t01_visperc +t02_cubes + t03_frmbord + t04_lozenges
    VA =~ t06_paracomp + t07_sentcomp + t09_wordmean
    PS =~ t10_addition + t12_countdot + t13_sccaps
    t10_addition ~~ t12_countdot
'
model_b <- sem(model_b_fit, data = my_data)	

semPaths(model_a)
semPaths(model_b)

# Is there a significant difference in the model fit of Model A and B? If yes, 
# which model fits the data better? Report the model fit or test statistic this 
# is based on. 

summary(model_a, fit.measures = T)
summary(model_b, fit.measures = T)

anova(model_a, model_b)

# The AIC score is more than 2 points lower of model B than model A, indicating that model B
# is a better fit. The chisq indicate a significant result. 


# Which of the manifest variables t01_visperc, t02_cubes, t03_frmbord, t04_lozenges 
# are least influenced by Visual perception ability (latent variable). 
# What do you base  this decision on? 

summary(model_b, standardized =T)

# t02_cubes seems to be the least influenced variable with an estimate of 0.44.

# Produce a path diagram of Model B in support of this answer, based  on 
# which this answer can be verified
semPaths(model_b, whatLabels = "std")

##### 3
# Based on this graph, if t01_visperc is increased by 1 unit, 
# what would be the expected 
# change in the value of t13_sccaps? Describe your reasoning. 
# (If it helps, you can 
#  reproduce this model in R). 

# Direct effect tvisp -> t13(c)
c = 0.31 
# Indirect effect tvisp -> t12(a) -> t13(b)
0.23 * 0.38 
# calculation: 
0.31 + 0.23 * 0.38
## Total effect = 0.4
