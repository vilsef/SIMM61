library(psych) # for describe\t
library(tidyverse) # for tidy code and ggplot\t
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(forcats)
library(ggplot2)
library(influence.ME)


influence_observation = influence(mod_rep_int_quad, obs = T)$alt.fixed # this can take a minute or so
influence_group = influence(mod_rep_int_quad, group = "ID")$alt.fixed

#Loading datasets

df_a.load <- read.table("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_1.csv", header = T,
                         sep = ",")
df_b.load <- read.table("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_2.csv", header = T,
                         sep =",")

#TASK
#We use age, sex, STAI, pain catastrophizing, 
#mindfulness, and serum cortisol as fixed effect predictors.

df_a <- df_a.load %>% 
  select(id = ID,
         hospital,
         age, 
         sex, 
         STAI = STAI_trait,
         pain,
         catastrophizing = pain_cat,
         mindfulness,
         serum_cortisol = cortisol_serum)

df_b <- df_b.load %>% 
  select(id = ID,
         hospital,
         age, 
         sex, 
         STAI = STAI_trait,
         pain,
         catastrophizing = pain_cat,
         mindfulness,
         serum_cortisol = cortisol_serum)

### DATA CLEANING
#Create sex factor variable. Merge single woman observation into female category:
df_a$sex[df_a$sex == "woman"] <- df_a$sex[df_a$sex == "woman"] <- "female"

# convert Hospital site variable to factor
df_a <- df_a %>% 
  mutate(hospital = factor(hospital))
df_b <- df_b %>% 
  mutate(hospital = factor(hospital))

#Tidying factors. Reorder levels 1:10
df_a$hospital <- factor(df_a$hospital, levels = c("hospital_1", "hospital_2",
                                                  "hospital_3", "hospital_4",
                                                  "hospital_5", "hospital_6",
                                                  "hospital_7","hospital_8",
                                                  "hospital_9","hospital_10"))

# We will now analyze if hospital site can explain some of the variability in our data. 
# Fitting line predicting serum cortisol on pain, sorted by hospital
#seems that it would be worthwhile to take into account the hospital sites when assessing pain

df_a %>%
  ggplot() + 
  aes(y = pain, x = serum_cortisol) +
  geom_point(aes(color = hospital),
             size = 3) + 
  geom_smooth(method = "lm", se = F)

# Fitting 10 lines predicting mindfullness on pain, 
# sorted by hospital sites

int_plot = df_a %>%
  ggplot() + 
  aes(y = pain, x = mindfulness, color = hospital) +
  geom_point(size = 3) + 
  geom_smooth(method = "lm", se = F, fullrange = TRUE)

int_plot

#Fit a random intercept model including the random intercept of hospital-ID. 
#Once the model is built, note the model coefficients and the confidence intervals 
#of the coefficients for all fixed effect predictors

#By using linear mixed models we can take into account that data is clustered according to 
#hospital site, without having to enter hospital as a classical predictor 
#in our model. Instead, we can enter it into the model as a random effect predictor.

#Analyze clustering of sites

df_a %>%
  ggplot() + 
  aes(y = pain, x = age) + geom_point(aes(color = hospital),
                                                                size = 4) + geom_smooth(method = "lm", se = F)
df_b %>%
  ggplot() + 
  aes(y = pain, x = age) + geom_point(aes(color = hospital),
                                      size = 4) + geom_smooth(method = "lm", se = F)
                                      

view(df_b)

# Fit random intercept model (age, sex, STAI, 
#pain catastrophizing, mindfulness, and serum cortisol, hospital as random effect predictor)

mod_rnd_int_a <-  lmer(pain ~ age + sex + STAI + 
                         catastrophizing + 
                         mindfulness + serum_cortisol + 
                         (1 | hospital), data = df_a)

summary(mod_rnd_int_a)

# Our model show that serum cortisol is our most influential predictor on pain with a positively
# correlated beta coefficient of 0.08 with significant results. 
#Other influential predictors are age and serum cortisol. 

### Compute variance on model A

# Marginal Rsquared
r2beta(mod_rnd_int_a, method = "nsj", data = df_a)
# Rsq = 0.39
# Conditional Rsq
r.squaredGLMM(mod_rnd_int_a)
# R2c = 0.46

#With model coefficients derived from Data A, predict pain on data file B: 

pain_predict_b <- predict(mod_rnd_int_a, 
                          newdata = df_b, 
                          allow.new.levels = T)

## Merge prediction from data file A to data file B
df_b <-  df_b %>% 
  cbind(df_b, pain_predict_b)	

# Build new linear model predicting pain on data file B using predictions
# derived from Data file A
df_b_mod <- lm(pain ~ pain_predict_b, data = df_b)
summary(df_b_mod)

# Rsquare data File B: 0.38

# Plotting the linear model on data file B
df_b_predicted %>% 	
  ggplot() +	
  aes(x = pain, y = pain_predict_b) +	
  geom_point() +	
  geom_point(data = df_b_predicted, aes(x = pain, y = pain_predict_b), col = "red", size = 4) +	
  geom_smooth(method = "lm", formula = 'y ~ x', se = F)	

#compute variance comparison A & B model:
# Marginal R squared with confidence intervals
### 
# A
r2beta(mod_rnd_int_a, method = "nsj", data = df_a)
# Rsq = 0.39

# B
r2beta(df_b_mod, method = "nsj", data = df_b_predicted)
# Rsq= 0.38

# Conditional R squared values
#A
r.squaredGLMM(mod_rnd_int_a)
# R2c = 0.46

#B
r.squaredGLMM(df_b_mod)
#R2c = 0.38

# Confidence intervals
confint(mod_rnd_int_a)
confint(df_b_mod)


#### Conclusion: A comparison between A and B model show that the Rsq of Model B (rsq =.38)
# was much closer to the marginal Rsq of model A (rsq =.39) than the conditional Rsq of Model 
# A (Rsq = .46). The reason for this result is likely due to that the marginal
# Rsquare did not account for the random effect terms, i.e. the hospital site predictor. 
# When adding the random effect term to the calculation, we saw a significantly 
# bigger difference between the models.

### 
# Build a new linear mixed effects model on dataset A predicting pain. However,
# instead of including all predictors, you should only include the most influential predictor
# from the previous model. Allow for both random intercept and random slope. Now visualize
# the fitted regression lines for each hospital separately.
### 

# We choose to include Serum Cortisol as our most influential predictor for our final models, 
# Based on prior regression models. 

# Here we also create a simple regression model and a random intercept model
# to easier compare model fit with our new random slope model

# Simple regression
simple_mod_final <- lm(pain ~ serum_cortisol, data = df_a)
summary(simple_mod_final)

# Random intercept model
rnd_int_mod_final <- lmer(pain ~ serum_cortisol + 
                          (1 | hospital), data = df_a)

summary(rnd_int_mod_final)

# Random slope model
rnd_slope_mod_final <- lmer(pain ~ serum_cortisol + (serum_cortisol| hospital),
                            data = df_a)

summary(rnd_slope_mod_final)

# Vizualizing models 

# Saving predictions into variables
df_a = df_a %>%
  mutate(pred_int = predict(rnd_int_mod_final), pred_slope = predict(rnd_slope_mod_final))



# Include the graph displaying the separate fitted regression lines for the hospitals from
# the mixed model including only the most influential predictor in the model.

# Regression line of random intercept model w/ 1 predictor
df_a %>%
  ggplot() + aes(y = pain, x = serum_cortisol, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                          aes(y = pred_int, x = serum_cortisol)) + 
  labs(title = "Random Intercept Model") + 
  facet_wrap(~hospital,
             ncol = 2)

# Regression line of random slope model w/ 1 predictor
df_a %>%
  ggplot() + aes(y = pain, x = serum_cortisol, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + 
  labs(title = "Random Slope Model") +
  geom_line(color = "red", 
            aes(y = pred_slope, x = serum_cortisol)) + facet_wrap(~hospital,
                                                                  ncol = 2)

# When we plot and compare the random intercept model with 1 predictor and the random slope model with 
# 1 predictor, we find that there is no large effect
# of serum cortisol on pain comparing different hospital sites. With other words, we would not assume there is
# an interaction effect between our predictor (serum cortisol) and our random effect predictor (hospital site). 
# We further confirm this by comparing our initial random intercept model with our random slope model
# through the following tests:


# comparing model fit indices, 3 models
sum(residuals(mod_rnd_int_a)^2)
## [1] 224.3138
sum(residuals(rnd_slope_mod_final)^2)
## [1] 286.1497
### The RSS is largest for the random slope model and lowest for the random intercept model, but we will
# further confirm this by performing cAIC and anova test

# cAIC test 
cAIC(mod_rnd_int_a)$caic
## 621.4255
cAIC(rnd_slope_mod_final)$caic
## 664.5371

# Anova test
anova(mod_rnd_int_a,rnd_slope_mod_final)

# smaller cAIC means better fit, and a difference in AIC of 2 or more means that 
# the models can be considered to be significantly different in their model fit. 
# The cAIC is lower in our initial random intercept model than in our random slope model
# In the Anova test we see that our random intercept model has the highest logLik score,
# further confirming that this structure is our best performing model

# marginal R squared with confidence intervals final model
r2beta(mod_rnd_int_a, method = "nsj", data = df_a)
## Effect Rsq upper.CL lower.CL
## 1 Model 0.385 0.488 0.301
r2beta(rnd_slope_mod_final, method = "nsj", data = df_a)
## Effect Rsq upper.CL lower.CL
## 1 Model 0.221 0.321 0.131

# marginal and conditional R squared values
r.squaredGLMM(mod_rnd_int_a)
r.squaredGLMM(rnd_slope_mod_final)
## R2m R2c
# 0.22 0.32

