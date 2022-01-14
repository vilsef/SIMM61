library(tidyverse)
library(ggplot2)
library(dplyr)
library(pscl)
library(lmtest)
library(skimr)
library(dominanceanalysis)
library(caret)
library(lattice)
library(stargazer)
library(sjPlot)

#Data cleaning
setwd("C:/Users/vilhe/OneDrive/Desktop/R_directory/datasets")
df <- read.csv("titanic_train.csv")

# Dataset obtained from: 
# https://raw.githubusercontent.com/kekecsz/SIMM61-Course-
#  materials/main/Home_assignment/Titanic%20-%20training%20set.csv")

# Some age values are missing in the dataset. We solve this by filling missing values with the overall
# mean age for the passengers rounded up , i.e. 30 (Mean age= 29.69) 
df$Age[is.na(df$Age)] = 30


# The title of passengers can reveal one's social status in society, for instance, a married woman might 
# posess higher economical capital than unmarried ones, and men that are titled Master might have another
# social status than men without a title at all. 

# We extract titles from the dataset by using the regex command:
df$Title <- gsub('(.*, )|(\\..*)', '', df$Name)

# Show title counts by sex
table(df$Sex, df$Title)

# Grouping into useful categories, reducing unnecessary ones: 
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Reassign mlle, ms, and mme to fitting categories
df$Title[df$Title == 'Mlle']        <- 'Miss' 
df$Title[df$Title == 'Ms']          <- 'Miss'
df$Title[df$Title == 'Mme']         <- 'Mrs' 
df$Title[df$Title %in% rare_title]  <- 'Others'


# Show title counts by sex again
table(df$Sex, df$Title)



# Subsetting variables for analysis. 
# We create factors of survived, passenger class, and child variable. We also create a variable called "family
# size that adds the variables parent/children & sibling/spouse into one variable counting family size (where 0 = Alone)

df <- df %>%
  mutate(sex = factor(Sex),
         survived = factor(Survived, 
                           levels = c(0,1), 
                           labels = c("Died", "Survived")),
         class = factor(Pclass,
                        levels = c(1,2,3),
                        labels = c("1st Class", "2nd Class",
                                   "3rd Class")),
         parch = factor(Parch),
         sibsp = factor(SibSp),
         price = Fare,
         family_size = SibSp + Parch + 1,
         age = Age,
         name = Name,
         title = factor(df$Title),
         child = factor(Age < 18), levels = c(1), labels= c("Child")
  )



# Feature engineering -  Family group size
family.size.group <- cut(as.numeric(df$family_size), breaks = c(0,2,3,4,12), labels = c("Alone", "2","3", "4 or more"))
family.size.group.num <- as.numeric(family.size.group)

# Age groups
age.groups <- cut(df$age, breaks = c(0,16,30,45,60,100), labels =c("0-15", "16-29","30-45","46-59","60+"))


#######################
# DATA VISUALIZATION

# Survival rate by sex
df %>%
  ggplot() + 
  aes(x = sex, fill = survived) + 
  geom_bar(position = "dodge") +
  ggtitle("Survival rate by Gender") +
  xlab("Survival ratio Females vs Males") + 
  theme_bw()

#Survival rate by age group
df %>%
  ggplot() + 
  aes(x = age.groups, fill = survived) + 
  ggtitle("Survival rate by Age group") +
  geom_bar(stat = "count", position = "dodge") +
  theme_bw()

# Conclusion: Passengers in third class much lower chances of survival
df %>% 	
  ggplot() +	
  aes(x = class, fill = survived) +	
  geom_bar(stat = "count", position = "dodge") + 
  ggtitle("Survival rate by Cabin Class") +
  xlab("Cabin class") + 
  theme_bw()

# Survival rate by family group size
df%>% 
  ggplot() + 
  aes(x = family.size.group, fill = survived) + 
  geom_bar(position = "dodge") + 
  ggtitle("Survival rate by family group size") +
  xlab("Family group size (including oneself)") +
  theme_bw()


# Survival rate by Title
df%>%
  ggplot() + 
  aes(x = title, fill = survived) + 
  geom_bar(stat = "count", position = "dodge") + 
  ggtitle("Survival rate by Title") +
  xlab("Titles") +
  theme_bw()


# Survival title gender
df %>% 
  ggplot() + 
  aes(x = title, fill = sex) +
  geom_bar(stat = "count", position = "dodge") + 
  ggtitle("Gender by Title") +
  xlab("Titles") +
  theme_bw()


# We initially constructed a model including sex as a variable. This model
# Did not pass the threshold of 72 % correct prediction, why we chose to exclude
# The sex variable. 
# mod1 <- glm(survived ~ sex + child + age + class + title + family.size.group, family = binomial, data = df)

# Logistic regression
final_mod <- glm(survived ~ child + age + class + title + family.size.group, family = binomial, data = df)

###########################

# Prediction accuracy and classification tables

#Predict_final_mod
df = df %>%
  mutate(pred_mod = predict(final_mod)) %>%
  mutate(pred_mod = case_when(pred_mod <= 0 ~ "Died",
                               pred_mod > 0 ~ "Survived"))

df = df %>%
  mutate(correct_prediction = case_when(pred_mod == survived ~ "correct",
                                        pred_mod != survived ~ "incorrect"))

# General correct categorization 
gen_correct_cat <-  df %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

# Xtable
df %>%
  group_by(survived, pred_mod) %>%
  summarize(n = n()) %>%
  spread(survived, n)

# correctly categorized as survived
df %>%
  filter(survived == "Survived") %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

# Did survive, correct accuracy: 73 %

# correctly categorized as did not survive
df %>%
  filter(survived == "Died") %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

#Did not survive, correct accuracy: 89 % 

#####################
# Null model comparison
#Predict null mod
mod_null = glm(survived ~ 1, family = binomial(), data = df)

df <- df %>%
  mutate(pred_mod_null = predict(mod_null)) %>%
  mutate(pred_mod_null = case_when(pred_mod_null <= 0 ~ "Died",
                                   pred_mod_null > 0 ~ "Survived"))
df = df %>%
  mutate(correct_prediction2 = case_when(pred_mod_null == survived ~ "correct",
                                         pred_mod_null != survived ~ "incorrect"))

df %>%
  group_by(correct_prediction2) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))
# Correct: 62 %


########

# Goodness of fit compared to Null model, McFaddenR square, AIC score etc

AIC(mod_null, final_mod)

pR2(final_mod)
pR2(final_mod)["llh"] * -2
pR2(mod_null)
pR2(mod_null)["llh"] * -2

lrtest(mod_null, final_mod)

### 


#############################################

# Calculating cha chance of survival in 4 cases: 
# 1. Kate w/ Leo, 2. Kate w/o Leo, 3. Sue w/ Leo, 4. Sue w/o Leo
# 1. Kate w/ Leo
log_odds3.84709 - (0.02091 * 20) - 2.27962 + 0.02853 - 0.05091 # Log odds = 1.12689
# Log odds - > Odds
exp(1.12689) # Odds = 3.086044
# Odds -> p
3.086044 / (1 + 3.086044) # = 0.7552645 = 76 %. 
#Kate has a 76 % survival rate w/ Leo onboard

# 2. Kate w/o Leo
log_odds2 <- 3.84709 - (0.02091 * 20) - 2.27962 + 0.02853 - 0.08632  # Log odds 1.0914
# Log odds -> Odds
odds2 <- exp(1.09148) # Odds = 2.978679
# Odds -> P
p2 <- 2.978679 / (1 + 2.978679) # P = 0.7486603 = 75 %
## Kate has a 75 % survival rate w/o Leo onboard

# 3. Sue w/ Leo
3.84709 + 0.09639 - (0.02091 * 4) - 2.27962 - 0.43260 + 0.05091  # Log odds = 1.19853
#Log odds -> Odds
exp(1.19853) # Odds = 3.31524
#Odds -> P 
3.31524 / (1 + 3.31524)  # P = 0.7682632 = 0.77
# Sue has a 77 % chance of survival w/ Leo onboard

# 4. Sue w/o Leo
3.84709 + 0.09639 - (0.02091 * 4) - 2.27962 - 0.43260 -0.08632   # Log odds = 1.0613
#Log odds -> Odds
exp(1.0613) # Odds = 2.890126
# Odds - > P 
2.890126 / (1 + 2.890126) # p = 0.7429389 = 0.74

# Sue has a 74 % chance of survival w/o Leo onboard

# According to our model, there is slight evidence that both Kate and Sue would be better off
# by having Leo on board when the Titanic sunk. For Kate, the chances of survival with Leo on board
# in our model increases by roughly 1 %, while for Sue, the chances of survival with Leo on board increases by roughly 3 %

# Final model table with Coeffiecients, confint, odds ratio, p values

final_mod_table <- tab_model(final_mod,show.se = FALSE,p.style = "numeric",
                        show.ci=0.95,show.std =TRUE,
                        show.stat = TRUE,show.est = TRUE,
                        show.aic = TRUE,show.dev = TRUE,
                        show.coefficients = TRUE,
                        show.loglik=TRUE,title = "Summary table")


