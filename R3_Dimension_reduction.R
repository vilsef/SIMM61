# assignment 3 - PCA & EFA

library(GGally) # for ggcorr	
library(corrr) # network_plot	
library(ggcorrplot) # for ggcorrplot	
library(FactoMineR) # multiple PCA functions	
library(factoextra) # visualisation functions for PCA (e.g. fviz_pca_var)	
library(skimr)
library(paran) # for paran	

library(psych) # for the mixedCor, cortest.bartlett, KMO, fa functions	
library(car) # for vif	
library(GPArotation) # for the psych fa function to have the required rotation functionalities	
library(MVN) # for mvn function	
library(ICS) # for multivariate skew and kurtosis test	
library(tidyverse) # for tidy code
library(sjstats)
library(sjPlot)
library(xtable)

df <- read.csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Exercise_06%20-%20CFA%20and%20EFA/animalrights.csv", sep= ",", header = T)

## EXPLORATION
#Descriptive statistics and correlations in the dataset
# Setup dataframe
df <- df %>% 
  mutate(sex = factor(sex, levels = c(1,2), 
         labels = c("Female", "Male")))
# Gender representation
table(df$sex)
## 120 Female, 29 Male (!)

describe(df)

# Lot of items have a moderate to high skewness ()

# drop NA's
df <- df %>% 
  drop_na()

df %>% 
  describe()

# This fviz_loadings_with_cor() is a custom function that will be used in this exercise the visualize some of the results of the principal component analysis and exploratory factor analysis.	
fviz_loadings_with_cor <- function(mod, axes = 1, loadings_above = 0.4){	
  require(factoextra)	
  require(dplyr)	
  require(ggplot2)	
  
# Code for visualizing data
  if(!is.na(as.character(mod$call$call)[1])){	
    if(as.character(mod$call$call)[1] == "PCA"){	
      contrib_and_cov = as.data.frame(rbind(mod[["var"]][["contrib"]], mod[["var"]][["cor"]]))	
      
      vars = rownames(mod[["var"]][["contrib"]])	
      attribute_type = rep(c("contribution","correlation"), each = length(vars))	
      contrib_and_cov = cbind(contrib_and_cov, attribute_type)	
      contrib_and_cov	
      
      plot_data = cbind(as.data.frame(cbind(contrib_and_cov[contrib_and_cov[,"attribute_type"] == "contribution",axes], contrib_and_cov[contrib_and_cov[,"attribute_type"] == "correlation",axes])), vars)	
      names(plot_data) = c("contribution", "correlation", "vars")	
      
      plot_data = plot_data %>% 	
        mutate(correlation = round(correlation, 2))	
      
      plot = plot_data %>% 	
        ggplot() +	
        aes(x = reorder(vars, contribution), y = contribution, gradient = correlation, label = correlation)+	
        geom_col(aes(fill = correlation)) +	
        geom_hline(yintercept = mean(plot_data$contribution), col = "red", lty = "dashed") + scale_fill_gradient2() +	
        xlab("variable") +	
        coord_flip() +	
        geom_label(color = "black", fontface = "bold", position = position_dodge(0.5))	
      
      
    }	
  } else if(!is.na(as.character(mod$Call)[1])){	
    
    if(as.character(mod$Call)[1] == "fa"){	
      loadings_table = mod$loadings %>% 	
        matrix(ncol = ncol(mod$loadings)) %>% 	
        as_tibble() %>% 	
        mutate(variable = mod$loadings %>% rownames()) %>% 	
        gather(factor, loading, -variable) %>% 	
        mutate(sign = if_else(loading >= 0, "positive", "negative"))	
      
      if(!is.null(loadings_above)){	
        loadings_table[abs(loadings_table[,"loading"]) < loadings_above,"loading"] = NA	
        loadings_table = loadings_table[!is.na(loadings_table[,"loading"]),]	
      }	
      
      if(!is.null(axes)){	
        
        loadings_table = loadings_table %>% 	
          filter(factor == paste0("V",axes))	
      }	
      
      
      plot = loadings_table %>% 	
        ggplot() +	
        aes(y = loading %>% abs(), x = reorder(variable, abs(loading)), fill = loading, label =       round(loading, 2)) +	
        geom_col(position = "dodge") +	
        scale_fill_gradient2() +	
        coord_flip() +	
        geom_label(color = "black", fill = "white", fontface = "bold", position = position_dodge(0.5)) +	
        facet_wrap(~factor) +	
        labs(y = "Loading strength", x = "Variable")	
    }	
  }	
  
  
  
  
  
  
  return(plot)	
  
}	
     

#Initial exploration, correlation of predictors, subsetting items 1:28
df_items_only = df %>%
  select(ar1:ar28)
cor = df_items_only %>% 
  cor()
cor

# VIF intercorrelations
vif(mod_allitems)

#Visualization of correlation
ggcorr(cor)
ggcorrplot(cor(df_items_only), p.mat = cor_pmat(df_items_only),
           hc.order = TRUE, type = "lower")

# Network correlation plot
cor(df_items_only) %>%
  network_plot(min_cor = 0.6)

#When testing factorability the question is whether there is enough correlation between 
#the observed variables to allow EFA to be used. We will now check the assumptions 
#of factorability and multivariate normality

#Correlation matrix
df_Cor <- cor(df_items_only)
df_Cor

#Bartlett sphericity test
bfi_factorability <- cortest.bartlett(df_Cor)
bfi_factorability

# The BFI test is significant, Meaning the null and the complex matrices are significantly different from each
# pointing to that the observed variablesare factorable. However, the BFI test is usually only confident if 
# the number of our observed variables are lower than 5. Since this is not the case, we will carry on with
# a KMO test. 

KMO(df_Cor) # KMO test

# KMO Test showing the difference between the partial and the regular correlation matrix. 
# As shown, the KMO is higher than 0.6 in all cases, meaning the data seems factorable

#Next we determine whether the data show a multivariate normal distribution. If the
#p-value of these tests is lower than 0.05, it indicates the violation of the 
#multivariate normality assumption

result <- mvn(df[, 1:28], mvnTest = "hz")
result$multivariateNormality
mvnorm.kur.test(na.omit(df[, 1:28]))
mvnorm.skew.test(na.omit(df[, 1:28]))
    
# Shown in the tests above where all p values are below 0.05, it is indicated 
# that the the observed variables do not show multivariate normal distribution
# Hence, assumption of normality is violated. For this reason, we turn on 
# to Principal Axis factoring which is preferred when the observed variables 
# do not show multivariate normal distribution.

########################################################

#PAF
#The communalities indicate the proportion of variance in the observed variable explained 
#by the extracted factors.

#1st iteration
EFA_mod <- fa(df_Cor, nfactors = 5, fm = "pa")
EFA_mod_common <- as.data.frame(sort(EFA_mod$communality, decreasing = TRUE))	
EFA_mod_common 	

# 2nd iteration
EFA_mod <- fa(df_Cor, nfactors = 2, fm = "pa")
EFA_mod_common <- as.data.frame(sort(EFA_mod$communality, decreasing = TRUE))	
EFA_mod_common 	

# Iteration 1: The communality table shows that ar5: "ar 5.    It is wrong to wear leather jackets and pants." is the best represented item in the 5-factor structure,
#with 90% of its total variance explained by the new factors. Ar3 on the other hand:  "ar 3.    It is morally wrong to drink milk and eat eggs",
# is the least represented item, with only 21% of its variance explained by the current factor structure.

#################

# Choosing the ideal number of factors
fa.parallel(df_Cor, n.obs = nrow(df), fa = "fa", fm = "pa") # Parallell test
nfactors(df_Cor, n.obs = nrow(df)) # VSS technique
#vss(df_Cor)
vss(df_Cor2)
EFA_scree <- PCA(df_items_only) # Scree test
fviz_screeplot(EFA_scree, addlabels = TRUE, ylim = c(0, 85)) # Scree test
get_eigenvalue(EFA_scree)

# The Eigeinvalue analysis tells us we should keep a total of 7 dimensions, as the 8th show a score < 1.0. 
# We do note that This particular method is not commonly used, or is only used in combination with other rules, why we don't put to much weight
# on these results. Both parallell analysis and Scree plot test indicate 
# retention of two factors, which is what we choose to do. 

# Finalizing our model reducing to 2 factors 
# Factor rotation on final model.
# We here choose the Oblique rotation method as it assumes that the factors 
# extracted from a Factor Analysis are correlated. Based on the 
# questions in our animal rights dataset, we assume that the majority of the questions
# are correlated.
EFA_mod2 <- fa(df_Cor, nfactors = 2, fm = "pa", rotate = "oblimin")
summary(EFA_mod2) # Correlation coefficient test between factors, supporting
# usage of oblique rotation method

# Factor analysis diagram + item loadings ( 2 factor structure)
fa.diagram(EFA_mod2)
fviz_loadings_with_cor(EFA_mod2, axes = 1, loadings_above = 0.4)
fviz_loadings_with_cor(EFA_mod2, axes = 2, loadings_above = 0.4)

# Based on the factor analysis diagram and loadings, we conclude that some items show very low loading on each factor
# (1, 8, 11, 14 and 25). Based on these results, we choose to exclude these 5 items out of our model.

# Excluding items with low loadings/communality (item 1,8,11,14,25. 
df_items_only2 <- subset(df_items_only, select = -c(1,8,11,14,25))
df_Cor2 <- cor(df_items_only2)

# Repeating previous steps (ideal factor number & which items belong to which factor.
fa.parallel(df_Cor2, n.obs = nrow(df), fa = "fa", fm = "pa") # Parallell test
nfactors(df_Cor2, n.obs = nrow(df)) # VSS technique
vss(df_Cor2)
EFA_scree2 <- PCA(df_items_only2) # Scree test
fviz_screeplot(EFA_scree2, addlabels = TRUE, ylim = c(0, 85)) # Scree test

get_eigenvalue(EFA_scree2)

# We conclude that the number of factor test show similar results as our first iteration,
# hence we choose to keep two factors for our final model. 

#Constructing final model
EFA_mod3 <- fa(df_Cor2, nfactors = 2, fm = "pa", rotate = "oblimin")

###### Analyze which items belong to which factors
fa.diagram((EFA_mod3))
fviz_loadings_with_cor(EFA_mod3, axes = 1, loadings_above = 0.4)
fviz_loadings_with_cor(EFA_mod3, axes = 2, loadings_above = 0.4)

# post extraction communalities
EFA_mod3_common <- as.data.frame(sort(EFA_mod3$communality, decreasing = TRUE))
EFA_mod3_common
mean(EFA_mod3$communality)

# The post extraction communality score of our final model showed that ar 6:"
# "6: Most medical research done on animals is unnecessary and invalid." and ar13: "
# "13: It is wrong to wear leather belts and shoes. " was best represented in our 2 factor
# structure, explaining 69 % and 63 % respectively of its total variance in our factors. 
# We also note that only 10 / 23 items resulted in a communality score of >.4. These limitations 
# will be adressed in our final discussion. 

# Saving factorscores
factorscores <- factor.scores(df_items_only2,EFA_mod3)$scores
factorscores
df_with_factorscores = cbind(df, factorscores)

## Perform regression analysis to predict how conservative 
#or liberal participants are (using the "liberal" variable as a
# dependent variable) with the factors we identified as the predictors.

# Renaming factors to new labels
df_with_factorscores <- df_with_factorscores %>% 
  rename(Exploitation = (PA1),
         Research = (PA2))

# Final linear model predicting liberal score with factor scores
lib_mod <- lm(liberal ~ Exploitation + Research, data = df_with_factorscores)
summary(lib_mod)

tab_model(lib_mod)

# Our final linear model predicting liberal values with our animal rights model scores show that liberal values are positively correlated with 
# factor 1 measuring opinions on animal exploitation (0 = support exploitation, 5 = do not support exploitation).
# Liberal values show a weak negative relationship with factor 2 measuring animal research (0 = Support animal research, 5 = Do not support animal research)
# To sum up, our model indicates that liberal values are correlated with support of non-abuse of animals, but are
# also slightly correlated with support of research on animals. However, we 
# do note that our model only show significant results for the Explotation predictor. 

# We conclude that factor 1 measuring animal exploitation was the most influential predictor of how liberal a person is.
# Based on theory and previous knowledge, we can assume that liberal standpoints 
# support non abuse of animals, but that liberal views also correlate to a society where
# research on animals may be necessary for development and liberal societies. 

