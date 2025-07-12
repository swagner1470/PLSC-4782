###
### TITLE: Political Analysis II, Lab 3
### Binary Outcome Models
###

##############################################

### Dataset: graduate school application  

# outcome: admit (binary)
# predictors: gre, gpa, (college) rank
setwd("E:/PLSC 4782/Labs/Lab 3")
admission <- read.csv("binary.csv")
summary(admission)

###

# To build a GLM, we use glm() instead of lm()

# The usage of glm() is largely the same to that of lm(),
# we need to specify model formula and data source using the same syntax.

# For glm(), we also need to describe what distribution we want to use for
# modeling the outcome variable. In binary outcome cases, the distribution 
# should be "binomial" (if you leave distribution unspecified, 
# "gaussian", that is the normal, will be used as default).

model1 <- glm(admit ~ gre + gpa + rank, data = admission, 
              family = binomial)

# logit is the default method for "binomial" type of GLM

summary(model1)

library(stargazer)

stargazer(model1, out = "model.html")

# If you want to switch to probit, add "(link = probit)" after 
# "binomial"

model2 <- glm(admit ~ gre + gpa + rank, data = admission, 
              family = binomial(link = probit))

summary(model2)

################

# Interpret coefficients in logit

summary(model1)

# In the lecture, we introduced four methods
# 1. use inverse logit to predict
# 2. use the first derivative of inverse logit to get an instantaneous effect
# 3. divide by 4
# 4. exponentiating coefficients and interpret by odds 

# exponentiation in R: exp()
# compute e to the power of 2
exp(2)

# We can make prediction by manually entering the inverse logit function
# exp()/(1+exp())
# We can also use invlogit() provided by arm package instead


# Example: How the probability would change if gpa increases from 2 to 3?

# Let's set the rest predictors at their means

coef(model1)

eta1 <- coef(model1)[1] + coef(model1)[2] * mean(admission$gre) + 
  coef(model1)[3] * 2 + coef(model1)[4] * mean(admission$rank)

eta2 <- coef(model1)[1] + coef(model1)[2] * mean(admission$gre) + 
  coef(model1)[3] * 3 + coef(model1)[4] * mean(admission$rank)

exp(eta2)/(1+exp(eta2)) - exp(eta1)/(1+exp(eta1))

# Or
library(arm)

invlogit(eta2) - invlogit(eta1)


### divide by 4

coef(model1)/4

# For GPA, the maximum possible change  
# in probability corresponding to 1 increase is +0.19


### exponentiation and odds

exp(coef(model1))

# For GPA, one unit increase leads to 17.5% increase in odds
exp(coef(model1))[3] - 1

# For rank, one unit increase leads to 42.9% decrease in odds
1 - exp(coef(model1))[4]

