###
### TITLE: Political Analysis II
### Topic: Matching
###

#########################################################################

# install.packages("MatchIt")
# install.packages("cobalt")
library(MatchIt)   # implement matching
library(cobalt)    # check covariate (control variable) balance
rm(list=ls())
#####################
# Built-in dataset: 
# A subset of the job training program by Lalonde et al.
# Question: does job training program increase earning?

# treat: 1 if participating in job training program (JTP)
# re78: 1978 real earning as the outcome
# and a few other variables of individual-level attributes

data("lalonde")

summary(lalonde)

################

# First, use regression with control variables to estimate the effect of JTP
# earning variables have larger scales than other variables, so I prefer taking log
# and because some units have 0 earning, so I take log on the original value + 1 ("log1p")
# to avoid computational problem of log(0), which is unfined.

m.unadjusted <- lm(log1p(re78) ~ treat + 
                     age + educ + log1p(re75), data = lalonde)

summary(m.unadjusted)

# significant and positive effect of JTP
# (we exponentiate the coefficient because the outcome has been logged)
exp(coef(m.unadjusted)[2])

##################

### exact matching

m.out1 <- matchit(treat ~ age + educ + log1p(re75), 
                  data = lalonde, method = "exact")

summary(m.out1)

# check balance
bal.tab(m.out1)

# we use match.data() function to extract the matched set

summary(match.data(m.out1))
head(match.data(m.out1))

m.adjusted1 <- lm(log1p(re78) ~ treat, data = match.data(m.out1))
summary(m.adjusted1)

#################################################################

### coarsened exact matching

#install.packages("cem")

library(cem)

m.out2 <- matchit(treat ~ age + educ + log1p(re75), 
                  data = lalonde, method = "cem")

# How to customize the cutpoints and strata for each covariates:
# https://cran.r-project.org/web/packages/cem/vignettes/cem.pdf

summary(m.out2)

plot(m.out2)
# deviations from the 45-degree-line indicate unequal distribution between
# controls and treateds

m.adjusted2 <- lm(log1p(re78) ~ treat + age + educ + log1p(re75), 
                  data = match.data(m.out2), weights = weights)
summary(m.adjusted2)

##########################################################################

### Mahalanobis distance matching

m.out3 <- matchit(treat ~ age + educ + re75, 
                  data = lalonde, method = "nearest", 
                  distance = "mahalanobis")

summary(m.out3)
plot(m.out3)

m.adjusted3 <- lm(log1p(re78) ~ treat + 
                    age + educ + log1p(re75), data = match.data(m.out3), 
                  weights = weights)
summary(m.adjusted3)


#################################################################

### propensity score matching

# PSM is the default in "nearest"

m.out4 <- matchit(treat ~ age + educ + re74, 
                  data = lalonde, method = "nearest")

summary(m.out4)

plot(m.out4)

# check balance 
# (if not using exact matching, it is a good idea to test covariate balance)
bal.tab(m.out4, m.threshold = 0.05)

# covariates not entirely balanced, 
# so run regression with those variables for control

m.adjusted4 <- lm(log1p(re78) ~ treat + age + educ + log1p(re74), 
                  data = match.data(m.out4), weights = weights)

summary(m.adjusted4)

