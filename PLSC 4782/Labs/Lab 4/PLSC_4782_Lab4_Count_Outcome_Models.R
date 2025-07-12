###
### TITLE: Political Analysis II, Lab 4
### Topic: Count Outcome Models
###

#########################################################################

### Running Example: 
### Interlocking directorates among major Canadian firms

# Source: Ornstein, M. (1976)
rm(list=ls())
setwd("E:/PLSC 4782/Labs/Lab 4")
library(carData)

data("Ornstein")

?Ornstein # Read the codebook

summary(Ornstein)

# firm-level dataset

# Outcome variable: interlocks (No. of interlocking director and executive positions) 

# Explanatory variable: assets, sector, nation

##############################################

# Step 1: possbility of doing a rate model
# Do we have a meaningful measure of the scale of each unit (e.g. size of directorate)?
# Unfortunately, no

# Step 2: explore the outcome variable
# Can we assume a normal distribution and therefore use OLS?
# Look at the distribution of the outcome and the mean

plot(density(Ornstein$interlocks))
abline(v = mean(Ornstein$interlocks))

# Long tail on the right, and not a very large mean to assume a normal distribution
# No good "size" variable to construct a rate measure either
# So we should use count GLM (Poisson and others)

#################

### Poisson Model

# still use glm(), but specify "family" type as "poisson"

model1 <- glm(interlocks ~ log(assets) + sector + nation, data = Ornstein, 
              family = poisson)

summary(model1)

# Interpret the effects of coefficients by exponentiation
exp(coef(model1))

# Remember that the effects are multiplicative, due to the exponential component

#########################

### Is Poisson regression a right choice? We need to test dispersion assumption 
# It is best to do it before running a Poisson regression

# The disperson test is based on "AER" package (make sure to install it first)

# install.packages("AER")
library(AER)

dispersiontest(model1, alternative = "greater") 
# we specify overdispersion as the alternative H

# p < 0.05, reject the null and choose the alternative H
# we do have over-dispersion
# So we need to use quasi-poisson or negative binomial instead

######################

### Run a quasi-Poisson

model2 <- glm(interlocks ~ log(assets) + sector + nation, 
                 data = Ornstein, family = quasipoisson)

summary(model2)

# Notice the estimated value of dispersion parameter (phi)

####

### Run a negative binomial model
library(MASS)
model3 <- glm.nb(interlocks ~ log(assets) + sector + nation, 
                 data = Ornstein)

summary(model3)

exp(coef(model3))

########################################################

### Use a likelihood ratio test to compare two nested models
# recall our lecture on MLE

# Let's compare two negative binomial models:

res.m <- glm.nb(interlocks ~ log(assets) + sector, data = Ornstein)
unres.m <- glm.nb(interlocks ~ log(assets) + sector + nation, 
                  data = Ornstein)

# The point for doing a likelihood ratio test is to figure out if
# the unrestricted model is SIGNIFICANTLY better than the restricted one 
# (in terms of the log likelihood score)

# In other words, whether including additional variable(s) in our model does
# help improve our model

# The test function is based on "lmtest" (install first if you don't have it)

#install.packages("lmtest")

library(lmtest)
lrtest(res.m, unres.m)

# p < 0.05, significant difference between the two
# Conclusion: the unrestricted model is SIGNIFICANTLY better
# in other words, "nation" is a meaningful explantory variable to be added



