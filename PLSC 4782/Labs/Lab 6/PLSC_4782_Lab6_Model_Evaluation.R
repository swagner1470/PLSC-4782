###
### TITLE: Political Analysis II
### Topic: Model Evaluation
###

#########################################################################
rm(list=ls())

### 1. Training Set/ Test Set
install.packages("faraway")
library(faraway)
data(fat)

# set a seed No. for random number generator
set.seed(123) 

# seriously, never forget to set your seed when doing *anything* 
# with a random number generator if you expect to replicate the result later

# create and index to divide the data with 30% in expectation
index <- rbinom(nrow(fat), 1, 0.3) 

# in practice, a little bit less than that
mean(index) 

# create a test set
test <- subset(fat, index == 1)

# create a training set
train <- subset(fat, index == 0)

# estimate a model on the training set
tmod <- lm(brozek ~ age + weight + height + neck + chest + abdom + 
             hip + thigh + knee + ankle + biceps + forearm + wrist, 
           data = train)

# generate predictions for the test set
preds <- predict(tmod, new = test, interval = "prediction") 

plot(preds[,1], test$brozek, 
     xlab = "predicted y", ylab = "actual y")
abline(lm(test$brozek ~ preds[,1]))



### 2. Two Cross Validation Methods

library(boot) # where cv.glm function comes from

glm1 <- glm(brozek ~ age + weight + height + neck + chest + abdom + hip + 
              thigh + knee + ankle + biceps + forearm + wrist, data = fat, 
            family = "gaussian")

summary(glm1)

# Four fold cross validation
cv4f <- cv.glm(data = fat, glm1, K = 4)
cv4f 
# first delta value is raw cv and second is adjusted (for the bias of not using LOOCV)

# LOOCV
loo <- cv.glm(data = fat, glm1, K = nrow(fat))
loo

### 3. Robust Standard Error

library(lmtest) # where coeftest() comes from
library(sandwich) # where "sandwich" and "vcovHC()" comes from

model1 <- lm(brozek ~ age + weight + height + neck + chest + abdom + 
               hip + thigh + knee + ankle + biceps + forearm + wrist, 
             data = fat)

summary(model1)

plot(model1) # use diagnostic plots to check potential heteroskedasticity

# get coefficients and their standard error estimates
coeftest(model1)

# re-estimate standard error using "sandwich" method (robust standard error)
coeftest(model1, vcov. = sandwich)
# or
coeftest(model1, vcov. = vcovHC, type = "HC1")

# "HC3" is the default (homoscedastic) SE

##########################

# report robust SE in stargazer tables

library(stargazer)

# take two steps to extract the list of robust standard errors only
cov1 <- vcovHC(model1, type = "HC1")
robust_se <- sqrt(diag(cov1))

stargazer(model1, se = list(NULL, robust_se), out = "table1.html")

