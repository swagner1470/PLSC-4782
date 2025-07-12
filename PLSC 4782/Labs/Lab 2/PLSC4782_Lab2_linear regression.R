###
### TITLE: Political Analysis II, Lab 2
### Linear Regression Basics
###


# Reading in data (the Quality of Government data, on Carmen)

# before loading the dataset, set the working directory first
qog <- read.csv("qog_long.csv", header = T, stringsAsFactors = F)

##################################################################
# Variables for this analysis:

# bti_ci: conflict intensity (our outcome variable of interest)
# bti_ffe: free and fair election, 10 for the best
# al_ethnic: ethnic fractionalization
# mad_gdppc: GDP per capita

##################################################################

# The original dataset is too large. Let's focus on just a few variables
# To make our dataset clean, we subset it by selected variables

qog_s <- subset(qog, select = c("bti_ci", "bti_ffe", "al_ethnic", 
                                "mad_gdppc"))

# first of all, explore the summary statistics of the dataset
# and the distribution of outcome variable
summary(qog_s)

plot(density(qog_s$bti_ci, na.rm = T))  # good enough for the normality assumption

# take a look at GDP per capita
plot(density(qog_s$mad_gdppc, na.rm = T))  # a long tail

# log can adjust it toward the normal distribution
plot(density(log(qog_s$mad_gdppc), na.rm = T))

##################################################################

## FIT THE REGRESSION MODEL

## Here we fit the linear regression model. The function for
## the model is lm() which takes arguments:
##  formula:  the expression of the model. We put the dependent
##            variable, then a tilde, then our independent 
##            variable. For example, Y ~ X.
##  data:     the name of the data frame where the variables
##            can be found.
##

### Let's first test if economic development helps to explain conflict intensity
model1 <- lm(bti_ci ~ mad_gdppc, data = qog_s)

### Get a full summary of the regression result, which includes:
# distribution of residuals
# coefficient table (estimate, standard error, t statistic, p value)
# residual standard error
# R^2 (proportion of the variation explained by the model)
# F statistic and test (test if the model is better than a model without any explanatory variables)

summary(model1)

# The coefficient is too small. So we would like to use log(GDP per capita) instead
# to the magnitude of the coefficient more visiable

model1 <- lm(bti_ci ~ log(mad_gdppc), data = qog_s)
summary(model1)


### Add more variables into the model

model2 <- lm(bti_ci ~ log(mad_gdppc) + bti_ffe, data = qog_s)

summary(model2)


model3 <- lm(bti_ci ~ log(mad_gdppc) + bti_ffe + al_ethnic, data = qog_s)

summary(model3)

# notice that R^2 increases and the coefficient of GDP per capita changes

#########################################################################

### important technique 1: 
### generate regression table

#install.packages("stargazer") # run this line only when the package is not installed

library(stargazer)

# generate a regression table to include model1 and model2 in a html file 
stargazer(model1, model2, model3, out = "model.html")

# open that html file, and then copy and paste the table to your word document 


### important technique 2: 
### standardized coefficients to compare effect size
### formula: beta * sd(X) / sd(Y)

# we can directly extract all the parameters by
model3$coefficients

# or
coef(model3)

# standardizing the coefficient of GDP per capita
coef(model3)[2] * sd(log(qog_s$mad_gdppc), na.rm = T) / sd(qog_s$bti_ci, na.rm = T)
# which is -0.39 (original coef = -0.73)

# standardizing the coefficient of free election score
coef(model3)[3] * sd(qog_s$bti_ffe, na.rm = T) / sd(qog_s$bti_ci, na.rm = T)
# which is -0.18 (original coef = -0.14)

# So the level of economic development 
# has a greater impact on conflict intensity


### important technique 3: 
### make prediction based on regression models

# get predicted values for all the observations (systematic component)
predict(model3)

# make prediction with a fake observation
new_data <- data.frame(mad_gdppc = 10000, bti_ffe = 5, al_ethnic = 0.5)

predict(model3, new_data)
# the predicted conflict intensity value is 4.73, with our model 3                    

# we can also ask for a 99% confidence interval for this prediction
predict(model3, new_data, interval = "confidence", level = 0.99)


### important technique 4: 
### deal with categorical explanatory variable and interpretation

# We will use Duncan dataset from car package for this part

library(car) 
?Duncan

summary(Duncan)

# types of occuption is one predictor for presitge value

# make sure what you put into the function is a factor
d_m1 <- lm(prestige ~ education + income + factor(type), data = Duncan)

summary(d_m1)

## "bc" is missing, which is used as the reference group for comparison
## coefficient of "prof" tells you the difference between "bc" and "prof"
## coefficient of "wc" tells you the difference between "bc" and "wc"

# you can choose which category you want to drop by relevel the variable
Duncan$type <- relevel(Duncan$type, ref = "prof")
d_m2 <- lm(prestige ~ education + income + factor(type), data = Duncan)

summary(d_m2)


### important technique 5: 
### use high-order and interactive terms

# back to QOG data

# assume this is your base model 

m2 <- lm(iiag_gov ~ bti_ffe + log(mad_gdppc), data = qog)
summary(m2)

# You want to test if the effect of GDP per capita is nonlinear

nl_m1 <- lm(iiag_gov ~ bti_ffe + log(mad_gdppc) + I(log(mad_gdppc)^2), 
            data = qog)
summary(nl_m1)
# It looks that a quadric specification does not work


## Now let's use an interactive term to see if the effect of election is 
## different across high- and low-income countries

# first let's dichotomize GDP per capita variable so the coefficients of 
# interactive model are more interpretable

qog$highincome <- ifelse(qog$mad_gdppc >= 8000, 1, 0)

inter_m <- lm(iiag_gov ~ bti_ffe + highincome + bti_ffe:highincome, 
              data = qog)

summary(inter_m)

# highincome is group indicator, 
# its coefficient represents baseline group difference in iiag_gov

# coefficient of election represents the effect of election in low-income group
# the sum of the coefficient of election and that of the interactive term
# represents the effect of election in high-income group


### additional technique: 
### use ggplot package to inform regression analysis

# in case you do not have this package in your computer, run the following:
# install.packages("ggplot2")

library(ggplot2) 

### gpplot starts with defining data source and setting up coordinates

ggplot(Duncan, aes(x = income, y = prestige))

# we can save this base plot in the memory and build a more sophisticated plot on it
g <- ggplot(Duncan, aes(x = income, y = prestige))

### add data points into the base

g + geom_point()  
# equivalent to ggplot(chic, aes(x = date, y = temp)) + geom_point()

### tweak color of data points
g + geom_point(color = "firebrick")

# don't want the default greyish background 
g + geom_point(color = "firebrick") + theme_bw()

### link education to color scheme and occupational type to symbol shape
ggplot(Duncan, aes(x = income, y = prestige, colour = education)) + 
  geom_point() + theme_bw()

ggplot(Duncan, aes(x = income, y = prestige, 
                   colour = education, shape = type)) + 
  geom_point() + theme_bw()

### add smooth lines to indicate trends by groups

ggplot(Duncan, aes(x = income, y = prestige, 
                   colour = type)) + 
  geom_point() + theme_bw() + stat_smooth(method = "lm")


### or use small facets to show patterns in different groups

ggplot(Duncan, aes(x = income, y = prestige, 
                   colour = education)) + 
  geom_point() + theme_bw() + stat_smooth(method = "lm") + 
  facet_wrap(~ type)


