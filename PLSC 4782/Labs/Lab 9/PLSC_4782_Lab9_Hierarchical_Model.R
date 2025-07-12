###
### TITLE: Political Analysis II
### Topic: Hierarchical (multilevel) Model
###

#########################################################################

#install.packages("lme4")
#install.packages("panelView")


library(lme4) # package for multilevel LM and GLM
library(panelView) # package for visualizing panel data, we will use its data

data(panelView) # load datasets from the package

# We use capacity dataset: county-year panel (non-nested)
# country: country name (individual ID)
# ccode: country code (alternative individual ID)
# year: (time ID)
# Capcaity: outcome, state capacity
# demo: binary indicator for democracy ("treatment")
# lnpop: population (log)
# lngdp: GDP per capita (log)
# polity2: index for democracy level

####################################

### Pooling model

m.pool <- lm(Capacity ~ demo + lnpop + lngdp, data = capacity)

summary(m.pool)

### No-pooling model (by country)

m.nopool <- lm(Capacity ~ factor(country) - 1, data = capacity)

# "- 1" to get rid of the common intercept

summary(m.nopool)

# note that country id alone explains 76% variation in the panel

### Country fixed-effects model

m.cfe <- lm(Capacity ~ demo + lnpop + lngdp + 
               factor(country) - 1, data = capacity)

summary(m.cfe)

# it shows the effect of demo on capacity within countries


### Two-way fixed-effects model (with country and year)

m.2fe <- lm(Capacity ~ demo + lnpop + lngdp + 
              factor(country) + factor(year) - 1, data = capacity)

summary(m.2fe)

# now the coefficient of demo is significant
# adding year fixed-effects help account for the unobserved factors that 
# equally affect all countries in each year

#######

# to do fixed-effects linear models more efficiently, use "lfe" package

library(lfe)

m.2fe.2 <- felm(Capacity ~ demo + lnpop + lngdp | 
                  country + year, data = capacity)

# use "|" to separate individual-level predictors and group indicators

summary(m.2fe.2)

########################################################################

### multilevel model with varying intercept

mle1 <- lmer(Capacity ~ demo + lnpop + lngdp + 
               (1 | country) + (1 | year), data = capacity)

# (1 | country) means to allow intercept to vary by country
# (1 | year) allows intercept to vary by year
# "1" standards for intercept 

summary(mle1)
# "fixed effects" in the output are estimated model averaging over country-year

# to look at individual intercepts
coef(mle1)
# Afghanistan's intercept is -6.44, Zimbabwe -5.41

##############

### multilevel model with varying intercept and varying slope on "demo"

# I drop years' intercepts simply because of the data constraint in this case

mle2 <- lmer(Capacity ~ demo + lnpop + lngdp + 
               (1 + demo | country), data = capacity)

summary(mle2)

# to look at individual intercept and slope (coefficient)
coef(mle2)

# to look at estimates of random coefficient and intercept
ranef(mle2)

###########################################################

## to estimate multilevel GLM, use glmer() instead of lmer()

## glmer() follow the same syntax 
#(but you need to decide a proper probability distribution as you do for glm)

