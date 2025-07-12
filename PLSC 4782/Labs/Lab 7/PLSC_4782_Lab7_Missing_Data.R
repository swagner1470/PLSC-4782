###
### TITLE: Political Analysis II
### Topic: Missing Data Imputation
###

#########################################################################

# NA for "not available" in R, meaning "missing value"

# the factory-fresh setting is "na.action = na.omit" (casewise deletion)

# If you want to change the setting to "na.fail"
# options(na.action = na.fail)
# which prevents R from running any computation if there is an "NA"

# If you made the change and want to change back the original setting
# options(na.action = na.omit) 

# Notice that this change applies to all computation you will run

####################################################################

# To do multiple imputation (IM) with Fully Conditional Specification (MICE), 
# use the following package:
library(mice)

# In the following, I want to show how casewise deletion will be biased and 
# how well MI can do 
# by creating missing data in a complete dataset and see how well MI can do 

##########################################################
rm(list=ls())
# Data from Greene's "Econometric Analysis"
# U.S. Investment Data, 15 Yearly Observations, 2000-2014
invest.df <- read.csv("invest.csv")

# Year = Date,
# RealGNP = GNP Quantity Index,
# Invest = Nominal Investment in $T,
# GNPDefl = GNP Deflator,
# Interest = Interest rate = Prime rate.
# Infl = Yearly change in consumer price index.
# RealInv = Nominal investment/(.01*GNP Deflator)

invest.df

################################################

# First, let's do a standard linear regression
# which gives us the "ground truth" (the coefficients with complete data)

invest.lm <- lm(RealInv ~ Trend + RealGNP + Interest + Infl, 
                data = invest.df)

summary(invest.lm)

#################################################

# Next, let's create "holes" in invest.df

# to avoid over-writing the original dataset, create a new one
invest.missing.df <- invest.df

invest.missing.df[1, 2] <- NA
invest.missing.df[4, 5] <- NA
invest.missing.df[6, 7] <- NA
invest.missing.df[15, 8] <- NA

# compute the ratio of missing values
sum(is.na(invest.missing.df)) / prod(dim(invest.missing.df))

invest.missing.df

############################################################

# Now, do another linear regression using the dataset with missing values

invest.missing.lm <- lm(RealInv ~ Trend + RealGNP + Interest + Infl, 
                        data = invest.missing.df, 
                        na.action = na.omit)

# compare the result by casewise deletion and the "ground truth"

summary(invest.lm)
summary(invest.missing.lm)

############################################################

# Now, let's use "mice" to do MI

imp.invest <- mice(invest.missing.df, m = 8) # do 8 multiples

# alternatively, if you want to do it longhand one by one
# imp.invest1 <- complete(imp.invest, 1)
# imp.invest2 <- complete(imp.invest, 1)


# run regression analysis with the multiples
invest.mids <- lm.mids(RealInv ~ Trend + RealGNP + Interest + Infl, 
                       data = imp.invest)

summary(invest.mids)

# then do Rubin's Combination to get the final result
summary(pool(invest.mids))

summary(invest.lm) # compare to the "ground truth"

##################################################################

# To make "mice" compatible to other regression models you may want to run

library(Zelig)

# once you have an output from "mice" function (e.g. "imp.invest")

# use the following function to make mice output workable for zelig()
# this is programmed by our Professor Skyler Cranmer 
mids2mi <- function(object){
  nmids <- object$m
  mi.data <- list()
  for(i in 1:nmids){
    mi.data[[i]] <- complete(object, action=i)	}
  class(mi.data) <- c("mi", "list")
  return(mi.data)   }

# implement mids2mi() on imp.invest to make it workable for zelig
imp.invest2 <- mids2mi(imp.invest)

# zelig() is a flexible function to different types of LM and GLM
invest.2 <- zelig(RealInv ~ Trend + RealGNP + Interest + Infl, 
                  data = imp.invest2, model = "ls")

summary(invest.2)

# report zelig result by stargazer
library(stargazer)
stargazer(lm(invest.2$zelig.out$z.out[[1]]), out = "MI.html")

