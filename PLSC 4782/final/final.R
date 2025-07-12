# Final exam data set cleaning
library(tidyverse)
library(nnet)
#library(poliscidata)
rm(list=ls())
gc()
set.seed(111546335)
#data(gss)
#write.csv(gss, file = "finaldata.csv")
finaldata <- read.csv("E:/PLSC 4782/final/finaldata.csv")
library(truncnorm)
?gss

finaldata <- finaldata %>%
  mutate(income = ifelse(income06 == "UNDER $1 000",rtruncnorm(n = length(income06 == "UNDER $1 000"),a=0,b=1000,mean = 500,sd = 250),
ifelse(income06 == "$1 000 TO 2 999",rtruncnorm(n = length(income06 == "$1 000 TO 2 999"),a=1000,b=3000,mean = 2000,sd = 500),
ifelse(income06 == "$3 000 TO 3 999",rtruncnorm(n = length(income06 == "$3 000 TO 3 999"),a=3000,b=4000,mean = 3500,sd = 250),
ifelse(income06 == "$4 000 TO 4 999",rtruncnorm(n = length(income06 == "$4 000 TO 4 999"),a=4000,b=5000,mean = 4500,sd = 250),
ifelse(income06 == "$5 000 TO 5 999",rtruncnorm(n = length(income06 == "$5 000 TO 5 999"),a=5000,b=6000,mean = 5500,sd = 250),
ifelse(income06 == "$6 000 TO 6 999",rtruncnorm(n = length(income06 == "$6 000 TO 6 999"),a=6000,b=7000,mean = 6500,sd = 250),
ifelse(income06 == "$7 000 TO 7 999",rtruncnorm(n = length(income06 == "$7 000 TO 7 999"),a=7000,b=8000,mean = 7500,sd = 250),
ifelse(income06 == "$8 000 TO 9 999",rtruncnorm(n = length(income06 == "$8 000 TO 9 999"),a=8000,b=10000,mean = 9000,sd = 500),
ifelse(income06 == "$10000 TO 12499",rtruncnorm(n = length(income06 == "$10000 TO 12499"),a=10000,b=12500,mean = 11250,sd = 500),
ifelse(income06 == "$12500 TO 14999",rtruncnorm(n = length(income06 == "$12500 TO 14999"),a=12500,b=15000,mean = 13750,sd = 500),
ifelse(income06 == "$15000 TO 17499",rtruncnorm(n = length(income06 == "$15000 TO 17499"),a=15000,b=175000,mean = 16250,sd = 500),
ifelse(income06 == "$17500 TO 19999",rtruncnorm(n = length(income06 =="$17500 TO 19999"),a=17500,b=20000,mean = 18750,sd = 500),
ifelse(income06 == "$20000 TO 22499",rtruncnorm(n = length(income06 == "$20000 TO 22499"),a=20000,b=22500,mean = 21250,sd = 500),
ifelse(income06 == "$22500 TO 24999",rtruncnorm(n = length(income06 == "$22500 TO 24999"),a=22500,b=25000,mean = 23750,sd = 500),
ifelse(income06 == "$25000 TO 29999",rtruncnorm(n = length(income06 == "$25000 TO 29999"),a=25000,b=30000,mean = 27500,sd = 750),
ifelse(income06 == "$30000 TO 34999",rtruncnorm(n = length(income06 == "$30000 TO 34999"),a=30000,b=35000,mean = 32250,sd =750),
ifelse(income06 == "$35000 TO 39999",rtruncnorm(n = length(income06 == "$35000 TO 39999"),a=35000,b=40000,mean = 37500,sd = 750),
ifelse(income06 == "$40000 TO 49999",rtruncnorm(n = length(income06 == "$40000 TO 49999"),a=40000,b=50000,mean = 450000,sd = 1000),
ifelse(income06 == "$50000 TO 59999",rtruncnorm(n = length(income06 == "$50000 TO 59999"),a=50000,b=60000,mean = 55000,sd = 1000),
ifelse(income06 == "$60000 TO 74999",rtruncnorm(n = length(income06 == "$60000 TO 74999"),a=60000,b=75000,mean = 675000,sd = 1000),
ifelse(income06 == "$75000 TO $89999",rtruncnorm(n = length(income06 == "$75000 TO $89999"),a=75000,b=90000,mean = 82500,sd = 1000),
ifelse(income06 == "$90000 TO $109999",rtruncnorm(n = length(income06 == "$90000 TO $109999"),a=90000,b=11000,mean = 100000,sd = 750),
ifelse(income06 == "$110000 TO $129999",rtruncnorm(n = length(income06 == "$110000 TO $129999"),a=110000,b=130000,mean = 120000,sd =750),
ifelse(income06 == "$130000 TO $149999",rtruncnorm(n = length(income06 == "$130000 TO $149999"),a=130000,b=150000,mean = 140000,sd =750),
ifelse(income06 == "$150000 OR OVER",rtruncnorm(n = length(income06 == "$150000 OR OVER"),a=150000,b=Inf,mean = 175000,sd = 750),NA
))))))))))))))))))))))))))%>%
  mutate(wordsum = as.numeric(wordsum)) 
finaldata <- finaldata %>%
  select(c(wordsum, wrkstat, age, pres08, 
           authoritarianism, income, con_govt, conarmy, degree)) 
finaldata <- na.omit(finaldata)
#write.csv(finaldata, file = "E:/PLSC 4782/final/finalsdata.csv")
rm(list=ls())
finaldata <- read.csv("E:/PLSC 4782/final/finalsdata.csv")
m1 <- multinom(pres08 ~ log(income) + wordsum + as.factor(wrkstat) + age + 
                          authoritarianism + con_govt + as.factor(conarmy) +
                          as.factor(degree), finaldata)
summary(m1)

m2 <- multinom(pres08 ~ wordsum + as.factor(wrkstat) + age + 
                          con_govt + as.factor(conarmy) +
                          as.factor(degree), finaldata)
summary(m2)
m3 <- multinom(pres08 ~ income + wordsum + as.factor(wrkstat) + age + 
                 authoritarianism + con_govt + as.factor(conarmy) +
                 as.factor(degree), finaldata)
summary(m3)
library(texreg)
screenreg(list(m1,m2))
screenreg(list(m3,m2))
coef(m3)/4
  
