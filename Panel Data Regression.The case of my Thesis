#load package
library(dplyr)
library(tidyverse)
library(magrittr)
library(knitr)

#load package of read table
library(readr)

#load the data
data <- read.csv("2019.02.12THESIS DATA MODELING2.csv")
attach(data)
#load panel data
library(plm)
#claim the data be panel data
data <- pdata.frame(data, index = c("n", "t"))

#make linear regression analysis
lmdata <- lm( win ~ hhi + sal_lta + expro + mgt + expro_mgt + change + exp + logexp2 + t)
summary(lmdata)

#set up panel data analysis
model <- ( win ~ hhi + sal_lta + expro + mgt + expro_mgt + change + exp + logexp2 )

plm1 <- plm(model, data = data, model = 'pooling')

summary(plm1)

#huasman test

re1<-plm(model, data=data, model='random')
summary(re1)

fe1<-plm(model, data=data, model='within', effect='individual')
summary(fe1)

fe2<-plm(model, data=data, model='within', effect='twoways')
summary(fe2)

phtest(fe1,re1)
phtest(fe2,re1)
