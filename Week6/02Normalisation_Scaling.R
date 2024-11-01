## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')


## ----------------------------------------------------------------------------------------------------------------------
mean.age <- mean(custdata$age)
custdata$age.normalised <- custdata$age/mean.age


## ----------------------------------------------------------------------------------------------------------------------
mean.age <- mean(custdata$age)
std.age <- sd(custdata$age)
custdata$age.normalised <- (custdata$age-mean.age)/std.age


## ----collapse=T--------------------------------------------------------------------------------------------------------
cat("mean.age =", mean.age, "std.age =", std.age)
# ages and normalised ages of 6 random customers
indices <- sample(1:nrow(custdata), 6, replace=F)
custdata[indices, c("age", "age.normalised")]

