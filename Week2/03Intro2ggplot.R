## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center")
library(ggplot2)
library(crayon)


## ----out.width = '60%', size="small"-----------------------------------------------------------------------------------
custdata <- read.table('custdata.tsv',header=T, sep='\t')
hist(custdata$age)


## ----out.width = '40%'-------------------------------------------------------------------------------------------------
x <- custdata$age
hist(x, breaks=seq(0,150,1), xlim=c(0,100), freq = FALSE)


## ----out.width = '60%'-------------------------------------------------------------------------------------------------
hist(custdata$age,main="Distribution of age",xlab="age")


## ----out.width = '50%'-------------------------------------------------------------------------------------------------
hist(custdata$age)
title('Distribution of age',xlab='age')


## ----eval=FALSE, message=FALSE, WARNING=FALSE--------------------------------------------------------------------------
## ggplot(data = <DATA>) +
##   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))


## ----out.width = '50%'-------------------------------------------------------------------------------------------------
library(ggplot2)
ggplot(data = custdata) +
  geom_histogram(mapping = aes(x=age),
                 binwidth=5, fill="gray")



## ----out.width = '40%', message = FALSE, warning = FALSE---------------------------------------------------------------
library(ggplot2)
ggplot(custdata) + geom_density(aes(x=age)) +
	theme(text = element_text(size = 24))

