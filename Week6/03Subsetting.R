## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)
library(dplyr)
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')


## ----collapse=T--------------------------------------------------------------------------------------------------------
x <- c(7,8);  y <- "Hello"
z <- list(name=c("Rose","Jon"), height=c(1.6,1.7))
cat(is.vector(x), is.matrix(x), is.character(y), is.list(z))

str(y)

x <- as.character(x)   # convert x to vector of character type
cat(is.vector(x), is.numeric(x), is.character(x))

as.logical("hello")  # can't convert this
as.numeric("hello")  # can't convert this


## ----------------------------------------------------------------------------------------------------------------------
attach(custdata)
newdata <- custdata[order(sex, -income),]
detach(custdata)


## ----collapse=T--------------------------------------------------------------------------------------------------------
myvars <- c("custid", "is.employed", "income", 
            "marital.stat", "health.ins", "age")
newdata <- custdata[myvars] # newdata has 6 columns


## ----collapse=T--------------------------------------------------------------------------------------------------------
myvars <- names(custdata) %in% c("sex", "state.of.res")
myvars
newdata <- custdata[!myvars] # newdata has 11 - 2 = 9 columns


## ----collapse=T--------------------------------------------------------------------------------------------------------
newdata <- custdata[1:3,]
newdata <- custdata[which(custdata$sex=="M" &
                              custdata$age < 30),]
cat(nrow(custdata), nrow(newdata))

attach(custdata)
newdata <- custdata[which(sex=="M" & age > 30),]
detach(custdata)
cat(nrow(custdata), nrow(newdata))


## ----eval=F, collapse=T------------------------------------------------------------------------------------------------
## subset(<x>, <subset>, <select>, ...)
##    <x>      - object to subset
##    <subset> - logical expression indicating rows to keep
##    <select> - expression, indicating columns to select


## ----------------------------------------------------------------------------------------------------------------------
newdata <- subset(custdata, age >= 65 | age < 24,
                  select=c("custid", "marital.stat"))


## ----------------------------------------------------------------------------------------------------------------------
# custid is column 1; age is column 10
newdata <- subset(custdata, sex=="M" & age < 25,
                  select=custid:age)


## ----warning=F---------------------------------------------------------------------------------------------------------
library(sqldf)
# sql is not case sensitive, so "income" and "Income" 
# are considered the same
newdf <- sqldf("select * from custdata where 
               income <= 1000 order by age", 
               row.names=TRUE)
# kable() requires the knitr library
kable(newdf[1:5, c("custid","sex","is.employed","income","age","health.ins")])

