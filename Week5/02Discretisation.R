## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)
load("01output.rData")


## ----eval=F, echo=T----------------------------------------------------------------------------------------------------
## library(scales)
## ggplot(data = custdata,
##        mapping = aes(x=income, y=as.numeric(health.ins))) +
##   geom_jitter(alpha=1/5, height = 0.1) + geom_smooth() +
##   scale_x_log10(breaks = c(100,1000,10000,100000,1000000), labels=dollar) +
##   labs(x="income (log10 scale)", y="health.ins")


## ----out.width="50%", eval=T, echo=F-----------------------------------------------------------------------------------
library(scales)
ggplot(data = custdata, 
       mapping = aes(x=income, y=as.numeric(health.ins))) +
  geom_jitter(alpha=1/5, height = 0.1) + geom_smooth() +
  scale_x_log10(breaks = c(100,1000,10000,100000,1000000),
                labels=dollar) +
  annotate("text", x=50000, y=0.5, size=7, colour="red",
	   label=paste("insured customers", "(health.ins=TRUE (or",
		       "(numerical value 1))", sep="\n")) +
  annotate("segment", x=45000, y=0.6, xend=10000, yend=0.9,
	   arrow=arrow(), size=1, colour="red") +
  annotate("text", x=1000, y=0.4, size=7, colour="red",
	   label=paste("uninsured customers", "(health.ins=FALSE (or",
		       "numerical value 0))", sep="\n")) +
  annotate("segment", x=4500, y=0.3, xend=10000, yend=0.1,
	   arrow=arrow(), size=1, colour="red") +
  labs(x="income (log10 scale)", y="health.ins") +
  theme(text = element_text(size=16)) 


## ----out.width="60%"---------------------------------------------------------------------------------------------------
ggplot(data = custdata, 
       mapping = aes(x=age, y=as.numeric(health.ins))) +
  geom_jitter(alpha = 1/5, height = 0.1) + geom_smooth() +
  theme(text = element_text(size=16))


## ----------------------------------------------------------------------------------------------------------------------
custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)


## ----------------------------------------------------------------------------------------------------------------------
brks <- c(0, 25, 65, Inf)
custdata$age.range <- cut(custdata$age,
                          breaks=brks, include.lowest=T)
summary(custdata$age.range)


## ----eval=FALSE--------------------------------------------------------------------------------------------------------
## # altering the original data
## custdata$age[custdata$age > 120] <- NA
## # or create a new variable
## custdata$age.alt <- ifelse(custdata$age > 120,
##                            NA, custdata$age)
## # then convert custdata$age.alt into range


## ----------------------------------------------------------------------------------------------------------------------
custdata$agecat[custdata$age > 120] <- NA
custdata$agecat[custdata$age > 65 
                & custdata$age <= 120] <- "Elder"
custdata$agecat[custdata$age > 25 
                & custdata$age <= 65] <- "Middle Aged"
custdata$agecat[custdata$age <= 25] <- "Young"


## ----size="small", collapse=T------------------------------------------------------------------------------------------
custdata <- within(custdata, {
  agecat <- NA
  agecat[age > 120] <- NA
  agecat[age > 65 & age <= 120] <- "Elder"
  agecat[age > 25 & age <= 65] <- "Middle Aged"
  agecat[age <= 25] <- "Young" })


## ----size="small", collapse=T------------------------------------------------------------------------------------------
custdata$agecat <- factor(custdata$agecat)


## ----size="small", collapse=T, eval=FALSE------------------------------------------------------------------------------
## fix(custdata)


## ----size="small", collapse=T, eval=FALSE------------------------------------------------------------------------------
## rename(dataframe, newname=oldname, newname=oldname, ...)


## ----size="small", collapse=T------------------------------------------------------------------------------------------
library(dplyr)
custdata <- rename(custdata, age.cat=agecat, gender=sex)
names(custdata)

