## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)
library(gridExtra)
theme_set(theme_grey(base_size = 18))
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')


## ----collapse=T, out.width='42%'---------------------------------------------------------------------------------------
ggplot(data = custdata) +
  geom_count(mapping = aes(x=marital.stat, y=housing.type))


## ----size="small", collapse=T, out.width='50%'-------------------------------------------------------------------------
library(dplyr)
counting <- count(custdata, marital.stat, housing.type)
ggplot(data = counting, mapping = aes(x = marital.stat, 
                               y = housing.type)) +
    geom_tile(mapping = aes(fill = n))


## ----eval=F------------------------------------------------------------------------------------------------------------
## counting <- count(custdata, marital.stat, housing.type)


## ----------------------------------------------------------------------------------------------------------------------
head(counting)


## ----collapse=T, out.width='40%'---------------------------------------------------------------------------------------
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins)) +
	theme(text = element_text(size = 22))


## ----collapse=T, out.width='55%'---------------------------------------------------------------------------------------
ggplot(custdata) + 
  geom_bar(aes(x=marital.stat, fill=health.ins), position="dodge")


## ----out.width="60%"---------------------------------------------------------------------------------------------------
ggplot(custdata) +
  geom_bar(aes(x=marital.stat, fill=health.ins), position="fill") +
  theme(text = element_text(size = 22))


## ----out.width='55%'---------------------------------------------------------------------------------------------------
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=housing.type),
                alpha=0.2,  position="identity") +
  theme(text = element_text(size = 18))


## ----out.width='55%'---------------------------------------------------------------------------------------------------
ggplot(custdata) + 
  geom_bar(aes(x= marital.stat, colour=housing.type),
          fill=NA,  position="identity") +
  theme(text = element_text(size = 18))


## ----out.width='70%'---------------------------------------------------------------------------------------------------
fig1 <- ggplot(mpg) + geom_bar(aes(x=class, fill=drv), width=0.5) + 
   theme(text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1), aspect.ratio=0.8)
fig2 <- ggplot(mpg) + geom_bar(aes(x=class, fill=drv), width=0.5, position="dodge") +
   theme(text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1), aspect.ratio=0.8)
grid.arrange(fig1, fig2, nrow=1)


## ----out.width='90%'---------------------------------------------------------------------------------------------------
fig1 <- ggplot(mpg) + geom_bar(aes(x=drv, fill=class), width=0.5) + 
   theme(text=element_text(size=16), aspect.ratio=1.2)
fig2 <- ggplot(mpg) + geom_bar(aes(x=drv, fill=class), width=0.5, position="dodge") +
   theme(text=element_text(size=16), aspect.ratio=1.2)
grid.arrange(fig1, fig2, nrow=1)

