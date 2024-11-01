## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)


## ----------------------------------------------------------------------------------------------------------------------
# variable gender with 20 "male" entries and
# 30 "female" entries
gender <- c(rep("male",20), rep("female", 30))
gender <- factor(gender) 


## ----out.width="35%", collapse=T---------------------------------------------------------------------------------------
custdata <- read.table('../../data/custdata.tsv', header=T, sep='\t')
y <- table(custdata$marital.stat)   # table() carries out the aggregation
print(y)
barplot(y, main="Marital Status", xlab="Status")


## ----out.width="60%"---------------------------------------------------------------------------------------------------
ggplot(custdata) + 
  geom_bar(aes(x=marital.stat), fill="gray") + 
  theme(text = element_text(size = 24))


## ----out.width="70%", size="small"-------------------------------------------------------------------------------------
ggplot(custdata) +
  geom_bar(aes(x=state.of.res), fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))


## ----size="footnotesize", collapse=T-----------------------------------------------------------------------------------
# table() aggregates according to state.of.res
statesums <- table(custdata$state.of.res) 
# as.data.frame() converts table object into a data frame
statef <- as.data.frame(statesums)
# define the column names for data frame statef
colnames(statef) <- c("state.of.res", "count")
# by default, order by statename alphabetically
summary(statef)


## ----size="footnotesize", collapse=T, out.width="50%"------------------------------------------------------------------
statef <- transform(statef,
          state.of.res=reorder(state.of.res, count))
ggplot(statef)+ 
  geom_bar(aes(x=state.of.res,y=count), stat="identity", 
           fill="gray") + coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))


## ----size="small", out.width="70%"-------------------------------------------------------------------------------------
library(WVPlots)
ClevelandDotPlot(custdata, "state.of.res",
                 sort = 1, title="Customers by state") +
coord_flip()

