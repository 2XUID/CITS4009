## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)



## ----collapse=T--------------------------------------------------------------------------------------------------------
path <- '../../data_v2/KDD2009/'
d <- read.table(paste0(path, 'orange_small_train.data.gz'), 
                header=T, sep='\t', na.strings=c('NA',''))

churn <- read.table(
  paste0(path,'orange_small_train_churn.labels.txt'),
  header=F,sep='\t')
d$churn <- churn$V1             #___churn___
appetency <- read.table(
  paste0(path,'orange_small_train_appetency.labels.txt'),
  header=F,sep='\t')
d$appetency <- appetency$V1    # ___appetency___
upselling <- read.table(
  paste0(path,'orange_small_train_upselling.labels.txt'),
  header=F,sep='\t')
d$upselling <- upselling$V1    # ___upselling___

# d - data frame having 5000 rows and 233 (=230+3) columns;
# churn, appetency, upselling - all having 5000 rows and 1 column.


## ----------------------------------------------------------------------------------------------------------------------
# do a 90/10 split to form the training and test sets.
set.seed(729375)
d$rgroup <- runif(dim(d)[1])
dTrainAll <- subset(d, rgroup<=0.9)
dTest <- subset(d, rgroup>0.9)
outcomes <- c('churn', 'appetency', 'upselling')
# names of columns that are categorical type and numerical type
vars <- setdiff(colnames(dTrainAll),  c(outcomes, 'rgroup'))
catVars <- vars[sapply(dTrainAll[, vars], class) %in% 
                  c('factor', 'character')]
numericVars <- vars[sapply(dTrainAll[, vars], class) %in%
                      c('numeric', 'integer')]
# remove the original tables
rm(list=c('d', 'churn', 'appetency', 'upselling'))

# split dTrainAll into a training set and a validation (or calibration) set
useForCal <- rbinom(n=dim(dTrainAll)[1], size=1, prob=0.1)>0
dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll, !useForCal)

