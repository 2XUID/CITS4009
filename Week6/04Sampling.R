## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)
custdata <- read.table('../../data/custdata.tsv', header=T, sep='\t')


## ----------------------------------------------------------------------------------------------------------------------
rows <- sample(1:nrow(custdata), 3, replace=FALSE)
mysample <- custdata[rows, ]

# show the first 6 columns of mysample
mysample[, 1:6]


## ----------------------------------------------------------------------------------------------------------------------
# create a new column
custdata$gp <- runif(dim(custdata)[1])


## ----collapse=T--------------------------------------------------------------------------------------------------------
split.ratio <- 0.1
testSet <- subset(custdata, custdata$gp <= split.ratio)
trainingSet <- subset(custdata, custdata$gp > split.ratio)
cat("Test set size:", dim(testSet)[1])
cat("Training set size:", dim(trainingSet)[1])


## ----collapse=T--------------------------------------------------------------------------------------------------------
s <- runif(dim(custdata)[1])
split.ratio <- 0.1
testSet <- subset(custdata, s <= split.ratio)
trainingSet <- subset(custdata, s > split.ratio)
cat("Test set size:", dim(testSet)[1])
cat("Training set size:", dim(trainingSet)[1])

