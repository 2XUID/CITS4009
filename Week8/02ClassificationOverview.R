## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)
custdata <- read.table('../../data/custdata.tsv', header=T, sep='\t')


## ----echo=F------------------------------------------------------------------------------------------------------------
NB_example <- data.frame(
    can.swim=c(74, 250), is.brown=c(57,62))
rownames(NB_example) <- c("Bird", "Dog")


## ----echo=F, collapse=T------------------------------------------------------------------------------------------------
kable(NB_example)


## ----echo=F------------------------------------------------------------------------------------------------------------
library(knitr)
set.seed(123)
df <- data.frame(
    can.swim = c(sample(rep(c(T,F), c(74,226))), sample(rep(c(T,F), c(250,50)))),
    is.brown = c(sample(rep(c(T,F), c(57,243))), sample(rep(c(T,F), c(62,238)))),
    label = c(rep("Bird",300), rep("Dog", 300))
)
df <- df[sample(1:nrow(df)),]
rownames(df) <- NULL
kable(df[1:6,])


## ----results="hide"----------------------------------------------------------------------------------------------------
library(naivebayes)
model <- naive_bayes(formula=label ~ can.swim + is.brown, data=df[1:6,])

## ----echo=F------------------------------------------------------------------------------------------------------------
p <- predict(model, data.frame(can.swim=T, is.brown=T), type = 'prob')


## ----collapse=T--------------------------------------------------------------------------------------------------------
library(dplyr)
# create a small data frame df to deal with seniors (65+ years old)
df <- within(custdata, {is.senior <- age > 65}) %>%
      subset(select=c("is.senior","health.ins"))

# ggplot(df) + geom_bar(aes(x=is.senior, fill=health.ins),
#                       position="fill") +
#        labs(x="is senior", y="proportion")

# compute the proportion of seniors having health insurance
p <- sum( df$is.senior & df$health.ins ) / sum( df$is.senior )

cat("Proportion of seniors having health insurance is: ", p)

