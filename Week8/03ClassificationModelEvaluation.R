## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
#library(ggplot2)
#library(crayon)
#custdata <- read.table('../data/custdata.tsv',header=T,sep='\t')
#load("../data/exampleData.rData")


## ----------------------------------------------------------------------------------------------------------------------
path <- "../../data_v2/Spambase/spamD.tsv"
spamD <- read.table(path,header=T,sep='\t')
spamTrain <- subset(spamD,spamD$rgroup>=10)
spamTest <- subset(spamD,spamD$rgroup<10)
spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"',
                                paste(spamVars,collapse=' + '),
                                sep=' ~ '))
spamModel <- glm(spamFormula,family=binomial(link='logit'),
                 data=spamTrain)
spamTrain$pred <- predict(spamModel,newdata=spamTrain,
                          type='response')
spamTest$pred <- predict(spamModel,newdata=spamTest,
                         type='response')


## ----------------------------------------------------------------------------------------------------------------------
sample <- spamTest[c(7,35,224,327),c('spam','pred')]
kable(sample)


## ----collapse=T--------------------------------------------------------------------------------------------------------
cM <- table(truth=spamTest$spam,prediction=spamTest$pred>0.5)
kable(cM)


## ----eval=T, echo=F, out.width="80%", warning=F------------------------------------------------------------------------
df <- data.frame(false=c("TN", "FN"), true=c("FP", "TP"),
		 row.names=c("non-spam", "spam"))
colnames(df) <- c("FALSE", "TRUE")
kable(df)

source('08-plotR.R')
# grid.arrange(fig, fig.Sensitivity, fig.Specificity, nrow=1)

