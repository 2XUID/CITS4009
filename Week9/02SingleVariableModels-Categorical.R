## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)
library(gridExtra)

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

set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])
dTrainAll <- subset(d,rgroup<=0.9)
dTest <- subset(d,rgroup>0.9)
outcomes <- c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))
catVars <- vars[sapply(dTrainAll[,vars],class) %in% 
                  c('factor','character')]
numericVars <- vars[sapply(dTrainAll[,vars],class) %in%
                      c('numeric','integer')]
rm(list=c('d','churn','appetency','upselling'))

useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)



## ----collapse=T--------------------------------------------------------------------------------------------------------
outcome <- 'churn'  # We can also try the 'appetency' and 'upselling' columns.
pos <- '1'          # We are interested in when 'churn' is positive. We need
                    # to put quotes around number 1 as it is the column name
                    # of the table created.
# Column 'Var218' is a categorical column containing 'cJvF' and 'UYBR'.
table218 <- table(Var218=dTrain[,'Var218'], churn=dTrain[,outcome], useNA='ifany')
kable(table218)

print(table218[,2] / (table218[,1] + table218[,2]))


## ----collapse=T--------------------------------------------------------------------------------------------------------
# outCol: vector holding the values (known in the training step) of the
#         output column that we want to predict, e.g., the 'churn' column.
# varCol: the single variable column that is of interest. Can we use this
#         column alone to predict outCol?
# appCol: after building the model, we can apply it to this column (same
#         as varCol but may come from the calibration or test set).
mkPredC <- function(outCol, varCol, appCol) {
  pPos <- sum(outCol == pos) / length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol), varCol)
  pPosWv <- (vTab[pos, ] + 1.0e-3*pPos) / (colSums(vTab) + 1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
# call the mkPredC() function for all the categorical columns
for(v in catVars) {
  pi <- paste('pred', v, sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTest[,v])
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
# We can inspect a few rows of the output column that has been added
# to dTrain for the categorical variable 'Var194'.
factor(dTrain[,"Var194"])["Levels"]  # how many levels are there?

rows <- c(620, 725, 9502, 40310)
dTrain[rows, c("Var194", "predVar194")]
rows <- c(842, 2885, 4507, 4510)
dCal[rows, c("Var194", "predVar194")]


## ----------------------------------------------------------------------------------------------------------------------
library('ROCR')
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}



## ----------------------------------------------------------------------------------------------------------------------
for(v in catVars) {
  pi <- paste('pred', v, sep='')
  aucTrain <- calcAUC(dTrain[,pi], dTrain[,outcome])
  if (aucTrain >= 0.8) {
    aucCal <- calcAUC(dCal[,pi], dCal[,outcome])
    print(sprintf(
      "%s: trainAUC: %4.3f; calibrationAUC: %4.3f",
      pi, aucTrain, aucCal))
  }
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
vars <- c('Var200', 'Var217')

for (var in vars) {
  aucs <- rep(0,100)
  for (rep in 1:length(aucs)) {
    useForCalRep <- rbinom(n=nrow(dTrainAll), size=1, prob=0.1) > 0
    predRep <- mkPredC(dTrainAll[!useForCalRep, outcome],
                     dTrainAll[!useForCalRep, var],
                     dTrainAll[useForCalRep, var])
    aucs[rep] <- calcAUC(predRep, dTrainAll[useForCalRep, outcome])
  }
  print(sprintf("%s: mean: %4.3f; sd: %4.3f", var, mean(aucs), sd(aucs)))
}


## ----out.width="85%", collapse=T, fig.asp=0.4--------------------------------------------------------------------------
str(factor(dTrain[,"Var200"]))
str(factor(dTrain[,"Var217"]))
fig1 <- ggplot(dCal) + geom_density(aes(x=predVar200, color=as.factor(churn)))
fig2 <- ggplot(dCal) + geom_density(aes(x=predVar217, color=as.factor(churn)))
grid.arrange(fig1, fig2, ncol=2)


## ----out.width="30%", fig.asp=1----------------------------------------------------------------------------------------
library(ROCit)
# colour_id 1-7 are: black,red,green,blue,cyan,purple,gold
plot_roc <- function(predcol, outcol, colour_id=2, overlaid=F) {
    ROCit_obj <- rocit(score=predcol, class=outcol==pos)
    par(new=overlaid)
    plot(ROCit_obj, col = c(colour_id, 1),
       legend = FALSE, YIndex = FALSE, values = FALSE)
}
plot_roc(dCal$predVar200, dCal[,outcome])  #red
plot_roc(dCal$predVar217, dCal[,outcome], colour_id=3, overlaid=T) # green

