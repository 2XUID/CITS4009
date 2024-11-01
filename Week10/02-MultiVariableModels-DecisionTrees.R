## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)


## ----echo=FALSE, include=FALSE-----------------------------------------------------------------------------------------
path <- '../../data_v2/KDD2009/'
d <- read.table(paste0(path, 'orange_small_train.data.gz'), 
                header=T, sep='\t', na.strings=c('NA',''),
                as.is=FALSE)

churn <- read.table(
  paste0(path,'orange_small_train_churn.labels.txt'),
  header=F, sep='\t')
d$churn <- churn$V1             #___churn___
appetency <- read.table(
  paste0(path,'orange_small_train_appetency.labels.txt'),
  header=F, sep='\t')
d$appetency <- appetency$V1    # ___appetency___
upselling <- read.table(
  paste0(path,'orange_small_train_upselling.labels.txt'),
  header=F, sep='\t')
d$upselling <- upselling$V1    # ___upselling___
selNumVars_df<- read.csv("selNumVars.csv")
selNumVars <- selNumVars_df$x
## ------------------------------------------------------------------------
set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])
dTrainAll <- subset(d, rgroup<=0.9)
dTest <- subset(d, rgroup>0.9)
outcomes=c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))
catVars <- vars[sapply(dTrainAll[,vars], class) %in% 
                  c('factor','character')]
numericVars <- vars[sapply(dTrainAll[,vars], class) %in%
                      c('numeric','integer')]
rm(list=c('d','churn','appetency','upselling'))
outcome <- 'churn'
pos <- '1'
useForCal <- rbinom(n=dim(dTrainAll)[[1]], size=1, prob=0.1)>0
dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll,!useForCal)

## ------------------------------------------------------------------------
table218 <- table(
  Var218=dTrain[,'Var218'],
  churn=dTrain[,outcome], useNA='ifany')
kable(table218)

print(table218[,2]/(table218[,1]+table218[,2]))

## ------------------------------------------------------------------------
mkPredC <- function(outCol, varCol, appCol) {
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol), varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}

## ------------------------------------------------------------------------
for (v in catVars) {
  pi <- paste('pred', v, sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome],
                         dTrain[,v], dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],
                       dTrain[,v], dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],
                        dTrain[,v], dTest[,v])
}

## ------------------------------------------------------------------------
library('ROCR')
calcAUC <- function(predcol, outcol) {
  perf <- performance(prediction(predcol, outcol==pos),'auc')
  as.numeric(perf@y.values)
}


## ------------------------------------------------------------------------

for (v in catVars) {
  pi <- paste('pred', v, sep='')
  aucTrain <- calcAUC(dTrain[,pi], dTrain[,outcome])
  if (aucTrain>=0.8) {
    aucCal <- calcAUC(dCal[,pi], dCal[,outcome])
    print(sprintf(
      "%s, trainAUC: %4.3f calibrationAUC: %4.3f",
      pi, aucTrain, aucCal))
  }
}
  

## ------------------------------------------------------------------------
mkPredN <- function(outCol, varCol, appCol) {
  cuts <- unique(as.numeric(
    quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T)))
  varC <- cut(varCol, cuts)
  appC <- cut(appCol, cuts)
  mkPredC(outCol, varC, appC)
}

## ------------------------------------------------------------------------
for (v in numericVars) {
  pi <- paste('pred', v, sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v], dTrain[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTest[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dCal[,v])
  aucTrain  <- calcAUC(dTrain[,pi], dTrain[,outcome])
  
  if (aucTrain>=0.55) {
    aucCal <- calcAUC(dCal[,pi], dCal[,outcome])
    print(sprintf(
      "%s, trainAUC: %4.3f calibrationAUC: %4.3f",
      pi, aucTrain, aucCal))
  }
}

## ----out.width="40%"-----------------------------------------------------
ggplot(data=dCal) +
geom_density(aes(x=predVar126, color=as.factor(churn)))

## ------------------------------------------------------------------------
var <- 'Var217'
aucs <- rep(0,100)

for (rep in 1:length(aucs)) {
  useForCalRep <- rbinom(n=nrow(dTrainAll), size=1, prob=0.1)>0
  predRep <- mkPredC(dTrainAll[!useForCalRep, outcome],
                   dTrainAll[!useForCalRep, var],
                   dTrainAll[useForCalRep, var])
  aucs[rep] <- calcAUC(predRep, dTrainAll[useForCalRep, outcome])
}
mean(aucs)
sd(aucs)


## ----echo=F, out.width="50%"-------------------------------------------------------------------------------------------
# source(10_decision_tree.R)
source("10_decision_tree.R")
dt_plot


## ----out.width="70%", collapse=T---------------------------------------------------------------------------------------
library(rpart)
dt <- rpart(formula = Species ~ ., data = iris)
library(rpart.plot)
rpart.plot(dt)


## ----collapse=T--------------------------------------------------------------------------------------------------------
library('rpart')
(fV <- paste(outcome,'> 0 ~ ', 
            paste(c(catVars, numericVars), collapse=' + '),
            sep=''))
tmodel <- rpart(fV, data=dTrain)

print(calcAUC(predict(tmodel, newdata=dTrain), dTrain[,outcome]))
print(calcAUC(predict(tmodel, newdata=dTest), dTest[,outcome]))
print(calcAUC(predict(tmodel, newdata=dCal), dCal[,outcome]))


## ----collapse=T--------------------------------------------------------------------------------------------------------
tVars <- paste('pred', c(catVars, numericVars), sep='')
cat(tVars[1:5])
(fV2 <- paste(outcome,'>0 ~ ',
             paste(tVars, collapse=' + '), sep=''))
# rpart stands for "recursive partitioning and regression trees"
tmodel <- rpart(fV2, data=dTrain)
# To inspect the model, type: summary(tmodel)

print(calcAUC(predict(tmodel, newdata=dTrain), dTrain[,outcome]))
print(calcAUC(predict(tmodel, newdata=dCal), dCal[,outcome]))
print(calcAUC(predict(tmodel, newdata=dTest), dTest[,outcome]))


## ----------------------------------------------------------------------------------------------------------------------
# ytrue should be a vector containing TRUE and FALSE
# ypred should be a vector of probabilities
logLikelihood <- function(ytrue, ypred, epsilon=1e-6) {
  sum(ifelse(ytrue, log(ypred+epsilon), log(1-ypred+epsilon)), na.rm=T)
}

# ytrue should be a vector containing 1s (or TRUE) and 0s (or FALSE);
# ypred should be a vector containing the predicted probability values for the target class.
# Both ytrue and ypred should have the same length.
performanceMeasures <- function(ytrue, ypred, model.name = "model", threshold=0.5) {
   # compute the normalised deviance
   dev.norm <- -2 * logLikelihood(ytrue, ypred)/length(ypred)
   # compute the confusion matrix
   cmat <- table(actual = ytrue, predicted = ypred >= threshold)
   accuracy <- sum(diag(cmat)) / sum(cmat)
   precision <- cmat[2, 2] / sum(cmat[, 2])
   recall <- cmat[2, 2] / sum(cmat[2, ])
   f1 <- 2 * precision * recall / (precision + recall)
   data.frame(model = model.name, precision = precision,
              recall = recall, f1 = f1, dev.norm = dev.norm)
}


## ----------------------------------------------------------------------------------------------------------------------
panderOpt <- function(){
  library(pander)
  # setting up Pander Options
  panderOptions("plain.ascii", TRUE)
  panderOptions("keep.trailing.zeros", TRUE)
  panderOptions("table.style", "simple")
  
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
# A function to pretty print the performance table of a model
# on the training and test sets.
pretty_perf_table <- function(model, xtrain, ytrain,
                              xtest, ytest, threshold=0.5) {
   # Option setting for Pander  
   panderOpt()
   perf_justify <- "lrrrr" 

   # call the predict() function to do the predictions
   pred_train <- predict(model, newdata=xtrain)
   pred_test <- predict(model, newdata=xtest)

   # comparing performance on training vs. test
   trainperf_df <- performanceMeasures(
      ytrain, pred_train, model.name="training", threshold=threshold)
   testperf_df <- performanceMeasures(
      ytest, pred_test, model.name="test", threshold=threshold)

   # combine the two performance data frames using rbind()
   perftable <- rbind(trainperf_df, testperf_df)
   pandoc.table(perftable, justify = perf_justify)
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
# tVars contains the reprocessed variables (defined 4 slides back)
pretty_perf_table(tmodel, dTrain[tVars], dTrain[,outcome]==pos,
                  dTest[tVars], dTest[,outcome]==pos)


## ----out.width="40%", collapse=T---------------------------------------------------------------------------------------
library(ROCit)
plot_roc <- function(predcol1, outcol1, predcol2, outcol2){
    roc_1 <- rocit(score=predcol1, class=outcol1==pos)
    roc_2 <- rocit(score=predcol2, class=outcol2==pos)
    plot(roc_1, col = c("blue","green"), lwd = 3,
      legend = FALSE,YIndex = FALSE, values = TRUE, asp=1)
    lines(roc_2$TPR ~ roc_2$FPR, lwd = 3, 
          col = c("red","green"), asp=1)
    legend("bottomright", col = c("blue","red", "green"),
       c("Test Data", "Training Data", "Null Model"), lwd = 2)
}
pred_test_roc <- predict(tmodel, newdata=dTest)
pred_train_roc <- predict(tmodel, newdata=dTrain)


## ----out.width="50%", collapse=T, fig.asp=1----------------------------------------------------------------------------
plot_roc(pred_test_roc, dTest[[outcome]],
         pred_train_roc, dTrain[[outcome]])


## ----collapse=T--------------------------------------------------------------------------------------------------------
tmodel2 <- rpart(fV2, data=dTrain,
                control=rpart.control(cp=0.001, minsplit=1000,
                                  minbucket=1000, maxdepth=5))

print(calcAUC(predict(tmodel2, newdata=dTrain[tVars]), dTrain[,outcome]))
print(calcAUC(predict(tmodel2, newdata=dTest[tVars]), dTest[,outcome]))
print(calcAUC(predict(tmodel2, newdata=dCal[tVars]), dCal[,outcome]))


## ----collapse=T--------------------------------------------------------------------------------------------------------
# tVars contains the reprocessed variables.
# Compare tmodel and tmodel2
pretty_perf_table(tmodel, dTrain[tVars], dTrain[,outcome]==pos, dTest[tVars], dTest[,outcome]==pos)

pretty_perf_table(tmodel2, dTrain[tVars], dTrain[,outcome]==pos, dTest[tVars], dTest[,outcome]==pos)


## ----collapse=T--------------------------------------------------------------------------------------------------------
selNumVars
f <- paste(outcome,'>0 ~ ',
           paste(selNumVars, collapse=' + '), sep='')
tmodel3 <- rpart(f, data=dTrain,
                control=rpart.control(cp=0.001, minsplit=1000,
                                  minbucket=1000, maxdepth=5))
print(calcAUC(predict(tmodel3, newdata=dTrain[selNumVars]), dTrain[,outcome]))
print(calcAUC(predict(tmodel3, newdata=dTest[selNumVars]), dTest[,outcome]))
print(calcAUC(predict(tmodel3, newdata=dCal[selNumVars]), dCal[,outcome]))                                



## ----collapse=T--------------------------------------------------------------------------------------------------------
cat('Number of features used in tmodel and tmodel2 is:', length(tVars))
cat('Number of features used in tmodel3 is:', length(selNumVars))


## ----collapse=T--------------------------------------------------------------------------------------------------------
pretty_perf_table(tmodel3, dTrain[selNumVars], dTrain[,outcome]==pos,
                  dTest[selNumVars], dTest[,outcome]==pos, threshold=0.1)


## ----out.width="45%", eval=T, echo=F, fig.asp=1------------------------------------------------------------------------
# ypred should be a vector of probabilities
# ytrue should be a vector of TRUE and FALSE values (or 1s and 0s)
# colour_id 1-7 are: black,red,green,blue,cyan,purple,gold
plot_roc_new <- function(ypred, ytrue, legend.text="", title="", ...) {
    colour_id <- 2
    roc <- rocit(score=ypred, class=ytrue)
    plot(roc, col = c(colour_id, 1), lwd = 3,
         legend = FALSE, YIndex = FALSE, values = TRUE, asp = 1,
         cex.lab=3, cex.axis=2, cex.main=3)
    # now process the ... arguments
    # The `for` loop below needs to be modified so that
    # the number of arguments in ... determines how many
    # times we should loop.
    for (i in seq(from=2, to=4, by=2)) {
        var_ypredi <- paste0("..", i-1)
        var_ytruei <- paste0("..", i)
        colour_id <- colour_id + 1
        ypredi <- eval(parse(text = var_ypredi))
        ytruei <- eval(parse(text = var_ytruei))
        roc <- rocit(score=ypredi, class=ytruei)
        lines(roc$TPR ~ roc$FPR, lwd = 3,
              col = c(colour_id, 1), asp = 1,
              cex.lab=3, cex.axis=2, cex.main=3)
    }
    if (!any(legend.text == "")) {
        legend("bottomright", col = c(seq(2,colour_id),1),
        c(legend.text, "Null model"), lwd = 2, cex=1.5)
    }
    if (title != "") title(title, cex.main=2)
}

tmodel.pred <- predict(tmodel, newdata=dTest[tVars])
tmodel2.pred <- predict(tmodel2, newdata=dTest[tVars])
tmodel3.pred <- predict(tmodel3, newdata=dTest[selNumVars])
dTest.gt <- dTest[,outcome] == pos
plot_roc_new(legend.text=c("tmodel", "tmodel2", "tmodel3"),
             title="Performance on the test set",
             tmodel.pred, dTest.gt, tmodel2.pred, dTest.gt,
             tmodel3.pred, dTest.gt)


## ----collapse=T--------------------------------------------------------------------------------------------------------
print(tmodel3)


## ----out.width="75%", collapse=T---------------------------------------------------------------------------------------
par(cex=1.2)
plot(tmodel3)
text(tmodel3)


## ----out.width="65%", collapse=T---------------------------------------------------------------------------------------
par(cex=1.5)
rpart.plot(tmodel3)

