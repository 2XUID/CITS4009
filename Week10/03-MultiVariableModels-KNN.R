## ----setup, include=FALSE, collapse=T----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)

# from 01-Model_n_Feature_Selections.Rmd
# Define a function to compute log likelihood so that we can reuse it.
logLikelihood <- function(ytrue, ypred) {
  sum(ifelse(ytrue==pos, log(ypred), log(1-ypred)), na.rm=T)
}


## ----echo=FALSE, include=FALSE, collapse=T-----------------------------------------------------------------------------
path <- '../../data_v2/KDD2009/'
d <- read.table(paste0(path, 'orange_small_train.data.gz'), 
                header=T, sep='\t', na.strings=c('NA',''),
                as.is = FALSE)

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
for(v in catVars) {
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
  perf <- performance(prediction(predcol, outcol==pos), 'auc')
  as.numeric(perf@y.values)
}


## ------------------------------------------------------------------------

for(v in catVars) {
  pi <- paste('pred', v, sep='')
  aucTrain <- calcAUC(dTrain[,pi], dTrain[,outcome])
  if (aucTrain >= 0.8) {
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
for(v in numericVars) {
  pi <- paste('pred', v, sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTest[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dCal[,v])
  aucTrain <- calcAUC(dTrain[,pi], dTrain[,outcome])
  
  if (aucTrain >= 0.55) {
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

for(rep in 1:length(aucs)) {
  useForCalRep <- rbinom(n=nrow(dTrainAll), size=1, prob=0.1)>0
  predRep <- mkPredC(dTrainAll[!useForCalRep, outcome],
                   dTrainAll[!useForCalRep, var],
                   dTrainAll[useForCalRep, var])
  aucs[rep] <- calcAUC(predRep, dTrainAll[useForCalRep, outcome])
}
mean(aucs)
sd(aucs)

## ------------------------------------------------------------------------


## Step 1: compute log likelihood


# Define a convenience function to compute log likelihood.
logLikelihood <- function(outCol, predCol) {
  sum(ifelse(outCol==pos, log(predCol), log(1-predCol)))
}

# Compute the base rate of a saturation model
baseRateCheck <- 
  logLikelihood(
    dCal[,outcome],
    sum(dCal[,outcome]==pos)/length(dCal[,outcome])
  )

## Step 2: Run through categorical variables

selVars <- c()
minStep <- 5

for(v in catVars) {
  pi <- paste('pred', v, sep='')
  liCheck <- 2*((logLikelihood(dCal[,outcome], dCal[,pi])
                 - baseRateCheck) - 1)
  if (liCheck > minStep) {
    print(sprintf("%s, calibrationScore: %g", pi, liCheck))
    selVars <- c(selVars, pi)
  }
}

## Step 3: Run through numerical variables


for(v in numericVars) {
  pi <- paste('pred', v, sep='')
  liCheck <- 2*((logLikelihood(dCal[,outcome], dCal[,pi])
                 - baseRateCheck) - 1)
  if (liCheck >= minStep) {
    print(sprintf("%s, calibrationScore: %g", pi, liCheck))
    selVars <- c(selVars, pi)
  }
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
set.seed(32238)
iris <- iris
# relabel the Species column for binary classification
iris <- within(iris, Species <- ifelse(Species == "versicolor",
               "versicolor", "non-versicolor"))
# set up the response variable that we try to predict and the
# input feature columns
outcome <- 'Species'          # response variable
(features <- setdiff(colnames(iris), outcome))

# split into training and calibration sets
intrain <- runif(nrow(iris)) < 0.75
train <- iris[intrain,]
calib <- iris[!intrain,]
cat('Training and calibration set sizes are:', nrow(train), 'and', nrow(calib))

library('class')
# The following is a no-no -- you always get 100% perfect predictions!
knnPred <- knn(train[features], train[features], train[,outcome], k=1, prob=T)
# exercise: inspect knnPred and compare it with train[outcome]


## ----collapse=T--------------------------------------------------------------------------------------------------------
# Now try the calibration set
knnPred <- knn(train[features], calib[features], train[,outcome], k=1, prob=T)
(accuracy <- sum(knnPred == calib[,outcome]) / nrow(calib))
(conf_mat <- table(actual=calib[,outcome], predicted=knnPred)) # confusion matrix

# using more neighbours (usually needed for complex datasets)
knnPred <- knn(train[features], calib[features], train[,outcome], k=3, prob=T)
(accuracy <- sum(knnPred == calib[,outcome]) / nrow(calib))
(conf_mat <- table(actual=calib[,outcome], predicted=knnPred))


## ----collapse=T--------------------------------------------------------------------------------------------------------
knnProb <- attributes(knnPred)$prob # prediction probability

# inspect a few cases
cases <- c(4, 14, 18, 26, 27)
calib[cases, outcome]     # ground truth 
knnPred[cases]            # knn predictions
knnProb[cases]            # predicted probabilities


## ----collapse=T--------------------------------------------------------------------------------------------------------
# for the non-target class, convert to 1-knnProb
knnProb <- ifelse(knnPred == "versicolor", knnProb, 1-knnProb)


## ----collapse=T--------------------------------------------------------------------------------------------------------
library(ROCR)
# ypred should be a vector of probabilities;
# ytrue should be a vector of TRUE and FALSE values or 1s and 0s.
calcAUC <- function(ypred, ytrue) {
  perf <- performance(prediction(ypred, ytrue), 'auc')
  as.numeric(perf@y.values)
}

knn_iris_AUC <- calcAUC(knnProb, calib[,outcome]=="versicolor")
cat('The AUC for kNN on the iris dataset is ', knn_iris_AUC)


## ----------------------------------------------------------------------------------------------------------------------
plotROC <- function(ypred, ytrue, titleString="ROC plot") {
  perf <- performance(prediction(ypred, ytrue), 'tpr', 'fpr')
  pf <- data.frame(FalsePositiveRate=perf@x.values[[1]],
                    TruePositiveRate=perf@y.values[[1]])
  ggplot() + geom_line(data=pf, aes(x=FalsePositiveRate, y=TruePositiveRate),
                       colour="red") +
    labs(title=titleString) +
    geom_line(aes(x=c(0,1), y=c(0,1))) +
    theme(text=element_text(size=24))
}


## ----out.width="50%", fig.asp=1----------------------------------------------------------------------------------------
plotROC(knnProb, calib[,outcome] == "versicolor",
        titleString="kNN predictions on the iris calibration set")


## ----collapse=T--------------------------------------------------------------------------------------------------------
library(class)
nK <- 200
outcome <- 'churn'
knnTrain <- dTrain[,selVars]
knnCl <- dTrain[,outcome]==pos

knnPredict <- function(df) {
  knnDecision <- knn(knnTrain, df, knnCl, k=nK, prob=T)
  ifelse(knnDecision == TRUE,
         attributes(knnDecision)$prob,
         1 - attributes(knnDecision)$prob)
}

# create a new column in dCal and dTest to store the predicted probabilities
dCal$knnProb  <- knnPredict(dCal[,selVars])
dTest$knnProb <- knnPredict(dTest[,selVars])
print(calcAUC(dCal$knnProb, dCal[,outcome]))
print(calcAUC(dTest$knnProb, dTest[,outcome]))


## ----out.width="50%", collapse=T---------------------------------------------------------------------------------------
ggplot(data=dCal) + 
  geom_density(aes(x=knnProb, color=as.factor(churn),
                   linetype=as.factor(churn))) +
  theme(text=element_text(size=20))


## ----out.width="55%", collapse=T, fig.asp=1----------------------------------------------------------------------------
plotROC(dTest$knnProb, dTest[,outcome]==pos,
        titleString="ROC curve for KNN on the KDD test set")


## ----collapse=T--------------------------------------------------------------------------------------------------------
x1 <- c(1,1);  x2 <- c(5,4)
(L2dist <- sqrt(sum((x1-x2)^2)))


## ----collapse=T--------------------------------------------------------------------------------------------------------
(L1dist <- sum(abs(x1-x2)))


## ----collapse=T--------------------------------------------------------------------------------------------------------
# suppose that x1 and x2 are categorical vectors of length 3.
x1 <- c("apple", "pear", "pear")
x2 <- c("apple", "banana", "apple")
hdist <- sum(x1 != x2)
cat("The Hamming distance between x1 and x2 is:", hdist)


## ----collapse=T--------------------------------------------------------------------------------------------------------
f <- paste(outcome,'>0 ~ ', paste(selVars, collapse=' + '), sep='')
cat(f)

cat("Feature dimension = ", length(selVars))

gmodel <- glm(as.formula(f), data=dTrain, family=binomial(link='logit'))

print(calcAUC(predict(gmodel, newdata=dTrain), dTrain[,outcome] > 0))
print(calcAUC(predict(gmodel, newdata=dTest), dTest[,outcome] > 0))
print(calcAUC(predict(gmodel, newdata=dCal), dCal[,outcome] > 0))


## ----collapse=T--------------------------------------------------------------------------------------------------------
library(ROCit)
plot_roc <- function(predcol1, outcol1, predcol2, outcol2){
  roc_1 <- rocit(score=predcol1, class=outcol1==pos)
  roc_2 <- rocit(score=predcol2, class=outcol2==pos)
  plot(roc_1, col = c("blue","green"), lwd = 3,
       legend = FALSE, YIndex = FALSE, values = TRUE, cex=3)
  lines(roc_2$TPR ~ roc_2$FPR, lwd = 3, col = c("red","green"), cex=3)
  legend("bottomright", col = c("blue","red", "green"),
         c("logistic", "kNN", "Null Model"), lwd = 2, cex=2)
}



## ----out.width="45%", collapse=T, fig.asp=1----------------------------------------------------------------------------
pred_gmodel_roc <- predict(gmodel, newdata=dTest)
pred_knn_roc <- knnPredict(dTest[,selVars])
plot_roc(pred_gmodel_roc, dTest[,outcome],
         pred_knn_roc, dTest[,outcome])

