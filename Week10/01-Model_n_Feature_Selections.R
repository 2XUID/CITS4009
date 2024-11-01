## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)



## ----echo=FALSE, include=FALSE-----------------------------------------------------------------------------------------
path <- '../../data_v2/KDD2009/'
d <- read.table(paste0(path, 'orange_small_train.data.gz'), 
                header=T, sep='\t', na.strings=c('NA',''),
                as.is = FALSE)

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

## ------------------------------------------------------------------------
set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])
dTrainAll <- subset(d,rgroup<=0.9)
dTest <- subset(d,rgroup>0.9)
outcomes=c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))
catVars <- vars[sapply(dTrainAll[,vars],class) %in% 
                  c('factor','character')]
numericVars <- vars[sapply(dTrainAll[,vars],class) %in%
                      c('numeric','integer')]
rm(list=c('d','churn','appetency','upselling'))
outcome <- 'churn'
pos <- '1'
useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)

## ------------------------------------------------------------------------
table218 <- table(
  Var218=dTrain[,'Var218'],
  churn=dTrain[,outcome], useNA='ifany')
kable(table218)

print(table218[,2]/(table218[,1]+table218[,2]))

## ------------------------------------------------------------------------
mkPredC <- function(outCol,varCol,appCol) {
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}

## ------------------------------------------------------------------------
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome],
                         dTrain[,v],dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],
                       dTrain[,v],dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],
                        dTrain[,v],dTest[,v])
}

## ------------------------------------------------------------------------
library('ROCR')
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}


## ------------------------------------------------------------------------

for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.8) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf(
      "%s, trainAUC: %4.3f calibrationAUC: %4.3f",
      pi, aucTrain, aucCal))
  }
}
  

## ------------------------------------------------------------------------
mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(
    quantile(varCol, probs=seq(0, 1, 0.1),na.rm=T)))
  varC <- cut(varCol,cuts)
  appC <- cut(appCol,cuts)
  mkPredC(outCol,varC,appC)
}

## ------------------------------------------------------------------------
for(v in numericVars) {
  pi<-paste('pred',v,sep='')
  dTrain[,pi]<-mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dTest[,pi]<-mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
  dCal[,pi]<-mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
  aucTrain<-calcAUC(dTrain[,pi],dTrain[,outcome])
  
  if(aucTrain>=0.55) {
    aucCal<-calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf(
      "%s, trainAUC: %4.3f calibrationAUC: %4.3f",
      pi,aucTrain,aucCal))
  }
}

## ----out.width="40%"-----------------------------------------------------
ggplot(data=dCal) +
geom_density(aes(x=predVar126,color=as.factor(churn)))

## ------------------------------------------------------------------------
var <- 'Var217'
aucs <- rep(0,100)

for(rep in 1:length(aucs)) {
  useForCalRep<-rbinom(n=nrow(dTrainAll),size=1,prob=0.1)>0
  predRep<-mkPredC(dTrainAll[!useForCalRep,outcome],
                   dTrainAll[!useForCalRep,var],
                   dTrainAll[useForCalRep,var])
  aucs[rep]<-calcAUC(predRep,dTrainAll[useForCalRep,outcome])
}
mean(aucs)
sd(aucs)



## ----out.width="80%", collapse=T---------------------------------------------------------------------------------------
pos <- '1'
# Define a function to compute log likelihood so that we can reuse it.
logLikelihood <- function(ytrue, ypred, epsilon=1e-6) {
  sum(ifelse(ytrue==pos, log(ypred+epsilon), log(1-ypred-epsilon)), na.rm=T)
}

# Compute the likelihood of the Null model on the calibration
# set (for the KDD dataset from previous lecture)
outcome <- 'churn'
logNull <- logLikelihood(
    dCal[,outcome], sum(dCal[,outcome]==pos)/nrow(dCal)
  )
cat(logNull)


## ----out.width="80%", collapse=T---------------------------------------------------------------------------------------
selCatVars <- c()
minDrop <- 10  # may need to adjust this number

for (v in catVars) {
  pi <- paste('pred', v, sep='')
  devDrop <- 2*(logLikelihood(dCal[,outcome], dCal[,pi]) - logNull)
  if (devDrop >= minDrop) {
    print(sprintf("%s, deviance reduction: %g", pi, devDrop))
    selCatVars <- c(selCatVars, pi)
  }
}


## ----out.width="80%", collapse=T---------------------------------------------------------------------------------------
selNumVars <- c()
minDrop <- 10  # may need to adjust this number
for (v in numericVars) {
  pi <- paste('pred', v, sep='')
  devDrop <- 2*(logLikelihood(dCal[,outcome], dCal[,pi]) - logNull)
  if (devDrop >= minDrop) {
    print(sprintf("%s, deviance reduction: %g", pi, devDrop))
    selNumVars <- c(selNumVars, pi)
  }
}
write.csv(selNumVars, "selNumVars.csv")

