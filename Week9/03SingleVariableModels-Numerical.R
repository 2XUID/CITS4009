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

library('ROCR')
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}

# from 02SingleVariableModels-Categorical.Rmd
library(ROCit)
# colour_id 1-7 are: black,red,green,blue,cyan,purple,gold
plot_roc <- function(predcol, outcol, colour_id=2, overlaid=F) {
    ROCit_obj <- rocit(score=predcol, class=outcol==pos)
    par(new=overlaid)
    plot(ROCit_obj, col = c(colour_id, 1),
       legend = FALSE, YIndex = FALSE, values = FALSE)
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
(q1 <- quantile(dTrain[,"Var1"], probs=seq(0, 1, 0.1), na.rm=T))
(q6 <- quantile(dTrain[,"Var6"], probs=seq(0, 1, 0.1), na.rm=T))

# dis.Var1 and dis.Var6 are the discretised version of dTrain[,"Var1"] and dTrain[,"Var6"]
dis.Var1 <- cut(dTrain[,"Var1"], unique(q1))
dis.Var6 <- cut(dTrain[,"Var6"], unique(q6))

# inspect the number of levels
dis.Var1["Levels"]
dis.Var6["Levels"]


## ----collapse=T--------------------------------------------------------------------------------------------------------
mkPredN <- function(outCol, varCol, appCol) {
  # compute the cuts
  cuts <- unique(
    quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T))
  # discretize the numerical columns
  varC <- cut(varCol,cuts)
  appC <- cut(appCol,cuts)

  mkPredC(outCol,varC,appC)
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
for(v in numericVars) {
  pi <- paste('pred', v, sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dCal[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome], dTrain[,v], dTest[,v])
  aucTrain <- calcAUC(dTrain[,pi], dTrain[,outcome])
  
  if(aucTrain >= 0.55) {
    aucCal <- calcAUC(dCal[,pi], dCal[,outcome])
    print(sprintf(
      "%s: trainAUC: %4.3f; calibrationAUC: %4.3f",
      pi, aucTrain, aucCal))
  }
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
vars <- c('Var7', 'Var13', 'Var73')

for (var in vars) {
  aucs <- rep(0,100)
  for (rep in 1:length(aucs)) {
    useForCalRep <- rbinom(n=nrow(dTrainAll), size=1, prob=0.1) > 0
    predRep <- mkPredN(dTrainAll[!useForCalRep, outcome],
                     dTrainAll[!useForCalRep, var],
                     dTrainAll[useForCalRep, var])
    aucs[rep] <- calcAUC(predRep, dTrainAll[useForCalRep, outcome])
  }
  print(sprintf("%s: mean: %4.3f; sd: %4.3f", var, mean(aucs), sd(aucs)))
}


## ----out.width="78%", collapse=T, fig.asp=0.4--------------------------------------------------------------------------
calcAUC(dTrain[,"predVar126"], dTrain[,outcome]); calcAUC(dCal[,"predVar126"], dCal[,outcome])
fig1 <- ggplot(dCal) + geom_density(aes(x=predVar6, color=as.factor(churn)))
fig2 <- ggplot(dCal) + geom_density(aes(x=predVar126, color=as.factor(churn)))
grid.arrange(fig1, fig2, ncol=2)


## ----eval=T, echo=F----------------------------------------------------------------------------------------------------
vars <- paste("predVar", c(7, 13, 73), sep="")
aucvars <- paste("auc.predVar", c(7, 13, 73), sep="")
for (i in seq(1,3)) assign(aucvars[i], calcAUC(dTest[,vars[i]], dTest[,outcome]))


## ----out.width="40%", fig.asp=1, collapse=T----------------------------------------------------------------------------
# call calcAUC() to calculate the AUC of each variable (code omitted)
cat("Var7's AUC:", auc.predVar7, "; Var13's AUC:", auc.predVar13, "; Var73's AUC:", auc.predVar73)
plot_roc(dTest$predVar7, dTest[,outcome]) #red
plot_roc(dTest$predVar13, dTest[,outcome], colour_id=3, overlaid=T) #green
plot_roc(dTest$predVar73, dTest[,outcome], colour_id=4, overlaid=T) #blue


## ----collapse=T--------------------------------------------------------------------------------------------------------
(Npos <- sum(dTrain[,outcome] == 1))
pred.Null <- Npos / nrow(dTrain)
cat("Proportion of outcome == 1 in dTrain:", pred.Null)


## ----collapse=T--------------------------------------------------------------------------------------------------------
TP <- 0; TN <- sum(dCal[,outcome] == -1); # using threshold 0.5
FP <- 0; FN <- sum(dCal[,outcome] == 1);  # using threshold 0.5
cat("nrow(dCal):", nrow(dCal), "TP:", TP, "TN:", TN, "FP:", FP, "FN:", FN)
(accuracy <- (TP + TN) / nrow(dCal))
(precision <- TP/(TP + FP))
(recall <- TP/(TP + FN))
pred.Null <- rep(pred.Null, nrow(dCal))
(AUC <- calcAUC(pred.Null, dCal[,outcome]))

