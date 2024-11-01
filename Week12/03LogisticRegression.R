## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)


## ----eval=T, echo=F, collapse=T, out.width="50%", fig.asp=0.5-----------------
z <- seq(-6, 6, by=0.05)
sz <- 1/(1 + exp(-z))
# plot with no axes turned off
plot(z, sz, typ="l",               # draw as line, default is "p"
     col="blue",                   # blue colour line
     xlim=c(-6,6), ylim=c(0,1),
     xlab='z', ylab='sigmoid(z)',  # axis label
     main="sigmoid(z) = 1 / (1 + exp(-z))",
     cex.lab=1.8,                  # make font size larger for the axis labels
     cex.main=2.0,                 # make font size larger for the title
     axes=F, xaxt="n", yaxt="n")   # no axes
# add in the axes and tick marks manually
xtick <- seq(-6, 6, by=2)
axis(side=1, pos=0, at=xtick, cex.axis=1.6)
ytick <- c(0, 0.5, 1)
# las=1 make the tick marks horizontal
axis(side=2, pos=0, at=ytick, las=1, cex.axis=1.6)
# put on grid lines manually
abline(h=c(0.25, 0.5, 0.75), v=seq(-4,4,by=2), lty=3, col="grey")


## ----eval=T, echo=F, out.width="50%", fig.asp=0.5-----------------------------
p <- seq(0.01, 0.99, by=0.01)
logit <- log(p / (1-p))
# plot with no axes turned off
plot(p, logit, typ="l",            # draw as line, default is "p"
     col="blue",                   # blue colour line
     xlim=c(0, 1), ylim=c(-4.0, 4.0),
     xlab='p', ylab='logit(p)',    # axis label
     main="logit(p) = log(p / (1 - p))",,
     cex.lab=1.5,                  # make font size larger for the axis labels
     cex.main=1.8,                 # make font size larger for the title
     axes=F, xaxt="n", yaxt="n")   # no axes
# add in the axes and tick marks manually
xtick <- c(0, seq(0.1, 1, by=0.2), 1)
axis(side=1, at=xtick, cex.axis=1.5)
ytick <- seq(-4, 4, by=2)
# las=1 make the tick marks horizontal
axis(side=2, pos=0, at=ytick, las=1, cex.axis=1.5)
# put on grid lines manually
abline(h=seq(-4,4), v=seq(0.1,1,0.2), lty=3, col="grey")


## -----------------------------------------------------------------------------
path <- "../../data_v2/CDC/"
load(paste0(path, "NatalRiskData.rData"))
train <- sdata[sdata$ORIGRANDGROUP <= 5,]
test <- sdata[sdata$ORIGRANDGROUP > 5,]
cat("dim(train) =", dim(train), "; dim(test) =", dim(test))


## -----------------------------------------------------------------------------
complications <- c("ULD_MECO", "ULD_PRECIP", "ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER", "URF_ECLAM")
y <- "atRisk"
x <- c("PWGT", "UPREVIS", "CIG_REC", "GESTREC3", "DPLURAL",
       complications, riskfactors)
fmla <- paste(y, paste(x, collapse=" + "), sep=" ~ ")
cat(fmla)


## -----------------------------------------------------------------------------
model <- glm(fmla, data=train, family=binomial(link="logit"))


## -----------------------------------------------------------------------------
train$pred <- predict(model, newdata=train, type="response")
test$pred <- predict(model, newdata=test, type="response")


## ----fig.asp=0.5, out.width="60%"---------------------------------------------
library(ggplot2)
ggplot(train, aes(x=pred, color=atRisk, linetype=atRisk)) + geom_density(size=1.5) +
     theme(text=element_text(size=20))


## ----collapse=T---------------------------------------------------------------
library(ROCR)
library(grid)
library(gridExtra)
perf <- prediction(train$pred, train$atRisk)
precObj <- performance(perf, measure="prec")
recObj <- performance(perf, measure="rec")

thresh <- (precObj@x.values)[[1]]     # threshold
precision <- (precObj@y.values)[[1]]  # precision
recall <- (recObj@y.values)[[1]]      # recall
ROCdf <- data.frame(threshold=thresh, precision=precision, recall=recall)

# Null probability
pnull <- mean(as.numeric(train$atRisk))
cat('pnull =', pnull)


## ----out.width="65%"----------------------------------------------------------
p1 <- ggplot(ROCdf, aes(x=threshold)) + geom_line(aes(y=precision/pnull)) +
  coord_cartesian(xlim = c(0,0.05), ylim=c(0,5) ) + labs(y="Enrichment rate")
p2 <- ggplot(ROCdf, aes(x=threshold)) + geom_line(aes(y=recall)) +
  coord_cartesian(xlim = c(0,0.05))
grid.arrange(p1, p2, nrow = 2)


## ----collapse=T---------------------------------------------------------------
cat("Confusion matrix of 'at risk' predictions:\n")
(ctab.test <- table(actual=test$atRisk, predicted=test$pred>0.02))
(precision <- ctab.test[2,2] / sum(ctab.test[,2]))   # TP / (TP+FP)
(recall <- ctab.test[2,2] / sum(ctab.test[2,]))      # TP / (TP+FN)
(enrich <- precision / mean(as.numeric(test$atRisk)))


## ----collapse=T---------------------------------------------------------------
summary(model)


## ----collapse=T---------------------------------------------------------------
loglikelihood <- function(y, py) {
  sum(y * log(py) + (1-y)*log(1 - py))
}

# Null probability
(pnull <- mean(as.numeric(train$atRisk)))

# Normalised Null deviance
null.dev <- -2*loglikelihood(as.numeric(train$atRisk), pnull) / nrow(train)
cat("Normalised deviance of the Null model is:", null.dev)


## ----collapse=T---------------------------------------------------------------
# on the training set
pred <- predict(model, newdata=train, type="response")

# deviance of the logistic regression model
resid.dev <- -2*loglikelihood(as.numeric(train$atRisk), pred) / nrow(train)
cat("Normalised deviance of the logistic regression model on the training set is:\n", 
    resid.dev)

# on the test set
pred <- predict(model, newdata=test, type="response")

# deviance of the logistic regression model
resid.dev <- -2*loglikelihood(as.numeric(test$atRisk), pred) / nrow(test)
cat("Normalised deviance of the logistic regression model on the test set is:\n", 
    resid.dev)


## ----collapse=T---------------------------------------------------------------
aic <- resid.dev + 2*(length(model$coefficients))

cat("AIC value of the logistic regression model on the test set is:", aic)

