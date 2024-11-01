## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)


## ----collapse=T---------------------------------------------------------------
# download https://github.com/WinVector/PDSwR2/raw/master/PUMS/psub.RDS
path <- "../../data_v2/PUMS/"
psub <- readRDS(paste0(path,"psub.RDS"))  # load in the data frame psub
cat("Dimension of the table frame psub =", dim(psub))

set.seed(3454351)
gp <- runif(nrow(psub))
dtrain <- subset(psub, gp >= 0.5)  # split 50-50 into training
dtest <- subset(psub, gp < 0.5)    # and testing sets

# perform linear regression
model <- lm(log(PINCP,base=10) ~ AGEP + SEX + COW + SCHL, data=dtrain)


## ----collapse=T---------------------------------------------------------------
dtrain$predLogPINCP <- predict(model, newdata=dtrain)
dtest$predLogPINCP <- predict(model, newdata=dtest)


## ----collapse=T, fig.asp=1, out.width="35%"-----------------------------------
ggplot(data=dtest, aes(x=predLogPINCP, y=log(PINCP,base=10))) +
  geom_point(alpha=0.2, color="blue") + geom_smooth(color="black") +
  geom_line(data=dtest, aes(x=log(PINCP,base=10), y=log(PINCP,base=10)),
            color="blue", linetype=2) +
  scale_x_continuous(limits=c(3.5,5.5)) + scale_y_continuous(limits=c(3.5,5.5)) +
  labs(x="predicted log PINCP", y="actual log PINCP") + theme(text=element_text(size=24))


## ----collapse=T, out.width="60%"----------------------------------------------
ggplot(data=dtest, aes(x=predLogPINCP, y=predLogPINCP-log(PINCP,base=10))) +
  geom_point(alpha=0.2, color="blue") + geom_smooth(color="black") +
  labs(x="predicted log PINCP", y="residual log PINCP") + theme(text=element_text(size=24))


## ----collapse=T---------------------------------------------------------------
coefficients(model)


## ----collapse=T---------------------------------------------------------------
levels(dtrain$SCHL)


## ----collapse=T---------------------------------------------------------------
levels(dtrain$COW)
levels(dtrain$SEX)


## ----collapse=T---------------------------------------------------------------
summary(model)


## ----eval=T, echo=F, out.width="70%", fig.asp=0.6-----------------------------
xx <- seq(-6, 6, by=0.01)
yy <- dnorm(xx)
plot(xx, yy, type="l", col="black", xlab="T", ylab="density")
lines(c(-6,6), c(0,0), typ="l")
threshold <- 0.05

# an example point t1
t1 <- 1.5

xpoly <- c(t1, seq(t1, 6, by=0.05))
ypoly <- c(0, dnorm(xpoly[-1]))
polygon(xpoly, ypoly, col="yellow")
polygon(-xpoly, ypoly, col="yellow")

lines(rep(t1,2), c(0, dnorm(t1)))   # draw a vertical line at t1
lines(rep(-t1,2), c(0, dnorm(-t1))) # draw a vertical line at -t1
lines(c(-t1,t1), c(0,0), typ="p", pch=16)     # draw a black dot at t1 and -t1

m.x <- -2.6;   m.y <- 0.15
p.x <- -2;   p.y <- 0.2
arrows(x0=m.x, y0=m.y, x1=-t1-0.1, y1=0.005, angle=20, length=0.12, lty=2, col="brown")
arrows(x0=p.x, y0=p.y, x1= t1-0.1, y1=0.005, angle=20, length=0.12, lty=2, col="brown")
text(m.x, m.y+0.01, labels="-t1", cex=1.1, col="brown")
text(p.x, p.y+0.01, labels=paste("t1",t1,sep="="), cex=1.1, col="brown")

area.x <- 4;  area.y <- 0.15
area.t1 <- round(1-pnorm(t1), 4)
arrows(x0=area.x, y0=area.y, x1=2, y1=0.02, angle=20, length=0.12, lty=2, col="blue")
text(x=area.x, y=area.y+0.02,
     labels=paste("area = 1-pnorm(t1)", paste("  =", area.t1), sep="\n"),
     col="blue")
area.x <- -4;  area.y <- 0.1
arrows(x0=area.x, y0=area.y, x1=-2.0, y1=0.02, angle=20, length=0.12, lty=2, col="blue")
text(x=area.x, y=area.y+0.02,
     labels=paste("area = pnorm(-t1)", paste("  =", area.t1), sep="\n"),
     col="blue")

text(-3.8, 0.3, expression(paste("threshold ", epsilon, " = 0.05")), cex = 1.1)
text( 4.0, 0.38, labels=paste("Pr(T > |t1|)",
                              "  = total area of the",
                              "    two yellow regions", sep="\n"),
     cex=1.1, col="brown")
text( 4.0, 0.34, expression(paste("  = 0.1336 > ", epsilon)), cex=1.1, col="brown")


# another example point t2
t2 <- 2.3
lines(t2, 0, typ="p", pch=16)     # draw a black dot at t2
arrows(x0=4, y0=0.05, x1=t2, y1=0.005, angle=20, length=0.12, lty=2, col="brown")
text(x=4, y=0.06, labels=paste("t2",t2,sep="="), cex=1.1, col="brown")
text( 4.0, 0.30, labels=paste("Pr(T > |t2|)",
                              "  = 2*(1-pnorm(t2))", sep="\n"),
     cex=1.1, col="brown")
text( 4.0, 0.27, expression(paste("  = 0.0214 < ", epsilon)), cex=1.1, col="brown")

grid()


## ----collapse=T---------------------------------------------------------------
coef_df <- data.frame(summary(model)$coefficients)  # extract the coefficients
str(coef_df, vec.len=8)     # coef_df is a data frame having 4 columns

# how the t-value column is computed? We can compare the computed results below
# with the output from str() 
t_value <- coef_df$Estimate / coef_df$`Std..Error`;   cat(t_value[1:8], "\n")

# how the p-value column is computed? 
p_value <- 2*(1 - pnorm(abs(coef_df$t.value)));   cat(p_value[1:8], "\n")


## ----collapse=T---------------------------------------------------------------
rsq <- function(y, yhat) {
    1 - sum((y-yhat)^2) / sum((y-mean(y))^2)
}


## ----collapse=T---------------------------------------------------------------

# R-squared value on the training set
rsq(log(dtrain$PINCP,base=10), predict(model, newdata=dtrain))

# R-squared value on the test set
rsq(log(dtest$PINCP,base=10), predict(model, newdata=dtest))

