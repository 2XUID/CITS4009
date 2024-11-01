## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)
theme_set(theme_grey(base_size = 22))


## ----out.width="70%"---------------------------------------------------------------------------------------------------
custdata<- read.table('../../data/custdata.tsv',
                          header=T,sep='\t')
boxplot(custdata$age, notch=TRUE, col="gold")


## ----out.width = '40%', collapse=T-------------------------------------------------------------------------------------
ggplot(custdata) + 
  geom_boxplot(aes(y=age), outlier.colour="red",
      outlier.shape=16, outlier.size=2, notch=FALSE)


## ----size="small", collapse=T------------------------------------------------------------------------------------------
boxplot.stats(custdata$age)$stats


## ----echo=T, eval=F, size="small", collapse=T, out.width="45%"---------------------------------------------------------
## x <- rnorm(1000); boxplot(x, col="gold"); grid()
## cat(boxplot.stats(x)$stats) # we call these numbers Q0,...,Q4


## ----echo=F, eval=T, size="small", collapse=T, out.width="67%"---------------------------------------------------------
z <- rnorm(1000)
boxplot(z, col="gold")
grid()
q <- boxplot.stats(z)$stats
qlab <- paste0("Q", 0:4)
qlab[2] <- paste(qlab[2], "(lower quartile)")
qlab[3] <- paste(qlab[3], "(median)")
qlab[4] <- paste(qlab[4], "(upper quartile)")
xx <- array(1.11, c(1,5))
xx[2:4] <- 1.22
text(x=xx, y=q, labels=qlab, col="blue", cex=1.5, adj=0)
#
midx <- c(1.2, 1, 1, 1.2)
midy <- (q[-1] + q[-5]) / 2
qtile_labels <- paste("Quartile group", 1:4, "(25%)")
text(x=midx, y=midy, labels=qtile_labels, col="red", cex=1.5)
#
xx <- c(0.9, 0.9)
yy0 <- c(midy[1]-0.2, midy[4]+0.2)
yy1 <- c(midy[1], midy[4])
arrows(x0=xx, y0=yy0, x1=xx+0.095, y1=yy1, col="green", angle=20, length=0.15)
text(x=xx-0.01, y=yy0, labels=paste(c("lower","upper"), "whisker"),
     cex=1.5, font=3, adj=1, col="green")
#
xm <- 0.77
xx <- c(xm, xm)
yy <- c(q[2], q[4])
arrows(x0=xx, y0=yy, x1=xx, y1=yy[2:1], col="black", angle=20, length=0.15)
text(x=xm-0.01, y=q[3], "IQR", cex=1.5, font=2, adj=1)
#
cat(q)

