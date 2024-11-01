## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)

knitr::knit_hooks$set(mysize = function(before, options, envir) {
  if (before) 
    return(options$size)
})


## ----collapse=T---------------------------------------------------------------
path <- '../../data_v2/Protein/'
df <- read.table(paste0(path,'protein.txt'), sep='\t', header=TRUE)
str(df)


## ----collapse=T---------------------------------------------------------------
vars.to.use <- colnames(df)[-1]  # all column names except for the 1st column
scaled_df <- scale(df[,vars.to.use]) # this is a 25-by-9 matrix where each
                                # column has 0 mean and unit standard deviation


## ----collapse=T---------------------------------------------------------------
# The scaled:center attribute (a vector of 9 elements) contains the mean values
# of all the columns.
attr(scaled_df, "scaled:center")

# The scaled:scale attribute (a vector of 9 elements) contains the variances
# of all the columns.
attr(scaled_df, "scaled:scale")


## ----echo=T, eval=F-----------------------------------------------------------
## # The following also work:
## attributes(scaled_df)$`scaled:center`
## attributes(scaled_df)$`scaled:scale`


## ----collapse=T---------------------------------------------------------------
# dist() returns an object of class 'dist', which includes the pairwise
# Euclidean distances of the 25 observations. In this case, we have
# 25-choose-2 pairs (i.e., 25*24/2 = 300). So 300 Euclidean distance values 
# are stored in variable `d` for further operation.
(d <- dist(scaled_df, method="euclidean"))


## ----out.width="55%"----------------------------------------------------------
pfit <- hclust(d, method="ward.D2")  # perform hierarchical clustering
# To examine `pfit`, type: summary(pfit)   and   pfit$height
plot(pfit, labels=df$Country, main="Cluster Dendrogram for Protein Consumption")
rect.hclust(pfit, k=5)   # k=5 means we want rectangles to be put around 5 clusters 
xx <- c(3, 7.5, 13.5, 19.5, 23.5); yy <- -3.5; clusterID <- c(3,4,2,1,5)
text(xx, yy, clusterID, col="red")


## ----collapse=T---------------------------------------------------------------
groups <- cutree(pfit, k=5)
groups

print_clusters <- function(df, groups, cols_to_print) {
  Ngroups <- max(groups) 
  for (i in 1:Ngroups) {
    print(paste("cluster", i))
    print(df[groups == i, cols_to_print])
  }
}


## ----collapse=T---------------------------------------------------------------
cols_to_print <- c("Country","RedMeat","Fish","Fr.Veg")
print_clusters(df, groups, cols_to_print)


## ----collapse=T---------------------------------------------------------------
princ <- prcomp(scaled_df) # Calculate the principal components of scaled_df
nComp <- 2        # focus on the first two principal components
# project scaled_df onto the first 2 principal components to form a new
# 2-column data frame.
project2D <- as.data.frame(predict(princ, newdata=scaled_df)[,1:nComp])
# combine with `groups` and df$Country to form a 4-column data frame

hclust.project2D <- cbind(project2D, cluster=as.factor(groups), country=df$Country)
head(hclust.project2D)


## -----------------------------------------------------------------------------
# finding convex hull
library('grDevices')
find_convex_hull <- function(proj2Ddf, groups) {
  do.call(rbind,
          lapply(unique(groups),
                 FUN = function(c) {
                   f <- subset(proj2Ddf, cluster==c);
                   f[chull(f),]
                 }
                )
         )
}
hclust.hull <- find_convex_hull(hclust.project2D, groups)


## ----out.width="60%"----------------------------------------------------------
library(ggplot2)
ggplot(hclust.project2D, aes(x=PC1, y=PC2)) +
    geom_point(aes(shape=cluster, color=cluster)) +
    geom_text(aes(label=country, color=cluster), hjust=0, vjust=1, size=3) +
    geom_polygon(data=hclust.hull, aes(group=cluster, fill=as.factor(cluster)),
                 alpha=0.4, linetype=0) + theme(text=element_text(size=20))


## -----------------------------------------------------------------------------
library(fpc)
kbest.p <- 5
cboot.hclust <- clusterboot(scaled_df, clustermethod=hclustCBI,
			    method="ward.D2", k=kbest.p)


## ----collapse=T---------------------------------------------------------------
summary(cboot.hclust$result)


## -----------------------------------------------------------------------------
groups.cboot <- cboot.hclust$result$partition
print_clusters(df, groups.cboot, "Country")


## -----------------------------------------------------------------------------
1 - cboot.hclust$bootbrd/100


## ----echo=F, eval=T, out.width="55%"------------------------------------------
plot(pfit, labels=df$Country, main="Cluster Dendrogram for Protein Consumption")
rect.hclust(pfit, k=5)   # k=5 means we want rectangles to be put around 5 clusters 
xx <- c(3, 7.5, 13.5, 19.5, 23.5); yy <- -3.5; clusterID <- c(3,4,2,1,5)
text(xx, yy, clusterID, col="red")


## -----------------------------------------------------------------------------
# Function to return the squared Euclidean distance of two given points x and y
sqr_euDist <- function(x, y) {
    sum((x - y)^2)
}

# Function to calculate WSS of a cluster, represented as a n-by-d matrix
# (where n and d are the numbers of rows and columns of the matrix)
# which contains only points of the cluster.
wss <- function(clustermat) {
    c0 <- colMeans(clustermat)
    sum(apply( clustermat, 1, FUN=function(row) {sqr_euDist(row, c0)} ))
}

# Function to calculate the total WSS. Argument `scaled_df`: data frame
# with normalised numerical columns. Argument `labels`: vector containing
# the cluster ID (starting at 1) for each row of the data frame.
wss_total <- function(scaled_df, labels) {
    wss.sum <- 0
    k <- length(unique(labels))
    for (i in 1:k) 
        wss.sum <- wss.sum + wss(subset(scaled_df, labels == i))
    wss.sum
}


## -----------------------------------------------------------------------------
# Function to calculate total sum of squared (TSS) distance of data
# points about the (global) mean. This is the same as WSS when the
# number of clusters (k) is 1.
tss <- function(scaled_df) {
   wss(scaled_df)
}


## ----eval=T, echo=F-----------------------------------------------------------
# Function to return the CH indices computed using hierarchical
# clustering (function `hclust`) or k-means clustering (`kmeans`)
# for a vector of k values ranging from 1 to kmax.
CH_index <- function(scaled_df, kmax, method="kmeans") {
    if (!(method %in% c("kmeans", "hclust"))) 
        stop("method must be one of c('kmeans', 'hclust')")

    npts <- nrow(scaled_df)
    wss.value <- numeric(kmax) # create a vector of numeric type
    # wss.value[1] stores the WSS value for k=1 (when all the
    # data points form 1 large cluster).
    wss.value[1] <- wss(scaled_df)

    if (method == "kmeans") {
        # kmeans
        for (k in 2:kmax) {
            clustering <- kmeans(scaled_df, k, nstart=10, iter.max=100)
            wss.value[k] <- clustering$tot.withinss
        } 
    } else {
        # hclust
        d <- dist(scaled_df, method="euclidean")
        pfit <- hclust(d, method="ward.D2")
        for (k in 2:kmax) {
            labels <- cutree(pfit, k=k)
            wss.value[k] <- wss_total(scaled_df, labels)
        }
    }
    bss.value <- tss(scaled_df) - wss.value   # this is a vector
    B <- bss.value / (0:(kmax-1))             # also a vector
    W <- wss.value / (npts - 1:kmax)          # also a vector

    data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
}


## ----eval=F, echo=T-----------------------------------------------------------
## # Function to return the CH indices computed using hierarchical
## # clustering (function `hclust`) or k-means clustering (`kmeans`)
## # for a vector of k values ranging from 1 to kmax.
## CH_index <- function(scaled_df, kmax, method="kmeans") {
##     if (!(method %in% c("kmeans", "hclust")))
##         stop("method must be one of c('kmeans', 'hclust')")
## 
##     npts <- nrow(scaled_df)
##     wss.value <- numeric(kmax) # create a vector of numeric type
##     # wss.value[1] stores the WSS value for k=1 (when all the
##     # data points form 1 large cluster).
##     wss.value[1] <- wss(scaled_df)


## ----eval=F, echo=T-----------------------------------------------------------
##     if (method == "kmeans") {
##         # kmeans
##         for (k in 2:kmax) {
##             clustering <- kmeans(scaled_df, k, nstart=10, iter.max=100)
##             wss.value[k] <- clustering$tot.withinss
##         }
##     } else {
##         # hclust
##         d <- dist(scaled_df, method="euclidean")
##         pfit <- hclust(d, method="ward.D2")
##         for (k in 2:kmax) {
##             labels <- cutree(pfit, k=k)
##             wss.value[k] <- wss_total(scaled_df, labels)
##         }
##     }
##     bss.value <- tss(scaled_df) - wss.value   # this is a vector
##     B <- bss.value / (0:(kmax-1))             # also a vector
##     W <- wss.value / (npts - 1:kmax)          # also a vector
## 
##     data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
## }
## 


## ----out.width="80%", fig.asp=0.4---------------------------------------------
library(gridExtra)

# calculate the CH criterion
crit.df <- CH_index(scaled_df, 10, method="hclust")

fig1 <- ggplot(crit.df, aes(x=k, y=CH_index)) +
  geom_point() + geom_line(colour="red") + 
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="CH index") + theme(text=element_text(size=20))

fig2 <- ggplot(crit.df, aes(x=k, y=WSS), color="blue") +
  geom_point() + geom_line(colour="blue") + 
  scale_x_continuous(breaks=1:10, labels=1:10) +
  theme(text=element_text(size=20))


## ----out.width="95%", fig.asp=0.4---------------------------------------------
grid.arrange(fig1, fig2, nrow=1)


## ----out.width="65%"----------------------------------------------------------
plot(pfit, labels=df$Country, main="Cluster Dendrogram for Protein Consumption")
rect.hclust(pfit, k=2)  

