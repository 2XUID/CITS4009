## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(ggplot2)
library(crayon)
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)

knitr::knit_hooks$set(mysize = function(before, options, envir) {
  if (before) 
    return(options$size)
})

source('02HierarchicalClustering-Cluster.R')


## ----collapse=T---------------------------------------------------------------
kbest.p <- 5
# run kmeans with 5 clusters, 100 random starts, and 100
# maximum iterations per run.
kmClusters <- kmeans(scaled_df, kbest.p, nstart=100, iter.max=100)


## ----collapse=T---------------------------------------------------------------
kmClusters$centers
kmClusters$size
cat("Total of cluster sizes =", sum(kmClusters$size))
cat("Total number of observations =", nrow(df))


## ----collapse=T---------------------------------------------------------------
groups <- kmClusters$cluster
print_clusters(df, groups, "Country")


## ----collapse=T---------------------------------------------------------------
library(fpc)
kmClustering.ch <- kmeansruns(scaled_df, krange=1:10, criterion="ch")
kmClustering.ch$bestk
kmClustering.asw <- kmeansruns(scaled_df, krange=1:10, criterion="asw")
kmClustering.asw$bestk

# Compare the CH values for kmeans() and hclust().
print("CH index from kmeans for k=1 to 10:")
print(kmClustering.ch$crit)

print("CH index from hclust for k=1 to 10:")
hclusting <- CH_index(scaled_df, 10, method="hclust")
print(hclusting$CH_index)


## ----collapse=T, out.width="65%", fig.asp=0.45--------------------------------
library(gridExtra)
kmCritframe <- data.frame(k=1:10, ch=kmClustering.ch$crit, 
                          asw=kmClustering.asw$crit)
fig1 <- ggplot(kmCritframe, aes(x=k, y=ch)) +
  geom_point() + geom_line(colour="red") + 
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="CH index") + theme(text=element_text(size=20))
fig2 <- ggplot(kmCritframe, aes(x=k, y=asw)) +  
  geom_point() + geom_line(colour="blue") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="ASW") + theme(text=element_text(size=20))
grid.arrange(fig1, fig2, nrow=1)


## ----collapse=T---------------------------------------------------------------
fig <- c()
kvalues <- seq(2,5)
for (k in kvalues) {
  groups <- kmeans(scaled_df, k, nstart=100, iter.max=100)$cluster
  kmclust.project2D <- cbind(project2D, cluster=as.factor(groups),
                             country=df$Country)
  kmclust.hull <- find_convex_hull(kmclust.project2D, groups)
  assign(paste0("fig", k),
    ggplot(kmclust.project2D, aes(x=PC1, y=PC2)) +
    geom_point(aes(shape=cluster, color=cluster)) +
    geom_polygon(data=kmclust.hull, aes(group=cluster, fill=cluster),
                 alpha=0.4, linetype=0) + 
    labs(title = sprintf("k = %d", k)) +
    theme(legend.position="none", text=element_text(size=20))
    )
}


## ----out.width="80%"----------------------------------------------------------
library(gridExtra)
grid.arrange(fig2, fig3, fig4, fig5, nrow=2)

