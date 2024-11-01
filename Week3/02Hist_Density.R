## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)


## ----warning = FALSE, message = FALSE, out.width = '60%', size="small"-------------------------------------------------
custdata_v2 <- readRDS('../../data_v2/Custdata/custdata.RDS')
library(ggplot2)
ggplot(custdata_v2, aes(x=gas_usage)) +
geom_histogram(binwidth=10, fill="blue")


## ----warning = FALSE, message = FALSE, out.width = '90%'---------------------------------------------------------------
custdata_v1 <- read.table('../../data/custdata.tsv',header=T,sep='\t')
income_stat <- boxplot.stats(custdata_v1$income)$stats
income_stat_str <- paste(income_stat, collapse=" ")
library(scales)
fig <- ggplot(custdata_v1) + geom_density(aes(x=income)) +
  labs(y="density") +
  scale_x_continuous(labels=dollar, breaks=c(35000,200000,400000)) +
  annotate("text", x = 180000, y = 1e-05,
    label = paste("Most of the distribution is concentrated",
      "at the low end: less than $100,000 a year.", sep="\n")) +
  annotate("text", x = 400000, y = 1.5e-06,
    label = paste("Subpopulation of", "wealthy customers",
      "in the $400,000 range.", sep="\n")) +
  annotate("text", x = 550000, y = 1e-06,
    label = paste("Wide data range", "may consider log scale.",
	 sep="\n")) +
  annotate("text", x=350000, y = 1e-05, hjust=0,
    label=paste("stats: ", income_stat_str, sep="\n"))


## ----out.width = '75%'-------------------------------------------------------------------------------------------------
fig


## ----out.width = '55%', warning=F--------------------------------------------------------------------------------------
ggplot(custdata_v1) + geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,35000,200000),labels=dollar) +
  annotation_logticks(sides="bt") + theme(text = element_text(size = 18))

