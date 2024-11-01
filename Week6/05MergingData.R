## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')


## ----eval=FALSE--------------------------------------------------------------------------------------------------------
## total <- rbind(dataframeA, dataframeB)


## ----------------------------------------------------------------------------------------------------------------------
df1 <- custdata[, c("custid","age")]
df2 <- custdata["income"]
# suppose that we now want to merge these two data frames
newdata <- cbind(df1, df2)
cat(nrow(newdata), ncol(newdata))
cat(names(newdata))


## ----------------------------------------------------------------------------------------------------------------------
authors <- data.frame(
    surname = c("Tukey", "Venables", "Ripley",
                  "Tierney", "Winton"),
    nationality = c("US", "Australia", "NZ", "US", "UK"),
    deceased = c("yes", "yes", rep("no", 3)))
# kable() requires the knitr library
kable(authors)


## ----------------------------------------------------------------------------------------------------------------------
books <- data.frame(
    name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
    title = c("Exploratory Data Analysis", "Modern Applied Statistics",
              "LISP-STAT", "Spatial Statistics", "Stochastic Simulation",
              "Interactive Data Analysis", "An Introduction to R"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA, "Venables & Smith"))
kable(books)


## ----collapse=T--------------------------------------------------------------------------------------------------------
# rename "surname" to "name" in the authors data frame
authorN <- within(authors, { name <- surname; rm(surname) })
kable(authorN)


## ----collapse=T--------------------------------------------------------------------------------------------------------
# R finds columns of matching names
m0 <- merge(authorN, books)
kable(m0)


## ----collapse=T--------------------------------------------------------------------------------------------------------
m1 <- merge(authors, books, by.x = "surname", by.y = "name")
kable(m1)


## ----------------------------------------------------------------------------------------------------------------------
m2 <- merge(authorN, books, all.x = TRUE)
kable(m2)


## ----------------------------------------------------------------------------------------------------------------------
m3 <- merge(authorN, books, all.y = TRUE)
kable(m3)


## ----------------------------------------------------------------------------------------------------------------------
m4 <- merge(authorN, books, all = TRUE)
kable(m4)


## ----collapse=T--------------------------------------------------------------------------------------------------------
median.income <- aggregate(custdata[, 'income'],
                           list(custdata$state.of.res), median)
kable(head(median.income))
cat(nrow(median.income), ncol(median.income))


## ----------------------------------------------------------------------------------------------------------------------
custdata <- merge(custdata, median.income,
                  by.x="state.of.res", by.y="Group.1")


## ----------------------------------------------------------------------------------------------------------------------
custdata$income.normalised <- with(custdata, income/x)
kable(custdata[1:6, c("custid","state.of.res","income","x","income.normalised")])


## ----eval=T, echo=F, collapse=T----------------------------------------------------------------------------------------
library(tibble)
table.dplyr_merge <- tribble(
  ~dplyr, 	~merge,
  "inner_join(x, y)", 	"merge(x, y)",
  "left_join(x, y)", 	"merge(x, y, all.x = TRUE)",
  "right_join(x, y)", 	"merge(x, y, all.y = TRUE)",
  "full_join(x, y)", 	"merge(x, y, all.x = TRUE, all.y = TRUE)"
  )
kable(table.dplyr_merge)


## ----eval=T, echo=F, collapse=T----------------------------------------------------------------------------------------
table.dplyr_sql <- tribble(
  ~dplyr, 	~SQL,
  'inner_join(x, y, by = "z")',	'SELECT * FROM x INNER JOIN y USING (z)',
  'left_join(x, y, by = "z")', 	'SELECT * FROM x LEFT OUTER JOIN y USING (z)',
  'right_join(x, y, by = "z")',	'SELECT * FROM x RIGHT OUTER JOIN y USING (z)',
  'full_join(x, y, by = "z")', 	'SELECT * FROM x FULL OUTER JOIN y USING (z)'
  )
kable(table.dplyr_sql)


## ----------------------------------------------------------------------------------------------------------------------
library(dplyr)
m5 <- semi_join(books, authorN)
kable(m5)


## ----------------------------------------------------------------------------------------------------------------------
m6 <- anti_join(books, authorN)
kable(m6)


## ----collapse=T--------------------------------------------------------------------------------------------------------
x <- authorN$name
y <- books$name

x

y


## ----collapse=T--------------------------------------------------------------------------------------------------------
setdiff(x, y) # What are in x but not y?

union(x, y)   # Duplications counted once only

intersect(x, y)  # Common elements in x and y


## ----eval=T, echo=F----------------------------------------------------------------------------------------------------
# save the following data frames for another Rmd file (in case they are needed)
if (!file.exists("authors_books.rData")) {
	save(authors, books, file="authors_books.rData")
}

