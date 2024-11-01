## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
custdata <- read.table('../../data/custdata.tsv', header=T, sep='\t')


## ----------------------------------------------------------------------------------------------------------------------
library(crayon)
little_bunny <- function(name) {
  return("Little bunny " %+% name)
} 

hop <- function(data, through) {
  return(data %+% "\nWent hopping through the " %+% through)
}

scoop <- function(data, up) {
  return(data %+% "\nScooping up the " %+% up) 
}

bop <- function(data, on) {
  return(data %+% "\nAnd bopping them on the " %+% on)
}


## ----collapse=T--------------------------------------------------------------------------------------------------------
s <- little_bunny("Foo Foo")
s <- hop(s, "forest")
s <- scoop(s, "field mice")
s <- bop(s, "head")
cat(s)


## ----collapse=T--------------------------------------------------------------------------------------------------------
s <-  bop(scoop(hop(little_bunny("Foo Foo"), "forest"), "field mice"), "head")
cat(s)


## ----------------------------------------------------------------------------------------------------------------------
library(dplyr)

little_bunny("Foo Foo") |>
hop(through = "forest") |>
scoop(up = "field mice") |>
bop(on = "head") |>
cat()


## ----eval=F------------------------------------------------------------------------------------------------------------
## customer_data <- customer_data %>%
##   mutate(
##     gas_with_rent = (gas_usage ==1),
##     gas_with_electricity = (gas_usage == 2),
##     no_gas_bill = (gas_usage == 3)
##   ) %>%
##   mutate(
##     gas_usage = ifelse(gas_usage < 4, NA, gas_usage)
##   )


## ----eval=T, echo=F----------------------------------------------------------------------------------------------------
load("authors_books.rData")


## ----collapse=T--------------------------------------------------------------------------------------------------------
# kable() requires the knitr library
merge(authors, books, by.x="surname", by.y="name") %>%
	subset(nationality=="NZ") %>%
	kable()


## ----collapse=T--------------------------------------------------------------------------------------------------------
result1 <- merge(authors, books, by.x="surname", by.y="name")
result2 <- inner_join(authors, books, by=c("surname"="name"))


## ----------------------------------------------------------------------------------------------------------------------
library(tibble)
students <- tribble (
   ~name, ~degree, ~start.year, ~mode,
   "John", "MDS", 2020, "part-time",
   "Jack", "MIT", 2019, "full-time",
   "Rose", "BSc", 2020, "full-time",
   "Mary", "MDS", 2018, "part-time",
   "Paul", "BPhil", 2020, "full-time"
   )

degrees <- tribble (
   ~degree, ~duration,
   "BPhil", 4,
   "BSc", 3,
   "MDS", 2,
   "MIT", 2,
   "MPE", 2
)


## ----------------------------------------------------------------------------------------------------------------------
units <- tribble (
   ~unit, ~degree,
   "CITS1401", "BSc",
   "CITS4009", "MDS",
   "CITS4009", "MIT",
   "CITS4401", "MIT",
   "CITS4402", "BPhil",
   "CITS5508", "MDS",
   "CITS5508", "MIT"
)


## ----------------------------------------------------------------------------------------------------------------------
Jack.units <- subset(students, name=="Jack", select="degree") %>%
   merge(units) %>% subset(select="unit")
cat("Jack's list of units:")
kable(Jack.units)


## ----eval=F------------------------------------------------------------------------------------------------------------
## Jack.units <- students %>% subset(name=="Jack", select="degree") %>%
##    merge(units) %>% subset(select="unit")


## ----------------------------------------------------------------------------------------------------------------------
Jack.units <- merge(students, units) %>% 
   subset(name=="Jack", select="unit")
cat("Jack's list of units:")
kable(Jack.units)


## ----------------------------------------------------------------------------------------------------------------------
df <- subset(students, name=="Mary") %>% inner_join(degrees, by="degree")
graduation.year <- df$start.year +
   df$duration * ifelse(df$mode == "part-time", 2, 1)
cat("Mary is expected to graduate in", graduation.year)


## ----eval=T, echo=F----------------------------------------------------------------------------------------------------
# save the following data frames for another Rmd file (in case they are needed)
if (!file.exists("students_degrees_units.rData")) {
        save(students, degrees, units, file="students_degrees_units.rData")
}

