## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
custdata <- read.table('../../data/custdata.tsv', header=T, sep='\t')


## ----------------------------------------------------------------------------------------------------------------------
custdata_v2 <- readRDS('../../data_v2/Custdata/custdata.RDS')


## ----------------------------------------------------------------------------------------------------------------------
library(dplyr)

customer_data <- 
  mutate(custdata_v2, 
         age = na_if(age, 0),
         income = ifelse(income < 0, NA, income))


## ----------------------------------------------------------------------------------------------------------------------
customer_data <- customer_data |>
  mutate(
    gas_with_rent = (gas_usage ==1),
    gas_with_electricity = (gas_usage == 2),
    no_gas_bill = (gas_usage == 3) 
  ) |>
  mutate(
    gas_usage = ifelse(gas_usage < 4, 
                       NA,
                       gas_usage
                       )
  )


## ----collapse=T--------------------------------------------------------------------------------------------------------
count.zero <- sum(custdata$income == 0)
count.neg <- sum(custdata$income < 0)
cat("Number of customers having 0 incomes: ", count.zero)
cat("Number of customers having negative incomes: ", count.neg)


## ----collapse=T--------------------------------------------------------------------------------------------------------
summary(custdata$income)
stat <- boxplot.stats(custdata$income)
stat$n           # number of non-NAs
stat$stats       # quartiles
length(stat$out) # number of outliers
cat("min, max outlying incomes are: ", min(stat$out), max(stat$out))


## ----collapse=T--------------------------------------------------------------------------------------------------------
# create a new variable called income.mod with negative incomes set to NA
custdata$income.mod <- ifelse(custdata$income < 0, NA, custdata$income)
stat <- boxplot.stats(custdata$income.mod)

stat$n           # number of non-NAs
q <- stat$stats  # quartiles
q
length(stat$out) # number of outliers
cat("min, max outlying incomes are: ", min(stat$out), max(stat$out))


## ----size="small", collapse=T------------------------------------------------------------------------------------------
NA > 5


## ----size="small", collapse=T------------------------------------------------------------------------------------------
10 == NA


## ----size="small", collapse=T------------------------------------------------------------------------------------------
NA == NA


## ----size="small", collapse=T------------------------------------------------------------------------------------------
count_missing <- function(df) {
  sapply(df, FUN = function(col) sum(is.na(col)) )
}

nacounts <- count_missing(customer_data)
hasNA = which(nacounts > 0)
nacounts[hasNA]


## ----------------------------------------------------------------------------------------------------------------------
custdata <- read.table('../../data/custdata.tsv',
		       header=T, sep='\t')
nacounts <- count_missing(custdata)
hasNA = which(nacounts > 0)
nacounts[hasNA]


## ----size="small", collapse=T------------------------------------------------------------------------------------------
summary(custdata[is.na(custdata$housing.type),
                c("recent.move","num.vehicles")])


## ----size="small"------------------------------------------------------------------------------------------------------
newdata <- na.omit(custdata)
nrow(custdata)
nrow(newdata)


## ----size="small"------------------------------------------------------------------------------------------------------
newdata <- 
  custdata[!is.na(custdata$housing.type),]
nrow(custdata)
nrow(newdata)


## ----size="small", collapse=T------------------------------------------------------------------------------------------
custdata$is.employed.fix <- 
  ifelse(is.na(custdata$is.employed),
         "missing", ifelse(custdata$is.employed==T,
                           "employed", "not-employed"))
summary(as.factor(custdata$is.employed.fix))


## ----------------------------------------------------------------------------------------------------------------------
library(vtreat)

varlist <- setdiff(colnames(customer_data), 
                   c("custid", "health_ins"))

treatment_plan <- design_missingness_treatment(
  customer_data, varlist = varlist)

training_prepared <- prepare(treatment_plan,
                             customer_data)

nacounts <- count_missing(training_prepared)
sum(nacounts)


## ----size="small", collapse=T------------------------------------------------------------------------------------------
missing.ht <- which(
  is.na(customer_data$housing_type))

columns_to_look_at <- 
  c("custid", "is_employed", "num_vehicles",
    "housing_type", "health_ins")

customer_data[missing.ht, columns_to_look_at] %>% head()


## ----size="small", collapse=T------------------------------------------------------------------------------------------
columns_to_look_at = c("custid", 
   "is_employed", "is_employed_isBAD", "num_vehicles",
   "num_vehicles_isBAD", "housing_type", "health_ins")
training_prepared[missing.ht, columns_to_look_at] %>% head()


## ----eval=T, echo=F----------------------------------------------------------------------------------------------------
# save the various data frame objects to file for the next Rmarkdown file
# for the lecture note.
save(custdata, customer_data, file="01output.rData")

