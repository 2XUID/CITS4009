## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')


## ----eval=FALSE--------------------------------------------------------------------------------------------------------
## ggplot(data = <DATA>) +
##   <GEOM_FUNCTION>(
##      mapping = aes(<MAPPINGS>),
##      stat = <STAT>,
##      position = <POSITION>
##   ) +
##   <COORDINATE_FUNCTION> +
##   <FACET_FUNCTION>


## ----eval=FALSE--------------------------------------------------------------------------------------------------------
## install.packages("hexbin")
## install.packages("gridExtra")
## install.packages("maps")


## ----out.width='70%'---------------------------------------------------------------------------------------------------
custdata <- read.delim('../../data/custdata.tsv', as.is=FALSE)
theme_set(theme_grey(base_size = 18))
pairs(custdata)


## ----out.width='70%'---------------------------------------------------------------------------------------------------
pairs(~sex+age+income+health.ins, data=custdata)


## ----out.width='70%'---------------------------------------------------------------------------------------------------
pairs(~displ+cty+hwy, data=mpg)


## ----echo=TRUE---------------------------------------------------------------------------------------------------------
# left
p1 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(title = "point geom")
  
# right
p2 <- ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  labs(title = "smooth geom")


## ----out.width='65%'---------------------------------------------------------------------------------------------------
library(gridExtra)
grid.arrange(p1, p2, ncol=2)


## ----out.width='60%'---------------------------------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy), 
    colour = "blue", shape = 24, fill = "red")


## ----collapse=T--------------------------------------------------------------------------------------------------------
library(gridExtra)
p1 <- ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

p2 <- ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv, 
                            linetype = drv))
              
p3 <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class),
             show.legend = TRUE)

p4 <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class, 
                           shape = drv))


## ----out.width='76%'---------------------------------------------------------------------------------------------------
grid.arrange(p1, p2, p3, p4, ncol=2)


## ----out.width='60%'---------------------------------------------------------------------------------------------------
ggplot(data = custdata) +
  geom_smooth(mapping = aes(x=age, y=income, color=marital.stat),
              show.legend = TRUE)


## ----out.width='60%'---------------------------------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))


## ----out.width='60%'---------------------------------------------------------------------------------------------------
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color = class)) + 
  geom_point() + geom_smooth()


## ----out.width="65%"---------------------------------------------------------------------------------------------------
ggplot(custdata, aes(x=age, y=income)) + geom_hex(binwidth=c(5, 10000)) +
  geom_smooth(color="white", se=F) + ylim(0,200000)

