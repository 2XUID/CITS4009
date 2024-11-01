## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)
library(gridExtra)
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')


## ----out.width='50%'---------------------------------------------------------------------------------------------------
ggplot(data = mpg) + 
  stat_count(mapping = aes(x = class))


## ----------------------------------------------------------------------------------------------------------------------
library(tibble)
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)


## ----collapse=T, out.width='50%'---------------------------------------------------------------------------------------
ggplot(data = demo) +
  geom_bar(mapping = aes(x=cut, y=freq), stat = "identity")


## ----out.width='50%'---------------------------------------------------------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))


## ----collapse=T, out.width="50%"---------------------------------------------------------------------------------------
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )


## ----eval=FALSE--------------------------------------------------------------------------------------------------------
## facet_wrap(~ class, nrow = 2)


## ----out.width='60%'---------------------------------------------------------------------------------------------------
ggplot(data = custdata) + 
  geom_point(mapping = aes(x = age, y = income)) + 
  facet_wrap(~ marital.stat, nrow = 2)


## ----out.width='55%'---------------------------------------------------------------------------------------------------
ggplot(data = custdata) + 
  geom_point(mapping = aes(x = age, y = log(income))) + 
  facet_wrap( sex ~ marital.stat )


## ----------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  theme(text = element_text(size = 12), aspect.ratio=1)
p2 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip() +
  theme(text = element_text(size = 12), aspect.ratio=1)


## ----out.width="90%"---------------------------------------------------------------------------------------------------
grid.arrange(p1, p2, ncol=2)


## ----------------------------------------------------------------------------------------------------------------------
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

p1 <- bar + coord_flip()
p2 <- bar + coord_polar()


## ----out.height='70%'--------------------------------------------------------------------------------------------------
grid.arrange(p1, p2, ncol=2)


## ----out.width='90%'---------------------------------------------------------------------------------------------------
library(maps)
w <- map_data("state")

p1 <- ggplot(w, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

p2 <- ggplot(w, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap() # coord_sf() is now preferred


## ----out.width="90%"---------------------------------------------------------------------------------------------------
grid.arrange(p1, p2, ncol=2)

