
##Quiz 3

library(datasets)
data(iris)
class(iris)
head(iris)

##Q1
irisbyspecies<-split(iris, iris$Species)
lapply (irisbyspecies, function (x) colMeans(x[, c("Sepal.Length", "Sepal.Width")]))

##Q2
columnmeans<-apply(iris[, 1:4], 2, mean)
class(columnmeans)

##Q3
library(datasets)
data(mtcars)
class(mtcars)
milesbycylinders<-split(mtcars$mpg, mtcars$cyl)
sapply(milesbycylinders, mean)

##Q4
hpbycyc <-split(mtcars$hp, mtcars$cyl)
averagehp<-sapply(hpbycyc, mean)
averagehp[3]-averagehp[1]


