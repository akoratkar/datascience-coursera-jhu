##Quiz 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mean(x)
##Answer: mu=0.0025

##2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19,-1.59, 1.23,-0.65, 1.49, 0.05)

plot (y ~ x)
regrline <- lm(y ~ x)
abline(regrline, lwd=3, col='red')
coef(summary(regrline)[2]
##Answer: Slope Coefficient: -1.713   

##3
plot (mpg ~ wt, mtcars)
regrline <- lm(mpg ~ wt, mtcars)
abline(regrline, lwd=3, col='red')
coef(summary(regrline)[2]
cor(mtcars$mpg, mtcars$wt)*sd(mtcars$mpg)/sd(mtcars$wt)
##slope = Cor(Y,X)*S(Y)/S(X)
##Answer: Slope Coefficient: -5.3445   

##4
##slope<-cor(outcome, predictor)*sd(outcome)/sd(predictor)
slope<-0.5*2
##Answer: 1

##5
##Y=Quiz-2, X = Quiz-1
mean(Y)=0
mean(X)=0
sd(Y)=1
sd(X)=1
cor(Y,X)=0.4

slope=cor(Y,X)*sd(Y)/sd(X)= 0.4
intercept=mean(Y)-slope*mean(X)=0-0.4*0=0
Y=intercept + slope*X
=0+0.4*1.5
##Answer=0.6

##6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
mux=mean(x)
sdx=sd(x)
y<-(x-mux)/sdx
-0.9718658  1.5310215 -0.3993969  0.4393366 -0.5990954
##Answer=-0.9718658

##7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
coef(regrline <- lm(y ~ x))[1]
##Answer=1.567461 

##8
For centered data, the intercept is 0

##9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)
##Answer 0.573

##10
slopeyx = cor(Y,X)*S(Y)/S(X)
slopexy = cor(Y,X)*S(X)/S(Y)

slopeyx/slopexy=(S(Y)/S(X))/(S(X)/S(Y))=var(Y)/var(X)
