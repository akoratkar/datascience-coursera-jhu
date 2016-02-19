##Quiz 3

##1
fit1<-lm(mpg~factor(cyl)+wt, data=mtcars)
summary(fit1)$coef
## Answer 2: factor(cyl)8 -6.070860 

##2
fit2<-lm(mpg~factor(cyl), data=mtcars)
summary(fit2)$coef
##Answer 4: Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is
disregarded.

##3
fit3<-lm(mpg~factor(cyl)+factor(cyl)*wt, data=mtcars)
summary(fit3)$coef
anova(fit2, fit3, test = "Chisq")
## Answer 3: The P-value is large (greater than 0.05). So, according to our criterion, we reject, which suggests that the interaction term is not necessary.

##4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
##Answer 3: The estimated expected change in MPG per one ton increase in weight for a specific number of
cylinders (4, 6, 8).

##5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127,-0.751, 1.344)

fit5<-lm(y~x)
influence(lm(y ~ x))$hat
hatvalues(fit5)
##Answer 1: 0.9946

##6
influence.measures(lm(y ~ x))
dfbetas(fit5)[,2]
##Answer 2: -134

##7
##Answer: 3 It is possible for the coefficient to reverse sign after adjustment. For example, it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment.

