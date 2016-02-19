##Quiz 2

##1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <-lm(y~x)
summary(fit)$coef
##0.05296

##2
summary(fit)$sigma
##0.223

##3
x<-mtcars$wt
y<-mtcars$mpg
fit<-lm(y ~ x)
predict(fit,data.frame(x=mean(x)), interval="confidence")

##fit      lwr      upr
##1 20.09062 18.99098 21.19027 OR below
fit <- lm(mpg ~ I(wt - mean(wt)), data = mtcars)
confint(fit)


##4
help(mtcars)
##The estimated 1,000 lb change in weight per 1 mpg increase.

##5
predict(fit,data.frame(x=3), interval="prediction")

##fit      lwr      upr
##1 21.25171 14.92987 27.57355

##6
fit2<-lm(y~I(x/2))
tbl2<-summary(fit2)$coefficients
mn<-tbl2[2,1]      #mean is the estimated slope
std_err<-tbl2[2,2] #standard error
deg_fr<-fit2$df    #degree of freedom
#Two sides T-Tests
mn + c(-1,1) * qt(0.975,df=deg_fr) * std_err

##[1] -12.97262  -8.40527 OR below
fit <- lm(mpg ~ wt, data = mtcars)
confint(fit)[2, ] * 2

##7
summary(fit)$coefficients
fit3<-lm(y~I(x/100))
##x           -5.344472   0.559101 -9.559044 1.293959e-10
summary(fit3)$coefficients
##I(x/100)    -534.44716  55.910105 -9.559044 1.293959e-10

##It would get multiplied by 100.

##8
c<-5
cf1<-summary(fit)$coefficients
cf1

fit4<-lm(y~I(x+c)) # add some constant c
cf2<-summary(fit4)$coefficients
cf2

b0<-cf1[1,1]
b1<-cf1[2,1]
c(b0,b1)

b0 - c*b1

##The new intercept would be (c) OR below
sse1 <- sum((predict(fit1) - mtcars$mpg)^2)
sse2 <- sum((predict(fit2) - mtcars$mpg)^2)
sse1/sse2

##9
fit5<-lm(y ~ 1)
fit6<-lm(y ~ x - 1)
plot(x,y)

abline(fit,col="red")
abline(fit5,col="blue")
abline(fit6,col="green")

anova(fit)
anova(fit5)

278.32/1126
##[1] 0.2471758

##10

sum(resid(fit))  #both intercept and slope
sum(resid(fit5)) #only intercept
sum(resid(fit6)) #only slope
summary(fit)$sigma 

##If an intercept is included, then they will sum to 0.

