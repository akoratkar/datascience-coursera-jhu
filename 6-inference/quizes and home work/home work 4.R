library(datasets)
data(mtcars)

##Q1
> mean(mtcars$mpg)
[1] 20.09062

> sd(mtcars$mpg)
[1] 6.026948

> z<-qnorm(0.05)
> z
[1] -1.644854

##mu0 <- mn - z * s / sqrt(nrow(mtcars))

> mu0<-mean(mtcars$mpg)-qnorm(0.05)*sd(mtcars$mpg)/sqrt(nrow(mtcars))
> mu0
[1] 21.84309

> t.test(mtcars$mpg, lower.tail=FALSE)$conf.int
[1] 17.91768 22.26357
attr(,"conf.level")
[1] 0.95

##Q2
g4 <- mtcars$mpg[mtcars$cyl == 4]
g6 <- mtcars$mpg[mtcars$cyl == 6]

t.test(g4,g6,paired=FALSE,alternative="two.sided", var.equal=TRUE)
##Two Sample t-test
##data:  g4 and g6
##t = 3.8952, df = 16, p-value = 0.001287
##alternative hypothesis: true difference in means is not equal to 0
##95 percent confidence interval:
##      3.154286 10.687272
##sample estimates:
##mean of x mean of y 
##26.66364  19.74286 

##Q3
n<-100
mu0<-3
sd<-1.1

##5% 2 sided tests
##95% T-confidence
mu0 + c(-1,1)*qt(.975,n-1)*sd/sqrt(n)


##Q4
> pbinom(54, size=100, prob=0.5, lower.tail=FALSE)
[1] 0.1841008 ##P-value

##Would you reject a 1 sided hypothesis at ??=.05 NO

##Q5
ppois(15800, lambda=520*30,lower.tail=FALSE)
##Would you reject a 1 sided hypothesis at ??=.05 NO

##Q6
n_x<-100 ##Old
mu_x<-10
df_x<-n_x-1
sd_x<-4
var_x <-sd_x^2

n_y<-100 ##New
mu_y<-11
df_y<-n_y-1
sd_y<-4
var_y <-sd_y^2

df_yx <-df_x+df_y
var_yx <- (df_x*var_x+df_y*var_y)/(df_yx)
sd_yx<-sqrt(var_yx)

se_yx <- sd_yx* sqrt(1 / n_x + 1 / n_y)

ts_yx <- (mu_y - mu_x) / se_yx
pv_yx <- 2 * pnorm(-abs(ts_yx))

##Q9
n_x<-100 ##H_0
mu_x<-10
df_x<-n_x-1
sd_x<-4
var_x <-sd_x^2

n_y<-100 ##New
mu_y<-11
df_y<-n_y-1
sd_y<-4
var_y <-sd_y^2

df_yx <-df_x+df_y
var_yx <- (df_x*var_x+df_y*var_y)/(df_yx)
sd_yx<-sqrt(var_yx)

se_yx <- sd_yx* sqrt(1 / n_x + 1 / n_y)
ts_yx <- (mu_y - mu_x) / se_yx
pv_yx <- 2 * pnorm(-abs(ts_yx))

z<-pnorm(0.95)
pnorm(mu_x+z, mean=mu_y, sd=sd_y, lower.tail=FALSE) ##; p-value
##[1] 0.5170552

power <- pnorm(10 + qnorm(.95) * 4, mean = 11, sd = 4, lower.tail = FALSE)

power.t.test(n=n_y, delta=(mu_y-mu_x), sd=sd_yx)$power

##Q10
n <- (qnorm(.95) + qnorm(.8)) ^ 2 * .04 ^ 2 / .01^2

##Q11
p <- t.test(mpg8, mpg6, paired = FALSE, alternative="two.sided", var.equal=TRUE)$p.value
mixprob <- (n8 - 1) / (n8 + n6 - 2)
s <- sqrt(mixprob * s8 ^ 2  +  (1 - mixprob) * s6 ^ 2)
z <- (m8 - m6) / (s * sqrt(1 / n8 + 1 / n6))
pz <- 2 * pnorm(-abs(z))
## Hand calculating the T just to check
#2 * pt(-abs(z), df = n8 + n6 - 2)