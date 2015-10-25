library(datasets)
data(mtcars)

##Q1
t.test(mtcars$mpg)$conf.int
##[1] 17.91768 22.26357
##attr(,"conf.level")
##[1] 0.95

##Q2
n<-9
sd<-1
meandif<-NULL ##So that the lower interval of t confidence is ZERO

##
mean_of_diff<-qt(0.975, n-1)*sd/sqrt(n)

##Q3

##Q4
g4 <- mtcars$mpg[mtcars$cyl == 4]
g6 <- mtcars$mpg[mtcars$cyl == 6]

t.test(g4,g6,paired=FALSE,var.equal=TRUE)$conf.int
##[1]  3.154286 10.687272
##attr(,"conf.level")
##[1] 0.95

##The interval is above zero, suggesting 4 is better than 6 in the terms of MPG

##Q9
##(n_x-1)(S_x)^2+(n_y-1)(S_y)^2
##ns<-(n_x-1)+(n_y-1)
##sp <- sqrt(sp/ns)
##sqrt(sum(1/n_x+1/n_y))
##Using R function t.test(g2,g1,paired=FALSE,var.equal=TRUE)$conf

n_x<-9 ##Pill
n_y<-9 ##Placebo
df_x<-n_x-1
df_y<-n_y-1
sd_x <-1.5
sd_y<-1.8
var_x<-sd_x^2
var_y<-sd_y^2
pooledvar <- (df_x*var_x+df_y*var_y)/(df_x+df_y)

##***Quiz 3

#Q1
n<-9
mu<-1100
sd<-30

##95% T-confidence
mu + c(-1,1)*qt(.975,n-1)*sd/sqrt(n)
##[1] 1076.94 1123.06

##Q2
n<-9
mu<--2
mu<--qt(0.975, n-1)*sd/sqrt(n)
sd<--mu*sqrt(n)/qt(0.975, n-1)

##Q4
n_x<-10 ##Old
n_y<-10 ##New
df_x<-n_x-1
df_y<-n_y-1
var_x <-0.68
var_y<-0.6
sd_x<-sqrt(var_x)
sd_y<-sqrt(var_y)
mu_x<-5
mu_y<-3

##Consider y-x
df_yx <-df_x+df_y
var_yx <- (df_x*var_x+df_y*var_y)/(df_yx)
sp_yx<-sqrt(var_yx)

mu_y-mu_x+c(-1,1)*qt(0.975, df_yx)*sp_yx*sqrt(1/n_y+1/n_x)

##[1] -2.751649 -1.248351

##Q6
n_x<-100 ##Old
n_y<-100 ##New
df_x<-n_x-1
df_y<-n_y-1
sd_x<-2
sd_y<-0.5
mu_x<-6
mu_y<-4

var_x<-sd_x^2
var_y<-sd_y^2

##Consider y-x
        
df_num<-((var_x/n_x)+(var_y/n_y))^2  
df_den<- ((var_x/n_x)^2)*df_x+((var_y/n_y)^2)*df_y

df_yx<-df_num/df_den
se_yx<-sqrt((var_x/n_x)+(var_y/n_y))

##Y'-X' +/- t_df * SE
mu_x-mu_y+c(-1,1)*qt(0.975, df_yx)*se_yx

##Q7
n_x<-9##Placebo
n_y<-9 ##Pill
df_x<-n_x-1
df_y<-n_y-1

sd_x<-1.8
sd_y<-1.5
mu_x<-1
mu_y<--3

var_x=sd_x^2
var_y=sd_y^2

##Consider y-x
df_yx <-df_x+df_y
var_yx <- (df_x*var_x+df_y*var_y)/(df_yx)
sp_yx<-sqrt(var_yx)

mu_y-mu_x+c(-1,1)*qt(0.95, df_yx)*sp_yx*sqrt(1/n_y+1/n_x)
##[1] -5.363579 -2.636421

