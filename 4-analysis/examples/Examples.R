
##---Start 1.Base Plotting System Examples------

library(datasets)

hist(airquality$Ozone) ##Histogram

with(airquality, plot(Wind, Ozone)) ##Scatter Plot

##airquality<-transform(airquality, Month=factor(Month))
boxplot(Ozone~Month, airquality, xlab="Month", ylab="Ozone") ##Box Plot

##Global Parameters
par("bg")
par("mar")
par("mfrow")

##Other base plotting functions
lines
points
text
title
mtext
axis

##Anotations
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City", type="n", pch=20))
with(subset(airquality, Month==5), points(Wind, Ozone, col="blue"))
with(subset(airquality, Month!=5), points(Wind, Ozone, col="red"))
legend("topright", pch=1, col=c("blue", "red"), legend=c("May", "Other"))
model<-lm(Ozone~Wind, airquality)
abline(model, lwd=2)


##Multiple plots
par(mfrow=c(1,2))
with (airquality, {
        plot(Wind, Ozone, main="Ozone and Wind")
        plot(Solar.R, Ozone, main="Ozone and Solar Radiation")
                        
        })

par(mfrow=c(1,3), mar=c(4,4,2,1), oma=c(0,0,2,0))
with (airquality, {
        plot(Wind, Ozone, main="Ozone and Wind")
        plot(Solar.R, Ozone, main="Ozone and Solar Radiation")
        plot(Temp, Ozone, main="Ozone and Temperature")
        mtext("Ozone and Weather in New York City", outer=TRUE)
        
})

example("points") ##Must see and remember to understandd all the options

x<-rnorm(100)
y<-rnorm(100)
z<-rnorm(100)

plot(x,y)
title("Scatter Plot")
text(-2,-2, "Label-2-2")
legend("topleft", legend="Data", pch=20)
fit<-lm(y~x)
abline(fit, lwd=3, col="blue")

plot(x,y, xlab="Weight", ylab="Height", main="Scatter Plot")

par(mar=c(4,4,2,2))
par(mfrow=c(2,2))

plot(x,y, pch=20)
plot(x,z, pch=20)
plot(z,x, pch=20)
plot(y,x, pch=20)

x<-rnorm(100)
y<-x+rnorm(100)
g<-gl(2,50, labels=c("Male", "Female"))
par(mfrow=c(1,1))
plot(x,y)

plot(x,y, type="n")
points(x[g=="Male"], y[g=="Male"], col="green")
points(x[g=="Female"], y[g=="Female"], col="blue")

windows() ## Graphics Device
dev.cur()
dev.set(1)

dev.copy(png, file="geyserplot.png")
dev.copy2pdf(file="geyserplot.pdf")

##Plotting to a PDF
pdf(file="myplot.pdf")
with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City", type="n", pch=20))
with(subset(airquality, Month==5), points(Wind, Ozone, col="blue"))
with(subset(airquality, Month!=5), points(Wind, Ozone, col="red"))
legend("topright", pch=1, col=c("blue", "red"), legend=c("May", "Other"))
model<-lm(Ozone~Wind, airquality)
abline(model, lwd=2)
dev.off()

##---End 1.Base Plotting System Examples------

