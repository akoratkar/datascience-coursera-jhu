
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

## --- Start 2. Lattice Plotting System Examples ----

library(lattice)
library(datasets)
airquality<-transform(airquality, Month=factor(Month))
xyplot(Ozone~Wind|Month, data=airquality, layout=c(5,1))


xyplot (y~x|f, panel=function(x,y, ...){
        
        panel.xyplot(x,y,...)
        panel.abline(x,y)
        panel.lmline(x,y,col=2)
        
})
## --- End 2. Lattice Plotting System Examples ----

## --- Start 3. gglot2 Plotting System Examples ----
library(gglot2)
library(datasets)
str(mpg)

qplot(displ, hwy, data=mpg)

##Aesthetics
qplot(displ, hwy, data=mpg, color=drv)

##Geom
qplot(displ, hwy, data=mpg, geom=c("point", "smooth"))

##Histogram
qplot(hwy, data=mpg,fill=drv)

##Facets
qplot(displ, hwy, data=mpg, facets=.~drv)
qplot(hwy, data=mpg, facets=drv~., binwidth=2)

qplot(log(hwy), data=mpg, geom="density", color=drv)
qplot(displ, hwy, data=mpg, shape=drv, color=drv)
qplot(displ, hwy, data=mpg, shape=drv, color=drv, geom=c("point", "smooth"), method="lm")
qplot(displ, hwy, data=mpg, shape=drv, color=drv, geom=c("point", "smooth"), method="lm", facets=.~drv)

## --- End 3. gglot2 Plotting System Examples ----

##---- Start Clustering

##Histogram
set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

##Dendogram

dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)

##Prettier cluster
myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)),
                      hang = 0.1, ...) {
        ## modifiction of plclust for plotting hclust objects *in colour*! Copyright
        ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
        ## of labels of the leaves of the tree lab.col: colour for the labels;
        ## NA=default device foreground colour hang: as in hclust & plclust Side
        ## effect: A display of hierarchical cluster with coloured leaf labels.
        y <- rep(hclust$height, 2)
        x <- as.numeric(hclust$merge)
        y <- y[which(x < 0)]
        x <- x[which(x < 0)]
        x <- abs(x)
        y <- y[order(x)]
        x <- x[order(x)]
        plot(hclust, labels = FALSE, hang = hang, ...)
        text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
             col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}


##Heatmap
dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)

##K-means cluster
dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)

kmeansObj$cluster

par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")

##PCA and SVD
set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

par(mar = rep(0.2, 4))
heatmap(dataMatrix)

##Add pattern
set.seed(678910)
for (i in 1:40) {
        # flip a coin
        coinFlip <- rbinom(1, size = 1, prob = 0.5)
        # if coin is heads add a common pattern to that row
        if (coinFlip) {
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
        }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector",
     pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)


##variance
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained",
     pch = 19)

svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1",
     ylab = "Right Singular Vector 1")
abline(c(0, 1))
12





constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")


##Colors in R
heat.colors()
topo.colors()

colors()

pal <- colorRamp(c("red", "blue"))
pal <- colorRampPalette(c("red", "yellow"))

##RColorBrewer Package
library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

## ---- End Clustering
