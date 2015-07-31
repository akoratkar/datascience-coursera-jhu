add2<-function(x,y){
        x+y
}

above<-function(x,n=10){
        x[x>n]
}

columnmean <-function(x){
        means<-numeric()
        for (i in 1:ncol(x)){
                means[i]<-mean(x[,i], na.rm=TRUE)
        }
        means
}
dt