
q1q2<-function(){
        
##Q1: Answer is 53
        
##Download the hosuing file from https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
        
housingurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
housinglocalfile <-"idahohousing.csv"

if (!file.exists(housinglocalfile)){
        
        download.file(housingurl, housinglocalfile)
        dateDownloaded<-date()
        
}

idahohousingdata <-read.csv(housinglocalfile, header=TRUE)

propertiesover1m<-subset(idahohousingdata, VAL==24, select=c(VAL,FES,SERIALNO))

print(nrow(propertiesover1m)) ##Anser is 5Q2
print(head(propertiesover1m,1))
##print(propertiesover1m)

##2 Answer is eavh variable should be in its own column. Here marital and employment status are fused

}

q3<-function(){
        
        ##Q3: Answer is 36534720
        
        ##Download the national gas program https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx 
        
        gasurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
        gaslocalfile <-"DATA.gov_NGAP.xlsx"
        
        if (!file.exists(gaslocalfile)){
                
                download.file(gasurl, gaslocalfile, mode='wb')
                dateDownloaded<-date()
                
        }
        
        ##nstall.packages("xlsx")
        
        library(xlsx)
                        
        dat <-read.xlsx(gaslocalfile, sheetIndex=1, colIndex=7:15, rowIndex=18:23, header=TRUE)
        
        print(sum(dat$Zip*dat$Ext,na.rm=T))
                      
        ##3 Answer is 36534720
        
}

q4<-function(){
        
        ##Q4: Answer is 127
        
        ##Download the Read the XML data on Baltimore restaurants from here: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml 
        
        restaurantsurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
        restaurantslocalfile <-"restaurants.xml"
        
        if (!file.exists(restaurantslocalfile)){
                
                download.file(restaurantsurl, restaurantslocalfile)
                dateDownloaded<-date()
                
        }
        
        ##nstall.packages("XML")
        library(XML)
                
        restaurantsdata <-xmlTreeParse(restaurantslocalfile, useInternal=TRUE)
        restaurantsrootnode <-xmlRoot(restaurantsdata)
        
        restaurantszipcodes <- xpathSApply(restaurantsrootnode, "//zipcode", xmlValue)       
        restaurantszip21231 <-restaurantszipcodes[restaurantszipcodes=="21231"]
        
        length(restaurantszip21231)
        
                
        ##4 Answer is 127
        
}

q5<-function(){
        
        ##Q5: Answer is 53
        
        ##Download the hosuing file from https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv 
        
        ##housingurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        
        housingurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
        housinglocalfile <-"idahohousing-new.csv"
        
        if (!file.exists(housinglocalfile)){
                
                download.file(housingurl, housinglocalfile)
                dateDownloaded<-date()
                
        }
        
        ##nstall.packages("data.table")
        library(data.table)
        
        ##DT <-fread(housinglocalfile, header=TRUE, sep=",", colClasses="numeric")
        DT <-fread(housinglocalfile, header=TRUE, sep=",", select=c("pwgtp15", "SEX"), colClasses=c(pwgtp15="numeric", SEX="numeric"))
        
        #1
        print("Ans 1: Calculated Separately")
        time1a<-system.time(mean(DT[DT$SEX==1,]$pwgtp15))
        time1b<-system.time(mean(DT[DT$SEX==2,]$pwgtp15))
        
        print(time1a)
        print(time1b)
        print(mean(DT[DT$SEX==1,]$pwgtp15))
        print(mean(DT[DT$SEX==2,]$pwgtp15))
        
        ##2
        print("Ans 2: This does not break down by SEX. Only gives 1 value. Incorrect Answer")
        time2<-system.time(mean(DT$pwgtp15,by=DT$SEX))
        print(time2)
        print(mean(DT$pwgtp15,by=DT$SEX))
        
        ##3
        print("Ans 3: Nicely broken down by SEX")
        time3<-system.time(DT[,mean(pwgtp15),by=SEX])
        print(time3)
        print(DT[,mean(pwgtp15),by=SEX])
        
        ##4
        print("Ans 4: Decntly broken down by SEX but seems slower than 3")
        time4<-system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
        print(time4)
        print(sapply(split(DT$pwgtp15,DT$SEX),mean))
        
        ##5
        print("Ans 5: Same as 4")
        time5<-system.time(tapply(DT$pwgtp15,DT$SEX,mean))
        print(time5)
        print(tapply(DT$pwgtp15,DT$SEX,mean))
        
        ##6
        print("Ans 6: This gives a long list! Incorrect Answer")
        time6a<-system.time(rowMeans(DT)[DT$SEX==1])
        time6b<-system.time(rowMeans(DT)[DT$SEX==2])
        print(time6a)
        print(time6b)
        ##print(rowMeans(DT)[DT$SEX==1])
        ##print(rowMeans(DT)[DT$SEX==2])
        
        ##5 Answer is eavh variable should be in its own column. Here marital and employment status are fused
        
}
