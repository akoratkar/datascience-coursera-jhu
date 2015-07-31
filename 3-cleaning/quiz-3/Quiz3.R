
q1<-function(){
        
        ##Q1: Answer is 1: 125, 238, 262
        
        ##Download the hosuing file from https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
        
        housingurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        housinglocalfile <-"idahohousing.csv"
        
        if (!file.exists(housinglocalfile)){
                
                download.file(housingurl, housinglocalfile)
                dateDownloaded<-date()
                
        }
        
        idahohousingdata <-read.csv(housinglocalfile, header=TRUE)
        
        ##SERIALNO: Housing Unit #
        ##ACR: Lot Size; 3 is > 10 acres
        ##AGS: Agriculural Goods: 6 is > $10K
        
        agricultureLogical<-(idahohousingdata$ACR==3 & idahohousingdata$AGS==6)
                
        selecthouseholds <-idahohousingdata[which(agricultureLogical==TRUE),]
        
        firstthree<-head(selecthouseholds, 3)

        print(firstthree) ##Answer is 1: 125, 238, 262
                
        ##selecthouseholds<-subset(idahohousingdata, ACR==3 & AGS==6, select=c(SERIALNO,ACR,AGS))
                        
        ## Answer is 1: 125, 238, 262
        
}

q2<-function(){
        
        ##Q2:  Answer is 1 -15259150 -10575416 
        
        ##Load the JPEG library
        library(jpeg)
        
        ##Download the image https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg  
        
        photourl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
        photolocalfile <-"jeff.jpg"
        
        if (!file.exists(photolocalfile)){
                
                download.file(photourl, photolocalfile, mode='wb')
                dateDownloaded<-date()
                
        }
                        
        jeffphoto<-readJPEG(photolocalfile, native=TRUE)
        
        ##30th and 80th percentile     
        print(quantile(jeffphoto, probs=c(0.3, 0.8)))
        
        ##[1] "nativeRaster"
        ##30%       80% 
        ##-15259150 -10575416 
        
        ## Answer is 1 -15259150 -10575416 
}

q3q4q5<-function(){
        
        ##Q3: Answer is 1 189 Matches, KNA Rank 178
        ##Q4: Answer is 4: 32.96667 91.91304
        ##Q5 Answer is 4: 5
        
        ##Download Gross Domestic Product data for the 190 ranked countries in this data set
        
        ##Let use dplyr and Hmisclibrary for fun
        library(dplyr)
        
        library(Hmisc)
        
        gdpurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        gdplocalfile <-"gdp.csv"
        
        if (!file.exists(gdplocalfile)){
                
                download.file(gdpurl, gdplocalfile)
                dateDownloaded<-date()
                
        }
        
        gdpdata <-read.csv(gdplocalfile, header=FALSE, na.strings=c("..", ""), stringsAsFactors=FALSE)
                
        ##This data is really untidy given it is in .csv format. Lets clean it up
        ##Select only the country rows and the four relevant columns
        gdpdata <-gdpdata[6:195,c(1,2,4,5)]
        
        ##Give meaningful names to the columsn and eliminate rows without the country code (empty rows)
        gdpdata<-rename(gdpdata, CountryCode=V1, Rank=V2, "Country Name"=V4, GDP=V5)
        gdpdata<-subset(gdpdata, !is.na(CountryCode)||is.na(Rank))
                        
        gdpdata<-mutate(gdpdata, Rank=as.numeric(Rank))
        
        print("Rows in original GDP file with valid country codes")
        print(nrow(gdpdata))
        print(head(gdpdata,1))
        print(tail(gdpdata,1))
                              
        ##Download Educational Data
        
        eduurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
        edulocalfile <-"edu.csv"
        
        if (!file.exists(edulocalfile)){
                
                download.file(eduurl, edulocalfile)
                dateDownloaded<-date()
                
        }
        
        edudata <-read.csv(edulocalfile, header=TRUE, na.strings=c(""), stringsAsFactors=FALSE)
        edudata<-subset(edudata, !is.na(CountryCode))
        
        print("Rows in original education file with valid country codes")
        print(nrow(edudata))
        print(head(edudata[,1],1))
        print(tail(edudata[,1],1))
                
        ##Merge the GDP and Education data
                             
        edugdpdata<-merge(gdpdata, edudata, by.x="CountryCode", by.y="CountryCode")
        
        print("Rows in merged GDP and Education file with valid country codes")
        print(nrow(edugdpdata))
        print(head(edugdpdata[,1],1))
        print(tail(edugdpdata[,1],1))
       
        edugdpdata<-arrange(edugdpdata, desc(Rank))
        print(head(edugdpdata[,c(1,2,3,4,6)]))
        
        ##Q3 Answer #1: 189 Matches, KNA Rank 178
        
        ##Group data by Income Group
        incomegroupdata <-group_by(edugdpdata, Income.Group)
                
        ##print(incomegroupdata[,c(1,2,3,4,6)])
        print(summarise(incomegroupdata, MeanRank=mean(Rank, na.rm=TRUE)))
        
        ##Income.Group  MeanRank
        ##1    High income: OECD  32.96667 *
        ##2 High income: nonOECD  91.91304 *
        ##3           Low income 133.72973
        ##4  Lower middle income 107.70370
        ##5  Upper middle income  92.13333
        
        ##Q4Answer 4: 32.96667 91.91304
        
        edugdpdata$RankGroups=cut2(edugdpdata$Rank, g=5)
        print(table(edugdpdata$Income.Group, edugdpdata$RankGroups))
        
        ##[  1, 39) [ 39, 77) [ 77,115) [115,154) [154,190]
        ##High income: nonOECD         4         5         8         5         1
        ##High income: OECD           18        10         1         1         0
        ##Low income                   0         1         9        16        11
        ##Lower middle income          5        13        12         8        16
        ##Upper middle income         11         9         8         8         9
        
        ##Q5 Answer 4: 5
        
}
