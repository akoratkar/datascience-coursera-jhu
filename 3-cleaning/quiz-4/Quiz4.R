
q1<-function(){
        
        ##Q1:  Answer is 4: [1] ""   "15"
        
        ##Download the file https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv 
        
        housingurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv "
        housinglocalfile <-"idahohousing.csv"
        
        if (!file.exists(housinglocalfile)){
                
                download.file(housingurl, housinglocalfile)
                dateDownloaded<-date()
                
        }
        
        idahohousingdata <-read.csv(housinglocalfile, header=TRUE)
        
        splitnamesbywgtp<-strsplit(names(idahohousingdata), "wgtp") 
                
        print(splitnamesbywgtp[123])
        ##Answer is 4: [1] ""   "15"
                
}

q2q3<-function(){
        
        ##Q2: Answer is 3: [1] 377652.4
        ##Q3: Answer is 2: [1] 180 181 182
        
        library(dplyr)
        ##library(reshape2)
        
        ##Download Gross Domestic Product data for the 190 ranked countries in this data set
        
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
        
        print(head(gdpdata, 1))
        ##Substitude "," with blank
        
        gdpdata <-mutate(gdpdata, GDP=as.numeric(gsub(",", "", gdpdata$GDP)))
        print(head(gdpdata, 1))
        
        print(mean(gdpdata$GDP))
        ##Answer Q2 is 3: [1] 377652.4
        
        countryNames<-sort(gdpdata[, "Country Name"])
        print(tail(countryNames,15))
        
        ##print(grep("*United",countryNames))
        print(grep("^United",countryNames))
        ####print(grep("United$",countryNames))
        ##print(grep("*United",countryNames))
        
        ##Q3: Answer is 2: [1] 180 181 182
}

q4<-function(){
        
        ##Q4: Answer 4: 2 13       
        ##Download Gross Domestic Product data for the 190 ranked countries in this data set
        
        ##Let use dplyr and Hmisclibrary for fun
        library(dplyr)
        
        library(Hmisc)
        
        gdpurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        gdplocalfile <-"gdp4.csv"
        
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
        
        gdpdata<-subset(gdpdata, !is.na(CountryCode))
        
        ##gdpdata<-mutate(gdpdata, Rank=as.numeric(Rank))
        
        print("Rows in original GDP file with valid country codes")
        print(nrow(gdpdata))
        print(head(gdpdata,1))
                
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
        
                
        ##Merge the GDP and Education data
        
        edugdpdata<-merge(gdpdata, edudata, by.x="CountryCode", by.y="CountryCode")
        
        print("Rows in merged GDP and Education file with valid country codes")
        print(nrow(edugdpdata))
        ##print(names(edudata))
        
        countrywithfiscalyear<-grep("^Fiscal year end: June", edugdpdata[, "Special.Notes"])
        ##print(count(countrywithfiscalyear))
        print(countrywithfiscalyear)
        print(length(countrywithfiscalyear))
                
        ##Q5 Answer 4: 2 13
        ##[1]   9  16  29  51  65  89  96 133 140 152 159 175 189
        ##[1] 13
        
}

q5<-function(){
        
        ##Q5: Answer 3: 240, 47      
        library(quantmod)
        
        amzn = getSymbols("AMZN",auto.assign=FALSE)
        sampleTimes = index(amzn)
        
        dates<-(format(sampleTimes, "%A %Y"))
        
        in2012<-grep("2012$",dates)
        print(length(in2012))

        OnMonday2012<-grep("^Monday 2012",dates)
        print(length(OnMonday2012))
        
        ##Answer is 3: 240, 47
        ##[1] 250
        ##[1] 47
        
}