
##Quiz 2

##Q1: Option 4: 2013-11-07T13:25:07Z

q1<-function(){

        ##Load JSon Lite
        library (jsonlite)
        
        ##URL for the API
        apiurl <-"https://api.github.com/users/jtleek/repos"
        
        ##Data sharing API JSON result
        datasharngJSON <-fromJSON(apiurl)
        
        ##subset for the datasharing repository and just get the date created.
        datasharngJSON <- subset(datasharngJSON, name=="datasharing", select=c(name, created_at))
        print(datasharngJSON)
        
        ##Answer to Q1 is
        ## created_at           name
        ## 2013-11-07T13:25:07Z datasharing

}

##Q1: Option 4: 2013-11-07T13:25:07Z


##Q2: Option 1:sqldf("select pwgtp1 from acs where AGEP < 50")
##Q3: Option 3: sqldf("select distinct AGEP from acsP")

q2q3<-function(){
        
        ##Load sqldf
        library (sqldf)
        
        ##Download the survey file from https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
        
        surveyurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
        surveylocalfile <-"survey.csv"
        
        if (!file.exists(surveylocalfile)){
                
                download.file(surveyurl, surveylocalfile)
                dateDownloaded<-date()
                
        }
        
        acs <-read.csv(surveylocalfile, header=TRUE)
        
        ##Q3 Option 1 is the answer
        ##Option 1
        print(sqldf("select pwgtp1 from acs where AGEP < 50"))
        
        ##Option 2
        ##print(sqldf("select * from acs where AGEP < 50 and pwgtp1"))
        
        ##Option 3
        ##print(sqldf("select * from acs where AGEP < 50"))
        
        ##Option 4
        ##print(sqldf("select pwgtp1 from acs"))  
        
        ##Q3 Option 3 is the answer
        
        ##sqldf("select unique AGEP from acs")
        ##sqldf("select AGEP where unique from acs")
        
        print(sqldf("select distinct AGEP from acsP"))
        
        ##sqldf("select unique * from acs")
        
}

##Q4: Option 6: 45 31 7 25

q4<-function(){
        
        target <-"http://biostat.jhsph.edu/~jleek/contact.html"
        connect <-url(target)
        
        html<-readLines(connect)
        ##10th, 20th, 30th and 100th
                
        print(nchar(html[10]))
        print(nchar(html[20]))
        print(nchar(html[30]))
        print(nchar(html[100]))
        
        ##Answer; 45 31 7 25
        close(connect)
        
}

##Q5: Option 2: 32426.7

q5<-function(){
        
              
        ##Download the file from https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for
        
        fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
        localfile <-"local.txt"
        
        if (!file.exists(localfile)){
                
                download.file(fileurl, localfile)
                dateDownloaded<-date()
                
        }
        
        df <-read.fwf(localfile,widths=c(-28, 4, -30), skip=4)
        
        ##Answer: 32426.7
        print(sum(df))
        
}