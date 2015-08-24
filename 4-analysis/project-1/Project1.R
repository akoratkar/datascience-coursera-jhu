##Combined Code for generating the 4 plots

run<-function(){
        
        library(sqldf)
        library(dplyr)
        library(lubridate)
        
                
        print("Program executing ...")
        ##Download and unzip the data file
        zippeddataURL<-("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
        
        ##Download the Zip File only if it is not already there. Unzip
        zippeddatafile <-"household_power_consumption.zip"
        
        if (!file.exists(zippeddatafile)){
                
                download.file(zippeddataURL, zippeddatafile)
                dateDownloaded<-date()
                
                ##Unzip the downloaded file
                unzip(zippeddatafile)
                
        }
        
        ##Check if file exists
        filename<-"household_power_consumption.txt"
        
        if (!file.exists(filename))
                error("Household Power Consumption data not found!")
        
      
        ##consumptiondataset <-read.table(filename, header=TRUE, sep=";", na.strings="?", 
                                        ##stringsAsFactors=FALSE,
                                        ##colClasses=c("character","character","numeric",
                                                    ## "numeric", "numeric", "numeric",
                                                     ##"numeric", "numeric", "numeric")
                                        ##)
             
        ##Trying to optimize with sqldf, but the where clause is not working
        selectstmt<-"select * from file where Date in ('1/2/2007','2/2/2007')"
        
        consumptiondataset<-read.csv.sql(filename, sql=selectstmt, header=TRUE, sep = ";",
                                         colClasses=c("character","character","numeric",
                                                      "numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric"),
                                         dbname=tempfile(), drv="SQLite")
        
        
        ##Mutate to convert Date into Date and Time into POSIXct
        
        consumptiondataset<-mutate(consumptiondataset, Date=as.Date(dmy(Date)), Time=ymd_hms(paste(Date, Time)))
        ##consumptiondataset<-subset(consumptiondataset, Date>=as.Date("2007-02-01") & Date<=as.Date("2007-02-02"), na.rm=TRUE)
        
       
        print(str(consumptiondataset))
        print(paste("Size of the data set for plotting", nrow(consumptiondataset)))
        

        ##1 Histogram of Global Active Power
        png(file="plot1.png", width = 480, height = 480)
        par(mfcol=c(1,1))
        hist(consumptiondataset$Global_active_power, xlab="Global Active Power (kilowatts)", 
                ylab="Frequency", main="Global Active Power", col="red")
        
        dev.off()
        print("Plo1 1 successfully generated in Plot1.png")
        
        ##2 Global Active Power By Day
        
        png(file="plot2.png", width = 480, height = 480)
        par(mfcol=c(1,1))
        with(consumptiondataset, plot(Time, Global_active_power, type="l", 
                        xlab="",
                        ylab="Global Active Power (kilowatts)"))
        
             
        dev.off()
        print("Plo1 2 successfully generated in Plot2.png")
        
        ##3 Energy Submetering By Day
        par(mfcol=c(1,1))
        png(file="plot3.png", width = 480, height = 480)
        
        with(consumptiondataset,{
             
             plot(Time, Sub_metering_1, type="n",
                xlab="",
                ylab="Energy sub metering"
             )
             
             lines(Time, Sub_metering_1, col="black")
             lines(Time, Sub_metering_2, col="red")
             lines(Time, Sub_metering_3, col="blue")
             
             legend("topright", lty=1, col=c("black", "red", "blue"),lwd=3,
                    legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
             
        })
        
        dev.off()
        print("Plo1 3 successfully generated in Plot3.png")
        
        ##4 All the plots together
        png(file="plot4.png", width = 480, height = 480)
        par(mar=c(4,4,2,2))
        par(mfcol=c(2,2))
        
        with(consumptiondataset,{
                
                plot(Time, Global_active_power, type="l", 
                     xlab="",
                     ylab="Global Active Power")
                
                plot(Time, Sub_metering_1, type="n",
                     xlab="",
                     ylab="Energy sub metering")
                
                legend("topright", lty=1, col=c("black", "red", "blue"), bty="n",
                       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
                
                lines(Time, Sub_metering_1, col="black")
                lines(Time, Sub_metering_2, col="red")
                lines(Time, Sub_metering_3, col="blue")
                
                plot(Time, Voltage, type="l", col="black",
                     xlab="datetime",
                     ylab="Voltage")
                
                plot(Time, Global_reactive_power, type="l", col="black",
                     xlab="datetime",
                     ylab="Global_reactive_power")
                
        })
        
        dev.off()

        print("Plo1 4 successfully generated in Plot4.png")
        print('...Program successfully executed')
        
}


        