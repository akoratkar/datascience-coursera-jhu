##R-code to generate Plot #4 All the plots together

##Load the required libraries

library(dplyr) ##Required to mutate
library(lubridate) ##Required for date manipulation


print("Downloading and loading data (may take a few moments) ...") ##Indicate start of the program

##Download and unzip the data file
zippeddataURL<-("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")

##Download the Zip File only if it is not already there and Unzip. 
##Skip if file already downloaded
zippeddatafile <-"household_power_consumption.zip"

if (!file.exists(zippeddatafile)){
        
        download.file(zippeddataURL, zippeddatafile)
        dateDownloaded<-date()
        
        ##Unzip the downloaded file
        unzip(zippeddatafile)
        
}

##Check if file exists post zip. Full-proof mechanism
filename<-"household_power_consumption.txt"

if (!file.exists(filename))
        error("Household Power Consumption data not found!")

##Now read the file in
consumptiondataset <-read.table(filename, header=TRUE, sep=";", na.strings="?", 
                                stringsAsFactors=FALSE,
                                colClasses=c("character","character","numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric")
)


##Mutate the dataframe to convert Date into Date format and Time into POSIXct format
consumptiondataset<-mutate(consumptiondataset, Date=as.Date(dmy(Date)), Time=ymd_hms(paste(Date, Time)))

##Subset the data frame for the required range
consumptiondataset<-subset(consumptiondataset, Date>=as.Date("2007-02-01") & Date<=as.Date("2007-02-02"), na.rm=TRUE)

##OK, we have the data ready for plotting
print(paste("Size of the data set for plotting:", nrow(consumptiondataset)))

##4 All the plots together
png(file="plot4.png", width = 480, height = 480)
par(bg = "white")
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

print("Plo1 4 successfully generated in plot4.png")
