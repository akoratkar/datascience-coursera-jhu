##R-code to generate Plot 1 Histogram of Global Active Power

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


##1 Out the Histogram of Global Active Power 
png(file="plot1.png", width = 480, height = 480)
par(bg = "white")
par(mfcol=c(1,1))
hist(consumptiondataset$Global_active_power, xlab="Global Active Power (kilowatts)", 
     ylab="Frequency", main="Global Active Power", col="red")
dev.off()

print("Plo1 1 successfully generated in plot1.png")

