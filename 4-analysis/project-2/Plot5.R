##-->Q5:How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

##-->Answer to Q5
##--> The Motor Vehicles related PM2.5 Emissions decreased in the Baltimore City from 1999 to 2008

##>source("Plot5.R")
##
##[1] "Downloading and reading data (may take a few moments) ..."
##[1] "Data files successfully downloaded and read"
##[1] "Now, generating Plot 5 to Plot5.png .."
##[1] "Plo1 5 successfully generated to Plot5.png"
##[1] "So the Motor Vehicles related PM2.5 Emissions decreased in the Baltimore City from 1999 to 2008"
##[1] "Program successfully executed"
##>

##Load required libraries
library(dplyr)
library(reshape2)

print("Downloading and reading data (may take a few moments) ...")
##Download and unzip the data file from the given URL
zippeddataURL<-("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip")

##Download the Zip File only if it is not already there. Unzip
zippeddatafile <-"NEI_data.zip"

if (!file.exists(zippeddatafile)){
        
        download.file(zippeddataURL, zippeddatafile)
        dateDownloaded<-date()
        
        ##Unzip the downloaded file
        unzip(zippeddatafile)
        
}

##Check if the NEI PM2.5 emissions data file exists
filename<-"summarySCC_PM25.rds"
if (!file.exists(filename))
        error("File summarySCC_PM25.rds not found!")

##Read the entire NEI dataset. This produces 6497651 observatios
NEI <- readRDS(filename)

##Check if source classification code data file exists
filename<-"Source_Classification_Code.rds"
if (!file.exists(filename))
        error("File Source_Classification_Code.rds not found!")

##Read the entire SCC dataset. This produces 11717 observatios
SCC <- readRDS(filename)

print("Data files successfully downloaded and read")

##Using the base plotting system make a plot showing the total PM2.5 emission from all sources for fips=24510 
##for each of the years 1999, 2002, 2005, and 2008.

print("Now, generating Plot 5 to Plot5.png ..")

##Retrieve motor vehicle related sources. Look for Sources beginning in Mobile
##and ending in Vehicle in the EI.Sector variable. This give us 1138 sources that match following
##three EI.Sectors
##as.data.frame(unique(motorvehiclesources[,"EI.Sector"], na.rm=TRUE))
##1            Mobile - On-Road Gasoline Light Duty Vehicles
##2            Mobile - On-Road Gasoline Heavy Duty Vehicles
##3              Mobile - On-Road Diesel Light Duty Vehicles
##4              Mobile - On-Road Diesel Heavy Duty Vehicles

motorvehiclesources <-subset(SCC[grep("^mobile -(.*)vehicles$", SCC$EI.Sector, ignore.case=T),],select=c(SCC,EI.Sector))

##First subset the date for Baltimore. This produces 2096 observations
NEI<-subset(NEI, fips=="24510")

##Merge the NEI for Baltimore and the motor vehicle sources dataset by SCC. This will give us only
##the motor vehicle data sources. This give us 1119 observations as 19 motor vehicle sources 
##do not have observations for Baltimore City

mergedNEI<-merge(NEI, motorvehiclesources, by="SCC")

##Then the data set with year as ID.
meltedNEI <- melt(mergedNEI, id.vars = c("year"), measure.vars=c("Emissions"))

##Now cast the data set with year as IDs and calculate the sum
castedNEI <- dcast(meltedNEI, year~variable,sum)

##Prep to generate Plot 5
png(file="Plot5.png", width = 640, height = 640)
par(mfcol=c(1,1))
par(mar=c(6,6,6,6))

##Since the data is only for PM25-PRI and for years 1999, 2002, 2005 and 2008
##and we are looking at emissions from motor vehicle sources for Baltimore City, no more subsettting is required

with(castedNEI, {plot(year, round(Emissions), type="n", main="Plot 5: Motor Vehicles PM2.5 Emissions for Baltimore City from 1999 to 2008",
                      xlab="Year",
                      ylab="Motor Vehicles PM 2.5 Emissions", yaxt="n", xaxt="n", xlim=c(1998, 2010))
                 
                 ##Draw points in blue
                 points(year, round(Emissions), col="blue", pch=20)
                 
                 ##Draw dotted lines in blues to show trend clearly
                 lines(year, round(Emissions), type="b", pch=22, col="blue", lty=2)
                 
                 ##Add text to the data points in grey to clearly indicate the values
                 text(year, round(Emissions), paste0("(", year, ",", round(Emissions), ")"), cex=0.75, pos=4, col="grey")
                 
                 ##Draw custom axis so that the labels show clearly
                 axis(4, at=round(Emissions),labels=round(Emissions), col.axis="black", las=2, cex=0.75)
                 axis(1, at=year,labels=year, col.axis="black", las=0, cex=0.75)
                 
                 ##Show the trend line in red
                 abline(lm(round(Emissions)~year), lwd=1, col="red")
                 
                 
})

dev.off()

print("Plo1 5 successfully generated to Plot5.png")
print("So the Motor Vehicles related PM2.5 Emissions decreased in the Baltimore City from 1999 to 2008")
print("Program successfully executed")
