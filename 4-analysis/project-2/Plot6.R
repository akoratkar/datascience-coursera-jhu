##-->Q6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los
##Angeles County, California ( fips == "06037" ). Which city has seen greater changes over time in motor vehicle
##emissions?

##-->Answer to Q6
##-->So Baltimore City has seen greater changes (~75%) in Motor Vehicles related PM2.5 Emissions 
##than that in Los Angeles County  from 1999 to 2008. This is apparent from the steeper gradient too.

##>source("Plot6.R")
##
##[1] "Downloading and reading data (may take a few moments) ..."
##[1] "Data files successfully downloaded and read"
##[1] "Now, generating Plot 6 to Plot6.png .."
##[1] "Plot 6 successfully generated to Plot6.png"
##[1] "So Baltimore City has seen greater changes (~75%) in Motor Vehicles related PM2.5 Emissions \n              than that in Los Angeles County  from 1999 to 2008"
##[1] "Program successfully executed"
##>

##Load required libraries
library(dplyr)
library(reshape2)
##library(ggplot2) ->Tried ggplot2 qplot first but I liked the base plot clarity to answer the question

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

print("Now, generating Plot 6 to Plot6.png ..")

##Retrieve motor vehicle related sources. Look for Sources beginning in Mobile
##and ending in Vehicle in the EI.Sector variable. This give us 1138 sources that match following
##three EI.Sectors
##as.data.frame(unique(motorvehiclesources[,"EI.Sector"], na.rm=TRUE))
##1            Mobile - On-Road Gasoline Light Duty Vehicles
##2            Mobile - On-Road Gasoline Heavy Duty Vehicles
##3            Mobile - On-Road Diesel Light Duty Vehicles
##4            Mobile - On-Road Diesel Heavy Duty Vehicles

motorvehiclesources <-subset(SCC[grep("^mobile -(.*)vehicles$", SCC$EI.Sector, ignore.case=T),],select=c(SCC,EI.Sector))

##First subset the data for Baltimore City + LA County. 
##This produces 11416 observations; Baltimore 2096 and LA County 9320
NEI<-subset(NEI, fips %in% c("24510","06037"))

##Substituting the county codes with proper names so that city names show in graph
NEI$fips[NEI$fips == "24510"] <- "Baltimore City"
NEI$fips[NEI$fips == "06037"] <- "Los Angeles County"

##Merge the NEI for Baltimore + LA County and the motor vehicle sources dataset by SCC. This will give us only
##the motor vehicle data sources. This give us 1138 observations as all motor vehicle sources 
##have observations for either Baltimore City or LA County

mergedNEI<-merge(NEI, motorvehiclesources, by="SCC")

##Then the data set with year as ID.
meltedNEI <- melt(mergedNEI, id.vars = c("year", "fips"), measure.vars=c("Emissions"))

##Now cast the data set with year as IDs and calculate the sum
castedNEI <- dcast(meltedNEI, year+fips~variable,sum)

##Prep for generating Plot 6
png(file="Plot6.png", width = 960, height = 640)

##Since the data is only for PM25-PRI and for years 1999, 2002, 2005 and 2008
##and we are looking at emissions from motor vehicle sources in Baltimore and LA, no subsettting is required

##Note --- Tried ggplot2 qplot first but I liked the base plot clarity to answer the question

##par(mfcol=c(1,1))
##par(mar=c(6,6,6,6))

##print(qplot(year, round(Emissions), data=castedNEI, color=fips,  
##geom=c("point", "smooth"), method="lm", facets=.~fips, 
##main="Plot 6: Motor Vehicle PM2.5 Emissions in Baltimore City & Los Angeles County from 1999 to 2008 ",
##xlab="Year", ylab="Motor Vehicle PM2.5 Emissions")              
##)

par(mfcol=c(1,2))
par(mar=c(6,6,6,6))

##Base Plot for Baltimore City
with(subset(castedNEI, fips=="Baltimore City"), {plot(year, round(Emissions), type="n", main="Plot 6a: Motor Vehicle PM2.5 Emissions for Baltimore City",
                                                      xlab="Year",
                                                      ylab="Motor Vehicle PM2.5 Emissions", yaxt="n", xaxt="n", xlim=c(1998, 2010))
                                                 
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

##Base Plot for Los Angeles County
with(subset(castedNEI, fips=="Los Angeles County"), {plot(year, round(Emissions), type="n", main="Plot 6b: Motor Vehicle PM2.5 Emissions for Los Angeles County",
                                                          xlab="Year",
                                                          ylab="Motor Vehicle PM2.5 Emissions", yaxt="n", xaxt="n", xlim=c(1998, 2010))
                                                     
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

print("Plot 6 successfully generated to Plot6.png")
print("So Baltimore City has seen greater changes (~75%) in Motor Vehicles related PM2.5 Emissions 
              than that in Los Angeles County  from 1999 to 2008")
print("Program successfully executed")
