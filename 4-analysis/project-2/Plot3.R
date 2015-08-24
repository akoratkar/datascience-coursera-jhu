##-->Q3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
##which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
##Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

##-->Answer to Q3
##-->The Total PM2.5 Emissions in Baltimore City decreased from 1999 to 2008, though it had increased in 2005, but
##-->The Total PM2.5 Emissions of type NON-ROAD in Baltimore City decreased from 1999 to 2008
##-->The Total PM2.5 Emissions of type NONPOINT in Baltimore City decreased from 1999 to 2008
##-->The Total PM2.5 Emissions of type ON-ROAD in Baltimore City decreased from 1999 to 2008
##-->The Total PM2.5 Emissions of type POINT in Baltimore City increased from 1999 to 2008


##>source("Plot3.R")
##
##[1] "Downloading and reading data (may take a few moments) ..."
##[1] "Data files successfully downloaded and read"
##[1] "Now, generating Plot 3 to Plot3.png .."
##[1] "Plo1 3 successfully generated to Plot3.png"
##[1] "So yes, the Total PM2.5 Emissions in Baltimore City decreased from 1999 to 2008, though it had increased in 2005"
##[1] "Total PM2.5 Emissions of type NON-ROAD in Baltimore City decreased from 1999 to 2008"
##[1] "Total PM2.5 Emissions of type NONPOINT in Baltimore City decreased from 1999 to 2008"
##[1] "Total PM2.5 Emissions of type ON-ROAD in Baltimore City decreased from 1999 to 2008"
##[1] "Total PM2.5 Emissions of type POINT in Baltimore City increased from 1999 to 2008"
##[1] "Program successfully executed"
##>

##Load required libraries
library(dplyr)
library(reshape2)
library(ggplot2) ##Only being used in Q3

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

print("Now, generating Plot 3 to Plot3.png ..")

##First subset the date for Baltimore City
NEI<-subset(NEI, fips=="24510")

##Then melt the data set with year as ID.
meltedNEI <- melt(NEI, id.vars = c("year", "type"), measure.vars=c("Emissions"))

##Now cast the data set with year as IDs and calculate the sum
castedNEI <- dcast(meltedNEI, year+type~variable,sum)

##Prep for generating plot3
png(file="Plot3.png", width = 960, height = 640)
par(mfcol=c(1,1))
par(mar=c(6,6,6,6))

##Since the data is only for PM25-PRI and for years 1999, 2002, 2005 and 2008
##and we are looking at emissions from all sources for Baltimore City, no more subsettting is required

print(qplot(year, round(Emissions), data=castedNEI, color=type,  
            geom=c("point", "smooth"), method="lm", facets=.~type, 
            main="Plot 3: Total PM2.5 Emissions in Baltimore City from 1999 to 2008 ",
            xlab="Year", ylab="Total PM2.5 Emissions")              
)


dev.off()

print("Plo1 3 successfully generated to Plot3.png")
print("So yes, the Total PM2.5 Emissions in Baltimore City decreased from 1999 to 2008, though it had increased in 2005")
print("Total PM2.5 Emissions of type NON-ROAD in Baltimore City decreased from 1999 to 2008")
print("Total PM2.5 Emissions of type NONPOINT in Baltimore City decreased from 1999 to 2008")
print("Total PM2.5 Emissions of type ON-ROAD in Baltimore City decreased from 1999 to 2008")
print("Total PM2.5 Emissions of type POINT in Baltimore City increased from 1999 to 2008")
print("Program successfully executed")
