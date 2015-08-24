##-->Q1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##Using the base plotting system make a plot showing the total PM2.5 emission from all sources 
##for each of the years 1999, 2002, 2005, and 2008.

##-->Answer to Q1
##-->YES,The Total PM2.5 Emissions for United States decreased from 1999 to 2008

##>source("Plot1.R")
##
##[1] "Downloading and reading data (may take a few moments) ..."
##[1] "Data files successfully downloaded and read"
##[1] "Now, generating Plo1 1 to Plot1.png .."
##[1] "Plo1 1 successfully generated to Plot1.png"
##[1] "So yes, the Total PM2.5 Emissions for United States decreased from 1999 to 2008"
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

print("Now, generating Plo1 1 to Plot1.png ..")

##First melt the data set with year as ID.
meltedNEI <- melt(NEI, id.vars = c("year"), measure.vars=c("Emissions"))

##Now cast the data set with year as IDs and calculate the sum
castedNEI <- dcast(meltedNEI, year~variable,sum)

##Prep for plotting plot1
png(file="Plot1.png", width = 640, height = 640)
par(mfcol=c(1,1))
par(mar=c(6,6,6,6))

##Since the data is only for PM25-PRI and for years 1999, 2002, 2005 and 2008
##and we are looking at emissions from all sources, no subsettting is required
with(castedNEI, {plot(year, round(Emissions), type="n", main="Plot 1: Total PM2.5 Emissions for US from 1999 to 2008",
                      xlab="Year",
                      ylab="Total PM 2.5 Emissions", yaxt="n", xaxt="n", xlim=c(1998, 2010))
                 
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

print("Plo1 1 successfully generated to Plot1.png")
print("So yes, the Total PM2.5 Emissions for United States decreased from 1999 to 2008")
print("Program successfully executed")

