##Combined Code for generating the 4 plots

run<-function(q){
        
        library(dplyr)
        library(reshape2)
        library(ggplot2)
        
        print("Downloading and reading data (may take a few moments) ...")
        ##Download and unzip the data file
        zippeddataURL<-("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip")
        
        ##Download the Zip File only if it is not already there. Unzip
        zippeddatafile <-"NEI_data.zip"
        
        if (!file.exists(zippeddatafile)){
                
                download.file(zippeddataURL, zippeddatafile)
                dateDownloaded<-date()
                
                ##Unzip the downloaded file
                unzip(zippeddatafile)
                
        }
        
        ##Check if file exists
        filename<-"summarySCC_PM25.rds"
        if (!file.exists(filename))
                error("File summarySCC_PM25.rds not found!")
        
        ##Read the entire NEI dataset. This produces 6497651 observatios
        NEI <- readRDS(filename)
        
         
        ##Check if file exists
        filename<-"Source_Classification_Code.rds"
        if (!file.exists(filename))
                error("File Source_Classification_Code.rds not found!")
        
        ##Read the entire SCC dataset. This produces 11717 observatios
        SCC <- readRDS(filename)
                        
        if (q==1) q1(NEI)
        if (q==2) q2(NEI)
        if (q==3) q3(NEI)
        if (q==4) q4(NEI, SCC)
        if (q==5) q5(NEI, SCC)
        if (q==6) q6(NEI, SCC)
        
        print("Program successfully executed")
}

q1<-function (NEI){
        
##Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##Using the base plotting system make a plot showing the total PM2.5 emission from all sources 
##for each of the years 1999, 2002, 2005, and 2008.
        
        ##Answer
        ##-->YES,The Total PM2.5 Emissions for United States decreased from 1999 to 2008
        
        print("Generating Plo1 1 to Plot1.png ..")
        
        ##First melt the data set with year as ID.
        meltedNEI <- melt(NEI, id.vars = c("year"), measure.vars=c("Emissions"))
        
        ##Now cast the data set with year as IDs and calculate the sum
        castedNEI <- dcast(meltedNEI, year~variable,sum)
        
        ##Prep for plotting plot1
        png(file="plot1.png", width = 640, height = 640)
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
        
}

q2<-function (NEI){
        
        ##Have total emissions from PM2.5 decreased in the Baltimore City, Maryland ( fips == "24510" )from 1999 to 2008?
        ##Use the base plotting system to make a plot answering this question.
        
        ##Using the base plotting system make a plot showing the total PM2.5 emission from all sources for fips=24510 
        ##for each of the years 1999, 2002, 2005, and 2008.
        
        print("Generating Plo1 2 to Plot2.png ..")
        
        ##First subset the date for Baltimore City
        NEI<-subset(NEI, fips=="24510")
        
        ##Then melt the data set with year as ID.
        meltedNEI <- melt(NEI, id.vars = c("year"), measure.vars=c("Emissions"))
        
        ##Now cast the data set with year as IDs and calculate the sum
        castedNEI <- dcast(meltedNEI, year~variable,sum)
        
        ##Prep for generating plot2
        png(file="plot2.png", width = 640, height = 640)
        par(mfcol=c(1,1))
        par(mar=c(6,6,6,6))
        
        ##Since the data is only for PM25-PRI and for years 1999, 2002, 2005 and 2008
        ##and we are looking at emissions from all sources for Baltimore, no more subsettting is required
        with(castedNEI, {plot(year, round(Emissions), type="n", main="Plot 2: Total PM2.5 Emissions for Baltimore City from 1999 to 2008",
                              xlab="Year",
                              ylab="Total PM2.5 Emissions", yaxt="n", xaxt="n", xlim=c(1998, 2010))
                         
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
        
        print("Plo1 2 successfully generated to Plot2.png")
        print("So yes, the Total Emissions for PM2.5 decreased in Baltimore City from 1999 to 2008, though it had increased in 2005")
        
}

q3<-function (NEI){
        
        ##Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
        ##which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
        ##Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.
        
        ##Answer
        ##-->The Total PM2.5 Emissions in Baltimore City decreased from 1999 to 2008, though it had increased in 2005
        ##-->The Total PM2.5 Emissions of type NON-ROAD in Baltimore City decreased from 1999 to 2008
        ##-->The Total PM2.5 Emissions of type NONPOINT in Baltimore City decreased from 1999 to 2008
        ##-->The Total PM2.5 Emissions of type ON-ROAD in Baltimore City decreased from 1999 to 2008
        ##-->The Total PM2.5 Emissions of type POINT in Baltimore City increased from 1999 to 2008
        
        print("Generating Plo1 3 to Plot3.png ..")
        
        ##First subset the date for Baltimore City
        NEI<-subset(NEI, fips=="24510")
        
        ##Then melt the data set with year as ID.
        meltedNEI <- melt(NEI, id.vars = c("year", "type"), measure.vars=c("Emissions"))
        
        ##Now cast the data set with year as IDs and calculate the sum
        castedNEI <- dcast(meltedNEI, year+type~variable,sum)
        
        ##Prep for generating plot3
        png(file="plot3.png", width = 960, height = 640)
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
        
        
}

q4<-function (NEI, SCC){
        
        ##Across the United States, how have emissions from coal combustion related
        ##sources changed from 1999-2008?
        
        ##Answer
        ##--> The Coal Combustion related PM2.5 Emissions decreased in the US from 1999 to 2008
        
        print("Generating Plo1 4 to Plot4.png ..")
        
        ##Retrieve coal combustion related sources. Look for Sources beginning in Fuel Comb
        ##and ending in Coal in the EI.Sector variable. This give us 99 sources that match following
        ##three EI.Sectors
        ##as.data.frame(unique(coalcombustionsources[,"EI.Sector"], na.rm=TRUE))
        ##1                     Fuel Comb - Electric Generation - Coal
        ##2                Fuel Comb - Industrial Boilers, ICEs - Coal
        ##3                      Fuel Comb - Comm/Institutional - Coal
        coalcombustionsources <-subset(SCC[grep("^fuel comb -(.*)- coal$", SCC$EI.Sector, ignore.case=T),],select=c(SCC,EI.Sector))
    
        ##Merge the NEI and the coalcombustion sources dataset by SCC. This will give us only
        ##the coal combustion data sources. This give us 28480 obervations as only 80 of the 99 sources
        ##have observations in NEI. E.g of missing ones are 2199003000, 10300307 etc.
        mergedNEI<-merge(NEI, coalcombustionsources, by="SCC")
        
        ##Thent the data set with year as ID.
        meltedNEI <- melt(mergedNEI, id.vars = c("year"), measure.vars=c("Emissions"))
        
        ##Now cast the data set with year as IDs and calculate the sum
        castedNEI <- dcast(meltedNEI, year~variable,sum)
        
        ##Prep to generate Plot4
        png(file="plot4.png", width = 640, height = 640)
        par(mfcol=c(1,1))
        par(mar=c(6,6,6,6))
        
        ##Since the data is only for PM25-PRI and for years 1999, 2002, 2005 and 2008
        ##and we are looking at emissions from coal combusion source, no more subsettting is required
        with(castedNEI, {plot(year, round(Emissions), type="n", main="Plot 4: Coal Combustion PM2.5 Emissions for US from 1999 to 2008",
                              xlab="Year",
                              ylab="Coal Combustion PM 2.5 Emissions", yaxt="n", xaxt="n", xlim=c(1998, 2010))
                         
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
        
        print("So the Coal Combustion related PM2.5 Emissions decreased in the US from 1999 to 2008")
        
        print("Plo1 4 successfully generated to Plot4.png")
       
        
}

q5<-function (NEI, SCC){
        
        ##How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
        
        ##Answer
        ##--> The Motor Vehicles related PM2.5 Emissions decreased in the Baltimore City from 1999 to 2008
        print("Generating Plo1 5 to Plot5.png ..")
        
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
        png(file="plot5.png", width = 640, height = 640)
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
        
        print("So the Motor Vehicles related PM2.5 Emissions decreased in the Baltimore City from 1999 to 2008")
        
        print("Plo1 5 successfully generated to Plot5.png")
        
        
}

q6<-function (NEI, SCC){
        
        ##Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los
        ##Angeles County, California ( fips == "06037" ). Which city has seen greater changes over time in motor vehicle
        ##emissions?
        
        ##Answer
        ##-->So Baltimore City has seen greater changes (~75%) in Motor Vehicles related PM2.5 Emissions 
        ##than that in Los Angeles County  from 1999 to 2008
        
        print("Generating Plo1 6 to Plot6.png ..")
        
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
        png(file="plot6.png", width = 960, height = 640)
        
        ##Since the data is only for PM25-PRI and for years 1999, 2002, 2005 and 2008
        ##and we are looking at emissions from motor vehicle sources in Baltimore and LA, no subsettting is required
        
        ##Note --- Tried ggplot2 qplot but I like the base plot clarity to answer the question
        
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
        
        print("So Baltimore City has seen greater changes (~75%) in Motor Vehicles related PM2.5 Emissions 
              than that in Los Angeles County  from 1999 to 2008")
        
        print("Plo1 6 successfully generated to Plot6.png")
        
        
}
        