## We will use this script to download the zip file of the exercise, unzip it,
## filter first and second of February 2007, and save it in another csv file.
## Load needed libraries

library(dplyr)
library(ggplot2)

## If the file is already downloaded, we will use the downloaded version

if(!file.exists("exdata_data_NEI_data.zip")) {
        url_zip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        zipname <- "exdata_data_NEI_data.zip"
        download.file(url_zip, zipname)
        unzip(zipname)
}

## Load both files in a dataframe as specified in instructions

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## In order to select data for Vehicles, having a look at the data, "ehicle"
## appears in SCC.Level.Two, SCC.Level.Three and SCC.Level.Four of the SCC file.
## In order to select the right SCC codes we will need to select the lines that
## contain this string in any of the three fields.

Vehicle.L2 <- grepl(pattern = "ehicle", SCC$SCC.Level.Two)
Vehicle.L3 <- grepl(pattern = "ehicle", SCC$SCC.Level.Three)
Vehicle.L4 <- grepl(pattern = "ehicle", SCC$SCC.Level.Four)

Vehicle <- Vehicle.L2 | Vehicle.L3 | Vehicle.L4
VehicleCodes <- SCC$SCC[Vehicle]

## Once obtained the lines of Vehicles, we will filter NEI to obtain
## a shorter data frame with the relevant data and for Baltimore City and
## another one for Los Angeles County

NEI.Vehicle.Balt <- filter(NEI, (SCC %in% VehicleCodes & fips == "24510") )
NEI.Vehicle.LAC <- filter(NEI, (SCC %in% VehicleCodes & fips == "06037") )

## Using dplyr capabilities, we will sum all values of Emissions per year and
## store them on a new variable, table data frame called Tot_Emis_Veh_Balt for
## Baltimore City data and Tot_Emis_Veh_LAC for Los Angeles County

Tot_Emis_Veh_Balt <- NEI.Vehicle.Balt %>% group_by(year)%>%
        summarise(sum(Emissions))
names(Tot_Emis_Veh_Balt) <- c("year", "TotEmissions")

Tot_Emis_Veh_LAC <- NEI.Vehicle.LAC %>% group_by(year)%>%
        summarise(sum(Emissions))
names(Tot_Emis_Veh_LAC) <- c("year", "TotEmissions")

## As the question is tricky, because it doesn't specify if the change has to be 
## measured in percentage or in value, we will do both. Now we have to calculate
## the percentage change of the vehicle emissions in both counties.

Tot_Emis_Veh_Balt <- mutate(Tot_Emis_Veh_Balt, 
                         pct_change = (TotEmissions/lag(TotEmissions) - 1)*100)
Tot_Emis_Veh_LAC <- mutate(Tot_Emis_Veh_LAC, 
                         pct_change = (TotEmissions/lag(TotEmissions) - 1)*100)

##Open png device with required parameters

png(filename = "plot6.png", width = 480, height = 480, units = "px")

## As there's no specification on the plotting system, we will use Base system

par(mfcol=c(2,2))

barplot(height = Tot_Emis_Veh_Balt$TotEmissions, 
        names.arg = Tot_Emis_Veh_Balt$year, 
        col = c(2, 3, 4, 5), 
        main = "Vehicle Emissions in Baltimore",
        xlab = "Years",
        ylab = "Emissions in Tons")

barplot(height = Tot_Emis_Veh_Balt$pct_change, 
        names.arg = Tot_Emis_Veh_Balt$year, 
        col = c(2, 3, 4, 5), 
        main = "Vehicle Emissions evol. in Baltimore",
        xlab = "Years",
        ylab = "% reduction")

barplot(height = Tot_Emis_Veh_LAC$TotEmissions, 
        names.arg = Tot_Emis_Veh_LAC$year, 
        col = c(2, 3, 4, 5), 
        main = "Vehicle Emissions in LA County",
        xlab = "Years",
        ylab = "Emissions in Tons")

barplot(height = Tot_Emis_Veh_LAC$pct_change, 
        names.arg = Tot_Emis_Veh_LAC$year, 
        col = c(2, 3, 4, 5), 
        main = "Vehicle Emissions evol. in LA County",
        xlab = "Years",
        ylab = "% reduction")

## close png device

dev.off()