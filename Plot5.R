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
## a shorter data frame with the relevant data and for Baltimore City

NEI.Vehicle <- filter(NEI, (SCC %in% VehicleCodes & fips == "24510") )

## Using dplyr capabilities, we will sum all values of Emissions per year and
## store them on a new variable, table data frame called Tot_Emis_Vehicle

Tot_Emis_Vehicle <- NEI.Vehicle %>% group_by(year)%>%summarise(sum(Emissions))
names(Tot_Emis_Vehicle) <- c("year", "TotEmissions")

##Open png device with required parameters

png(filename = "plot5.png", width = 480, height = 480, units = "px")

## First we will create the base graphic with the Tot_Emis_Vehicle dataframe
## having on x axis the year as a factor, and the Total Emissions on the y axis.

g <- ggplot(data = Tot_Emis_Vehicle, aes(factor(year), 
                                            TotEmissions))

## And now we will add layers to print the graphic

g + geom_col(fill = 2:5) + ## Columns graphics
        labs(title = "Emissions from motor vehicle in Baltimore City") + 
        labs(x = "Years", y = "Emissions in Tons")

## close png device

dev.off()