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

## Select data only for Baltimore

Baltimore <- filter(NEI, fips == "24510")

## Using dplyr capabilities, we will sum all values of Emissions of Baltimore 
## per type and year. We will store them on a new variable, table data frame 
## called Baltimore.SubTotals

Baltimore.SubTotals <- Baltimore %>% group_by(type, year)%>%summarise(sum(Emissions))
names(Baltimore.SubTotals) <- c("type", "year", "TotEmissions")

##Open png device with required parameters

png(filename = "plot3.png", width = 480, height = 480, units = "px")

## First we will create the base graphic with the Baltimore.Subtotals dataframe
## having on x axis the year as a factor, and the Total Emissions on the y axis.

g <- ggplot(data = Baltimore.SubTotals, aes(factor(Baltimore.SubTotals$year), 
                                            TotEmissions))

## And now we will add layers to print the graphic

g + geom_col(fill = rep(2:5, times = 4)) + ## Columns graphics
        facet_grid(.~type) +               ## One facet per type
        labs(title = "Total PM2.5 emission by source in Baltimore City") + 
        labs(x = "Years", y = "Emissions in Tons")

## close png device

dev.off()