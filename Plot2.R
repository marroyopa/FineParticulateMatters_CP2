## We will use this script to download the zip file of the exercise, unzip it,
## filter first and second of February 2007, and save it in another csv file.
## Load needed libraries

library(dplyr)

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

## Using dplyr capabilities, we will sum all values of Emissions per year and
## store them on a new variable, table data frame called Total_Emissions

Baltimore.Totals <- Baltimore %>% group_by(year)%>%summarise(sum(Emissions))
names(Baltimore.Totals) <- c("year", "TotEmissions")

##Open png device with required parameters

png(filename = "plot2.png", width = 480, height = 480, units = "px")

## Create the barplot with different color for each year, title and labels

barplot(height = Baltimore.Totals$TotEmissions, 
        names.arg = Baltimore.Totals$year, 
        col = c(2, 3, 4, 5), 
        main = "Total PM2.5 emission from all sources in Baltimore City",
        xlab = "Years",
        ylab = "Emissions in Tons")

## close png device

dev.off()