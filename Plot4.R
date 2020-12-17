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

## In order to select data for Coal Combustion, having a look at the data
## coal appears in SCC.Level.Three and SCC.Level.Four of the SCC file. But not
## all are related to combustion. Combustion appears in SCC.Level.One and 
## SCC.Level.Two. In order to select the right SCC codes we will need to select
## the lines that contain both strings in any of those fields. We will not use
## the short names because as they are shortened, we don't know if every "comb"
## corresponds to combustion or other words.

Comb.L1 <- grepl(pattern = "Combustion", SCC$SCC.Level.One)
Comb.L2 <- grepl(pattern = "Combustion", SCC$SCC.Level.Two)
Coal.L3 <- grepl(pattern = "Coal", SCC$SCC.Level.Three)
Coal.L4 <- grepl(pattern = "Coal", SCC$SCC.Level.Four)

CoalComb <- (Comb.L1 | Comb.L2) & (Coal.L3 | Coal.L4)
CoalCombCodes <- SCC$SCC[CoalComb]

## Once obtained the lines of Coal Combustion, we will filter NEI to obtain
## a shorter data frame with the relevant data

NEI.CoalComb <- filter(NEI, SCC %in% CoalCombCodes )

## Using dplyr capabilities, we will sum all values of Emissions per year and
## store them on a new variable, table data frame called Tot_Emis_CoalComb

Tot_Emis_CoalComb <- NEI.CoalComb %>% group_by(year)%>%summarise(sum(Emissions))
names(Tot_Emis_CoalComb) <- c("year", "TotEmissions")

##Open png device with required parameters

png(filename = "plot4.png", width = 480, height = 480, units = "px")

## First we will create the base graphic with the Tot_Emis_CoalComb dataframe
## having on x axis the year as a factor, and the Total Emissions on the y axis.

g <- ggplot(data = Tot_Emis_CoalComb, aes(factor(year), 
                                            TotEmissions))

## And now we will add layers to print the graphic

g + geom_col(fill = 2:5) + ## Columns graphics
        labs(title = "Total emissions from coal combustion-related sources") + 
        labs(x = "Years", y = "Emissions in Tons")

## close png device

dev.off()