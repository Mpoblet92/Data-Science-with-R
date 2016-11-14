#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Project 3
# Dr. Spence
# October 6, 2016
#--------------------------------------------------------

# Load ggplot & Reshape2 & Scales(for formatting from Scientific Notation)
library(ggplot2)
library(reshape2)
library(scales)

# Load the data set from the file
NEI <- readRDS("summarySCC_PM25.rds")

# Create common theme for all graphs
mytheme <- theme_dark() + theme(plot.title = element_text(color = "firebrick"))

# Set up a dataframe with columns that are being used. (Emissions, year)
onlyEmissions <- subset(NEI, select = c(Emissions, year))

# Reshape the date so each year is set with each emissions value
totalEmissionsYear <- melt(onlyEmissions, id = c("year"))
# Sum up all the emissions per year and remake data frame with years and total emissions
totalEmissionsYear <- dcast(totalEmissionsYear, year ~ variable, sum)

# Load ggplot using NEI year and emissions.
allEmissions <- ggplot(data = totalEmissionsYear, aes(year, Emissions)) + 
                geom_point(color= totalEmissionsYear$year) + geom_line(color = totalEmissionsYear$Emissions) +
                ggtitle("PM2.5 Emissions in the U.S.") + labs(x = "Year", y = "Emissions (Per Ton)") +
                scale_x_continuous(breaks=c(1999,2002, 2005,2008)) + scale_y_continuous(label = comma)
# Display graph with theme.
allEmissions + mytheme

# Creat data set with only Baltimore, Maryland and columns needed
onlyBaltimore <- subset(NEI, fips == "24510", select = c(Emissions, year))
totalBaltimore <- melt(onlyBaltimore, id = c("year"))
totalBaltimore <- dcast(totalBaltimore, year ~ variable, sum)

# Set up graph calling year for x and emissions for y, and containing just Baltimore, Maryland
baltimoreEmissions <- ggplot(totalBaltimore, aes(year, Emissions)) + 
                      geom_point(color= totalBaltimore$year) + geom_line(color = totalBaltimore$Emissions)  +
                      ggtitle("PM2.5 Emissions in Baltimore, Maryland") + 
                      labs(x = "Year", y = "Emissions (Per Ton)", color = "Years") + 
                      scale_x_continuous(breaks=c(1999,2002, 2005,2008))
# Display graph with theme
baltimoreEmissions + mytheme

# Creat data frame of baltimore with type
baltimoreType <- subset(NEI, fips == "24510", select = c(Emissions, year, type))
# Melt data to set emissions as a value
totalBaltType <- melt(baltimoreType, id = c("year","type"))
# Sum emissions and recreate data frame with proper columns.
totalBaltType <- dcast(totalBaltType, type + year ~ variable, sum)

# Set up graph with a facet_grid for each emission source (type)
baltimoreTypeEmissions <- ggplot(totalBaltType, aes(year,Emissions)) + facet_grid(.~type) +
                          geom_point(color= totalBaltType$year) + 
                          geom_line(color = totalBaltType$Emissions)  +
                          ggtitle("PM2.5 Emission Sources in Baltimore, Maryland") + 
                          labs(x = "Year", y = "Emissions (Per Ton)", color = "Years")+ 
                          scale_x_continuous(breaks=c(1999,2002, 2005,2008))
# Display graph with theme
baltimoreTypeEmissions + mytheme

# Create a data frame with Las Vegas and Baltimore, only emission type ON-Road
california_Maryland_NEI <- subset(NEI, (fips == "06037" | fips == "24510") & type == "ON-ROAD", select = c(fips,Emissions, year))
# Replace fips with proper city names
california_Maryland_NEI[california_Maryland_NEI == "24510"] <- "Baltimore, Maryland"
california_Maryland_NEI[california_Maryland_NEI == "06037"] <- "Los Angeles, California"

# Melt data to include set emissions as a value
cali_Mary_Melt <- melt(california_Maryland_NEI, id = c("fips","year"))
# Sum emissions and recreate data frame with proper columns.
cali_Mary_Melt <- dcast(cali_Mary_Melt, fips + year ~ variable, sum)
# Set up graph to call year and emissions on a facet_grid for each city
cityComparison <- ggplot(cali_Mary_Melt, aes(year,Emissions)) + facet_grid(.~fips) +
                  geom_point(color= cali_Mary_Melt$year) + 
                  geom_line(color = cali_Mary_Melt$Emissions)  +
                  ggtitle("PM2.5 On-Road Emission in Baltimore & Los Angeles") + 
                  labs(x = "Year", y = "Emissions (Per Ton)", color = "Years")+ 
                  scale_x_continuous(breaks=c(1999,2002, 2005,2008))
# Display graph with theme
cityComparison + mytheme
