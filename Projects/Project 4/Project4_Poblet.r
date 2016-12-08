#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Project 4
# Dr. Spence
# October 18, 2016
#--------------------------------------------------------

# Load dplyr library
library(dplyr)

# Load the data sets from thier files
NEI <- readRDS("summarySCC_PM25.rds")
SCC_Ref <- readRDS("Source_Classification_Code.rds")

# 1.
# Filter NEI with a master data frame of all the columns needed.
caliCountiesMaster <- NEI  %>% 
                select(fips, SCC, Emissions, year) %>% 
                filter(fips == '06037' | fips == '06059' | fips == '06071')

# Pull columns needed for final data frame
caliCounties <- caliCountiesMaster %>% select(fips, SCC, Emissions)

# Print coompleted Question 1 data frame.
View(caliCounties)

# 2.
# Copy fips column so names can be assigned based on fips code.
fipsNames <- caliCountiesMaster %>%  select(fips)

# Rename the column
colnames(fipsNames) <- "County"
# Set the proper names to associated fips code.
fipsNames[fipsNames == "06037"] <- "Los Angeles"
fipsNames[fipsNames == "06059"] <- "Orange"
fipsNames[fipsNames == "06071"] <- "San Bernadino"

# Bind the created county column to the data frame.
caliCountiesMaster <- bind_cols(caliCountiesMaster, fipsNames)

# Pull the EI.Secotr and SCC columns out of SCC Reference data frame.
SCC_Cut <- SCC_Ref %>% select(EI.Sector, SCC)

# Create a logical data frame that is true when Solvent is present and false otherwise.
solventPresent <- data_frame(grepl("solvent", SCC_Cut$EI.Sector, ignore.case = TRUE))

# Label column name apropriatly.
colnames(solventPresent) <- "Solvent"

# Replace boolean values with Yes for true and No for false.
solventPresent[solventPresent == TRUE] <- "YES"
solventPresent[solventPresent == FALSE] <- "NO"
SCC_Cut <- bind_cols(SCC_Cut, solventPresent)

caliCountiesMaster <- left_join(caliCountiesMaster, SCC_Cut,  by = "SCC")
caliCounties2 <- caliCountiesMaster %>% select(fips, SCC, Emissions, County, Solvent)

# Display completed data frame for question 2
View(caliCounties2)

# 3.
# Filter out year 1999 out of our master data frame and retrieve columns needed.
caliCounties3 <- caliCountiesMaster %>% filter(year != 1999) %>% 
              select(County, year, Emissions, Solvent)

# Display completed data from for Question 3.
View(caliCounties3)

# 4.
# Group by county, year, and solvent and sumarise total of all emmissions in each group. 
caliEmissions <- caliCounties3 %>% 
                 group_by(County, year, Solvent) %>% 
                 summarise("Total Emissions" = sum(Emissions))
# Display Results
View(caliEmissions)

# 5.
# Group by county and solvent then sumarise total of all emmissions in each group.
caliSum <- caliCounties3 %>% 
           group_by(County, Solvent) %>%
           summarise("Total Emissions" = sum(Emissions))
# Display Results
View(caliSum)

# 6.
# Group by year and solvent then sumarise total of all emmissions in each group.
solventEmissions <- caliCounties3 %>% 
                group_by(year, Solvent) %>%
                summarise("Total Emissions" = sum(Emissions))
# Display Results
View(solventEmissions)

# 7.
# Group by county and year then sumarise total of all emmissions in each group.
countyEmissions <- caliCounties3 %>% 
               filter(year == 2008) %>% 
               group_by(County) %>% 
               summarise("Total 2008 Emissions" = sum(Emissions))
# Display Results
View(countyEmissions)

# 8.
# Sumarise total of all emmissions in each group.
caliCountyEmissions <- caliCounties3 %>% 
                       summarise("Total Emissions" = sum(Emissions))
# Label row
row.names(caliCountyEmissions) <- "All Three Counties"
# Display Results
View(caliCountyEmissions)
