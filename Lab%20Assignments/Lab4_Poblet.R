#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Lab 4
# Dr. Spence
# September 29, 2016
#--------------------------------------------------------

library(reshape2)

# Load Mtcars data set
data("mtcars")

# mean of mpg and weight for all cars in data set
mpg_wtMeans <- c(mean(mtcars$mpg),mean(mtcars$wt))
# label values
names(mpg_wtMeans) <- c("MPG","Weight")
# Display means
mpg_wtMeans

# Add a row to original data frame of names for each car
mtcars$carname <- rownames(mtcars)
# melt the data to display mpg and wiegth based on transmission
melt_mtcars <- melt(mtcars, id = c("carname", "am"), measure.vars = c("mpg", "wt"))
# construct a table showing means of mpg and weight based on transmission type
meanByTransmission <- dcast(melt_mtcars, am ~ variable, mean)
# Display table
meanByTransmission
