#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Lab 5
# Dr. Spence
# October 25, 2016
#--------------------------------------------------------

# Load dplyr library
library(dplyr)

# Load the data sets from thier files
IowaHousing <- read.csv("IowaHousing.csv")

# Set seed based on my birthday Octobet 14
set.seed(1014)

# Grab a random sample of 50 homes from IowaHousing data set
myIowaSample <- sample_n(IowaHousing, 50)

# Pull the column out that we will be calcualting.
myGr_Liv_Area <- myIowaSample$Gr.Liv.Area

# 1.
# Mean of my samples graded living area
mean(myGr_Liv_Area)

# 2. Ho: Mu = 2000 , Ha: Mu < 2000
# Conduct a t-test on my sample
t.test(myGr_Liv_Area, mu = 2000, alternative = "less")
# The T-value is -8.4385 and the p-value is 0.00000000002056
# The probablity of having a sample mean of 1508.04 with a population having a
# true mean of 2000 is highly unlikely. The sample doesn't provide sufficient
# evidence that the true mean graded living area of all houses in this
# population is less than 2000 square feet, reject the null.

# 3 Ho: Mu = 1500 , Ha: Mu < 1500
# Conduct a t-test on my sample with true mean of 1500
t.test(myGr_Liv_Area, mu = 1500, alternative = "less")
# The T-value is 0.13791 and the p-value is 0.5546
# The probablity of having a sample mean of 1508.04 with a population having a
# true mean of 1500 is significant. The sample does provide sufficient
# evidence that the true mean graded living area of all houses in this
# population can be less than 1500 square feet, fail to reject the null.

# 4.
# Conduct a t-test on IowaHousing graded living area population with a 95% confidence interval
conInterval95 <- t.test(IowaHousing$Gr.Liv.Area)

# 5
# Conduct a t-test on IowaHousing graded living area population with a 90% confidence interval
conInterval90 <- t.test(IowaHousing$Gr.Liv.Area, conf.level = 0.90)
# 6.
# Conduct a t-test on IowaHousing graded living area population with a 99% confidence interval
conInterval99 <- t.test(IowaHousing$Gr.Liv.Area, conf.level = 0.99)

# Set up data frame to display values
confidenceIntervals <- data.frame(Min = c(conInterval90$conf.int[1],conInterval95$conf.int[1],conInterval99$conf.int[1]),
                                  Max = c(conInterval90$conf.int[2], conInterval95$conf.int[2], conInterval99$conf.int[2]),
                                  row.names = c("90", "95", "99"))
# display data frame
View(confidenceIntervals)
