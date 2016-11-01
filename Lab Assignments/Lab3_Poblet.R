#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Lab 3
# Dr. Spence
# September 20, 2016
#--------------------------------------------------------

# Load USArrests data set
data("USArrests")

# Setup Assignment function
AssignLevel <- function(p, quartiles)
{
  if (p < quartiles[1])
    rlevel <- "LOW"
  else if (p < quartiles[2])
    rlevel <- "MODERATE"
  else if (p < quartiles[3])
    rlevel <- "HIGH"
  else
    rlevel <- "VERY HIGH"
  
  return (rlevel)
}

# Calculate the quartile of USAressts Urbanpop
usarrest_Q <- quantile(USArrests$UrbanPop, c(.25, .5, .75))

# Call function AssignLevel on USArrest Urbanpop and its quartile values with sapply.
UrbanPopRating <- sapply(USArrests$UrbanPop, AssignLevel, quartiles = usarrest_Q)

# Set UrbanPopRating to a factor and reorganize levels
UrbanPopRating <- factor(UrbanPopRating, levels = c("LOW", "MODERATE", "HIGH", "VERY HIGH"))

# Bind UrbanPopRating to existing USArrests data frame
USArrests <- cbind(USArrests, UrbanPopRating)

# Display new data frame
USArrests

# Load Lattice Library
library(lattice)

# Create histograms of the number of arrests for assault grouped by Urban Pop. level
histogram(~Assault | UrbanPopRating, data = USArrests, col = "grey",
          main = "Assault Arrests by Urban Population Level",
          xlab = "State Arrests per 100,000", layout = c(1,4),
          panel = function(x, ...){
            panel.histogram(x, ...)
            panel.abline(v = median(x), lwd = 3, lty = 2, col = "red")
          })

# Create a box plot of the number of arrests for assault grouped by Urban Pop. level
bwplot(~Assault | UrbanPopRating, data = USArrests, col = "grey",
       main = "Assault Arrests by Urban Population Level",
       xlab = "State Arrests per 100,000", layout = c(1,4),
       panel = function(x, ...){
         panel.bwplot(x, ...)
         panel.abline(v = mean(x), lwd = 3, lty = 3, col = "blue")
       })

# Create a scatter plot of the number of arrests for assault to murder relation grouped by Urban Pop. level
xyplot(Murder ~ Assault | UrbanPopRating, data = USArrests, col = "grey",
       main = "Assault & Murder Arrests by Urban Population Level", layout = c(1,4),
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.lmline(x,y)
       })
