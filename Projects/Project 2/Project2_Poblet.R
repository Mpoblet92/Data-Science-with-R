#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Project 2
# Dr. Spence
# September 22, 2016
#--------------------------------------------------------

# Set seed
set.seed(519)

# Set =up GenUnifSamples function 
GenUnifSamples <- function(s, n = 10000){
  # Set variable to start from beginning of loop
  i <- 1
  # instantiate the vector that will be storing means
  meanUnif <- 0
  # Start loop to go till total amount of samples n
  while(i <= n){
    # Generate random values of the sample size s between 1 and 10
    rando <- runif(s, min = 1, max = 10)
    # Store the mean of the sample generated in our returning vector
    meanUnif[i] <- mean(rando)
    # Add one to value being checked for loop
    i <- i + 1
  }
  # Return the generated vector of means
  return(meanUnif)
}

# Setup GenNormSamples function
GenNormSamples <- function(s, n = 10000){
  # Set variable to start from beginning of loop
  i <- 1
  # instantiate the vector that will be storing means
  meanNorm <- 0
  # Start loop to go till total amount of samples n
  while(i <= n){
    # Generate random values of the sample size s from the normal standard distribution
    rando <- rnorm(s)
    # Store the mean of the sample generated in our returning vector
    meanNorm[i] <- mean(rando)
    # Add one to value being checked for loop
    i <- i + 1
  }
  # Return the generated vector of means
  return(meanNorm)
}

# Setup GenExpSamples Function
GenExpSamples <- function(s, n = 10000){
  # Set variable to start from beginning of loop
  i <- 1
  # instantiate the vector that will be storing means
  meanExp <- 0
  # Start loop to go till total amount of samples n
  while(i <= n){
    # Generate random values of the sample size s from exponential distribution
    rando <- rexp(s)
    # Store the mean of the sample generated in our returning vector
    meanExp[i] <- mean(rando)
    # Add one to value being checked for loop
    i <- i + 1
  }
  # Return the generated vector of means
  return(meanExp)
}

# Create function to calculate the mean, sd, and five number summary
importantValues <- function(f){
  # Store each value in a vector
  num <- c(mean(f),sd(f),fivenum(f))
  # Label each number appropriatly
  names(num) <- c("Mean", "S.D.","Min","1st Qu.", "Meadian", "3rd Qu.", "Max")
  return(num)
}

# Uniform Distribution Theoretical plot
x <- seq(0.9, 10.1, length = 200)
hx <- dunif(x, min = 1, max = 10)
plot(x, hx, type = "l", xlab = "", ylab = "Density",
     main = "Theoretical Uniform Distribution")

# Generate one sample with 10000 values from uniform distribution
unifSample <- runif(10000, min = 1, max = 10)
#Display histogram of sample
hist(unifSample, main = "Single Uniform Sample", xlab = "", col = 2)
# Display mean, Standard Deviation, 5 number summary
importantValues(unifSample)

# Generate 10000 samples of size 5 from uniform distribution
unifSmall <- GenUnifSamples(5)
# Display histogram
hist(unifSmall, main = "Small Uniform Sample(5)", xlab = "", col = 3)
# Display mean, standard deviation, and five number summary
importantValues(unifSmall)

# Generate 10000 samples of size 25 from uniform distribution
unifMedium <- GenUnifSamples(25)
# Display histogram 
hist(unifMedium, main = "Medium Uniform Sample(25)", xlab = "", col = 4)
# Display mean, standard deviation, and five number summary
importantValues(unifMedium)

# Generate 10000 samples of size 500 from uniform distribution
unifLarge <- GenUnifSamples(500)
# Display histogram 
hist(unifLarge, main = "Large Uniform Sample(500)", xlab = "", col = 6)
# Display mean, standard deviation, and five number summary
importantValues(unifLarge)

# Normal Distribution Theoretical plot
x <- seq (-3.5, 3.5, length=200)
hx <- dnorm(x)
plot (x,hx,type="l",xlab = "", ylab = "Density", 
      main = "Theoretical Normal Distribution")

# Generate one sample with 10000 values from normal distribution
normSample <- rnorm(10000)
#Display histogram of sample
hist(normSample, main = "Single Normal Sample", xlab = "", col = 2)
# Display mean, Standard Deviation, 5 number summary
importantValues(normSample)

# Generate 10000 samples of size 5 from normal distribution
normSmall <- GenNormSamples(5)
# Display histogram
hist(normSmall, main = "Small Normal Sample(5)", xlab = "", col = 3)
# Display mean, standard deviation, and five number summary
importantValues(normSmall)

# Generate 10000 samples of size 25 from normal distribution
normMedium <- GenNormSamples(25)
# Display histogram
hist(normMedium, main = "Medium Normal Sample(25)", xlab = "", col = 4 )
# Display mean, standard deviation, and five number summary
importantValues(normMedium)

# Generate 10000 samples of size 500 from normal distribution
normLarge <- GenNormSamples(500)
# Display histogram
hist(normLarge, main = "Large Normal Sample(500)", xlab = "", col = 6)
# Display mean, standard deviation, and five number summary
importantValues(normLarge)

# Exponential Distribution Theoretical plot
x <- seq (0, 5, length=200)
hx <- dexp(x)
plot (x,hx,type="l",xlab = "", ylab = "Density", 
      main = "Theoretical Exponential Distribution")

# Generate one sample with 10000 values from exponential distribution
expSample <- rexp(10000)
#Display histogram of sample
hist(expSample, main = "Single Exponential Sample", xlab = "", col = 2)
# Display mean, Standard Deviation, 5 number summary
importantValues(expSample)


# Generate 10000 samples of size 5 from exponential distribution
expSmall <- GenExpSamples(5)
# Display histogram
hist(expSmall, main = "Small Exponential Sample(5)", xlab = "", col = 3)
# Display mean, standard deviation, and five number summary
importantValues(expSmall)

# Generate 10000 samples of size 25 from exponential distribution
expMedium <- GenExpSamples(25)
# Display histogram
hist(expMedium, main = "Medium Exponetial Sample(25)", xlab = "", col = 4)
# Display mean, standard deviation, and five number summary
importantValues(expMedium)

# Generate 10000 samples of size 500 from exponential distribution
expLarge <- GenExpSamples(500)
# Display histogram
hist(expLarge, main = "Large Exponential Sample(500)", xlab = "", col = 6)
# Display mean, standard deviation, and five number summary
importantValues(expLarge)