#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Lab 7
# Dr. Spence
# November 10, 2016
#--------------------------------------------------------

# Load data set
cars <- read.csv("04cars.csv")
head(cars)

#Simple model: Predict Horsepower by Engine Size
plot(HP ~ EngineSize, data = cars, pch=19, col="blue")
hpModel <- lm(HP ~ EngineSize, data = cars)
summary(hpModel)
abline(hpModel, lwd=2)

#Intervals

#Interval for individual value
xVar <- data.frame(EngineSize=4)
singleConfInt <- predict(hpModel,xVar,interval = "confidence")
singleConfInt

singlePredInt <- predict(hpModel,xVar,interval = "prediction")
singlePredInt

#Plotting interval for data set
explanatory <- data.frame(EngineSize = cars$EngineSize)
hpConfInt <- predict(hpModel,explanatory,interval = "confidence")
str(hpConfInt)
hpConfInt[1,]
lines(cars$EngineSize,hpConfInt[,2],lty=3,lwd=2,col="darkgrey") #lower bound of confidence interval
lines(cars$EngineSize,hpConfInt[,3],lty=3,lwd=2,col="darkgrey") #upper bound of confidence interval

hpPredInt <- predict(hpModel,explanatory,interval = "prediction")
hpPredInt[1,]
lines(cars$EngineSize,hpPredInt[,2],col="lightblue") #lower bound of confidence interval
lines(cars$EngineSize,hpPredInt[,3],col="lightblue") #upper bound of confidence interval


#Examine for sport and non-sport
plot(HP ~ EngineSize, data = cars, pch=19)
points(cars$EngineSize,cars$HP,pch=19,col=((cars$sport==1)*1 + 1))

sportFilter <- cars$sport==1
hpModelSport <- lm(cars$HP[sportFilter] ~ cars$EngineSize[sportFilter])
hpModelNotSport <- lm(cars$HP[!sportFilter] ~ cars$EngineSize[!sportFilter])
abline(hpModelSport,lwd=2,col="red")
abline(hpModelNotSport ,lwd=2)
hpModelSport$coefficients
hpModelNotSport$coefficients

hpModelBoth <- lm(HP ~ EngineSize + sport, data = cars)
summary(hpModelBoth)

# Function for setting up prediction intervals for sport and engine size
predSport <- function(enginesize, bool = 0){
  explan <- data.frame(EngineSize=enginesize, sport=bool)
  # Prediction
  p <- predict(hpModelBoth, newdata=explan, interval = "prediction")
  # Display prediction.
  return(p)
}

#1.
# Prediction
hpModel3.5Sport <- predSport(3.5,1)
# Display prediction.
hpModel3.5Sport

# 2.
# Prediction
hpModel4NonSport <- predSport(4,0)
# Display prediction.
hpModel4NonSport

#Simple model: Predict MPG by Engine Size
plot(City.MPG ~ EngineSize, data = cars, pch=19, col="blue")
mpgModel <- lm(City.MPG ~ EngineSize, data = cars)
summary(mpgModel)
abline(mpgModel, lwd=2)

#Examine for RWD and non-RWD
plot(City.MPG ~ EngineSize, data = cars, pch=19)
points(cars$EngineSize,cars$City.MPG,pch=19,col=((cars$RWD==1)*1 + 1))

rwdFilter <- cars$RWD==1
mpgModelRWD <- lm(cars$City.MPG[rwdFilter] ~ cars$EngineSize[rwdFilter])
mpgModelNotRWD <- lm(cars$City.MPG[!rwdFilter] ~ cars$EngineSize[!rwdFilter])
abline(mpgModelRWD,lwd=2,col="red")
abline(mpgModelNotRWD ,lwd=2)
mpgModelRWD$coefficients
mpgModelNotRWD$coefficients

mpgModelBoth <- lm(City.MPG ~ EngineSize + RWD + EngineSize*RWD, data = cars)
summary(mpgModelBoth)

# Function for setting up prediction intervals for RWD and engine size
predRwd <- function(enginesize, bool = 0){
  explan <- data.frame(EngineSize=enginesize, RWD=bool)
  # Prediction
  p <- predict(mpgModelBoth, newdata=explan, interval = "prediction")
  # Display prediction.
  return(p)
}
# 3.
# Prediction
mpgModel2Rwd <- predRwd(2,1)
# Display prediction.
mpgModel2Rwd

# 4.
# Prediction
mpgModel2.5NonRwd <- predRwd(2.5,0)
# Display prediction.
mpgModel2.5NonRwd

# 5.
# Prediction
mpgModel2.5NonRwd <- predRwd(4,0)
# Display prediction.
mpgModel2.5NonRwd
