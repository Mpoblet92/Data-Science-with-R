#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Dr. Spence
# September 1, 2016
#--------------------------------------------------------

# A.Load "Titanic" Data Set
data("Titanic")

# 1. How many first class female children survived?
Titanic[1,2,1,2]
Titanic["1st","Female","Child","Yes"]

# 2. How many third class passengers survived?
sum(Titanic[3,,,2])

# 3. Among all 4 classes of passenger, how many survived and how many did not?
apply(Titanic,4,sum)

# 4. How many total passengers are reported in this data set?
sum(Titanic)

# 5. Report the composition of passengers by gender.
apply(Titanic,2,sum)

# 6. How many of each gender survived?
m <- sum(Titanic[,1,,2])
# Males Survived
m

f <- sum(Titanic[,2,,2])
# Females Survived
f

# 7. What percentage of each gender survived?
# Percentage of Males Survived
(m/sum(Titanic[,1,,]))*100

# Percentage of Females Survived
(f/sum(Titanic[,2,,]))*100

# B. Load "USArrests" Data Set
data("USArrests")

# 1. How many arrests for murder occurred per 100,000 people in Oregon?
USArrests['Oregon',1]

# 2. What is the total number of arrests (per 100,000 people) reported in this data for Vermont?
sum(USArrests['Vermont',c(1,2,4)])

# 3. What is the average number of arrests (per 100,000 people) for assault across all 50 states?
mean(USArrests[,2])


# 4. What is the total number of arrests (per 100,000 people) for assault OR rape in Georgia?

# Total Arrests for Assualt and Rape in Georgia.
sum(USArrests['Georgia',c(2,4)])


# 5. Add a total column to the data set that represents the total arrests reported for each state. Display the total for each state. 

# Add every arrest for each state.
totalArrests <- USArrests$Murder + USArrests$Assault + USArrests$Rape

# Add column total to data frame USArrests
USArrests <- cbind(USArrests, totalArrests)

USArrests




