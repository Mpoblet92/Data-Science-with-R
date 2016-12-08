#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Project 1
# Dr. Spence
# September 13, 2016
#--------------------------------------------------------

# Load the USJudgeRatings data frame
data("USJudgeRatings")

# Initialize variables for the loop.
i <- 1
firstJ <- ""
secondJ <- ""
thirdJ <- ""

# Begin loop to go through each of the columns 1:12
while(i < 13){
  # Store the current category in a sorted list if indecies from 
  # highest rated to lowest. Ties are broken alphabetically. A > Z 
  current <- sort.list(USJudgeRatings[,i], decreasing = TRUE)
  # Save the top three rated indecies in seperate vectors
  first <- c(current[1])
  second <- c(current[2])
  third <- c(current[3])
  
  # Save the names of top 3 judges in a vector.
  firstJ[i] <-row.names(USJudgeRatings[first,])
  secondJ[i] <- row.names(USJudgeRatings[second,])
  thirdJ[i] <- row.names(USJudgeRatings[third,])
  
  # Increase i to go to the next category.
  i <- i + 1
}

# Create a data frame with 12 rows and 3 columns.
# Each row labeled the same as original data frame columns.
# Each column displaying the highest 3 judges for each category.
top_Three <- data.frame(row.names = names(USJudgeRatings), 
                        First = firstJ[], Second = secondJ[], Third = thirdJ[])

# Display data frame
top_Three

# Combine all top 3 judges in a vector
all_Judges <- c(firstJ, secondJ, thirdJ)

# Place vector in a table to see how many of each name appears.
judge_table <- table(all_Judges)

# Sets most frequent judges name to true, including tied judges. 
topJudgeFilter <- judge_table == max(judge_table)

# Display the names of the judge(s) who were rated top the most.
names(judge_table[topJudgeFilter])
