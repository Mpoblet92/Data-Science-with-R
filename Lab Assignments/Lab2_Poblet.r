#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Lab 2
# Dr. Spence
# September 8, 2016
#--------------------------------------------------------

# A.

# 1.
pnorm(450, mean = 496, sd = 114)

# 2.
pnorm(580, mean = 496, sd = 114)

# 3.
qnorm(.75, mean = 496, sd = 114)

# 4.
qnorm(.8, mean = 496, sd = 114)

# 5.
qnorm(.05, mean = 496, sd = 114, lower.tail = FALSE)

#====================================================================================
# B.

# 1.
set.seed(519)  # My Publix Store number!
sample <- runif(100, min = 1, max = 10)

# 2.
qunif(.25, min = 1, max = 10)

# 3.
quantile(sample, .25)

# 4.
hist(sample)

# 5.
sample_Two <- runif(1000, min = 1, max = 10)

# 6.
quantile(sample_Two, .25)

# 7.
hist(sample_Two)

# 8.
sample_Three <- runif(10000, min = 1, max = 10)

# 9.
quantile(sample_Three, .25)

# 10.
hist(sample_Three)

# 11.

# a. The samples stayed close to the same, and fluctuated near the theoretical uniform distribution. 
#    The larger the sample size, the closer the first quartile came to 3.25. 

# b. Aside from the first histogram having a larger 8 to 10 range, they all were similar in view.
#     each sample size increase had a histogram that became more uniform.

# c. The "Law of Large Numbers" is what occured on our samples. 
#    As our sample size increases, it will converge with the expected value of 3.25 for the first quartile and holds true for the other quartiles aswell.
