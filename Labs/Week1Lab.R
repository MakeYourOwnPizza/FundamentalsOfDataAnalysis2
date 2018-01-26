# R tutorials
library(SDSFoundations)
survey <- StudentSurvey

mean(survey$age)
sd(survey$age)

hist(survey$age)
# very positively skewed

# Generate a SRS for a sample size of 30
sample(survey$age, size = 30)

# We want to generate the samples again and again
# an empty vector, contaning a bunch of NA's, with a length of 1000.
myxbar <- rep(NA,1000)
# generate samples, calculate means, and put them into the myxbar vector.
for (i in 1:1000){
  mysamp <- sample(survey$age, size = 30)
  myxbar[i] <- mean(mysamp)
}

# Show the sampling distribution
hist(myxbar)
mean(myxbar)
sd(myxbar)
sd(survey$age)/sqrt(30)

# 
# Pre-Lab 1: UT Student Survey Data
# Primary Research Question
# How many letters long is the typical UT student's name?  
# How does our estimate change as we increase the size of our sample?
 
# Let's break this analysis into its required steps:
   
# Determine the population parameters:
# 1. Visualize the shape of the population data by making a histogram.  
hist(survey$name_letters)
# 2. Calculate the "true" mean and standard deviation of the population.
fivenum(survey$name_letters)
mean(survey$name_letters)
sd(survey$name_letters)

# Compare the sample statistics:  
# 3. Draw 1,000 samples of size n=5 from the population data.  Calculate the mean of each sample. 
xbar5 <- rep(NA, 1000)
for (i in 1:1000){
  mysamp <- sample(survey$name_letters, size = 5)
  xbar5[i] <- mean(mysamp)
}
# 4. Graph these 1,000 sample means in a histogram and examine the shape.
hist(xbar5)
# 5. Calculate the mean and standard deviation of the sampling distribution.
mean(xbar5)
sd(xbar5)
sd(survey$name_letters)/sqrt(5)
# 6. Repeat this process for samples of size n=15 and n=25.
n = 15
xbar15 <- rep(NA, 1000)
for (i in 1:1000){
  mysamp <- sample(survey$name_letters, size = n)
  xbar15[i] <- mean(mysamp)
}
hist(xbar15)
mean(xbar15)
sd(xbar15)
sd(survey$name_letters)/sqrt(n)

n = 25
xbar25 <- rep(NA, 1000)
for (i in 1:1000){
  mysamp <- sample(survey$name_letters, size = n)
  xbar25[i] <- mean(mysamp)
}
hist(xbar25)
mean(xbar25)
sd(xbar25)
sd(survey$name_letters)/sqrt(n)
# 7. Compare the results you get to the predictions of the Central Limit Theorem.

# We used the following code to try to show the sampling distribution of ages:
xbar5 <-rep(NA, 1000)
for (i in 1:1000)
{x <-sample(survey$age, size =5)
xbar5[i] <- mean(x)}
hist(xbar5,xlim=c(2,10))
hist(xbar5)
# Why was the histogram that R produced blank?
# The scale of the x-axis is set from 2 to 10, but the ages are not in this range.


# Lab 1: UT Student Survey Data
# 1a) In this lab, we will draw samples to answer the following question: What percentage of the time are college students happy? How does the Central Limit Theorem predict our answer to this question will change as sample size increases?
#   As sample size increases, our sample means should become less variable and be closer to the true mean. 
# 1b) What does it mean to increase the sample size in a simulation?
#   It means to draw more individuals in each of our samples. correct
# 1c) What should be true about our sampling distributions as we increase our sample size?
#   The means should remain about the same, but the standard errors should decrease. correct.
 
# Primary Research Question
# What percentage of the time are college students happy?  How does our estimate of the true mean change as sample size increases?
# Determine the population parameters:
# 1. Visualize the shape of the population data by making a histogram.  
hist(survey$happy)
# 2. Calculate the "true" mean and standard deviation of the population.
mean(survey$happy)
sd(survey$happy)
# 1d) Is it more common for students to have high or low happiness percent scores relative to the range of percent scores in the population?
# High

# Compare the sample statistics:  
# 3. Draw 1,000 samples of size n=5 from the population data.  Calculate the mean of each sample. 
# 4. Graph these 1,000 sample means in a histogram and examine the shape.
# 5. Calculate the mean and standard deviation of the sampling distribution.
n = 5
xbar5 <- rep(NA, 1000)
for (i in 1:1000){
  mysamp <- sample(survey$happy, size = n)
  xbar5[i] <- mean(mysamp)
}
hist(xbar5)
mean(xbar5)
sd(xbar5)
sd(survey$happy)/sqrt(n)
# 6. Repeat this process for samples of size n=15 and n=25.
n = 15
xbar15 <- rep(NA, 1000)
for (i in 1:1000){
  mysamp <- sample(survey$happy, size = n)
  xbar15[i] <- mean(mysamp)
}
hist(xbar15)
mean(xbar15)
sd(xbar15)
sd(survey$happy)/sqrt(n)

n = 25
xbar25 <- rep(NA, 1000)
for (i in 1:1000){
  mysamp <- sample(survey$happy, size = n)
  xbar25[i] <- mean(mysamp)
}
hist(xbar25)
mean(xbar25)
sd(xbar25)
sd(survey$happy)/sqrt(n)
# 7. Compare the results you get to the predictions of the Central Limit Theorem.


# Question 1
# On a scale of 1 to 10, how much do UT Austin students like Austin?
#   1. What are the true mean and standard deviation for our population of UT Austin students?
hist(survey$austin)
mean(survey$austin)
sd(survey$austin)
#   2. What should the sampling distribution of the mean look like, as predicted by the Central Limit Theorem?
sd(survey$austin)/sqrt(10)
#   3. How do our simulated values compare to these predicted values?
# 1e. Simulate drawing 1,000 random samples of sample size n=10 from the "austin" distribution, then create a histogram of the sampling distribution and calculate it's mean and standard deviation. How do these simulated values compare to the those predicted by the Central Limit Theorem?
n = 10
xbar10 <- rep(NA,1000)
for (i in 1:1000){
  mysamp <- sample(survey$austin, n)
  xbar10[i] <- mean(mysamp)
}
hist(xbar10)
mean(xbar10)
sd(xbar10)

