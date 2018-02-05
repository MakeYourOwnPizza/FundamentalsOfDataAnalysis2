#Week 2 Tutorials
bull <- read.csv('C:/Users/mengn/OneDrive/Documents/GitHub/FundamentalsOfDataAnalysis2/Labs/BullRiders.csv')

age <- 2014 - bull$YearBorn
age
# Get a quick look of our variable for its distribution, approximately normal
hist(age)

# Do a one sample t test
# t.test(vector of numbers, mean of null hypothesis)
# Two tailed hypothesis
t.test(age, mu = 30)
# One tailed hypothesis, mu_Ha < 30
t.test(age, mu = 30, alternative = 'less')
t.test(age, mu = 30, alternative = 'greater')

# Pre-Lab 2: Bull Rider Data
# Over 1,200 bull riders from around the world are members of Professional Bull Riders (PBR) and compete in the more than 300 PBR affiliated bull riding events per year. This data set includes information about the top 50 ranked bull riders for the 2013, according to the PBR standings reported in July of 2013. Rankings are based on a system which awards points for qualified rides at events throughout the season.

# Primary Research Question
# The average American adult man weighs 190 pounds.  Do professional bull riders from the US weigh the same?

# Which method should we be using for this analysis and why?

# 2a. We will use a one-sample t-test to help us answer this lab question. Why?
# We want to compare the average weight of these bull riders to a claimed value. correct
# 2b. We want to test a hypothesis that professional bull riders weigh 190 pounds on average. What will the null hypothesis look like for this one-sample t-test?
# ??=190 pounds correct
# 2c. The formula to calculate a t-statistic is below. What does the denominator of this test tell us?
# t=(x¯-??)/SE?
# the difference you would expect, based on chance alone correct

# Breakdown Your Analysis

# 1. Create a data frame for the US bull riders, and then calculate the sample mean and standard deviation for the weight of the bull-riders.
USA <- bull[bull$Country == 'USA',]
weight <- USA$Weight
mean(weight)
sd(weight)
# 2. Create a histogram to visualize the distribution of bull-riders' weights.  
hist(weight,main = 'Histogram of Bull Rider Weights', xlab= 'Weight (lbs)')
# 3. Confirm the assumptions of a one-sample t-test
# 4. Run the t-test and interpret the results.
t.test(weight, mu = 190)

# 4. If you wanted to calculate the standard error for this sample of 37 riders, what additional line of code would you need to add?
sd(USA$Weight)/sqrt(37) correct

# 4. The p-value of the test was very small (< 0.05). How should we interpret this p-value?
# If bull-riders really do weigh 190 pounds on average, observing this sample mean is very unlikely. correct


# Lab 2: Bull Rider Data
# Review of the One-Sample t-Test
 
# In this lab, you will use a one-sample t-test to answer a question of interest. Let's start by remembering why we use hypothesis tests.
 
# 1a. What is the goal of a hypothesis test?
# To determine if the sample data is consistent, or inconsistent, with the null hypothesis about the population.
 
# 1b. For your test result to be considered trustworthy, your data must meet the assumptions for a one-sample t-test. Which of the following is not an assumption of this test?
# The data was collected voluntarily from all subjects. (Answer)
# The sample is made up of independent observations.
# The population distribution should be nearly Normal, or the sample should be large.
# A random sample is used.

# 2. One of the following questions will be answered in this lab using a one-sample t-test. Select the question that can be answered with this method.
# Is there a difference in number of buck-offs for riders with and without event wins?
# Is the average ride percentage of professional bull riders at least 50%? correct
# Do professional bull riders with more cup points earn more money?
#   Do older professional bull riders have a higher ride percentage than younger riders?

# Primary Research Question
# Do professional bull riders stay on their bulls 50% of the time? Test the hypothesis that the mean ride percentage is 0.500 in 2014, using riders with at least 5 events in 2014. 
# Analysis
# 1. Select the riders that participated in at least 5 events in 2014.
Riders5 <- bull[bull$Events14 >= 5,]
# 2. Calculate the sample mean and standard deviation of ride percentage in 2014.
RiderPer5 <- Riders5$RidePer14
mean(RiderPer5)
sd(RiderPer5)
# 3. Generate a histogram to look at the distribution of the ride percentage in 2014.
hist(RiderPer5)
# 4. Confirm the assumptions of a one-sample t-test.
# 5. Run the t-test and interpret the results.
t.test(RiderPer5, mu = 0.500)

# Question 1
# How much money do professional bull riders earn by participating in an event?
# 1. Create a new variable that equals the "average earnings per event" in the 2012 season for each bull rider in the dataset. Call this new variable "earnings_per"
bull12 <- bull[bull$Earnings12 > 0,]
bull12$earnings_per <- bull12$Earnings12/bull12$Events12
#earnings_per
# 2. Make a histogram of your "earnings per event" variable.
hist(bull12$earnings_per)
# 3. Use this data to answer the following questions.
# 1a. Have we met the assumptions for being able to calculate a 95% confidence interval to estimate the true mean earnings-per-event for a professional bull rider (using t)? Use the histogram to help answer this question.
# No, the distribution of "earnings_per" is positively skewed, with an outlier correct

# When a variable is highly skewed, we can transform the data into a shape that allows us to conduct our analysis. 
# 1. Create a new variable that is the log of your "earnings_per" variable.  
# 2. Here is the code to make a log transformation of a variable: 
  bull$newvariable <- log(bull$originalvariable)
# 3. Now use this new variable to answer the following questions. 
bull12$earnings_per_log <- log(bull12$earnings_per)
hist(bull12$earnings_per_log)
  
# 1b. Make a histogram of this log-transformed variable. Notice how the distribution shape has changed. Can we reliably calculate a 95% confidence interval for the mean of this transformed variable?
# Yes, the distributuon of the log-transformed variable looks relatively normal (some slight positive skew). correct
mean(bull12$earnings_per_log)
t.test(bull12$earnings_per_log, mu = mean(bull12$earnings_per_log))
exp(8.572169)
exp(9.120605)

# Question 2
# Students collected 8 random bags of a specific brand of potato chips and carefully weighed the contents of each bag, recording the following weights (in grams): 
#  29.4      29.0      28.4      28.8      28.9      29.3      28.5      28.2 
# The students want to test the claim that the mean weight of these bags is 28.5 grams.  They think it may be different. 
chipweight <- c(29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2)
mean(chipweight)
sd(chipweight)
t.test(chipweight, mu = 28.5)


# Question 3
# An industrial plant dumps its waste into a nearby river, but claims that it is not impacting the native species of frogs that live in the river.  The frogs are able to tolerate calcium concentrations up to 91 mg/L.  
# You measure the concentration of calcium in 25 random samples from the river.  Your measurements are approximately normally distributed, with a mean of 93.6 mg/L, with a standard deviation of 7.8 mg/L.  

# 3b. Calculate the test statistic. (Round to 2 decimal places.)
1.67
# 3c. What is the t-critical value? Assume an alpha level of .05.(Round to 3 decimal places.)
1.711 # Read from t-table
# 3e. Suppose as part of a broader investigation into the plant's impact on the river's ecosystem, an environmental group conducted a large-scale study and found that the actual mean calcium concentration level downstream from the plant is 95 mg/L. Did you make an error in your hypothesis test, and if so, what type was it?
# Yes, a Type II Error correct

# Question 4
# You are studying a population of peregrine falcons and want to estimate their average wingspan.  So you collect a random sample of 12 adult male birds and measure a mean wingspan of 42.6 cm, with a standard deviation of 5.3 cm. 
# Assume that the distribution of measurements was approximately normal.

# 4a. What is t-critical for a 90% confidence interval? (Report as a positive value rounded to 3 decimal places.)
t <- 1.796 #correct  (t-table)
# 4b. Calculate a 90% confidence interval for the mean wingspan for the population of male peregrine falcons. (Round to 2 decimal places.)
mean <- 42.6
sd <- 5.3
n <- 12
LB <- mean - t*sd/sqrt(n)
UB <- mean + t*sd/sqrt(n)
