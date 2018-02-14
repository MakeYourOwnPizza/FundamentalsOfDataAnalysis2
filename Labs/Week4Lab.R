# # FoDA 2 Week 4 Hypothesis Testing (Categorical Data)
# Tutorial
# Table Proportions
library(SDSFoundations)
acl <- AustinCityLimits

# Review Proportion Calculations
# Create a table for the counts of grammy winners
gtab <- table(acl$Grammy)
# Calculate the proportion in each category
prop.table(gtab)

# Create a contingency table with Grammy winners and gender
gtab2 <- table(acl$Grammy, acl$Gender)
gtab2
# See proportion in certain category
prop.table(gtab2)
# Conditional proportion across each row
prop.table(gtab2,1)
# Conditional proportion across each colume
prop.table(gtab2,2)

# Review Grouped Bar charts
# Visualize one variable
barplot(gtab, main = 'ACL Grammy Winners', xlab = 'Grammy Winner', ylab = 'Counts')
# Visualize two variables in stacked bar chart
barplot(gtab2, legend = T, main = 'Gender by Grammy Winners', xlab = 'Gender', ylab = 'Counts')
# Visualize two variables in side-by-side bar chart
barplot(gtab2, legend = T, beside = T, main = 'Gender by Grammy Winners', xlab = 'Gender', ylab = 'Counts')
# Relative Frequency stacked bar chart (aka. mosaic bar chart)
barplot(prop.table(gtab2,2), legend = T, main = 'Gender by Grammy Winners', xlab = 'Gender', ylab = 'Proportion')

# Chi-Squared Goodness of Fit Test
# H0: one third of acl has won Grammy, N category, Y category
gtab
claimp <- c(2/3, 1/3)
chisq.test(gtab, p = claimp)
# See expected count in each category
chisq.test(gtab, p = claimp)$expected

# Chi-Squared Test of Independence
grammyage <- table(acl$Grammy, acl$Age.Group)
grammyage
# check the count of each expected value
chisq.test(grammyage)$expected
# by dafult, R will apply a correction for not meeting an assumption,
# But we dont need it here since we already met the assumptions
chisq.test(grammyage, correct = F)


# Pre-Lab 4: Austin City Limits
# Primary Research Questions
# 1. Are there an equal number of male and female performers on Austin City Limits?
# 2. Are male performers just as likely to have had a Top 10 hit as female performers?
   
# 3a. We will use a Chi Square Goodness of Fit test to check whether there were an equal number of male and female performers. Why?
# We want to see if the distribution of a categorical variable matches a proposed distribution model. correct
# 3b. We will use a Chi Square Test of Independence to determine if male and female performers were equally likely to have had a Top 10 hit. Why?
# We want to determine if there is an association between two categorical variables.   

# Breakdown Your Analysis
# Let's break this analysis into its required steps:
 
# Goodness of Fit Test:
# 1. Make a table of counts for gender.
gender_tab <- table(acl$Gender)
gender_tab
# 2. Create a vector of the expected proportions.
ExpGender <- c(.5, .5)
# 3. Check the expected counts assumption.
chisq.test(gender_tab, p = ExpGender)$expected
# 4. Run the chi square test.
chisq.test(gender_tab, p = ExpGender)
# 5. Interpret the chi square statistic and p-value.
X-squared = 18.241, df = 1, p-value = 1.946e-05

# Test of Independence:
# 1. Create a two-way table for gender and Top 10 hits.
gender_top10 <- table(acl$Gender, acl$BB.wk.top10)
gender_top10
# 2. Check the expected counts assumption.
chisq.test(gender_top10)$expected
# 3. Run the chi square test.
chisq.test(gender_top10, correct = F)
# 4. Interpret the chi square statistic and p-value.
X-squared = 0.70023, df = 1, p-value = 0.4027

# Answer the question and support your answer with statistics:

# First we examined whether there were an equal number of 
# male and female artists on Austin City Limits. In our 
# sample, there were 81   correct  males and 35   correct 
# females . A chi square   correct  goodness of fit test 
# showed that this difference  correct  was statistically 
# significant (chi square=   correct 18.24 df=1, p<.05). 
# There are more   correct  males than  correct  females 
# on the show. Second, we asked whether male and female 
# artists were equally likely to have had a Top 10 hit. 
# Approximately 55% of the   correct  female artists had 
# a Top 10 hit, and 46% of the   correct  male artists 
# had a Top 10 hit. This difference  correct  was not 
# statistically significant. A chi square test of 
# independence found top 10 hits to be independent of 
# gender (chi square= 0.700, df=1, p=  correct  0.403 ). 
# The assumptions for each test   correct  were met.


# Lab 4: Austin City Limits
# Review of Chi Square Tests
# In this lab, you will use Chi Square Tests to answer a question of interest. Let's start by remembering why we use each Chi Square test.

# 1a. In a Chi Square Goodness of Fit test, a proposed distribution model is compared to an observed
# marginal distribution. correct
 
# 1b. Two categorical variables are said to be independent if their conditional distribution matches
# the distribution of expected counts, when the variables are assumed not to be related. correct

# 2a. Are each of the four musical genres equally represented on Austin City Limits?
# goodness of fit test
# 2b. Are some genres more likely to draw a large (100K+) Twitter following than others?
# test of independence

Analysis
Let's break this question down into the different descriptive statistics that you will need to construct your answer.  Be sure that your R output includes all of the following components. 


# Goodness of Fit Test:
# 1. Create a table to show the counts of each genre.
genre_tab <- table(acl$Genre)
genre_tab
# 2. Create a vector of expected proportions.
ExpGenre <- c(.25,.25,.25,.25)
# 3. Check the expected counts assumption.
chisq.test(genre_tab, p = ExpGenre)$expected
# 4. Run a chi square test.
chisq.test(genre_tab, p = ExpGenre)
# 5. Interpret the chi square statistic and p-value.
X-squared = 70.414, df = 3, p-value = 3.481e-15
reject H0, it is not equally represented
 
# Test of Independence:
# 1. Create a two-way table for genre and Twitter following.
genre_twitter100k <- table(acl$Genre, acl$Twitter.100k)
genre_twitter100k
# 2. Check the expected counts assumption.
chisq.test(genre_twitter100k)$expected
# 3. Run a chi square test.
chisq.test(genre_twitter100k)
chisq.test(genre_twitter100k, correct = F)
# 4. Interpret the chi square statistic and p-value.
both: X-squared = 5.6919, df = 3, p-value = 0.1276
fail to reject H0, they are independent
# 2a. Using the data from your two-way table, compute the proportion of artists in each genre with 100K+ Twitter followers. (Round to 3 decimal places).
prop.table(genre_twitter100k, 1)

# Question 1
# You want to know if the proportion of female performers on Austin City Limits Live has changed in the past two years. 
# 1. Create a new variable in the dataset called "Recent" that is equal to a 1 for rows from years 2012 or 2013 and is equal to 0 for all other rows.
# 2. Make a table that shows the number of male and female performers in "recent" and non-recent years.
# 3. Use this data to answer the following questions.
acl$Recent[acl$Year < 2012] <- 0
acl$Recent[acl$Year >= 2012] <- 1

# 1a. How many female performers have been on the show in the past two years (2012 and 2013)?
recent_gender <- table(acl$Recent, acl$Gender)  
# 1b. What is the appropriate method to test if representation by female performers is different before 2012 compared to since 2012?
Chi-Square Test of Independence correct
# 1c. Report expected counts for the following performer groups.
# Females before 2012
chisq.test(recent_gender)$expected
# 1d. What is the Chi Square statistic?
chisq.test(recent_gender, correct = F)
X-squared = 2.8188, df = 1, p-value = 0.09317
