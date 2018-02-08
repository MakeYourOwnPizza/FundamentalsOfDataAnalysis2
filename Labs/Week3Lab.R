# FoDA 2 Week 3 Hypothesis Testing (Two Group Means)
# Tutorial
# Paired sample t test
library(SDSFoundations)
post <- PostSurvey
hist(post$exclusive)
hist(post$post_exclusive)

diff <- post$exclusive - post$post_exclusive
hist(diff)
# No huge outlier, no skewed

t.test(post$exclusive, post$post_exclusive, paired = T)
mean(diff)

t.test(post$exclusive, post$post_exclusive, paired = T, alternative = 'greater')

# Independet t test
fsleep <- post$sleep_Tues[post$gender == 'Female']
msleep <- post$sleep_Tues[post$gender == 'Male']

hist(fsleep)
hist(msleep)
# Both nearly normal

t.test(fsleep,msleep)

# Pre-Lab 3: Post Student-Survey Data
# Primary Research Questions
# 1.  Who is happier at the beginning of the semester:  under-classmen or upper-classmen?
# 2.  Does student happiness change from the beginning of the semester to the end?

# Breakdown Your Analysis
# Let's break this analysis into its required steps:
 
# Question 1:  Independent t-test
# 1. Make a vector of happiness scores for each sample (under- and upper-classmen).
underclass_happy <- post$happy[post$classification == 'Freshman' | post$classification == 'Sophomore']
upperclass_happy <- post$happy[post$classification == 'Junior' | post$classification == 'Senior']

# 2. Generate histograms to check the Normality assumption. 
hist(underclass_happy)
hist(upperclass_happy)
# 3. Run an independent t-test.
t.test(underclass_happy, upperclass_happy)
# 4. Interpret the results.
 
# Question 2:  Dependent t-test
# 1. Make a vector of difference scores for student happiness from the beginning to the end of semester.
post$diff_happy <- post$happy - post$post_happy
# 2. Generate a histogram of the difference scores to check the Normality assumption.
hist(post$diff_happy)
# 3. Run a dependent t-test.
t.test(post$happy, post$post_happy, paired = T)
# 4. Interpret the results.

# Lab 3: Post Student-Survey Data
# Question 1: Do students at UT spend more time on homework per week in college than they did in high school?
dependent t-test
# Question 2: Do students in fraternities and sororities get less sleep on the weekends than other college students?
independent t-test

# Primary Research Questions
# 1. Do students at UT spend more time on homework per week in college than they did in high school?
# 2. Do students in fraternities and sororities get less sleep on the weekends than other college students?
   
# Analysis
# Let's break this question down into the different statistics that you will need to construct your answer.  Be sure that your R output includes all of the following components. 
 
# For each hypothesis test, 
 
# 1. Create vectors of the scores that you wish to analyze.
hw_hours_diff <- post$hw_hours_college - post$hw_hours_HS

# 2. Check the assumption of normality by generating a histogram for each variable of interest. 
hist(hw_hours_diff)
# 3. Find the t-statistic and p-value.
t.test(hw_college, hw_hs, paired = T, alternative = 'greater')
# 4. Interpret the results of each test. 
mean(hw_college) - mean(hw_hs)

# 1. Create vectors of the scores that you wish to analyze.
sleep_greek <- post$sleep_Sat[post$greek == 'yes']
sleep_non_greek <- post$sleep_Sat[post$greek == 'no']
# 2. Check the assumption of normality by generating a histogram for each variable of interest. 
hist(sleep_greek)
hist(sleep_non_greek)
# 3. Find the t-statistic and p-value.
t.test(sleep_greek, sleep_non_greek, alternative = 'less')
# 4. Interpret the results of each test. 
mean(sleep_greek) - mean(sleep_non_greek)

# Question 1
# Is the increase in time spent studying from high school to college the same for nursing majors and biology majors?  
# 1. Create a new variable that equals the difference in hours spent studying per week in college versus high school for each student. 
post$hw_hours_diff <- post$hw_hours_college - post$hw_hours_HS
# 2. Create two vectors of those differences, one for nursing majors and one for biology majors.
hw_nursing <- post$hw_hours_diff[post$major == 'Nursing']
hw_biology <- post$hw_hours_diff[post$major == 'Biology']
# 3. Use this data to answer the following questions.  
# Two-sample independent t test
hist(hw_nursing)
hist(hw_biology)
t.test(hw_nursing, hw_biology)
