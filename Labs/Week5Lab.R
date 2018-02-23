# FoDA 2 Week 5 Hypothesis Testing (More Than Two Group Means)
# boxplot
library(SDSFoundations)
film <- FilmData

# stats for the boxplot
fivenum(film$Days)
boxplot(film$Days, main = 'Days in Theaters', ylab = 'Days', xlab = 'All Films')

# We can see if its normally distributed
hist(film$Days)

# Create a grouped box plot by a categorical variable
# To check if the distribution at each group is symmetric, and the spread is roughly even.
boxplot(film$Days~film$Genre, main = 'Days in Theaters', ylab = 'Days', xlab = 'Genre')

# One-Way ANOVA
# takes a numeric variable as a function of a categoricalvariable,
# and produce a certain descriptive statistic for every single group.
#aggregate(numeric variable~categorical variable, data, function)
# We can see if the mean and the standard deviation of days at each genre is approximately equal.
aggregate(Days~Genre, film, mean)
aggregate(Days~Genre, film, sd)

daysmodel <- aov(film$Days~film$Genre)
summary(daysmodel)
TukeyHSD(daysmodel)


# Pre-Lab 5: Top Grossing Films
# Primary Research Questions
# 1. Does a film's rating (PG, PG-13, or R) impact its cost to produce?
# 2. Does a film's rating (PG, PG-13, or R) influence its IMDB score?
# 
# 3a. We will use ANOVA to help us answer each of these questions. Why?
# We want to determine if the category to which a film belongs has an impact on some other quantitative measure. correct
# 3b. We will conduct post-hoc tests, specifically Tukey's HSD, if the result of either ANOVA is significant. Why?
# We want to locate which group means are different from each other. correct
  
# Breakdown Your Analysis

# For each ANOVA:
# 1. Identify the number of films in each rating group (PG, PG-13, R). 
table(film$Rating)
# 2. Compute the mean and standard deviation of the variable of interest for each group.
aggregate(Budget~Rating, film, mean)
aggregate(Budget~Rating, film, sd)
# 3. Create boxplots to help visualize group differences and check test assumptions.
boxplot(film$Budget~film$Rating, main = "Film Budgets by Rating",
        ylab= "Budget", xlab= "MPAA Rating")
# 4. Run ANOVA.
modelbud <- aov(film$Budget~film$Rating)
summary(modelbud)
# 5. If the F statistic is significant, run a Tukey HSD test to determine which groups are different.
TukeyHSD(modelbud)

Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = film$Budget ~ film$Rating)

$`film$Rating`
diff       lwr        upr     p adj
PG13-PG  51.99755  27.12917 76.8659209 0.0000060
R-PG     17.95234 -19.99271 55.8973842 0.5031892
R-PG13  -34.04521 -68.45570  0.3652895 0.0531681

# In a Tukey HSD test,   PG-13   films were shown to cost significantly more on average ($127M) than PG films ($   97   M). PG-13 films fell just shy of costing significantly more than   R   rated films (p=.053).

# 2d. The average budget for a PG-13 film was significantly higher than the budget for a(n) ________ rated film.
# PG correct
# 2e. The difference between a PG-13 film and a(n) ________ rated film was almost, but not quite significant, at p = 0.053.
# R correct

# Calculate avg IMDB score of each group
aggregate(IMDB~Rating,film,mean)

#Calculate sd of IMDB scores within each group
aggregate(IMDB~Rating,film,sd)

# Visualize the group means and variability
boxplot(film$IMDB~film$Rating, main= "IMDB Scores by Rating",
        ylab= "IMDB Score", xlab= "MPAA Rating")

# Run ANOVA
modelscore <- aov(film$IMDB~film$Rating)
summary(modelscore)

# Run post-hod text if F statistic is significant
TukeyHSD(modelscore)

# 2. Which of the following comes closest to what it sounds like to "read aloud" this line of code?
#   aggregate(Budget~Rating,film,mean)
# For all the cases in film, take the variable Budget and, given the Rating group, find the mean correct

# 3. If group differences are present, what should be true about the output of this line of code?
#   aggregate(Budget~Rating,film,mean)
# The average budget for each group should be different. correct

# 4. If we are to satisfy the assumptions of ANOVA, what should be true about the output of this line of code?
#   aggregate(Budget~Rating,film,sd)
# The largest standard deviation should be no more than twice the smallest standard deviation. correct

# 5. Suppose we wanted to test if each type of Genre had the same level of Ratings. What has caused the error below? (You may want to refer to the dataset in R for help.)
# 
# film <- FilmData
# modelRating <- aov(film$Rating~film$Genre)
# 
# Warning messages:
# 1: In model.response(mf, "numeric") : using type = "numeric" with a factor response will be ignored
# 2: In Ops.factor(y, z$residuals) : - not meaningful for factors
# 
# We should have run a Chi Square Test of Independence. correct

# Lab 5: Top Grossing Films
# In this lab, you will use ANOVA to answer a question of interest. Let's start by remembering why we use ANOVA.
# 1a. What is the goal of an ANOVA analysis?
# to determine if significant mean differences exist between multiple groups correct
# 1b. Two specific group means can be said to be significantly different if:
# a Tukey HSD pairwise comparison shows p < 0.05 (or the identified level of significance) correct

# 2. Two of the following questions will be answered in this lab using ANOVA. Select the questions that can be answered with this method.
# Which studio(s) earn a greater percentage of their earnings domestically? correct
# Which studio(s) are more successful in keeping their films in the theaters longer? correct
# 
# Primary Research Questions
# 1. Are some studios more successful in keeping their films in the theaters longer?
# 2. Do some studios earn a greater percentage of their earnings domestically than others?
#   
# Analysis
# 
# For each lab question:
#   
# 1. Identify the number of films in each studio group.
table(film$Studio)
# 2. Find the mean and standard deviation of the variable of interest for each group.
aggregate(Days~Studio, film, mean)
aggregate(Days~Studio, film, sd)
# 3. Create boxplots to help visualize group differences and check test assumptions.
boxplot(film$Days~film$studio)
# 4. Run ANOVA.
daysaov <- aov(Days~Studio, film)
summary(daysaov)
# 5. If the F statistic is significant, run a Tukey HSD test to determine which groups are different.
TukeyHSD(daysaov)

# 1. Identify the number of films in each studio group.
table(film$Studio)
# 2. Find the mean and standard deviation of the variable of interest for each group.
aggregate(Pct.Dom~Studio, film, mean)
aggregate(Pct.Dom~Studio, film, sd)
# 3. Create boxplots to help visualize group differences and check test assumptions.
boxplot(film$Pct.Dom~film$Studio)
# 4. Run ANOVA.
pctdomaov <- aov(Pct.Dom~Studio, film)
summary(pctdomaov)
# 5. If the F statistic is significant, run a Tukey HSD test to determine which groups are different.
TukeyHSD(pctdomaov)


# Question 1
# Do low-budget movies make a different percentage of their profits domestically than movies with medium- or high-budgets? 
# 1. Suppose films with a budget less than $100 Million are considered "low-budget"; films with a budget of $100 million up to but not including 150 Million are considered "medium-budget", and those with a budget of $150 million or more are "high-budget."  
# 2. Create a new categorical variable in the dataset that defines each film under these criteria.
# 3. Use this data to answer the following questions.
film <- FilmData
film$BudgetLevel[film$Budget < 100] <- "low-budget"
film$BudgetLevel[film$Budget >= 100 & film$Budget < 150] <- "medium-budget"
film$BudgetLevel[film$Budget >= 150] <- "high-budget"

# 1a. How many films fall into each of the budget categories?
table(film$BudgetLevel)
# 1b. Calculate the mean percent domestic for each group 
aggregate(Pct.Dom~BudgetLevel, film, mean)
# Create a boxplot to check the assumptions of ANOVA.  Then run an ANOVA and report your findings below.
boxplot(film$Pct.Dom~film$BudgetLevel)
summary(aov(film$Pct.Dom~film$BudgetLevel))
# 1g. Run a Tukey HSD post-hoc analysis and chose the correct adjusted p-values for each pairwise comparison listed below.
TukeyHSD((aov(film$Pct.Dom~film$BudgetLevel)))
# 1h. What is the appropriate conclusion for the post-hoc analysis?
# Low-budget films earn a greater percentage of their profit domestically than either medium-budget or high-budget films. correct
