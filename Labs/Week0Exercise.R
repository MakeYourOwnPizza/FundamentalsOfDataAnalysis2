x <- 6^2+2
x^2
x <- 6+2
z <- c(2,4,6,8,10)
z[3]

library(SDSFoundations)
BikeData <- read.csv('BikeData')
BikeData[,7]
bike <- BikeData

# Pre Lab
# Primary Research Question
# How many of the cyclists were students, how often did they ride, and what was the average distance they rode?
 
#   Let's break this analysis into its required steps:
 
# 1. Find the number of students in the dataset.
table(bike$student)
# 2. Pull out the student data into a separate dataframe for analysis.
student <- bike[bike$student == 1,]
# 3. Make a table to find how often the students ride.
table(student$cyc_freq)
# 4. Find the average distance ridden.
distance <- student$distance
mean(distance)

# Lab 0
# Primary Research Question
# How many of the daily riders are male, how many are female, and what are their average ages?  

# 1. Make a table to show how many daily riders are in the original dataset.
table(bike$cyc_freq)
# 2. Create a new datafile that includes only the daily riders.
daily <- bike[bike$cyc_freq == 'Daily',]
# 3. Make a table to show the number of male and female daily riders.
table(daily$gender)
# 4. Make a vector of the ages of these daily riders.
age <- daily$age
menage <- daily$age[daily$gender == 'M']
womenage <- daily$age[daily$gender == 'F']
# 5. Find the mean age for men and for women daily riders.
mean(age)
mean(menage)
mean(womenage)


# You are being asked to subset data based on a numerical condition rather than a categorical condition.  
# subset only the male daily riders that are 30 or older.
thirty <- daily[daily$age >= 30 & daily$gender == 'M',]
