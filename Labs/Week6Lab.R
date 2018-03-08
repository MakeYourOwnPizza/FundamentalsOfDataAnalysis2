# FoDA 2 Week 6 Correlation and Regression

# Correlation Testing
clerk <- res[res$Group =='Clerkship',]
names(clerk)
vars <- c("BDI", "Resilience", "State.Anxiety", "Trait.anxiety")
cor(clerk[,vars])

library(psych)
# Check each of the correlation coefficients
corr.test(clerk[,vars])
corr.test(clerk[,vars])$r
corr.test(clerk[,vars])$p
corr.test(clerk[,vars])$t

# Simple Linear Regression
names(clerk)
linFit(x = clerk$Resilience, y = clerk$BDI)

# Linear Regression
# BDI is predicted by Resilience
# Save the linear model into an object: bdi_mod
bdi_mod <- lm(BDI ~ Resilience, data = clerk)
# I can see more info by using summary
summary(bdi_mod)
# See the confident intervals
confint(bdi_mod)
# What is the standardized betas?
install.packages('lm.beta')
library(lm.beta)
lm.beta(bdi_mod)

# Regression Diagnostic Plots
# residual vs. fitted plot, which type == 1
plot(bdi_mod, which = 1)
# Everything is random around low fiited value. There is some tunneling at the low fiited values, no outliers, 

# cook's distance plot
cutoff <- 4/(bdi_mod$df)
# id.n default = 3
plot(bdi_mod, which = 4, levels = cutoff, id.n = 5)
# 105, 824, and 1122 are observations that are influential obs.. They will be removed.

# Multiple Regression
bdi_mult <- lm(BDI ~ Resilience + State.Anxiety + Trait.anxiety, data = clerk)
summary(bdi_mult)

# residual vs. fitted plot, which type == 1
plot(bdi_mult, which = 1)
# some tunneling happening, some outliers

# cook's distance plot
cutoff <- 4/(bdi_mult$df)
# id.n default = 3
plot(bdi_mult, which = 4, levels = cutoff, id.n = 6)

confint(bdi_mult)
lm.beta(bdi_mult)
# Trait.anxiety is the biggest player now

# install.packages("devtools")
# library(devtools)
# install_github("MichaelJMahometa/SDSRegressionR")
# library(SDSRegressionR)
pCorr(bdi_mult)


# Pre-lab 6: Medical School Quality of Life
# In a 2015 study, Tempski and associates examined a measurement they called Quality of Life among medical school students in Brazilian medical schools. They borrowed measurement scales from the World Health Organization, the Dundee Ready Education Environment Scale, and the Beck Depression Inventory to assess the dependent variable in potential relation to a number of predictor variables.
# 
# Primary research questions
# 1) Can you confirm the claim that Beck Depression Inventory score is a significant predictor of Overall Quality of Life among students enrolled in the Clinical Sciences program?
# 2) For students enrolled in the Clinical Sciences program, examine the effects of DREEM: Social Self Perception, DREEM: Academic Self Perception, Resilience, BDI, and Age on Med School Quality of Life? 

# Breakdown Your Analysis
# Let's break this analysis into its required steps:

# 1. Subset out just students in the Clinical Sciences program.
clin <- res[res$Group == 'Clinical Sciences',]
# 2. Run a basic correlation matrix for Research Question 1.
vars <- c('QoL','BDI')
cor(clin[,vars])
# 3. Run the model for Research Question 1 and examine.
ov_mod <- lm(QoL ~ BDI, data = clin)
summary(ov_mod)
confint(ov_mod)
plot(ov_mod, which = 1)
cutoff <- 4/(ov_mod$df)
plot(ov_mod, which = 4, cook.levels = cutoff)


# 1a) What is the inital correlation coefficient between overall Quality of Life and Beck Depression Inventory? (Round to three decimal places.)
summary(ov_mod)
-0.375
# 1b) What is the t-value of the Simple Regression slope for Beck Depression Inventory? (Round to three decimal places.)
-8.935

# 4. Run a basic correlation matrix for Research Question 2.
vars2 <- c("MS.QoL", "DREEM.S.SP", "DREEM.A.SP", "Resilience", "BDI", "Age")
cor(clin[,vars2])
# cor(clin[,vars], use="pairwise.complete.obs")
library(psych)
corr.test(clin[,vars2], use="pairwise.complete.obs")
# 5. Run the model for Research Question 2 and examine.
ms_mod <- lm(MS.QoL ~ DREEM.S.SP + DREEM.A.SP + Resilience + BDI + Age, data = clin)
summary(ms_mod)
confint(ms_mod)
library(car)
vif(ms_mod)
plot(ms_mod, which=1)
cutoff <- 4/(ms_mod$df) 
plot(ms_mod, which=4, cook.levels=cutoff)
# 6. Follow up Research Question 2 with contextual analysis.
lm.beta(ms_mod)
round(pCorr(ms_mod), 4)

# 2a) What is the R-squared value for the overall model? (Report as a percentage, and round to 2 decimal places.)
summary(ms_mod)
33.37
# 2b) What is the Adjusted R-squared for the overall model? (Report as a percentage, and round to 2 decimal places.)
32.69
# 2c) What is the F-value for the overall model? (Round to 2 decimal places.)
48.59


# 3a) The Standardized Beta for DREEM Social Self Perception is (Round to three decimal places):
lm.beta(ms_mod)
0.387
# 3b) This means that as DREEM Social Self Perception increases by ______ standard deviation(s), the outcome of Med School Quality of Life increases by ______(Round to three decimal places):
1
0.387
# 3c) What amount of unique variance can be accounted for in the model by DREEM Social Self Perception? (Report as a percentage and round to two decimal places.)
round(pCorr(ms_mod), 4)
8.23
# 4) According to the results from the full Multiple Regression Model, what is the best predictor of Med School Quality of Life, among Clinical Science students?
DREEM Social Self Perception correct

To answer our primary research questions about Clinical Sciences med students, we conducted two primary tests: a simple linear regression and a multiple linear regression. First, we investigated the claimed effect of Beck Depression Inventory score on Overall Quality of Life. There was a   correct  negative correlation between Beck Depression Inventory score and Overall Quality of Life. The corresponding model showed a   unsubmitted  significant simple slope for Beck Depression Inventory (t(489)=   correct  -8.935 , p<0.05) indicating that as the Beck Depression Inventory score increases, the Overall Quality of Life score  unsubmitted  decreases 
The multiple linear regression examined the impact of DREEM: Social Self Perception, DREEM: Academic Self Perception, Resilience, BDI, and Age on Med School Quality of Life. Overall, the model was   correct  significant (F(5,485)=  correct  48.59 , p<.05), and could account for  correct  33.37% of the variance in the outcome (Adjusted R2=  correct  32.69% ). The best predictor of Med School Quality of Life was  correct  DREEM: Social Self Perception which could account for a unique proportion of variance in the outcome of  correct  8.23% .

# LAB 6: MEDICAL SCHOOL QUALITY OF LIFE
# In this lab, you will use Regression Analysis to answer a question of interest. Let's start by remembering why we use Multiple Linear Regression Analysis.
# 1a. In a Multiple Linear Regression model, not only can we compare overall model fit, but for each predictor we can also find the ______.
# slope and significance correct
# 1b. Which measure, asked for after we run our Multiple Linear Regression model, tells us the unique proportion of variance accounted for by each predictor?
# the part (or semi-partial) correlation squared value correct
# 2. Two of the following questions will be answered in this lab using Linear Regression. Select the questions that can be answered with this method.
# Correct: Of the four measures of Quality of Life (Physical Health, Psychological, Social Relationships, and Environment), which has the greatest impact on Med School Quality of Life?
# Correct: What is the overall proportion of varaince explained by the predictors in the model?
# Wrong: Can program selection group be predicted by Quality of Life and Resilience?

# PRIMARY RESEARCH QUESTIONS
# 1. For students in the Basic Sciences program, of the four measures of Quality of Life (Physical Health, Psychological, Social Relationships, and Environment), which has the greatest impact on Med School Quality of Life?
# 2. What is the overall proportion of variance accounted for by all four scales?
# 
# ANALYSIS
# 
# To answer both lab questions:
# 1. Subset the data for students in the Basic Sciences Program.
bsci <- res[res$Group == 'Basic Sciences',]
# 2. Determine the variables of interest and the outcome variable.
vars3 <- c("MS.QoL","WHOQOL.PH","WHOQOL.PSY","WHOQOL.SOC","WHOQOL.ENV")
cor(bsci[, vars3])
# 3. Run a Multiple Linear Regression.
bs_mod <- lm(MS.QoL ~ WHOQOL.PH + WHOQOL.PSY + WHOQOL.SOC + WHOQOL.ENV, data = bsci)
summary(bs_mod)
plot(bs_mod, which = 1)
cutoff2 <- 4/(bs_mod$df)
plot(bs_mod, which =4, levels = cutoff2)
# 4. Find the Standardized Betas for the coefficients using lmBeta().
lm.beta(bs_mod)
# 5. Use pCorr() to find the partial and part correlations.  
pCorr(bs_mod)  

# Provide the intitial bivariate Pearson Correlation Coefficients for the following variables and the outcome of Med School Quality of Life. Round to 4 decimal places:
cor(bsci[, vars3])
# 1a. Physical Health QoL
0.4606
# 1b. Psychological QoL
0.5085
# 1c. Social Relationships QoL
0.3574
# 1d. Environment QoL
0.4410

# Provide the t-statistic for the predictors in the Multiple Linear Regression for the following variables predicting Med School Quality of Life. Round to 3 decimal places:
summary(bs_mod)  
# 2a. Physical Health QoL
2.455
# 2b. Psychological QoL
4.452
# 2c. Social Relationships QoL
1.291
# 2d. Environment QoL
3.927
# What are the Overall Model statistics for the prediction of Med School Quality of life?
# 3a. Overall Model F-value (Round to 2 decimal places.)
50.91
# 3b. Overall Model R-squared (Round to 4 decimal places)
0.3097
# 4a. Which of the following coefficeints in the model would be considered the best predictor of Med School Quality of Life?
Psychological QoL correct
# 4b. What is the amount of unique proportion of variance accounted for in the prediction of Med School Quality of Life by the best predictor (the correct answer above)?
3.0% correct

# Answer the question and support your answer with statistics:
# Our primary research question investigated the predictive impact 
# of several Quality of Life items (Physical Health, Psychological, 
# Social Relationships, and Environment) on the outcome of Med School 
# Quality of Life score for Basic Sciences enrolled students. The model
# showed a  correct  Significant overall effect (F(4, 454) =   correct
# 50.91 , p<0.05), The four predictor variables accounted for correct 
# 30.97 percent of the variance in the outcome of Med School Quality 
# of Life. The best predictor of Med School Quality of Life was the 
# Psychological QoL scale (t(454) =  unsubmitted  4.452 , p<0.05). 
# As Psychological QoL scale increases one unit, Med School Quality 
# of Life increased by   correct  0.0265 (Standardized beta =  correct
# 0.2723 Although significant, and the best predictor in the model, 
# Psychological QoL could only uniquely account for   correct  3.0% 
# of the variance in the outcome.

# Question 1
# Answer the following research question using multiple regression on the Timpskey et al. dataset:
# For clinical science students, after controlling for Gender and Age, what predictor has a greater impact on BDI score - State or Trait anxiety?
# Use the results to answer the following questions.
vars4 <- c("BDI","Female", "Age","State.Anxiety","Trait.anxiety" )
cor(clin[,vars4])
bdi_cs_mod <- lm(BDI ~ Female + Age + State.Anxiety + Trait.anxiety, data = clin)
summary(bdi_cs_mod)
plot(bdi_cs_mod, which = 1)
cutoff3 <- 4/(bdi_cs_mod$df)
plot(bdi_cs_mod, which =4, levels = cutoff2)
# After running the full model, with all predictors:
# 1a. What was the overall model fit (R-squared value)? Report as a percentage and round to 2 decimal places.
 54.04
# 1b. What was the overall model F-statisic? Round to 1 decimal place.
 142.8
# 1c. How many of the varaibles in the model show a significant effect for the prediction of the BDI score?
2
# 1d. What was the Standardized Beta score for State Anxiety? Round to 3 decimal places.
lm.beta(bdi_cs_mod)
 0.177
# 1e. What was the Standardized Beta score for Trait Anxiety? Round to 3 decimal places.
 0.608
# 1f. What was the proportion of variance that State Anxiety could uniquely account for in the prediction of BDI? Report as a percentage and round to 2 decimal places.
 pCorr(bdi_cs_mod)
 1.70
# 1g. What was the proportion of varaince that Trait Anxiety could uniquely account for in the prediction of BDI? Report as a percentage and round to 2 decimal places.
 19.72
# 1h. After controlling for Age and Gender, what was the best predictor of BDI?
 Trait Anxiety correct