######################
#load packages
library(tidyverse) #for data manipulation
library(lme4) #for mixed effects models
library(lmerTest) #for p-values in mixed effects models
library(effectsize) #for effect size
library(sjPlot) #for plots
library(report) #for reporting model outputs
######################

#In the first example, we are going to import data from a lexical decision task.
#Participants were presented with a series of letter strings, and they had to quickly decide 
#whether each string forms a valid word or not, while their response accuracy and reaction times (RTs)
#were being recorded. Below we only focus on reaction times. We are interested
#in the potential effects of L2 proficiency and whether L2 speakers' RTs to words and nonwords were significantly
#different. 

######################

#load data for Lexical Decision Task

design_lexc <-read.csv("design_lexc.csv")
head(design_lexc) #inspect data

model00<-lm(RT~prof, design_lexc) #a linear regression model to examine the effect of proficiency on reaction times (RT)
summary(model00) #to inspect the model output

#(1) How do you interpret the intercept?
#(2) What is the right way to interpret the estimate for prof (=-15.8751)
#(3) What does Std. Error show?
#(4) How is the t-value calculated based on Estimate and Std. Error?
#(5) Is there a significant effect of proficiency?

######################

#To help with the interpretation, it is a good idea to center continuous predictors before entering them
#into a regression analysis. This involves subtracting the mean of a predictor variable from each individual
#score of that variable.

######################

design_lexc$cprof <- design_lexc$prof - mean(design_lexc$prof) #center proficiency and store results in a new variable cprof
model01<-lm(RT~cprof, design_lexc) #refit the same model but with centred proficiency as the predictor
summary(model01)

#(6) Compare the output of model00 and model01. What is the difference?
#(7) What is the right way to interpret the intercept 445.3355?
#(8) Is there a significant effect of proficiency?

######################
#In a multiple regression analysis, we typically deal with more than one predictor. Let's add
#condition into the model formula. This helps us answer the question of whether there is a
#significant RT difference between words and nonwords.

model02<-lm(RT~condition+cprof, design_lexc)
summary(model02)

#(9) What is the right way to interpret the intercept 473.715?
#(10) Is there a significant effect of proficiency?
#(11) How do you interpret the estimate of -56.759 for the slope term conditionword?
#(12) Is there a significant RT difference between words and nonwords?

effectsize(model02) #to obtain Cohen's d effect size, a measure of the standardised difference between
#two means, indicating the size of the effect.

report(model02) #this helps with the reporting of the model -- you may want to customise this to your own needs/preferences




