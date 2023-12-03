######################
#load packages
library(tidyverse) #for data manipulation
library(lme4) #for mixed effects models
library(lmerTest) #for p-values in mixed effects models
library(effectsize) #for effect size
library(sjPlot) #for plots
library(report) #for reporting model outputs
library(lmerTest) #for p-values
######################

#In the previous exerciser, our models only included fixed effects. This is problematic, since
#our data involves repeated measures across subjects and items. We need to take this into
#account. We need to construct a new model to allow for by-subject and by-item adjustments to 
#the model parameters (both intercept and slopes). This can be achieved by adding (1+condition|subject) and 
#(1+condition|item), respectively. We will use the lmer function from the lme4 package. We
#have already activated the lmerTest package, which will add p-values to the output of lmer models.

######################

#load data for Lexical Decision Task

design_lexc <-read.csv("design_lexc.csv")
design_lexc$cprof <- design_lexc$prof - mean(design_lexc$prof) #center proficiency and store results in a new variable cprof

model03 <- lmer(RT~condition+cprof+(1+condition|subject)+(1+condition|item), design_lexc)
summary(model03)

#(1)How do you interpret the coefficients for random effects?

######################
#As another example, we will now look at the data for a self-paced reading (SPR) experiment. 
#In an SPR task, participants read sentences or passages presented on a computer screen word by word (or region by region), 
#and they control the pace of the reading themselves.The time taken to read each word (or region) 
#is recorded to reflect comprehension difficulty. In the following example, L1-English participants read 
#subject and object relative clauses. We want to examine if there is a significant RT difference between these two structures,
#while accounting for random effects of subjects and items. Below, we will create a maximal model (including both
#random intercepts and slopes).

######################

groadner_dat <-read.csv("groadner.csv") #load data
head(groadner_dat) #inspect data

model04 <- lmer(rawRT ~ condition + (1+condition|subject) + (1+condition|item), groadner_dat) #build a maximal model
summary(model04)

#(2) How should you deal with the message "boundary (singular) fit: see help('isSingular')"?
#(3) Find the maximal model that converges successfully, and refit the model.

model05 <- lmer(rawRT ~ condition + (.... |subject) + (....|item), groadner_dat) #fill in the gaps
summary(model05)




