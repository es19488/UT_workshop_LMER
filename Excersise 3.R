######################
#load packages
library(tidyverse) #for data manipulation
library(lme4) #for mixed effects models
library(lmerTest) #for p-values in mixed effects models
library(effectsize) #for effect size
library(sjPlot) #for plots
library(report) #for reporting model outputs
######################
#As the final example, we are going to look at another case of SPR data. The focus of
#the experiment was to examine relative clause ambiguous structures, e.g.,
#the assistant of the pharmacists who was preparing the medicine* --> DP1 attachment
#the assistants of the pharmacist who was preparing the medicine* --> DP2 attachment
#This was a 2-by-2 study, crossing attachment (DP1, DP2) and context (DP1-supporting, DP2-supporting).
#Our question is whether reaction times (RTs) are different in DP1-attachment and
#DP2-attachment sentences, and whether this is influenced by context. Below we create a model
#to examine the interaction between attachment and context, while controlling for by-subject and
#by-item variability around the intercept.
######################

RC_amb <-read.csv("context_amb.csv") #load data
head(RC_amb) #inspect data

model05 <- lmer(RT ~ attachment*context+(1|subject)+ (1|item), RC_amb) #model to examine interaction
summary(model05)

#(1) What is the right way to interpret the intercept 390.560 ?
#(2) How do you interpret the rest of the fixed effects table,
#attachmentDP2, contextDP2, and attachmentDP2:contextDP2?
#(3) Does the above model allow us to address our research question (interaction attachment and context)?
######################

#Below we set sum contrasts to attachment and context before refitting the same model.
#This allows us to examine main and interaction effects as in a typical ANOVA analysis.

RC_amb$attachment <-as.factor(RC_amb$attachment) #instruct R that attachment is a categorical (or factor) variable
contrasts(RC_amb$attachment) <- c(-0.5,0.5)
contrasts(RC_amb$attachment) #DP2 - DP1 (note the signs for DP1 and DP2 in contrast the matrix)

RC_amb$context <-as.factor(RC_amb$context) #instruct R that context is a categorical (or factor) variable
contrasts(RC_amb$context) <- c(-0.5,0.5)
contrasts(RC_amb$context) #DP2 - DP1 (note the signs for DP1 and DP2 in contrast the matrix)

######################

model06 <- lmer(RT ~ attachment*context+(1|subject)+ (1|item), RC_amb)
summary(model06)

#(4)What is the right way to interpret the intercept 398.427?
#(5)How do you interpret the estimate for context1 -28.149 ?
#(6)How do you interpret the estimate for attachment1:context1 -48.761?
#(7) Does model06 allow us to address our research question? How is this different from model05 above?

plot_model(model06, type = "eff" ,terms = c("context", "attachment")) #plot to show interaction


######################
#Finally, given the significant interaction in model06, we want to create two additional models to examine
#how attachment is affected in a DP1-supporting context (model07) and how attachment is affected in a
#DP2-supporting context (model08)

RC_amb_DP1 <- RC_amb %>% filter(context=="DP1") #create a new dataset that includes only DP1-supporting context items
model07 <- lmer(RT ~ attachment+(1|subject)+ (1|item), RC_amb_DP1) #fit a new model to examine attachment in a DP1-supporting context
summary(model07)

#(8) Is there a significant RT difference in a DP1-supprting context between DP1 and DP2 attachment?

RC_amb_DP2 <- RC_amb %>% filter(context=="DP2") #create a new dataset that includes only DP2-supporting context items
model08 <- lmer(RT ~ attachment+(1|subject)+ (1|item), RC_amb_DP2) #fit a new model to examine attachment in a DP2-supporting context
summary(model08)

#(9) Is there a significant RT difference in a DP2-supprting context between DP1 and DP2 attachment?

