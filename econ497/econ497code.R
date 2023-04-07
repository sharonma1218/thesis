#-----------------------------------
# Econ 497
# Honors Thesis 
# By Sharon Ma
# Updated: April 7th, 2023 at 5 PM
#------------------------------------

#---------------
# Set Directory 
#---------------
setwd("~/Downloads")
# install.packages("tidyverse")
library(tidyverse)
# install.packages("stargazer")
library(stargazer)

#--------------
# Import Data
#--------------
orig_survey<-read.csv("orig_survey.csv",encoding="UTF-8")

#---------------
# Create Groups
#---------------

# Subset non/first gen groups 

first_gen_group<-orig_survey%>%
  filter(Q34==1) # n=25; matches w var

non_first_gen_group<-orig_survey%>%
  filter(Q34==3) # n=93; matches w var

# Subset treatment groups 

treatment_group<-orig_survey%>%
  filter((Q8==1)|(Q8==2)|(Q8==3)|(Q8==4)|(Q8==5)|(Q8==6)|(Q8==7))# n=60

control_group<-orig_survey%>%
  filter(Q20_1>=0) # n=69

test<-orig_survey%>%
  filter(Q20_1>=0 & Q20_1<=100)

# Subset non/first & treatment groups

treated_first_gen<-orig_survey%>%
  filter((Q8==1)|(Q8==2)|(Q8==3)|(Q8==4)|(Q8==5)|(Q8==6)|(Q8==7))%>%
  filter(Q34==1) # n=14 

control_first_gen<-orig_survey%>%
  filter(Q20_1>=0)%>%
  filter(Q34==1) # n=11; numbers add up & matches w var

treated_non_gen<-orig_survey%>%
  filter((Q8==1)|(Q8==2)|(Q8==3)|(Q8==4)|(Q8==5)|(Q8==6)|(Q8==7))%>%
  filter(Q34==3) # n=45 

control_non_gen<-orig_survey%>%
  filter(Q20_1>=0)%>%
  filter(Q34==3) # n=48; numbers add up & matches w var

# Create variables

mod_survey<-orig_survey%>%
  mutate(first_gen_status=
           case_when(
             Q34==1 ~ 1, # 1 if first gen 
             Q34==3 ~ 0), # 0 if non first gen 
         treatment_status=
           case_when(
             ((Q8==1)|(Q8==2)|(Q8==3)|(Q8==4)|(Q8==5)|(Q8==6)|(Q8==7)) ~ 1, # 1 if treated
             (Q20_1>=0) ~ 0 # 0 if control
           ))

#---------------
# Patience Game
#---------------

# Create variable

mod_survey<-mod_survey%>%
  mutate(patience=
           case_when(
             (Q26_1==1)&(Q26_2==1)&(Q26_3==1)&(Q26_4==1)&(Q26_5==1)~1,
             (Q26_1==1)&(Q26_2==1)&(Q26_3==1)&(Q26_4==1)&(Q26_5==2)~2,
             (Q26_1==1)&(Q26_2==1)&(Q26_3==1)&(Q26_4==2)&(Q26_5==2)~3,
             (Q26_1==1)&(Q26_2==1)&(Q26_3==2)&(Q26_4==2)&(Q26_5==2)~4,
             (Q26_1==1)&(Q26_2==2)&(Q26_3==2)&(Q26_4==2)&(Q26_5==2)~5,
             (Q26_1==2)&(Q26_2==2)&(Q26_3==2)&(Q26_4==2)&(Q26_5==2)~6
           ))

# Summaries

mod_survey%>%
  filter(!is.na(patience) & !is.na(first_gen_status))%>%
  group_by(first_gen_status)%>%
  summarize(mean(patience))

prop.test(c(5.48,5.68),c(93,25))

mod_survey%>%
  filter(!is.na(patience))%>%
  group_by(treatment_status)%>%
  summarize(mean(patience))

prop.test(c(5.57,5.43),c(93,25))

# Regression

patience_reg<-lm(patience~first_gen_status+treatment_status+(first_gen_status*treatment_status),data=mod_survey)
summary(patience_reg)
stargazer(patience_reg)

#-------------
# Risk Games
#-------------

# Variables 

mod_survey<-mod_survey%>%
  mutate(risk1=
           case_when(
             Q27==1 ~ 1,
             Q27==2 ~ 2,
             Q27==3 ~ 3,
             Q27==4 ~ 4
           ))

mod_survey<-mod_survey%>%
  mutate(risk2=
           case_when(
             Q28==1 ~ 6,
             Q28==2 ~ 5,
             Q28==3 ~ 4,
             Q28==4 ~ 3,
             Q28==5 ~ 2,
             Q28==6 ~ 1,
           ))

# Summaries 

mod_survey%>%
  filter(!is.na(first_gen_status))%>%
  group_by(first_gen_status)%>%
  summarize(mean(risk1))

prop.test(c(2.83,2.88),c(93,25))

mod_survey%>%
  filter(!is.na(first_gen_status))%>%
  group_by(first_gen_status)%>%
  summarize(mean(risk2))

prop.test(c(1.38,1.8),c(93,25))

# Regression

risk1_reg<-lm(risk1~first_gen_status+treatment_status+(first_gen_status*treatment_status),data=mod_survey)
summary(risk1_reg)
stargazer(risk1_reg)

risk2_reg<-lm(risk2~first_gen_status+treatment_status+(first_gen_status*treatment_status),data=mod_survey)
summary(risk2_reg)
stargazer(risk2_reg)

# Aggregate

mod_survey%>%
  filter(!is.na(first_gen_status))%>%
  mutate(agg_risk=risk1+risk2)%>%
  group_by(first_gen_status)%>%
  summarize(mean(agg_risk))

prop.test(c(4.20,4.68),c(93,25)) # there may be smth here 

temp_mod_survey<-mod_survey%>%
  filter(!is.na(first_gen_status))%>%
  mutate(agg_risk=risk1+risk2)

agg_risk_reg<-lm(agg_risk~first_gen_status+treatment_status+(first_gen_status*treatment_status),data=temp_mod_survey)
summary(agg_risk_reg)
stargazer(agg_risk_reg)

#------------------
# Confidence Games
#------------------

# Summaries

mod_survey%>%
  filter(!is.na(Q29_1),!is.na(first_gen_status))%>%
  group_by(first_gen_status)%>%
  summarize(mean(as.numeric(Q29_1)))

prop.test(c(1.34,1.08),c(93,25))

mod_survey%>%
  filter(!is.na(Q30_1),!is.na(first_gen_status))%>%
  group_by(first_gen_status)%>%
  summarize(mean(as.numeric(Q30_1)))

prop.test(c(0.677,0.52),c(93,25))

# Regressions 

confidence1_reg<-lm(Q29_1~first_gen_status+treatment_status+(first_gen_status*treatment_status),data=mod_survey)
summary(confidence1_reg)
stargazer(confidence1_reg)

confidence2_reg<-lm(Q30_1~first_gen_status+treatment_status+(first_gen_status*treatment_status),data=mod_survey)
summary(confidence2_reg)
stargazer(confidence2_reg)

# Aggregate 

mod_survey%>%
  filter(!is.na(Q29_1),!is.na(Q30_1),!is.na(Q31_1),!is.na(first_gen_status))%>%
  mutate(Q29_1=as.numeric(Q29_1),
         Q30_1=as.numeric(Q30_1),
         agg_conf=Q29_1+Q30_1)%>%
  group_by(first_gen_status)%>%
  summarize(mean(agg_conf))

prop.test(c(2.02,1.6),c(93,25))

agg_conf_reg<-lm(agg_conf~first_gen_status+treatment_status+(first_gen_status*treatment_status),data=temp_mod_survey)
summary(agg_conf_reg)
stargazer(agg_conf_reg)

#-----------------
# Competitiveness
#-----------------

mod_survey%>%
  filter(!is.na(Q31_1),!is.na(first_gen_status))%>%
  group_by(first_gen_status)%>%
  summarize(mean(as.numeric(Q31_1)))

prop.test(c(5.78,6.08),c(93,25)) # there may be smth here 

confidence3_reg<-lm(Q31_1~first_gen_status+treatment_status+(first_gen_status*treatment_status),data=mod_survey)
summary(confidence3_reg)
stargazer(confidence3_reg)

#--------
# Exp 1 
#--------

# mean first gen 4.090909 
# mean non first gen 4.361582

# 44 first gen 
# 177 non first gen 

prop.test(c(4.090909,4.361582),c(44,177))

prop.test(c(4.435,3.714),c(44,44),alternative="greater")
