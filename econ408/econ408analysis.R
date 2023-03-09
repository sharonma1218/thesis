#-------------------------
# Econ 408 Final Project 
# Version 01_sharon
#-------------------------

#--------------------
# 1a: Set Directory 
#--------------------
#setwd("~/Downloads")
library(tidyverse)
library(tidyr)
library(haven) #to import data 
library(dplyr) #to filter 
library(ggplot2)
library(stargazer)
library(lemon)
library(knitr)
library(kableExtra)
library("ggpubr")

#------------------
# 1b: Import Data & Clean
#------------------
survey<-read.csv("survey.csv",encoding="UTF-8")

#Remove extraneous columns and rows
survey <-  survey[ -c(1:17) ]
survey<- survey[-2,]

survey_w_question <- survey
survey <-survey[-1,]

#factorize variables
survey[, c(2:4,6,7,9,10,14:35,38)] <- lapply(survey[, c(2:4,6,7,9,10,14:35,38)], as.factor)

#convert continuous variables to numeric
survey[, c(8,36:37,39:45)] <- lapply(survey[, c(8,36:37,39:45)], as.numeric)

#remove responses where they did not disclose their first gen status
n_removed <- survey %>% filter(Q20.1=="") %>% nrow()
survey <- survey %>% filter(!Q20.1=="", !Q3=="")

table(survey$student_status)
table(survey$student_group)

#-------------------
# Part 1c: Subsets 
#-------------------

prop.table(table(survey$Q7,survey$Q20.1))
table(survey$Q7, survey$Q8)


# Make a variable name for treatment/control

survey <- survey %>% mutate(group=ifelse(Q16=="A lot of Support"|
                                           Q16=="Little Support"|
                                           Q16=="Moderate Support"|
                                           Q16=="None","Treatment", "Control")) %>% mutate(student_status=ifelse(Q20.1=="Yes","First-gen","Not First-gen"))
survey$student_group <- as.factor(paste(survey$group, survey$student_status))

# Create the treatment group (the group of first-gen students who were primed):
# Those who said yes to q20.1
# AND those who answered q16
primed_first_gen<-filter(survey,Q20.1=="Yes" & (Q16=="A lot of Support"|
                                                  Q16=="Little Support"|
                                                  Q16=="Moderate Support"|
                                                  Q16=="None"))

# Create the control group (the group of first-gen students who were NOT primed):
# Those who said yes to q20.1
# AND those who answered q18
control_first_gen<-filter(survey,Q20.1=="Yes" & (Q19=="Duderstadt"|Q19=="Hatcher"|
                                                   Q19=="Law"|Q19=="Shapiro"))

# Check: total obs of groups above approx add up to total obs for following: 
first_gen<-filter(survey,Q20.1=="Yes")



# Create the primed non-first-gen group: 
# Those who said no to q20.1
# AND those who answered q16
primed_non_first_gen<-filter(survey,Q20.1=="No" & (Q16=="A lot of Support"|
                                                     Q16=="Little Support"|
                                                     Q16=="Moderate Support"|
                                                     Q16=="None"))

# Create the non-primed non-first-gen group: 
# Those who said no to q20.1
# AND those who answered q18
control_non_first_gen<-filter(survey,Q20.1=="No" & (Q19=="Duderstadt"|Q19=="Hatcher"|
                                                      Q19=="Law"|Q19=="Shapiro"))

# Check: total obs of groups above approx add up to total obs for following:
non_first_gen<-filter(survey,Q20.1=="No")

#-------------------
# 1d: Summary Stats
#-------------------

summary(survey$Q7) #134 female, 83 male, 4 other 
summary(first_gen$Q7) #26 female, 17 male, 1 other 
summary(non_first_gen$Q7) #108 female, 66 male, 3 other 

summary(survey$Q8) #96 Asian, 3 Black, 7 Other, 107 White, 8 White/Asian
summary(first_gen$Q8) #19 Asian, 1 Black, 4 Other, 20 White 
summary(non_first_gen$Q8) #77 Asian, 2 Black, 3 Other, 87 White, 8 White/Asian

summary(survey$Q9) #17 fresh, 34 soph, 77 jr, 90 snr, 1 fifth yr 
summary(first_gen$Q9) #7 fresh, 8 soph, 10 jr, 18 snr, 1 fifth yr 
summary(non_first_gen$Q9) #10 fresh, 26 soph, 67 jr, 72 snr, 2 fifth yr

#-----------------
# Patience Game 
#-----------------

# what is the proportion of primed first gen kids who choose a over b (impatient)

prop.table(table(primed_first_gen$Q21.1_1)) # 74% of primed first-gen are impatient, 26% patient 
prop.table(table(control_first_gen$Q21.1_1)) # 75% of control first-gen are impatient, 25% patient 
# no sig diff b/w primed and control first-gen

prop.table(table(primed_first_gen$Q21.1_2)) # 61% impatient, 39% patient
prop.table(table(control_first_gen$Q21.1_2)) # 60% impatient, 40% patient 
# no sig diff 

prop.table(table(primed_first_gen$Q21.1_3)) # 48% impatient, 52% patient 
prop.table(table(control_first_gen$Q21.1_3)) # 40% impatient, 60% patient
# sig diff 

prop.table(table(primed_first_gen$Q21.1_4)) # 26% impatient, 74% patient
prop.table(table(control_first_gen$Q21.1_4)) # 30% impatient, 70% patient
# sig diff

prop.table(table(primed_first_gen$Q21.1_5)) # 13% impatient, 87% patient
prop.table(table(control_first_gen$Q21.1_5)) # 10% impatient, 90% patient
# sig diff

prop.table(table(primed_first_gen$Q21.1_6)) # 9% impatient, 91% patient
prop.table(table(control_first_gen$Q21.1_6)) # 10% impatient, 90% patient 
# no sig diff 

prop.table(table(primed_first_gen$Q21.1_7)) # 4% impatient, 96% patient
prop.table(table(control_first_gen$Q21.1_7)) #10% impatient, 90% patient
# sig diff

primed_first_gen<-filter(primed_first_gen,Q21.1_8=="A (1 month)"|Q21.1_8=="B (7 months)")
prop.table(table(primed_first_gen$Q21.1_8)) # 4% impatient, 91% patient
prop.table(table(control_first_gen$Q21.1_8)) # 5% impatient, 95% patient 



prop.table(table(primed_non_first_gen$Q21.1_1)) # 53% of primed non-first-gen are impatient, 41% patient 
prop.table(table(control_non_first_gen$Q21.1_1)) # 68% of control non-first-gen are impatient, 30% patient 
# sig diff

primed_non_first_gen<-filter(primed_non_first_gen,Q21.1_2=="A (1 month)"|Q21.1_2=="B (7 months)")
control_non_first_gen<-filter(control_non_first_gen,Q21.1_2=="A (1 month)"|Q21.1_2=="B (7 months)")
prop.table(table(primed_non_first_gen$Q21.1_2)) # 42% impatient, 52% patient
prop.table(table(control_non_first_gen$Q21.1_2)) # 45% impatient, 53% patient
# sig diff 

primed_non_first_gen<-filter(primed_non_first_gen,Q21.1_3=="A (1 month)"|Q21.1_3=="B (7 months)")
control_non_first_gen<-filter(control_non_first_gen,Q21.1_3=="A (1 month)"|Q21.1_3=="B (7 months)")
prop.table(table(primed_non_first_gen$Q21.1_3)) # 26% impatient, 67% patient
prop.table(table(control_non_first_gen$Q21.1_3)) # 31% impatient, 68% patient 
# sig diff

prop.table(table(primed_non_first_gen$Q21.1_4)) # 17% impatient, 77% patient
prop.table(table(control_non_first_gen$Q21.1_4)) # 20% impatient, 80% patient 
# sig diff

prop.table(table(primed_non_first_gen$Q21.1_5)) # 7% impatient, 88% patient 
prop.table(table(control_non_first_gen$Q21.1_5)) # 11% impatient, 86% patient 
# sig diff 

prop.table(table(primed_non_first_gen$Q21.1_6)) # 6% impatient, 89% patient
prop.table(table(control_non_first_gen$Q21.1_6)) # 5% impatient, 93% patient
# no sig diff

prop.table(table(primed_non_first_gen$Q21.1_7))  # 5% impatient, 90% patient
prop.table(table(control_non_first_gen$Q21.1_7)) # 5% impatient, 93% patient
# no sig diff 
primed_non_first_gen<-filter(primed_non_first_gen,Q21.1_8=="A (1 month)"|Q21.1_8=="B (7 months)")
prop.table(table(primed_non_first_gen$Q21.1_8)) # 3% impatient, 97% patient
prop.table(table(control_non_first_gen$Q21.1_8)) # 2% impatient, 95% patient
# no sig diff 



#--------------------
# Risk Pref Game #1
#--------------------

risk_1 <- survey[ 22:30]


risk_1 <- risk_1 %>% 
  mutate(cutoff=ifelse(Q28.1_1!=Q28.1_2, 1, 
                       ifelse(Q28.1_2!=Q28.1_3,2, 
                              ifelse(Q28.1_3!=Q28.1_4,3,
                                     ifelse(Q28.1_4!=Q28.1_5,4,
                                            ifelse(Q28.1_5!=Q28.1_6,5,
                                                   ifelse(Q28.1_6!=Q28.1_7,6,
                                                          ifelse(Q28.1_7!=Q28.1_8,7,
                                                                 ifelse(Q28.1_8!=Q28.1_9,8,9)))))))))

survey$risk_game_1_cutoff <-risk_1$cutoff
survey$risk_game_1_cutoff <-10-survey$risk_game_1_cutoff

#higher score means more risky

survey %>% group_by(student_status) %>% summarize(mean_risk_game_1_cutoff=mean(risk_game_1_cutoff))
t.test(risk_game_1_cutoff~student_status, data=survey)
reg1<-lm(risk_game_1_cutoff~group+student_status+(group*student_status),data=survey)

#----------------------------
# Part 3b: Risk Pref Game #2
#----------------------------
# Create variable for risk:

# Convert character vector to factor
survey$Q29_numeric <- as.numeric(factor(survey$Q29, levels = as.character(unique(survey$Q29))))
reg2<-lm(Q29_numeric~group+student_status+(group*student_status),data=survey)

summary(reg2)

#higher score means more risky

t.test(Q29_numeric~student_status, data=survey)

#--------------------
# Risk Pref Game #3
#--------------------

# Create risk variable (1 being least, 4 being highest): 
survey$riskgame3<-0
survey$riskgame3[survey$Q32=="Salary of $40,000 with 0% probability of being laid-off"]<-1
survey$riskgame3[survey$Q32=="Salary of $75,000 with 10% probability of being laid-off"]<-2
survey$riskgame3[survey$Q32=="Salary of $100,000 with 20% probability of being laid-off"]<-3
survey$riskgame3[survey$Q32=="Salary of $200,000 with 30% probability of being laid-off"]<-4

# Checks:
table(survey$Q32)
table(survey$riskgame3)

# Regress group, student status, and interaction term on risk: 
reg3<-lm(riskgame3~group+student_status+(group*student_status),data=survey)
summary(reg)

#----------------------------
# Part 3d: Add up risk preference scores
#----------------------------

survey$risk_score <- (survey$riskgame3+survey$risk_game_1_cutoff+survey$Q29_numeric)

#a higher score correlates to being more risky

#----------------------------
# Part 4: Regression Analysis
#----------------------------

risk_mod <- lm(risk_score~group+student_status+group*student_status, data=survey)
summary(risk_mod)
t.test(risk_score~student_status, data=survey) #first gen students statistically significantly more risk averse

boxplot(risk_score~student_status*group, data=survey, col="light blue")

stargazer(reg1, reg2, reg3,risk_mod, dep.var.labels= c("Risk Game 1", "Risk Game 2", "Risk Game 3", "Aggregate Risk Score"), covariate.labels= c("Treatment", "Not First-Generation", "Treatment x Not First-Generation", "First-Generation (Constant)"),title="Risk Games Regression", out="risk.htm")

#There is a close statistically significant effect of priming in sentiment score for first-gen students.
t.test(survey$risk_score[survey$student_status=="First-gen"]~survey$group[survey$student_status=="First-gen"])

#There is no statistically significant effect of priming in sentiment score for non first-gen students.
t.test(survey$risk_score[survey$student_status=="Not First-gen"]~survey$group[survey$student_status=="Not First-gen"])

risk_table <- survey %>% group_by(student_group) %>% summarize( mean_risk1=mean(risk_game_1_cutoff), mean_risk2=mean(Q29_numeric), mean_risk3=mean(riskgame3), mean_risk=mean(risk_score)) %>% mutate(across(2:5, round, 3))

risk_table %>%
  kbl(caption = "Risk Scores by Experimental Group and First-Generation Status", col.names=c("Group", "Risk Game 1", "Risk Game 2", "Risk Game 3", "Aggregate Risk Score")) %>%
  kable_classic(full_width = F, html_font = "Cambria")

#----------------------------
# Part 5: Sentiment Score
#----------------------------

#Reverse scoring and score calculation
reverse_cols <- c("Q26_1", "Q26_3", "Q26_7")
survey[ , reverse_cols] <- 5 - survey[ , reverse_cols]

survey <- survey %>% mutate(sentiment_score = select(., Q26_1:Q26_7) %>% rowSums(na.rm = TRUE)) %>% mutate(sentiment_score=sentiment_score/7)

#difference in the sentiment score for first generation status and group assignment
survey %>% group_by(Q20.1, group) %>% summarize(mean_sentiment_score=mean(sentiment_score))

#There is no statistically significant difference in sentiment score and first generation status
t.test(survey$sentiment_score~survey$Q20.1)

#There is no statistically significant effect of priming in sentiment score for first-gen students.
t.test(survey$sentiment_score[survey$student_status=="First-gen"]~survey$group[survey$student_status=="First-gen"])

#There is no statistically significant effect of priming in sentiment score for non first-gen students.
t.test(survey$sentiment_score[survey$student_status=="Not first_gen"]~survey$group[survey$student_status=="Not first_gen"])

aov1 <- aov(sentiment_score~student_group, data=survey)
summary(aov1)

boxplot(sentiment_score~student_status*group, data=survey, col="light blue")

#Fit a linear regression model

lm1 <- lm(sentiment_score~student_status+group+student_status*group, data=survey)
summary(lm1)
stargazer(lm1, dep.var.labels= c("Sentiment Score"), covariate.labels= c( "Not First-Generation","Treatment", "Treatment x Not First-Generation", "First-Generation (Constant)"),title="Sentiment Score Regression", out="risk.htm")
out="sentiment.htm")

lm2 <- lm(sentiment_score~risk_score*student_status, data=survey)
summary(lm2)

cor(survey$sentiment_score,survey$risk_score, method = "pearson")

#----------------------------
# Part 6: Exploratory Analysis
#----------------------------

prop.table(table(survey$student_status, survey$Q25))

#within the treatment group, how are risk and time preferences dependent on parents education pedigrees?
library(ggplot2)
survey %>% filter(group=="Treatment") %>% ggplot(aes(x=Q13, y=risk_score, fill=student_status)) + geom_boxplot()
survey %>% filter(group=="Treatment") %>% ggplot(aes(x=Q13, y=sentiment_score, fill=student_status)) + geom_boxplot()

parent_edu_mod <- lm(sentiment_score~Q13*student_status+Q13+student_status, data=survey)
summary(parent_edu_mod)

#expected earnings
boxplot(Q23~student_status,data=survey, main="Expected Earnings by First Generation Status",
        xlab="First Generation Status", ylab="Expected Earnings after Graduation")

boxplot(Q23~Q7,data=survey, main="Expected Earnings by Gender",
        xlab="Gender", ylab="Expected Earnings after Graduation")

earnings_mod <- lm(Q23~student_status+group+student_status*group, data=survey)

#do ppl think college is harder for first gen students?
#parental education background
