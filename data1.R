#-----------------------------------------------
# Econ 495 Term Paper
# Title: The Long-Term Benefits of Education: 
#        First-Generation College Students
# Author: Sharon Ma 
# Date: 03/23/2022  
#-----------------------------------------------

#-------------------
# Set Up Directory 
#-------------------

setwd("~/Downloads/Econ 495 Research")
library(dplyr)
library(mosaic) 
library(haven) 
library(ggplot2)
library(stargazer)
library(ivpack)

#-----------------
# Importing Data 
#-----------------

shed_data <- read_dta("shed2020.dta")
scf_data <- read_dta("scf2019.dta", encoding = "UTF-8")

#----------------------------------------
# Regress parents' educ on child's educ  
#----------------------------------------

# Remove unknowns and refusals 
clean_shed_data <- filter(shed_data, shed_data$CH2!=-2 & shed_data$CH2!=-1 & 
                            shed_data$CH3!=-2 & shed_data$CH3!=-1)
# Checks 
table(clean_shed_data$CH2) 
table(clean_shed_data$CH3)

# Create the variable mother_educ 
clean_shed_data$mother_educ <- 0 
clean_shed_data$mother_educ[clean_shed_data$CH2==1] <- 1
clean_shed_data$mother_educ[clean_shed_data$CH2==2] <- 2
clean_shed_data$mother_educ[clean_shed_data$CH2==3 | clean_shed_data$CH2==4 | 
                              clean_shed_data$CH2==5] <- 3 
clean_shed_data$mother_educ[clean_shed_data$CH2==6 | clean_shed_data$CH2==7] <- 4 

# Checks 
table(clean_shed_data$CH2)
table(clean_shed_data$mother_educ)

# Create the variable father_educ
clean_shed_data$father_educ <- 0 
clean_shed_data$father_educ[clean_shed_data$CH3==1] <- 1
clean_shed_data$father_educ[clean_shed_data$CH3==2] <- 2
clean_shed_data$father_educ[clean_shed_data$CH3==3 | clean_shed_data$CH3==4 | 
                              clean_shed_data$CH3==5] <- 3 
clean_shed_data$father_educ[clean_shed_data$CH3==6 | clean_shed_data$CH3==7] <- 4 

# Checks 
table(clean_shed_data$CH3)
table(clean_shed_data$father_educ)

# Regress mother_educ & father_educ on ppeducat
reg1 <- lm(ppeducat~mother_educ+father_educ, data=clean_shed_data)
summary(reg1)
stargazer(reg1)

#---------------------------------------------------------------------
# Regression the interaction term mother_educ*father_educ on ppeducat 
#---------------------------------------------------------------------

reg2 <- lm(ppeducat~mother_educ+father_educ+(mother_educ*father_educ), data=clean_shed_data)
summary(reg2)
stargazer(reg2)

#-------------------------------------------
# Grad Rates B/w First-Gen & Non-First-Gen 
#-------------------------------------------

# Subset students w/ two college-educated parents 
both_educ <- filter(shed_data, (CH2==6 | CH2==7) & (CH3==6 | CH3==7))
prop.table(table(both_educ$ppeduc))
# 0.7910272 or approx 79% of students whose fathers AND mothers are college-educated
# are also college-educated. This entails obtaining a bachelor's and/or graduate degree. 

# Subset students w/ one college-educated parent
one_educ <- filter(shed_data, (CH2==6 | CH2==7) | (CH3==6 | CH3==7))
prop.table(table(one_educ$ppeduc))
# 0.6905028 or approx 69% of students whose fathers OR mothers are college-educated
# are also college-educated.

# Subset students w/ no college-educated parents 
no_educ <- filter(shed_data, (CH2==1 | CH2==2 | CH2==3 | 
                              CH2==4 | CH2==5) & (CH3==1 | 
                              CH3==2 | CH3==3 | CH3==4 | 
                              CH3==5))
prop.table(table(no_educ$ppeduc))
# 0.3200579 or 32% of students who do not have college-educated parents are college-
# educated.

#--------------------------------------------------------------
# Subset First-Gen & Non-First-Gen Household Heads & Graduates
#--------------------------------------------------------------

first_gen_hhh <- filter(no_educ, pphhhead==1 & ppeducat==4)
non_first_gen_hhh <- filter(both_educ, pphhhead==1 & ppeducat==4)

#---------
# Income
#---------

favstats(first_gen_hhh$ppincimp) # mean = 15.7949
favstats(non_first_gen_hhh$ppincimp) # mean = 16.34685
print_labels(shed_data$ppincimp)

#------------
# Employment
#------------

# Employment Rate 
print_labels(shed_data$ppwork)
prop.table(table(first_gen_hhh$ppwork)) # 0.6709184 of first-gen students are employed
prop.table(table(non_first_gen_hhh$ppwork)) # while 0.8602243 of non-first-gen students are employed

# Industry 
print_labels(shed_data$ind1)

prop.table(table(first_gen_hhh$ind1)) # the highest proportion (0.329760082) falls
# within -2, which means that the individuals are either unemployed or do not know
# how to describe their industry. 0.125063808

prop.table(table(non_first_gen_hhh$ind1)) # the highest proportion (0.210526316) 
# falls within Professional, Scientific, Technical, and Business Services; which 
# gets paid more. 




# now i want to see what type of employer that the groups tend to have.
# ppcm1301 - employer type

print_labels(first_gen_hhh$ppcm1301)

prop.table(table(first_gen_hhh$ppcm1301)) # the majority of them (0.330612245) 
# were not asked, which means they were unemployed and so this question would not
# apply to them. the second highest (0.314285714) was 2, which is a private-for-
# profit company. only 

prop.table(table(non_first_gen_hhh$ppcm1301)) # the majority of them (0.476272649)
# worked at a private-for-profit company, which makes sense that their avg incomes 
# would be higher 




table(first_gen_hhh$ppfs0596)
table(non_first_gen_hhh$ppfs0596)

clean_first_gen_hhh <- filter(first_gen_hhh, first_gen_hhh$ppfs0596!=-1)

clean_non_first_gen_hhh <- filter(non_first_gen_hhh, non_first_gen_hhh$ppfs0596!=-1)

#------------
# Wealth
#------------
favstats(first_gen_hhh$ppfs0596) # mean = 3.223871, around $133,580.4
favstats(non_first_gen_hhh$ppfs0596) # mean = 2.939326, around $
print_labels(shed_data$ppfs0596)











# reference: first_gen_hhh <- filter(no_educ, pphhhead==1 & ppeducat==4)

# I will also compare the incomes between first-generation and non-first-generation 
# students at each level of education

first_gen_less_high <- filter(no_educ, pphhhead==1 & ppeducat==1)
first_gen_high <- filter(no_educ, pphhhead==1 & ppeducat==2)
first_gen_some_college <- filter(no_educ, pphhhead==1 & ppeducat==3)
first_gen_college_more <- filter(no_educ, pphhhead==1 & ppeducat==4)

non_first_gen_less_high <- filter(both_educ, pphhhead==1 & ppeducat==1)
non_first_gen_high <- filter(both_educ, pphhhead==1 & ppeducat==2)
non_first_gen_some_college <- filter(both_educ, pphhhead==1 & ppeducat==3)
non_first_gen_college_more <- filter(both_educ, pphhhead==1 & ppeducat==4)

print_labels(shed_data$ppincimp)

favstats(first_gen_less_high$ppincimp) # mean = 8.554859
favstats(first_gen_high$ppincimp) # mean = 11.51969
favstats(first_gen_some_college$ppincimp) # mean = 13.31463
favstats(first_gen_college_more$ppincimp) # mean = 15.7949

favstats(non_first_gen_less_high$ppincimp) # mean = 16.2
favstats(non_first_gen_high$ppincimp) # mean = 12
favstats(non_first_gen_some_college$ppincimp) # mean = 13.08075 
favstats(non_first_gen_college_more$ppincimp) # mean = 16.34685


# Perhaps the two groups tend to select different majors, have different levels 
# of career readiness, and/or attend different types of universities. 

# ED1  Which one of the following broad categories best describes your educational program?
print_labels(shed_data$ED1)

prop.table(table(first_gen_hhh$ED1)) # the majority (0.214805825) in #8, which is business/management
prop.table(table(non_first_gen_hhh$ED1)) # the majority (0.204630321) also in #8. maybe business is just a popular major in general



