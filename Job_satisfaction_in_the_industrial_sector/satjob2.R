########################################
#here we will throw away some columns (husbhr, wifehr, wifeft, husbft)


#########################################

#clearing commands 

# clear wokspace 
rm(list = ls())

#clear console
cat("\014")

#clear plots 
dev.off(dev.list()["RStudioGD"])

#########################################
#LIBRARIES 

library(ggplot2)
library(dplyr)
library(ltm)
library(mgcv)
library(cowplot)
#########################################

# import our data choose the txt or the csv file, both of them work well
df1 <- read.csv(file.choose(), header = T, sep = ",")

# df1 <- read.table(file.choose())

# satjob 2 dependent 
# all the others independent

# attach the dataframe so we can the columns names without refering to the 
# dataframe name
attach(df1)

# #basic dataset check to make sure that it is loaded correctly and explore it
head(df1)


#########################################

#throw away the missing values
df1 <- subset(df1, select = -c(husbft, husbhr, wifeft, wifehr))
df1 = na.omit(df1)
df1 = subset(df1, jobinc!="NAP")
df1 = subset(df1, rincom91!="REFUSED")
df1 = subset(df1, rincom91!="DK")
df1 = subset(df1, rincom91!="NAP")


############################################




#############################################

#########################################
# from categorical to factor and then summary  


# sex from categorical to factor so that summary has an intuition 
df1$sex <- factor(df1$sex, levels = c("Male", "Female"))
summary(df1$sex)

# degree from categorical to factor so that summary has an intuition 
df1$degree <- factor(df1$degree, levels = c("Less than HS", "High school", 
                                            "Junior college", "Bachelor","Graduate"
                                            ,"NAP","DK","NA"))
summary(df1$degree)

# satjob from categorical to factor so that summary has an intuition 
df1$satjob <- factor(df1$satjob, levels = c("NAP", "Very satisfied", 
                                            "Mod satisfied", "A little dissatisfied"
                                            ,"Very dissatisfied"
                                            ,"DK","NA"))
summary(df1$satjob)

# satjob2 from categorical to factor so that summary has an intuition 
df1$satjob2 <- factor(df1$satjob2, levels = c("Very satisfied", "Not very satisfied"))
summary(df1$satjob2)

# income4 from categorical to factor so that summary has an intuition 
df1$income4 <- factor(df1$income4, levels = c("24,999 or less", "25,000 to 39,999", 
                                              "40,000 to 59,999", "60,000 or more"))
summary(df1$income4)

# rincom91 from categorical to factor so that summary has an intuition 
df1$rincom91 <- factor(df1$rincom91, levels = c("NAP", "LT $1000", 
                                                "$1000-2999", "$3000-3999","$4000-4999",
                                                "$5000-5999","$6000-6999","$7000-7999",
                                                "$8000-9999","$10000-12499","$12500-14999",
                                                "$15000-17499","$17500-19999","$20000-22499",
                                                "$22500-24999","$25000-29999","$30000-34999",
                                                "$35000-39999","$40000-49999","$50000-59999",
                                                "$60000-74999","$75000+","REFUSED","DK","NA"))
summary(df1$rincom91)

# wrkstat from categorical to factor so that summary has an intuition 
df1$wrkstat <- factor(df1$wrkstat, levels = c("NAP", "Working fulltime", 
                                              "Working parttime", "Temp not working"
                                              ,"Unempl, laid off",
                                              "Retired","School","Keeping house",
                                              "Other","NA"))
summary(df1$wrkstat)

# jobinc from categorical to factor so that summary has an intuition 
df1$jobinc <- factor(df1$jobinc, levels = c("NAP", "MOST IMPT", 
                                            "SECOND", "THIRD"
                                            ,"FOURTH", "FIFTH",
                                            "DK","NA"))
summary(df1$jobinc)

# impjob from categorical to factor so that summary has an intuition 
df1$impjob <- factor(df1$impjob, levels = c("NAP", "One of most important", 
                                            "Very important", "Somewhat important"
                                            ,"Not too important", "Not at all important",
                                            "DK","NA"))
summary(df1$impjob)

# bothft from categorical to factor so that summary has an intuition 
df1$bothft <- factor(df1$bothft, levels = c("No", "Yes"))
summary(df1$bothft)

# # husbft from categorical to factor so that summary has an intuition 
# df1$husbft <- factor(df1$husbft, levels = c("no", "yes"))
# summary(df1$husbft)

# # wifeft from categorical to factor so that summary has an intuition 
# df1$wifeft <- factor(df1$wifeft, levels = c("no", "yes"))
# summary(df1$wifeft)

# agecat4 from categorical to factor so that summary has an intuition 
df1$agecat4 <- factor(df1$agecat4, levels = c("18-29", "30-39","40-49","50+"))
summary(df1$agecat4)

#attach
attach(df1)
summary(df1)

####################################

# #husbhr from categorical to numeric  
# husbhr <- as.numeric(husbhr)
# df1$husbhr <- husbhr 
# 
# #wifehr from categorical to numeric  
# wifehr <- as.numeric(wifehr)
# df1$wifehr <- wifehr 
# 
#hrs1 from categorical to numeric
hrs1 <- as.numeric(hrs1)
df1$hrs1 <- hrs1
summary(hrs1)
df1 = na.omit(df1)
hrs1 = df1$hrs1
attach(df1)
# 
#check if done
# summary(df1)
# attach(df1)
# length(df1$hrs1)
# length(hrs1)
# attach(df1)

############################################################
#data trasformation 


levels(df1$rincom91)[c(2,3,4,5,6,7,8,9,10
                   ,11,12,13,14,15,16,17,18)] = 'less than 60000$'
levels(df1$rincom91)[c(3,4,5,6)] = '$60000 or more'
# df1$rincom91 <- factor(df1$rincom91, levels = c("NAP", "less than 40000$", 
#                                                 "$40000 or more","REFUSED","DK","NA"))
summary(df1$rincom91)

levels(df1$degree)[c(1,2,3,4)] = "Less than HS until bachelor"
# df1$degree <- factor(df1$degree, levels = c("Less than HS until bachelor","Graduate"
#                                             ,"NAP","DK","NA"))
summary(df1$degree)

attach(df1)

###################################
# boxplots for numeric 


#age
bp_age <- ggplot(df1, aes(y=age)) +
  geom_boxplot()
bp_age

#educ
bp_educ <- ggplot(df1, aes(y=educ)) +
  geom_boxplot()
bp_educ

#hrs1
bp_hrs1 <- ggplot(df1, aes(y=hrs1)) +
  geom_boxplot()
bp_hrs1

#all in 1 plot
par(mfrow = c(1,3))
boxplot(age, main="boxplot of age")
boxplot(educ, main="boxplot of education")
boxplot(hrs1, main="boxplot of hrs1")



###################################

#histograms for numeric

#age
hist_age <- ggplot(df1, aes(x=age)) +
  geom_histogram(binwidth = 2)
hist_age

#educ
hist_educ <- ggplot(df1, aes(x=educ)) +
  geom_histogram(binwidth = 1)
hist_educ

# #all in 1 plot
par(mfrow = c(1,3))
hist(age, main="histogram of age")
hist(educ, main="histogram of education")
hist(hrs1, main="histogram of hrs1")


###########################################

#bar_charts for categorical variables 

bar_sex = ggplot(df1, aes(x = sex)) +
  geom_bar()
# bar_sex
#
bar_degree = ggplot(df1, aes(x = degree)) +
  geom_bar()
# bar_degree
#
bar_satjob = ggplot(df1, aes(x = satjob)) +
  geom_bar()
# bar_satjob
#
bar_satjob2 = ggplot(df1, aes(x = satjob2)) +
  geom_bar()
# bar_satjob2
#
bar_income4 = ggplot(df1, aes(x = income4)) +
  geom_bar()
# bar_income4
#
bar_rincom91 = ggplot(df1, aes(x = rincom91)) +
  geom_bar()
# bar_rincom91
#
bar_wrkstat = ggplot(df1, aes(x = wrkstat)) +
  geom_bar()
# bar_wrkstat
# #
bar_jobinc = ggplot(df1, aes(x = jobinc)) +
  geom_bar()
# bar_jobinc
# #
bar_impjob = ggplot(df1, aes(x = impjob)) +
  geom_bar()
# bar_impjob
# #
bar_bothft = ggplot(df1, aes(x = bothft)) +
  geom_bar()
# bar_bothft

#5 in one
plot_grid(bar_sex, bar_degree,bar_satjob,bar_satjob2,bar_income4, labels = "AUTO")

#4 in one
plot_grid(bar_jobinc,bar_impjob,bar_bothft,bar_rincom91, labels = "AUTO")


##########################################
# q-q plot


par(mfrow=c(1,3))
#q-q plot for the dependent variable education
qqnorm(educ, main="Normal Q Q Plot for education")
qqline(educ)

#q-q plot for the dependent variable age
qqnorm(age, main="Normal Q-Q Plot for age")
qqline(age)

#q-q plot for the dependent variable hrs1
qqnorm(hrs1, main="Normal Q-Q Plot for hrs1")
qqline(hrs1)

#log of the aforementioned variables
par(mfrow=c(1,2))

#q-q plot for the dependent variable age
qqnorm(log(age), main="Normal Q-Q Plot for log(age)")
qqline(log(age))

#q-q plot for the dependent variable hrs1
qqnorm(log(hrs1), main="Normal Q-Q Plot for log(hrs1)")
qqline(log(hrs1))

########################################










##########################################
#correlation and relationships between satjob2 and the rest 

#satjob2 percentages
prop.table(table(satjob2))
table(satjob2)

#satjob2-age (no-dependency)

#1st way
biserial.cor(age,satjob2)
#2nd way
lm1 = lm(age~satjob2)
sum1 = summary(lm1)
rsq1 = sum1$r.squared
cor1 = sqrt(rsq1)
print(cor1)
#visualization of this correlation
ggplot(df1, aes(x=satjob2, y= age)) + geom_boxplot()
#mean
tapply(age,satjob2,mean)
#wilcoxon test
wilcox.test(age~satjob2)
# p = 0.06 > 0.05 so we reject Ho and say that
# the independent age does not
# appear to have a statistical significant
# deviation between its' values with regard
# the dependent variable satjob2.
# 
# #satjob2-educ (no - dependency)
# 
#1st way
biserial.cor(educ,satjob2)
#2nd way
lm1 = lm(educ~satjob2)
sum1 = summary(lm1)
rsq1 = sum1$r.squared
cor1 = sqrt(rsq1)
print(cor1)
#visualization of this correlation
ggplot(df1, aes(x=satjob2, y= educ)) + geom_boxplot()
#mean
tapply(educ,satjob2,mean)
#wilcoxon test
wilcox.test(educ~satjob2)
# p = 0.08 > 0.05 so we can not reject Ho and so
# we say that the independent educ
# appears to have a statistical significant deviation
# between its' values with regard
# the dependent variable satjob2.
# 
# 
# 
# #satjob2-hrs1 (no-dependency)
# 
# #1st way
biserial.cor(hrs1,satjob2)
#2nd way
lm1 = lm(hrs1~satjob2)
sum1 = summary(lm1)
rsq1 = sum1$r.squared
cor1 = sqrt(rsq1)
print(cor1)
#visualization of this correlation
ggplot(df1, aes(x=satjob2, y= hrs1)) + geom_boxplot()
# #mean
tapply(hrs1,satjob2,mean)
#wilcoxon test
wilcox.test(hrs1~satjob2)
# p = 0.18 > 0.05 so we can not reject Ho and so we say that
# the independent hrs1 does not
# appear to have a statistical significant deviation
# between its' values with regard
# the dependent variable satjob2.



# #3 in one
plot_grid(ggplot(df1, aes(x=satjob2, y= age)) + geom_boxplot(),
          ggplot(df1, aes(x=satjob2, y= educ)) + geom_boxplot(),
          ggplot(df1, aes(x=satjob2, y= hrs1)) + geom_boxplot(), labels = "AUTO", nrow =1, ncol=3)


#satjob2-sex (no-dependency)

table(satjob2,sex)
prop.table(table(satjob2,sex),2)
#
# chi - squared test
chisq.test(sex, satjob2, correct=FALSE)
#Since we get a p-Value = 0.9 more than the significance level of 0.05,
#we can not reject
#the null hypothesis and conclude that the two variables are
#in fact non - dependent.
#
#fisher exact test
fisher.test(sex, satjob2)
# Since we get a p-Value = 1 more than the significance level of 0.05,
# we can not reject
# the null hypothesis and conclude that the two variables
# are in fact non - dependent.



#satjob2-degree (dependency)

# levels(degree)[c(1,2,3,4)] = "Less than HS until graduate"


levels(degree)
table(satjob2,degree)
prop.table(table(satjob2,degree),2)

# chi - squared test
chisq.test(degree, satjob2, correct=FALSE)
# we have a 4<5(chi squared wants at least 5) in one category
# thats why we have the warning message
#Since we get a p-Value = 0.32 more than the significance level of 0.05,
# we can not reject
#the null hypothesis and conclude that the two variables
# are in fact non - dependent.

#fisher exact test
fisher.test(degree, satjob2)
#Since we get a p-Value = 0.33 more than the significance level of 0.05,
# we can not reject
#the null hypothesis and conclude that the two variables
# are in fact non - dependent.


#satjob2-satjob (dependency)

table(satjob2,satjob)
prop.table(table(satjob2,satjob),2)

# chi - squared test
chisq.test(satjob, satjob2, correct=FALSE)
# we have a 4<5(chi squared wants at least 5) in one category
# thats why we have the warning message
#Since we get a p-Value < 2.2e-16 less than the significance level of 0.05,
# we can reject
#the null hypothesis and conclude that the two variables
# are in fact dependent.

#fisher exact test
fisher.test(satjob, satjob2)
#Since we get a p-Value < 2.2e-16 less than the significance level of 0.05,
# we can reject
#the null hypothesis and conclude that the two variables
# are in fact dependent.


#satjob2-income4 (dependency)

table(satjob2,income4)
prop.table(table(satjob2,income4),2)

# chi - squared test
chisq.test(income4, satjob2, correct=FALSE)
#Since we get a p-Value = 0.002 less than the significance level of 0.05,
# we can reject
#the null hypothesis and conclude that the two variables
# are in fact dependent.

#fisher exact test
fisher.test(income4, satjob2)
#Since we get a p-Value = 0.001 more than the significance level of 0.05,
# we can reject
#the null hypothesis and conclude that the two variables
# are in fact dependent.



#satjob2-rincom91 ( dependency)

# levels(rincom91)[c(2,3,4,5,6,7,8,9,10
#                    ,11,12,13,14,15,16,17,18)] = 'less than 40000'
# levels(rincom91)[c(3,4,5,6)] = '40000 or more'


levels(rincom91)
#
table(satjob2,rincom91)
prop.table(table(satjob2,rincom91),2)

# chi - squared test
chisq.test(rincom91, satjob2, correct=FALSE,simulate.p.value = T)
# we have a <5(chi squared wants at least 5) in one category
# thats why we have the warning message
#Since we get a p-Value = 0.1 more than the significance level of 0.05,
# we can not reject
#the null hypothesis and conclude that the two variables
# are in fact non-dependent.

#fisher exact test
fisher.test(rincom91, satjob2, simulate.p.value=TRUE)
# simulate.p.value=TRUE to avoid warning issue
#Since we get a p-Value = 0.08 more than the significance level of 0.05,
# we can not reject
#the null hypothesis and conclude that the two variables
# are in fact non-dependent.



#satjob2-workstat (workstat has only one category so no need to inspect any dependencies)

table(satjob2,wrkstat)
prop.table(table(satjob2,wrkstat),2)




#satjob2-jobinc (dependency)

table(satjob2,jobinc)
prop.table(table(satjob2,jobinc),2)

# chi - squared test
chisq.test(jobinc, satjob2, correct=FALSE)
#Since we get a p-Value = 0.02 less than the significance level of 0.05,
# we can reject
#the null hypothesis and conclude that the two variables
# are in fact dependent.

#fisher exact test
fisher.test(jobinc, satjob2, simulate.p.value=TRUE)
# simulate.p.value=TRUE to avoid warning issue
#Since we get a p-Value = 0.02 less than the significance level of 0.05,
# we can reject
#the null hypothesis and conclude that the two variables
# are in fact dependent.




#satjob2-impjob (dependency)

table(satjob2,impjob)
prop.table(table(satjob2,impjob),2)

# chi - squared test
chisq.test(impjob, satjob2, correct=FALSE, simulate.p.value=TRUE)
# we have a <5(chi squared wants at least 5) in one category
# that's why we have the warning message
#Since we get a p-Value = 0.03501 less than the significance level of 0.05,
# we can reject
#the null hypothesis and conclude that the two variables
#are in fact dependent.

#fisher exact test
fisher.test(impjob, satjob2, simulate.p.value=TRUE)
# simulate.p.value=TRUE to avoid warning issue
#Since we get a p-Value = 0.01899 less than the significance level of 0.05,
# we can reject
#the null hypothesis and conclude that the two variables
# are in fact dependent.



#satjob2-bothft (no - dependency)

table(satjob2,bothft)
prop.table(table(satjob2,bothft),2)

# chi - squared test
chisq.test(bothft, satjob2, correct=FALSE)
#Since we get a p-Value = 0.8724 more than the significance level of 0.05,
# we can not reject
#the null hypothesis and conclude that the two variables
# are in fact non-dependent.

#fisher exact test
fisher.test(bothft, satjob2)
#Since we get a p-Value = 0.9225 more than the significance level of 0.05,
# we can not reject
#the null hypothesis and conclude that the two variables
# are in fact non-dependent.



#satjob2-agecat4 (no - dependency)

table(satjob2,agecat4)
prop.table(table(satjob2,agecat4),2)

# chi - squared test
chisq.test(agecat4, satjob2, correct = FALSE)
#Since we get a p-Value = 0.1298 more than the significance level of 0.05,
# we can not reject
#the null hypothesis and conclude that the two variables
# are in fact non-dependent.

#fisher exact test
fisher.test(agecat4, satjob2)
#Since we get a p-Value = 0.1277 more than the significance level of 0.05,
# we can not reject
#the null hypothesis and conclude that the two variables
# are in fact non-dependent.




##########################################
#GLM modelling

# attach(df1)
# summary(df1)

# 1st model
# this the best performing model in terms of accuracy after a lot of 
# experimentation regarding which parameters to mention in the model 
# and which transformation of the numeric variables to use 
# we also transformed the categories of the variables rincom91 and degree 
# so that we have the best possible correlation with the dependent variable satjob2 
#65 mean accuracy
model1 =  glm(satjob2~jobinc+income4+impjob+rincom91+degree+I(educ^2)+log(age),binomial)
summary(model1)
probs = predict(model1,type ="response")
pred.classes = ifelse(probs > 0.5, "pos", "neg")
prop.table(table(satjob2,pred.classes),1)


# 2nd model
#63 mean accuracy
model2 =  glm(satjob2~jobinc+income4+impjob,binomial)
summary(model2)
probs = predict(model2,type ="response")
pred.classes = ifelse(probs > 0.5, "pos", "neg")
prop.table(table(satjob2,pred.classes),1)




##########################################

######################################
#practise section 






######################################
#next steps







