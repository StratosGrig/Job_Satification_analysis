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

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(effects)
library(ggplot2)
library(dplyr)
library(ltm)
library(mgcv)
library(ggpubr)
library(fitdistrplus)
library(tree)

#########################################

# import our data choose the txt or the csv file, both of them work well
df1 <- read.csv(file.choose(), header = T, sep = ",")

# df1 <- read.table(file.choose())

# income4 dependent 
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

#hrs1 from categorical to numeric
hrs1 <- as.numeric(hrs1)
df1$hrs1 <- hrs1
summary(hrs1)
df1 = na.omit(df1)
hrs1 = df1$hrs1
attach(df1)
df1
# 

prop.table(table(income4))

table(income4)
barplot(table(income4))

income_numeric <- as.numeric(income4)


#var test 
var.test(ï..age,income_numeric)
var.test(educ,income_numeric)

#chi square

chisq.test(sex,income4,correct = FALSE)

chisq.test(degree,income4,correct = FALSE)

chisq.test(satjob,income4,correct = FALSE)

chisq.test(satjob2,income4,correct = FALSE)

chisq.test(rincom91,income4,correct = FALSE)

chisq.test(wrkstat,income4,correct = FALSE)

chisq.test(jobinc,income4,correct = FALSE)

chisq.test(impjob,income4,correct = FALSE)

chisq.test(agecat4,income4,correct = FALSE)

chisq.test(bothft,income4,correct = FALSE)

#p-value = 2.923e-06,  tau = 0.1649479  

cor.test(ï..age,income_numeric,alternative = "two.sided",method = "kendall")

#p-value =  1.7e-12, tau = 0.2637792
cor.test(educ,income_numeric,alternative = "two.sided",method = "kendall")


#p-value = 0.1466, tau = -0.06188627 
cor.test(as.numeric(sex),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 2.678e-11, tau = 0.2639889
cor.test(as.numeric(degree),income_numeric,alternative = "two.sided",method = "kendall")
#p-value =  0.001051,  tau = -0.1336362
cor.test(as.numeric(satjob),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 0.0008737, tau = -0.1418819 
cor.test(as.numeric(satjob2),income_numeric,alternative = "two.sided",method = "kendall")
#p-value < 2.2e-16, tau = 0.570585 
cor.test(as.numeric(rincom91),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 0.04727, tau = 0.1074044
cor.test(hrs1,income_numeric,alternative = "two.sided",method = "kendall")
#p-value = NA, tau = NA
cor.test(as.numeric(wrkstat),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 0.1567, tau = 0.05488447 
cor.test(as.numeric(jobinc),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 0.4772, tau = 0.02900473
cor.test(as.numeric(impjob),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 2.2e-16, tau = 0.3534867  
cor.test(as.numeric(bothft),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 0.4887, tau = 0.04558031
cor.test(as.numeric(husbft),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 0.003147, tau = 0.1683168 
cor.test(as.numeric(husbhr),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 0.0491, tau = 0.1295377 
cor.test(as.numeric(wifeft),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 0.005539, tau = 0.1605507
cor.test(as.numeric(wifehr),income_numeric,alternative = "two.sided",method = "kendall")
#p-value = 0.001345, tau = 0.1943909
cor.test(as.numeric(agecat4),income_numeric,alternative = "two.sided",method = "kendall")

#spearman
cor.test(ï..age,income_numeric,alternative = "two.sided",method = "spearman")


#fisher

fisher.test(income4,satjob)

summary(df1)

#Age
tapply(income_numeric,ï..age,mean)
#educ
tapply(income_numeric,educ,mean)
#sex
tapply(income_numeric,sex,mean)

#tree
modeltree <- tree(income4~.,data = df1)
summary(modeltree)
plot(modeltree)
text(modeltree)


#For building this model, we will be using the polr command to estimate an ordered logistic regression. 
#Then, we’ll specify Hess=TRUE to let the model output show the observed information matrix from optimization which is used to get
#standard errors
#model <-polr(income4~ï..age+bothft, Hess=TRUE, data = df1)
model <-polr(income4~educ+bothft+hrs1+ï..age, Hess=TRUE, data = df1)
summary(model)

#1st version
predict_income = predict(model,df1)
table(df1$income4, predict_income)
mean((df1$income4) != (predict_income))

#Plotting the effects 


Effect(focal.predictors = "bothft",model)
plot(Effect(focal.predictors = "bothft",model))
plot(Effect(focal.predictors = c("bothft", "ï..age"),model))
plot(effect("bothft:ï..age", model), style = "stacked")


#Now we’ll calculate some essential metrics such as p-Value, CI, Odds ratio
ctable <- coef(summary(model))
ctable

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

# confidence intervals
ci <- confint(model)
ci

#
exp(coef(model))
## OR and CI
exp(cbind(OR = coef(model), ci))

#Let’s now try to enhance this model to obtain better prediction estimates.

summary(update(model, method = "probit", Hess = TRUE), digits = 3)

summary(update(model, method = "logistic", Hess = TRUE), digits = 3)

summary(update(model, method = "cloglog", Hess = TRUE), digits = 3)

#predict
head(predict(model, df1, type = "p"))
haddterm(model, ~.^2, test = "Chisq")

m2 <- stepAIC(model, ~.^2)
m2

summary(m2)

m2$anova
head(predict(m2, df1, type = "p"))


summary(df1)




