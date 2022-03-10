###########################################
library(ggplot2)
library(gridExtra)
library(viridis)
library(caret)
library(xgboost)
library(randomForest)
library(MASS)
library(ordinal)
library(foreign)
library(Hmisc)
library(reshape2)
library(caTools)
library(gbm)
library(ranger)
library(mltools)
library(tidyverse)

rm(list = ls())

#clear console
cat("\014")
rm(list=ls())
set.seed(123)

dataset<- read.csv("/fullds_labels.csv")

# Scale the data
dataset$age <- scale(dataset$age)
dataset$educ <- scale(dataset$educ)
dataset$hrs1 <- scale(dataset$hrs1)

dataset$sex <- factor(dataset$sex,
                      levels = c("Male", "Female"))

dataset$degree <-factor(dataset$degree,
                        levels = c("Less than HS", "High school","Junior college", "Bachelor","Graduate"))

dataset$satjob <- factor(dataset$satjob,
                         levels = c("Very satisfied","Mod satisfied", "A little dissatisfied","Very dissatisfied"))

# dataset$rincom91 <- factor(dataset$rincom91,
#                                       levels = c("Under 15K","15K-22.5K","22.5K-30K","30-40K","Over 40K"))


# dataset$rincom91 <- factor(dataset$rincom91,
#                                       levels = c("Under 25K","Over 25K"))

dataset$rincom91 <- factor(dataset$rincom91,
                                      levels = c("Under 20K","20-40K","Over 40K"))
summary(dataset$rincom91)

dataset$impjob <- factor(dataset$impjob,
                         levels = c("One of most important","Very important", "Somewhat important","Not too important", "Not at all important"))


dataset$agecat4 <- factor(dataset$agecat4,
                          levels = c("18-29", "30-39","40-49","50+"))


# library(smotefamily)
# newData <- SMOTE(rincom91 ~ ., dataset, perc.over = 600,perc.under=100)

head(dataset)
chisq.test(dataset$rincom91, dataset$educ, correct=FALSE)
chisq.test(dataset$rincom91, dataset$hrs1, correct=FALSE)
chisq.test(dataset$rincom91, dataset$age, correct=FALSE)
chisq.test(table(dataset$rincom91, dataset$sex))
chisq.test(table(dataset$rincom91, dataset$degree))
chisq.test(table(dataset$rincom91, dataset$satjob))
chisq.test(table(dataset$rincom91, dataset$impjob))
chisq.test(table(dataset$rincom91, dataset$agecat4))
dataset<-dataset[-8]

# ggplot(dataset, aes(x = rincom91, y = age, fill = rincom91)) +
#   geom_boxplot(size = .75) +
#   facet_grid(degree ~ sex, margins = FALSE) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# 
# p1<-ggplot(dataset) +
#   aes(x = rincom91, fill = degree) +
#   geom_bar(position = "dodge") +
#   scale_color_viridis(discrete = TRUE, option = "D")+
#   scale_fill_viridis(discrete = TRUE) +
#   theme_minimal()
# p2<-ggplot(dataset) +
#   aes(x = rincom91, fill = sex) +
#   geom_bar(position = "dodge") +
#   scale_color_viridis(discrete = TRUE, option = "D")+
#   scale_fill_viridis(discrete = TRUE) +
#   theme_minimal()
# p3<-ggplot(dataset) +
#   aes(x = rincom91, fill = impjob) +
#   geom_bar(position = "dodge") +
#   scale_color_viridis(discrete = TRUE, option = "D")+
#   scale_fill_viridis(discrete = TRUE) +
#   theme_minimal()
# p4<-ggplot(dataset) +
#   aes(x = rincom91, fill = agecat4) +
#   geom_bar(position = "dodge") +
#   scale_color_viridis(discrete = TRUE, option = "D")+
#   scale_fill_viridis(discrete = TRUE) +
#   theme_minimal()
# p5<- ggplot(dataset) +
#   aes(x = rincom91, fill = satjob) +
#   geom_bar(position = "dodge") +
#   scale_color_viridis(discrete = TRUE, option = "D")+
#   scale_fill_viridis(discrete = TRUE) +
#   theme_minimal()
# grid.arrange(p1, p2, p3, p4, p5, nrow = 3)

# model_fit <- polr(rincom91 ~ sex + age + hrs1 + educ + degree + impjob + satjob, data = dataset,Hess=TRUE)
# MASS::dropterm(model_fit, trace=FALSE, test="Chisq")

# dummy <- dummyVars(" ~ .", data=dataset)
# dataset <- data.frame(predict(dummy, newdata=dataset))
# dataset$rincom91 <- factor(dataset$rincom91,
#                               levels = c(1,2,3),
#                               labels = c("Under 20K","20-40K","Over 40K"))

split = sample.split(dataset$rincom91, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

tr.control = trainControl(
  method = "cv",
  number = 10,
  # preProc = c("scale"),
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)


polrFit = train(form = rincom91 ~ ., data = training_set, metric = "Accuracy", trControl = tr.control, method = 'polr')
polrClasses <- predict(polrFit, newdata = test_set)

multinomFit = train(form = rincom91 ~ ., data = training_set, metric = "Accuracy",tuneGrid = expand.grid(decay = seq(0, 1, by = 0.1)),
                    trControl = tr.control, method = 'multinom')
multinomClasses <- predict(multinomFit, newdata = test_set)

rfGrid <- expand.grid(mtry = c(2, 3, 4, 5),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 3, 5))

rfFit = train(form = rincom91 ~ ., data = training_set, metric = "Accuracy", tuneGrid = rfGrid, trControl = tr.control, method = 'ranger')
rfClasses <- predict(rfFit, newdata = test_set)

xgbGrid <- expand.grid(nrounds = c(100,200),  
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1 )

xgbFit = train(form = rincom91 ~ ., data = training_set, metric = "Accuracy", trControl = tr.control, tuneGrid = xgbGrid, method = 'xgbTree')
xgbClasses <- predict(xgbFit, newdata = test_set)

confusionMatrix(data = polrClasses, test_set$rincom91)
confusionMatrix(data = multinomClasses, test_set$rincom91)
confusionMatrix(data = rfClasses, test_set$rincom91)
confusionMatrix(data = xgbClasses, test_set$rincom91)


trellis.par.set(caretTheme())
ggplot(polrFit)
plot(multinomFit)
plot(rfFit)
plot(xgbFit)