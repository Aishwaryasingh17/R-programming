library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
install.packages("psych", dependencies=TRUE)
install.packages("psychTools")
library(psych)
install.packages("ggpubr")
library("ggpubr")

#Importing data
german_credit <- read_excel("German_Credit.xls")

#creating a data frame by identifying the RESPONSE variable as our Target
gc_data <- german_credit%>%
  mutate(Target = as.factor(ifelse(RESPONSE ==1,"GOOD","BAD")))

#Setting RESPONSE variable as Null
gc_data$RESPONSE <- NULL


#Calculating the proportion of GOOD and BAD 
good <- gc_data%>%
  filter(Target=="GOOD")
bad <- gc_data%>%
  filter(Target=="BAD")

proprtion_data <-nrow(good)/nrow(bad)
proprtion_data


gc_data$`OBS#` <-NULL

#coverting the variables in dataframe into factors.
gc_new <- gc_data%>%
  mutate(CHK_ACCT = as.factor(CHK_ACCT),
         HISTORY =as.factor(HISTORY),NEW_CAR = as.factor(NEW_CAR),
         USED_CAR= as.factor(USED_CAR),FURNITURE =as.factor(FURNITURE),
         `RADIO/TV` = as.factor(`RADIO/TV`),EDUCATION = as.factor(EDUCATION),
         RETRAINING = as.factor(RETRAINING),SAV_ACCT = as.factor(SAV_ACCT),
         EMPLOYMENT = as.factor(EMPLOYMENT),MALE_DIV =as.factor(MALE_DIV),
         MALE_SINGLE = as.factor(MALE_SINGLE),MALE_MAR_or_WID = as.factor(MALE_MAR_or_WID),
         `CO-APPLICANT` = as.factor(`CO-APPLICANT`),
         GUARANTOR = as.factor(GUARANTOR),PRESENT_RESIDENT = as.factor(PRESENT_RESIDENT),
         REAL_ESTATE = as.factor(REAL_ESTATE),PROP_UNKN_NONE= as.factor(PROP_UNKN_NONE),
         OTHER_INSTALL = as.factor(OTHER_INSTALL),RENT = as.factor(RENT),
         OWN_RES = as.factor(OWN_RES),JOB = as.factor(JOB),TELEPHONE =as.factor(TELEPHONE),
         FOREIGN = as.factor(FOREIGN))

#Finding out OUTLIERS
boxplot(gc_new$NUM_DEPENDENTS)
#Duration
boxplot(gc_new$DURATION,plot = FALSE)$out
outliers_1 <- boxplot(gc_new$DURATION,plot = FALSE)$out
x<-gc_new
x<- x[-which(x$DURATION %in% outliers_1),]
boxplot(x$DURATION)
gc_new<- x
boxplot(gc_new$DURATION)

#Amount(doubt)
boxplot(gc_new$AMOUNT,plot = FALSE)$out
outliers_2 <- boxplot(gc_new$AMOUNT,plot = FALSE)$out
x1<-gc_new
x1<- x1[-which(x1$AMOUNT %in% outliers_2),]
boxplot(x1$AMOUNT)
gc_new<- x1
boxplot(gc_new$AMOUNT,gc_new$Target)

#Install_rate- NO OUTLIERS
boxplot(gc_new$INSTALL_RATE,plot = FALSE)$out


#AGE(1 outlier remaining)
boxplot(gc_new$AGE,plot = FALSE)$out
outliers_4 <- boxplot(gc_new$AGE,plot = FALSE)$out
x3<-gc_new
x3<- x3[-which(x3$AGE %in% outliers_4),]
boxplot(x3$AGE)
gc_new<- x3
boxplot(gc_new$AGE)


#NUM_CREDITS
boxplot(gc_new$NUM_CREDITS,plot = FALSE)$out
outliers_5 <- boxplot(gc_new$NUM_CREDITS,plot = FALSE)$out
x4<-gc_new
x4<- x4[-which(x4$NUM_CREDITS %in% outliers_5),]
boxplot(x4$NUM_CREDITS)
gc_new<- x4
boxplot(gc_new$NUM_CREDITS)



#NUM_DEPENDENTS
boxplot(gc_new$NUM_DEPENDENTS,plot = FALSE)$out
outliers_6 <- boxplot(gc_new$NUM_DEPENDENTS,plot = FALSE)$out
x5<-gc_new
x5<- x5[-which(x5$NUM_DEPENDENTS %in% outliers_6),]
boxplot(x5$NUM_DEPENDENTS)
gc_new<- x5
boxplot(gc_new$NUM_DEPENDENTS)

#Summary of cleanded dataset
dim(gc_new)
describe(gc_new)

#correlation between the numeric variables
col1<-gc_new[,2]
col2<-gc_new[,10]
col3 <- gc_new[,13]
cor(col1,col3)
m1<-t.test(gc_new$DURATION,gc_new$AMOUNT, data=gc_new, var.equal=FALSE, na.rm=TRUE)
print(m1)

#DURATION 
mean(gc_new$DURATION)              #18.20228
sd(gc_new$DURATION, na.rm = FALSE) #8.43531
duration.mean <- gc_new %>% 
  select(DURATION, Target) %>%
  group_by(Target) %>% 
  summarise( mean =mean(DURATION))
duration.mean
#we observe that the mean value for bad records is larger in general as compared to good ones for Duration

#AMOUNT
mean(gc_new$AMOUNT)                #2392.504
sd(gc_new$AMOUNT, na.rm = FALSE)   #1466.742
amount.mean <- gc_new %>%
  select(AMOUNT,Target) %>% 
  group_by(Target) %>% 
  summarise(mean =mean(AMOUNT))
amount.mean
#we observe that the amount for good records is larger in general as compared to bad ones

#INSTALLEMTN_RATE
mean(gc_new$INSTALL_RATE)                #3.075499
sd(gc_new$INSTALL_RATE, na.rm = FALSE)   #1.087471
ggplot(gc_new, aes(factor(INSTALL_RATE), ..count..)) + 
  geom_bar(aes(fill = Target), position = "dodge") + xlab("Installment Rates")

#AGE
mean(gc_new$AGE)                #33.53989
sd(gc_new$AGE, na.rm = FALSE)   #9.987323


#B

#b- train and test as 50% each
set.seed(1236)
d <- sample(2,nrow(gc_new), replace = TRUE, prob = c(0.5,0.5))
train1 <-gc_new[d==1,]
test1<- gc_new[d==2,]

formula<-Target~.
mytree<-rpart(formula,data=train1, parms = list(split="information"),
              control=rpart.control(minsplit = 2,minbucket = 10,cp=-1.5))
print(mytree)
rpart.plot(mytree)


##Error rate of predicted test and training data compared with are true vale###
#train(50%)
pred_train1 = predict(mytree, data_train=train1, type= "class")
mean(train1$Target!=pred_train1)     #0.183844
#test(50%)
pred_test1 = predict(mytree, data_test=test1, type= "class")
mean(test1$Target!=pred_test1)       #0.3286908
#error rate in test data is more the train data



#accuracy of model before pruning train data
prop.table(table(train1$Target))
mat1 <- table(train1$Target, pred_train1)
mat1
accuracy_Train_o <- sum(diag(mat1)) / sum(mat1)
accuracy_Train_o

#accuracy of model after pruning 
control <- rpart.control(minsplit = 10,
                         minbucket = 5,
                         maxdepth = 15,
                         cp = -1)
accuracy_tune <- function(fit) {
  pred_train1 <- predict(fit, train1, type = 'class')
  mat1 <- table(train1$Target, pred_train1)
  accuracy_Train <- sum(diag(mat1)) / sum(mat1)
  accuracy_Train
}
tune_fit <- rpart(Target~., train1, method = 'class', control = control)
accuracy_tune(tune_fit)


#b- train and test as 70% - 30%
d1 <- sample(2,nrow(gc_new), replace = TRUE, prob = c(0.7,0.3))
train2 <-gc_new[d1==1,]
test2<- gc_new[d1==2,]

formula1<-Target~.
mytree1<-rpart(formula1,data=train2, parms = list(split="information"),
               control=rpart.control(minsplit = 15,minbucket = 20,cp = -1.5))
print(mytree1)
rpart.plot(mytree1)


#train(70-30%)
pred_train2 = predict(mytree1, data_train1=train2, type= "class")
mean(train2$Target!=pred_train2)  #0.18825091
#test(70-30%)
pred_test2 = predict(mytree1, data_test1=test2, type= "class")
mean(test2$Target!=pred_test2)    #0.3279352
#error rate in train data is more than tets data

#accuracy of model before pruning train data
prop.table(table(train2$Target))
mat1_70 <- table(train2$Target, pred_train2)
mat1_70
accuracy_Train_o_70 <- sum(diag(mat1)) / sum(mat1)
accuracy_Train_o_70

#accuracy of model after pruning 
control <- rpart.control(minsplit = 10,
                         minbucket =5,
                         maxdepth = 15,
                         cp = -1)
accuracy_tune <- function(fit) {
  pred_train2 <- predict(fit, train2, type = 'class')
  mat1_70 <- table(train2$Target, pred_train2)
  accuracy_Train_70 <- sum(diag(mat1_70)) / sum(mat1_70)
  accuracy_Train_70
}
tune_fit_train_70 <- rpart(Target~., train2, method = 'class', control = control)
accuracy_tune(tune_fit_train_70)


#accuracy of model before pruning test data
prop.table(table(test2$Target))
mat2_70 <- table(test2$Target, pred_test2)
mat2_70
accuracy_Test_o_70 <- sum(diag(mat2_70)) / sum(mat2_70)
accuracy_Test_o_70   #0.8701923


control_test <- rpart.control(minsplit = 5,
                              minbucket = 4,
                              maxdepth = 15,
                              cp = -1)
accuracy_tune1 <- function(fit) {
  pred_test2 <- predict(fit, test2, type = 'class')
  mat2_70 <- table(test2$Target, pred_test2)
  accuracy_Test_70 <- sum(diag(mat2_70)) / sum(mat2_70)
  accuracy_Test_70
}
tune_fit_test_70 <- rpart(Target~., test2, method = 'class', control = control_test)
accuracy_tune(tune_fit_test_70) #0.731794 change it

#b- train and test as 80% - 20%
d2 <- sample(2,nrow(gc_new), replace = TRUE, prob = c(0.8,0.2))
train3 <-gc_new[d2==1,]
test3<- gc_new[d2==2,]

formula2<-Target~.
mytree3_train<-rpart(formula2,data=train3)
mytree3_test<-rpart(formula2,data=test3)
print(mytree3_train)
rpart.plot(mytree3_train)

print(mytree3_test)
rpart.plot(mytree3_test)

#train(80-20%)
pred_train3 = predict(mytree3_train, data_train2=train3, type= "class")
mean(train3$Target!=pred_train3)  #0.1634103
#test(80-20%)
pred_test3 = predict(mytree3_test, data_test2=test3, type= "class")
mean(test3$Target!=pred_test3)    #0.1366906
#error rate in train data is more than test data
#ERROR RATE FOR FIRST MODEL(50-50) IS MORE THAN THE REST,
#ERROR RATE FOR SECOND MODEL(70-30%) IS THE LEAST.

#accuracy of model before pruning train data
prop.table(table(train3$Target))
mat1_80 <- table(train3$Target, pred_train3)
mat1_80
accuracy_Train_o_80 <- sum(diag(mat1_80)) / sum(mat1_80)
accuracy_Train_o_80   #0.8365897

#accuracy of model after pruning 
control <- rpart.control(minsplit = 10,
                         minbucket =5,
                         maxdepth = 15,
                         cp = -1)
accuracy_tune <- function(fit) {
  pred_train3 <- predict(fit, train3, type = 'class')
  mat1_80 <- table(train3$Target, pred_train3)
  accuracy_Train_80 <- sum(diag(mat1_80)) / sum(mat1_80)
  accuracy_Train_80
}
tune_fit_train_80 <- rpart(Target~., train3, method = 'class', control = control)
accuracy_tune(tune_fit_train_80)  #0.8756661

#accuracy of model before pruning test data
prop.table(table(test3$Target))
mat2_80 <- table(test3$Target, pred_test3)
mat2_80
accuracy_Test_o_80 <- sum(diag(mat2_80)) / sum(mat2_80)
accuracy_Test_o_80   #0.8633094

control_test <- rpart.control(minsplit = 10,
                              minbucket = 5,
                              maxdepth = 25,
                              cp = 1)
accuracy_tune1 <- function(fit) {
  pred_test3 <- predict(fit, test3, type = 'class')
  mat2_80 <- table(test3$Target, pred_test3)
  accuracy_Test_80 <- sum(diag(mat2_80)) / sum(mat2_80)
  accuracy_Test_80
}
tune_fit_test_80 <- rpart(Target~., test3, method = 'class', control = control_test)
accuracy_tune(tune_fit_test_80)  #0.740675


#C

#Importing data
german_credit <- read_excel("German_Credit.xls")
colnames(german_credit)

#Confusion matrix for loss matrix
loss_1 <-matrix(c(0,5,1,0),byrow = TRUE, ncol = 2)
new_tree <- rpart(RESPONSE~., data=german_credit, method = "class",parms = list(loss=loss_1))
pred <- predict(new_tree,german_credit,type = "class")
table(actual = german_credit$RESPONSE, pred=pred)
x<-mean(pred!=german_credit$RESPONSE)
x
rpart.plot(new_tree)
german_credit

#Confusion matrix for Original tree
new_tree_1 <- rpart(RESPONSE~., data=german_credit, method = "class")
pred1 <- predict(new_tree_1,german_credit,type = "class")
table(actual = german_credit$RESPONSE, pred=pred1)
x<-mean(pred1!=german_credit$RESPONSE)
x
rpart.plot(new_tree_1)
german_credit

