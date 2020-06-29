setwd("C:/R/R_test")

#installing libraries

#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('e1071')
#install.packages('caTools')
#install.packages('pROC')
#install.packages('lubridate')

library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(e1071)
library(caTools)
library(pROC)
library(lubridate)
##########################################################################################


#Q1
#1.1: Read the file R and create a dataframe
carinsurance.raw <- read.csv("carInsurance_train.csv")
carinsurance <- carinsurance.raw
summary(carinsurance)
str(carinsurance)


#checking the features, removing nonuseful features
carinsurance$Id <- NULL
carinsurance$CarInsurance <- as.factor(carinsurance$CarInsurance)
str(carinsurance)

ggplot(carinsurance, aes(Age,CarInsurance)) + geom_boxplot()
ggplot(carinsurance, aes(Job,CarInsurance)) + geom_col()
ggplot(carinsurance, aes(Marital,CarInsurance)) + geom_col()
ggplot(carinsurance, aes(Education, CarInsurance)) + geom_col()
ggplot(carinsurance, aes(Default,CarInsurance)) + geom_col()
ggplot(carinsurance, aes(Balance, CarInsurance)) + geom_col()
ggplot(carinsurance, aes(HHInsurance,CarInsurance)) + geom_bar()
ggplot(carinsurance, aes(CarLoan,CarInsurance)) + geom_col()
ggplot(carinsurance, aes(Communication,CarInsurance)) + geom_col()
ggplot(carinsurance, aes(LastContactMonth,CarInsurance)) + geom_col()
ggplot(carinsurance, aes(LastContactDay,CarInsurance)) + geom_col()
ggplot(carinsurance, aes(NoOfContacts, CarInsurance)) + geom_col()
ggplot(carinsurance, aes(DaysPassed)) + geom_bar()
ggplot(carinsurance, aes(PrevAttempts)) + geom_bar()
ggplot(carinsurance, aes(Outcome,CarInsurance)) + geom_col()

str(carinsurance)

#1.2: create a new column and removing useless columns  
end.time <- hms(carinsurance$CallEnd)
start.time <- hms(carinsurance$CallStart)

call <- end.time - start.time
class(call)

#add column of call time and removing unnecesary columns
carinsurance$CallTime <- call
carinsurance$CallEnd <- NULL
carinsurance$CallStart <- NULL
carinsurance$HHInsurance <- NULL
carinsurance$NoOfContacts <- NULL
carinsurance$LastContactDay <- NULL
carinsurance$LastContactMonth <- NULL
carinsurance$PrevAttempts <- NULL
carinsurance$DaysPassed <- NULL
carinsurance$Education <- NULL

str(carinsurance)

#look at time impact
ggplot(carinsurance, aes(as.factor(CallTime), CarInsurance)) + geom_line()

carinsurance.prepared <- carinsurance

#1.3: turn the right features to factor
carinsurance.prepared$Age <- factor(carinsurance.prepared$Age)
carinsurance.prepared$Default <- factor(carinsurance.prepared$Default)
carinsurance.prepared$Balance <- factor(carinsurance.prepared$Balance)
carinsurance.prepared$CarLoan <- factor(carinsurance.prepared$CarLoan)

str(carinsurance.prepared)


#changing columns
table(carinsurance$Job)
table(carinsurance$Marital)
table(carinsurance$Job)
table(carinsurance$Communication)
table(carinsurance$Outcome)

carinsurance$Balance <- factor(carinsurance$Balance)

carinsurance.prepared$Default <- factor(carinsurance.prepared$Default, levels = c(0,1), labels = c('no', 'yes'))
carinsurance.prepared$CarLoan <- factor(carinsurance.prepared$CarLoan, levels = c(0,1), labels = c('no', 'yes'))
carinsurance.prepared$CarInsurance <- factor(carinsurance.prepared$CarInsurance, levels = c(0,1), labels = c('no', 'yes'))

str(carinsurance.prepared)


#Q2

is.na(carinsurance.prepared$CarInsurance)
sum(is.na(carinsurance$CarInsurance))

####
#seperating into train and test
library(caTools)

filter <- sample.split(carinsurance.prepared$Age, SplitRatio = 0.7)
insurance.train <- subset(carinsurance.prepared, filter==T)
insurance.test <- subset(carinsurance.prepared, filter==F)

dim(carinsurance.prepared)
dim(insurance.train)
dim(insurance.test)

#model <- lm(CarInsurance ~ ., insurance.train)
#summary(model)
