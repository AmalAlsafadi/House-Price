
# CKME 136 Capstone Project 
# Ames Housing dataset 

install.packages("corrplot")
install.packages("party")
install.packages("caret")
install.packages("class")
install.packages("e1071")
library(corrplot)
library(party)
library(caret)
library(class)
library(e1071)

# read dataset file and check data summary
HP_Train <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/train.csv",header=T,sep=",")
View(HP_Train)
str(HP_Train)
summary(HP_Train)

# print a list of the column names
names(HP_Train)

# attch HP_Train dataset to be the current data
attach(HP_Train)

# check for missing values
length(which(is.na(HP_Train)))

MissingVariables <- sapply(HP_Train, function(x) sum(is.na(x))); M[M>0]

HP_Train$countNA<- apply(HP_Train, 1, function(x) sum(is.na(x)))
  rowSums(is.na(HP_Train))
HP_Train[HP_Train$countNA>15]

# Plot the home price
hist(SalePrice, breaks = 50)
skewness(SalePrice)
kurtosis(SalePrice)

