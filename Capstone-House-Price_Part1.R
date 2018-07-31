
###############################################################################################
# CKME 136 Capstone Project   - House Prices: Advanced Regression Techniques                  #                                              #
# Ames Housing dataset                                                                        #
# By: Amal Alsafadi                                                                           #
###############################################################################################

# Part 1: Exploratory Data Analysis

# install required packages

install.packages("corrplot")      # a graphical display of a correlation matrix, confidence interval
#install.packages("party")
install.packages("caret")         # classification and regression training package
install.packages("class")         # various functions for classification, including k-nearest neighbour
install.packages("e1071")         # Explore the Skewness of various features of the dataset
install.packages("data.table")    # Fast aggregation of large data 
install.packages("dplyr")         # data manipulation
#install.packages("vcd")           # Visualizing Categorical Data
#install.packages("mlbench")       # A collection of artificial and real-world machine learning benchmark problems
install.packages("randomForest") 
#install.packages("MASS")          # stepwise regression
#install.packages("leaps")         # all subsets regression
install.packages("glmnet")        # Lasso and ridge regression
install.packages("DAAG")          # using CVlm
install.packages("lmtest")
install.packages("xgboost")       #xgboost regression
install.packages("ggplot2")
install.packages("caretEnsemble")
install.packages("gridExtra")
install.packages("RCurl")
library(gridExtra)
library(caretEnsemble)
library(ggplot2)
library(xgboost)
library(MASS)
library(leaps) 
library(glmnet)
library(corrplot)
library(party)
library(caret)
library(class)
library(e1071)
library(data.table)
library(dplyr)
library(vcd)
library(mlbench)
library(randomForest)
library(DAAG)
library(lmtest)
library(RCurl)
#####################################################################################

# read train and test dataset files and check data summary
HP_Train <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/train.csv",header=T,sep=",")
HP_Test <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/test.csv",header=T,sep=",")

# Have a look at the data
View(HP_Train)
View(HP_Test)

# Check variables data types and summary
str(HP_Train)
str(HP_Test)

summary(HP_Train)
summary(HP_Test)

# check for outliers for numerical attributes .
# relation between attribute: GrLivArea and target attribute: SalePrice in train set
par(mfrow=c(1,2))
boxplot(HP_Train$GrLivArea, main="GrLivArea")
plot(HP_Train$SalePrice~HP_Train$GrLivArea, main ="SalePrice vs. GrLivArea")

#remove outliers from the train data

outliersT <- c((which(HP_Train$GrLivArea>4000)))
HP_Train <- HP_Train[-outliersT,]
SalePrice<-HP_Train[,81]

# Combine both test and train dataset (without SalePrice attribute) to apply consistant changes.
FullData <- rbind(HP_Train[1:80],HP_Test)

# save Test Id and drop Id attributes for both train and test data
IdTest <- HP_Test[,1]
FullData <- FullData[,-1]

# check for missing values in train and test dataset
FullMissing <- length(which(is.na(FullData)))
TrainMissing <- length(which(is.na(HP_Train)))
TestMissing <- length(which(is.na(HP_Test)))

# transform MSSubClass into nominal values
FullData$MSSubClass <- as.factor(FullData$MSSubClass)

# find the missing values grouped by column name
MissingTrain <- sapply(HP_Train, function(x) sum(is.na(x))); MissingTrain[MissingTrain>0]
MissingTest <- sapply(HP_Test, function(x) sum(is.na(x))); MissingTest[MissingTest>0]


# Replace NA with None or NoVariable
#Alley >>>NoAlley
  levels <- levels(FullData$Alley)
  levels[length(levels) + 1] <- "NoAlley"
  FullData$Alley <- factor(FullData$Alley, levels = levels)
  FullData$Alley[is.na(FullData$Alley)] <-"NoAlley"

#Pool >>>NoPool
  levels <- levels(FullData$PoolQC)
  levels[length(levels) + 1] <- "NoPool"
  FullData$PoolQC <- factor(FullData$PoolQC, levels = levels)
  FullData$PoolQC[is.na(FullData$PoolQC)] <-"NoPool" 
  
#Fence >>>NoFence
  levels <- levels(FullData$Fence)
  levels[length(levels) + 1] <- "NoFence"
  FullData$Fence <- factor(FullData$Fence, levels = levels)
  FullData$Fence[is.na(FullData$Fence)] <-"NoFence"  
  
#Fireplace >>>NoFireplace
  levels <- levels(FullData$FireplaceQu)
  levels[length(levels) + 1] <- "NoFireplace"
  FullData$FireplaceQu <- factor(FullData$FireplaceQu, levels = levels)
  FullData$FireplaceQu[is.na(FullData$FireplaceQu)] <-"NoFireplace" 

#MiscFeature >>>None
  levels <- levels(FullData$MiscFeature)
  levels[length(levels) + 1] <- "None"
  FullData$MiscFeature <- factor(FullData$MiscFeature, levels = levels)
  FullData$MiscFeature[is.na(FullData$MiscFeature)] <-"None" 
    
#Garage >>>NoGarage
  levels <- levels(FullData$GarageQual)
  levels[length(levels) + 1] <- "NoGarage"
  FullData$GarageQual <- factor(FullData$GarageQual, levels = levels)
  FullData$GarageQual[is.na(FullData$GarageQual)] <-"NoGarage"
  
  levels <- levels(FullData$GarageType)
  levels[length(levels) + 1] <- "NoGarage"
  FullData$GarageType <- factor(FullData$GarageType, levels = levels)
  FullData$GarageType[is.na(FullData$GarageType)] <-"NoGarage"
  
  FullData$GarageYrBlt[is.na(FullData$GarageYrBlt)] <-median(FullData$GarageYrBlt, na.rm = TRUE)
  
  levels <- levels(FullData$GarageCond)
  levels[length(levels) + 1] <- "NoGarage"
  FullData$GarageCond <- factor(FullData$GarageCond, levels = levels)
  FullData$GarageCond[is.na(FullData$GarageCond)] <-"NoGarage"
  
  levels <- levels(FullData$GarageFinish)
  levels[length(levels) + 1] <- "NoGarage"
  FullData$GarageFinish <- factor(FullData$GarageFinish, levels = levels)
  FullData$GarageFinish[is.na(FullData$GarageFinish)] <-"NoGarage"  
  
#Basement >>>NoBasement
  levels <- levels(FullData$BsmtQual)
  levels[length(levels) + 1] <- "NoBasement"
  FullData$BsmtQual <- factor(FullData$BsmtQual, levels = levels)
  FullData$BsmtQual[is.na(FullData$BsmtQual)] <-"NoBasement"
  
  levels <- levels(FullData$BsmtFinType1)
  levels[length(levels) + 1] <- "NoBasement"
  FullData$BsmtFinType1 <- factor(FullData$BsmtFinType1, levels = levels)
  FullData$BsmtFinType1[is.na(FullData$BsmtFinType1)] <-"NoBasement"
 
  levels <- levels(FullData$BsmtFinType2)
  levels[length(levels) + 1] <- "NoBasement"
  FullData$BsmtFinType2 <- factor(FullData$BsmtFinType1, levels = levels)
  FullData$BsmtFinType2[is.na(FullData$BsmtFinType2)] <-"NoBasement" 
  
  levels <- levels(FullData$BsmtExposure)
  levels[length(levels) + 1] <- "NoBasement"
  FullData$BsmtExposure <- factor(FullData$BsmtExposure, levels = levels)
  FullData$BsmtExposure[is.na(FullData$BsmtExposure)] <-"NoBasement"
  
  levels <- levels(FullData$BsmtCond)
  levels[length(levels) + 1] <- "NoBasement"
  FullData$BsmtCond <- factor(FullData$BsmtCond, levels = levels)
  FullData$BsmtCond[is.na(FullData$BsmtCond)] <-"NoBasement"

#  MasVnrType,MasVnrArea,Electrical  >> mode
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
 
#  Functional >> None
  levels <- levels(FullData$Functional)
  levels[length(levels) + 1] <- "None"
  FullData$Functional <- factor(FullData$Functional, levels = levels)
  FullData$Functional[is.na(FullData$Functional)] <-"None"
  
  FullData$MasVnrType[is.na(FullData$MasVnrType)] <-Mode(FullData$MasVnrType)
  FullData$MasVnrArea[is.na(FullData$MasVnrArea)] <-Mode(FullData$MasVnrArea)
  FullData$Electrical[is.na(FullData$Electrical)] <-Mode(FullData$Electrical)
  FullData$GarageArea[is.na(FullData$GarageArea)] <-median(FullData$GarageArea, na.rm = TRUE)
  FullData$GarageCars[is.na(FullData$GarageCars)] <-0
    FullData$MSZoning[is.na(FullData$MSZoning)] <-Mode(FullData$MSZoning)
  FullData$Utilities[is.na(FullData$Utilities)] <-Mode(FullData$Utilities)
  FullData$Exterior1st[is.na(FullData$Exterior1st)] <-Mode(FullData$Exterior1st)
  FullData$Exterior2nd[is.na(FullData$Exterior2nd)] <-Mode(FullData$Exterior2nd)
  FullData$SaleType[is.na(FullData$SaleType)] <-Mode(FullData$SaleType)
  FullData$KitchenQual[is.na(FullData$KitchenQual)] <-Mode(FullData$KitchenQual)
  FullData$BsmtFinSF1[is.na(FullData$BsmtFinSF1)] <- 0
  FullData$BsmtFinSF2[is.na(FullData$BsmtFinSF2)] <- 0
  FullData$BsmtUnfSF[is.na(FullData$BsmtUnfSF)] <- 0
  FullData$TotalBsmtSF[is.na(FullData$TotalBsmtSF)] <- 0
  FullData$BsmtFullBath[is.na(FullData$BsmtFullBath)] <- 0
  FullData$BsmtHalfBath[is.na(FullData$BsmtHalfBath)] <- 0
  
# replace NA in LotFrontage with the median of LotFrontage in the corresponding neighborhood
# calculate the median Frontage for each neighborhood.
  
FrontageByNe<-aggregate(LotFrontage ~ Neighborhood, FullData, median)
index <- which(is.na(FullData$LotFrontage))
  for (i in index) {
    med_frontage = FrontageByNe[FrontageByNe$Neighborhood == FullData$Neighborhood[i], 'LotFrontage']
    # then replace the missing value with the median
    FullData[i, 'LotFrontage'] = med_frontage[[1]]
  }

# Check again the missing values grouped by column name
  FMissing <- sapply(FullData, function(x) sum(is.na(x))); FMissing[FMissing>0]

# seperate train and test data
  HP_Train <- FullData[1:nrow(HP_Train),]
  HP_Test <- FullData[nrow(HP_Train)+1:nrow(HP_Test),]
  
# add target variable to HP_train
  HP_Train <- cbind(HP_Train,SalePrice)
  

# save backup of the clean datasets (free of missing and outliers values)
  HP_TrainB <- HP_Train
  HP_TestB <- HP_Test
  FullDataB <- FullData
# save clean data as CSV file
  write.csv(HP_Train, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/CleanTrain.csv")
  write.csv(HP_Test, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/CleanTest.csv")
  write.csv(FullData, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/CleanFullData.csv")
  
# Seperate numeric variables to check correlation
  numVars <- which(sapply(HP_Train, is.numeric)) #index vector numeric variables
  numVarNames <- names(numVars) 
 
  Tr_numVar <- HP_Train[, numVars]
  cor_numVar <- cor(Tr_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
  
  #sort on decreasing correlations with SalePrice
  cor_sorted <- as.matrix(sort(cor_numVar[,"SalePrice"], decreasing = TRUE))
  #select only high corelations
  CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
  cor_numVar <- cor_numVar[CorHigh, CorHigh]
  
  corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
 
  cor.test(HP_Train$LotArea,HP_Train$SalePrice)
  cor.test(YearBuilt,GarageYrBlt)
  cor.test(GarageArea,GarageCars)
  cor.test(GrLivArea,TotRmsAbvGrd)
  cor.test(X2ndFlrSF,GrLivArea)
  cor.test(X1stFlrSF,TotalBsmtSF)
  cor.test(BsmtFinSF1,BsmtFullBath)
  cor.test(YearBuilt,YearRemodAdd)
  cor.test(SalePrice, GrLivArea)
  cor.test(SalePrice, GarageArea)
  cor.test(SalePrice, GarageCars)
  cor.test(SalePrice,TotalBsmtSF)
  cor.test(SalePrice, TotRmsAbvGrd)
  cor.test(SalePrice, X1stFlrSF)
  cor.test(SalePrice, X2ndFlrSF)
  cor.test(SalePrice, X2ndFlrSF)
  cor.test(SalePrice,BsmtFullBath)
  cor.test(LotArea,SalePrice)
  
  #num_data = subset(num_data, select = -c(GarageCars, TotRmsAbvGrd, GarageYrBlt, X1stFlrSF, X2ndFlrSF, BsmtFullBath))
  #num_data = subset(num_data, select = -c(OverallCond, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, PoolArea, YrSold, MoSold, ScreenPorch,X3SsnPorch, EnclosedPorch, OpenPorchSF, WoodDeckSF, KitchenAbvGr, HalfBath, FullBath, BsmtHalfBath, LowQualFinSF))
  #num_data = subset(num_data, select = -c(LotArea, YearRemodAdd, MiscVal, Fireplaces, BedroomAbvGr, MasVnrArea, LotFrontage))
  
  
# Analyze and Plot the histogram of SalePrice and the curve density
 summary(HP_Train$SalePrice)
 hist(HP_Train$SalePrice, col="peachpuff", border="black", prob = TRUE)
 lines(density(HP_Train$SalePrice), lwd = 2,    col = "chocolate3")
 
# Skewness is a measure of the symmetry in a distribution. Skewness essentially measures the relative size of the two tails. 
#A normal distribution will have a skewness of 0. 
# Kurtosis is a measure of the combined sizes of the two tails.  
# It measures the amount of probability in the tails.  The kurtosis of the normal distribution is equal to 3.  
#If the kurtosis is greater than 3, the dataset has heavier tails than a normal distribution (more in the tails). 
# If the kurtosis is less than 3, then the dataset has lighter tails than a normal distribution (less in the tails).    

skewness(HP_Train$SalePrice)
kurtosis(HP_Train$SalePrice)

# plot the histogram of log of SalePrice and the curve density
hist(log(HP_Train$SalePrice), col="peachpuff", border="black", prob = TRUE)
lines(density(log(HP_Train$SalePrice)), lwd = 2,    col = "chocolate3")


#This step to measure the skewed numeic features that more than 0.75
classes <- lapply(FullData,function(x) class(x))
numeric_feats <- names(classes[classes=="integer" | classes=="numeric"])
factor_feats <- names(classes[classes=="factor"| classes=="character"])

skewed_feats <- sapply(numeric_feats, function(x) skewness(FullData[[x]]))
skewed_feats <- skewed_feats[abs(skewed_feats) > .75]
skewed_feats

#Take log transformation of features for which skewness more than 0.75

for (x in names(skewed_feats)) {FullData[[x]] <- log(FullData[[x]]+1)}


# Check variable significance from the plot.
plot(HP_Train$Alley, main="Alley")
plot(HP_Train$Street, main="Street")
plot(HP_Train$Fence, main="Fence")
plot(HP_Train$PoolQC, main="PoolQC")
plot(HP_Train$Utilities, main="Utilities")
plot(HP_Train$Heating, main="Heating")
plot(HP_Train$Electrical, main="Electrical")
plot(HP_Train$PavedDrive, main="PavedDrive")
plot(HP_Train$MiscFeature, main="MiscFeature")
plot(HP_Train$LandSlope, main="LandSlope")
plot(HP_Train$RoofMatl, main="RoofMatl")
plot(HP_Train$Condition2, main="Condition2")
plot(HP_Train$CentralAir, main="CentralAir")
plot(HP_Train$RoofMatl, main="RoofMatl")
plot(HP_Train$Exterior1st)

#delete variables with no sufficient significance in predicting 
#HP_TrainD = subset(HP_Train, select = -c(Electrical,PoolQC,Fence, Heating, RoofMatl, Utilities, Condition2,CentralAir, LandContour, LandSlope,MiscFeature, Alley
 #                                        , Street, PavedDrive, SaleCondition,SaleType, GarageCond, GarageQual, ExterCond, Condition1,GarageCars, TotRmsAbvGrd, GarageYrBlt, X1stFlrSF, X2ndFlrSF, BsmtFullBath) )

#HP_TestD = subset(HP_Test, select = -c(Electrical,PoolQC,Fence, Heating, RoofMatl, Utilities, Condition2,CentralAir, LandContour, LandSlope,MiscFeature, Alley
  #                                   , Street, PavedDrive, SaleCondition,SaleType, GarageCond, GarageQual, ExterCond, Condition1, GarageCars, TotRmsAbvGrd, GarageYrBlt, X1stFlrSF, X2ndFlrSF, BsmtFullBath) )

#Use dummyvars function in caret package for one-hot encoding of categorical variables

dummies <- dummyVars(~., data = FullData,fullRank=T)
FullReadyData <- data.frame(predict(dummies, newdata = FullData))

TrainUpdated <-FullReadyData[1:nrow(HP_Train),]
TrainUpdated <- cbind(TrainUpdated,SalePrice)
TrainUpdated$SalePrice <- log(TrainUpdated$SalePrice+1)
TestUpdated <- FullReadyData[nrow(HP_Train)+1:nrow(HP_Test),]

# save clean data as CSV file
write.csv(TrainUpdated, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TrainUpdated.csv")
write.csv(TestUpdated, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TestUpdated.csv")
write.csv(FullReadyData, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/UpdatedFullData.csv")

