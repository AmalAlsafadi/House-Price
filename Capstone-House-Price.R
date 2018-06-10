
# CKME 136 Capstone Project 
# Ames Housing dataset 
###############################################################################################

# install required packages

install.packages("corrplot")
install.packages("party")
install.packages("caret")
install.packages("class")
install.packages("e1071")
install.packages("data.table")
install.packages("dplyr")
library(corrplot)
library(party)
library(caret)
library(class)
library(e1071)
library(data.table)
library(dplyr)
#####################################################################################

# read train and test dataset files and check data summary
HP_Train <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/train.csv",header=T,sep=",")
HP_Test <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/test.csv",header=T,sep=",")
View(HP_Train)
View(HP_Test)

str(HP_Train)
str(HP_Test)

summary(HP_Train)
summary(HP_Test)

# print a list of the column names
names(HP_Train)

# attch HP_Train dataset to be the current data
attach(HP_Train)

# check for outliers for numerical attributes
# attribute: GrLivArea
boxplot(GrLivArea, main="GrLivArea")
plot(SalePrice~GrLivArea)

length(which(GrLivArea>4000))
outliers <- c((which(GrLivArea>4000)))
HP_Train1 <- HP_Train[-outliers,]

# Do the same for test dataset
boxplot(HP_Test$GrLivArea, main="GrLivArea - Test")

length(which(HP_Test$GrLivArea>4000))
outliersTest <- c((which(HP_Test$GrLivArea>4000)))
HP_Test1 <- HP_Test[-outliersTest,]

# check for missing values in train dataset
TrainMissing <- length(which(is.na(HP_Train)))
TrainMissing <- length(which(is.na(HP_Test)))

# find the missing values grouped by column name
MissingTrain <- sapply(HP_Train, function(x) sum(is.na(x))); MissingTrain[MissingTrain>0]
MissingTest <- sapply(HP_Test, function(x) sum(is.na(x))); MissingTest[MissingTest>0]

# Replace NA with None or NoVariable
#Alley >>>NoAlley
  levels <- levels(HP_Train$Alley)
  levels[length(levels) + 1] <- "NoAlley"
  HP_Train$Alley <- factor(HP_Train$Alley, levels = levels)
  HP_Train$Alley[is.na(Alley)] <-"NoAlley"

#Pool >>>NoPool
  levels <- levels(HP_Train$PoolQC)
  levels[length(levels) + 1] <- "NoPool"
  HP_Train$PoolQC <- factor(HP_Train$PoolQC, levels = levels)
  HP_Train$PoolQC[is.na(PoolQC)] <-"NoPool" 
  
#Fence >>>NoFence
  levels <- levels(HP_Train$Fence)
  levels[length(levels) + 1] <- "NoFence"
  HP_Train$Fence <- factor(HP_Train$Fence, levels = levels)
  HP_Train$Fence[is.na(Fence)] <-"NoFence"  
  
#Fireplace >>>NoFireplace
  levels <- levels(HP_Train$FireplaceQu)
  levels[length(levels) + 1] <- "NoFireplace"
  HP_Train$FireplaceQu <- factor(HP_Train$FireplaceQu, levels = levels)
  HP_Train$FireplaceQu[is.na(FireplaceQu)] <-"NoFireplace" 

#MiscFeature >>>None
  levels <- levels(HP_Train$MiscFeature)
  levels[length(levels) + 1] <- "None"
  HP_Train$MiscFeature <- factor(HP_Train$MiscFeature, levels = levels)
  HP_Train$MiscFeature[is.na(MiscFeature)] <-"None" 
    
#Garage >>>NoGarage
  levels <- levels(HP_Train$GarageQual)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Train$GarageQual <- factor(HP_Train$GarageQual, levels = levels)
  HP_Train$GarageQual[is.na(GarageQual)] <-"NoGarage"
  
  levels <- levels(HP_Train$GarageType)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Train$GarageType <- factor(HP_Train$GarageType, levels = levels)
  HP_Train$GarageType[is.na(GarageType)] <-"NoGarage"
  
  HP_Train$GarageYrBlt[is.na(GarageYrBlt)] <-median(HP_Train$GarageYrBlt, na.rm = TRUE)
  
  levels <- levels(HP_Train$GarageCond)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Train$GarageCond <- factor(HP_Train$GarageCond, levels = levels)
  HP_Train$GarageCond[is.na(GarageCond)] <-"NoGarage"
  
  levels <- levels(HP_Train$GarageFinish)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Train$GarageFinish <- factor(HP_Train$GarageFinish, levels = levels)
  HP_Train$GarageFinish[is.na(GarageFinish)] <-"NoGarage"  
  
#Basement >>>NoBasement
  levels <- levels(HP_Train$BsmtQual)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Train$BsmtQual <- factor(HP_Train$BsmtQual, levels = levels)
  HP_Train$BsmtQual[is.na(BsmtQual)] <-"NoBasement"
  
  levels <- levels(HP_Train$BsmtFinType1)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Train$BsmtFinType1 <- factor(HP_Train$BsmtFinType1, levels = levels)
  HP_Train$BsmtFinType1[is.na(BsmtFinType1)] <-"NoBasement"
 
  levels <- levels(HP_Train$BsmtFinType2)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Train$BsmtFinType2 <- factor(HP_Train$BsmtFinType1, levels = levels)
  HP_Train$BsmtFinType2[is.na(BsmtFinType2)] <-"NoBasement" 
  
  levels <- levels(HP_Train$BsmtExposure)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Train$BsmtExposure <- factor(HP_Test$BsmtExposure, levels = levels)
  HP_Train$BsmtExposure[is.na(BsmtExposure)] <-"NoBasement"
  
  levels <- levels(HP_Train$BsmtCond)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Train$BsmtCond <- factor(HP_Train$BsmtCond, levels = levels)
  HP_Train$BsmtCond[is.na(BsmtCond)] <-"NoBasement"

#  MasVnrType,MasVnrArea,Electrical  >> mode
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  HP_Train$MasVnrType[is.na(MasVnrType)] <-Mode(MasVnrType)
  HP_Train$MasVnrArea[is.na(MasVnrArea)] <-Mode(MasVnrArea)
  HP_Train$Electrical[is.na(Electrical)] <-Mode(Electrical)
  
# replace NA in LotFrontage with the median of LotFrontage in the corresponding neighborhood
# calculate the median Frontage for each neighborhood.
  
FrontageByNe<-aggregate(LotFrontage ~ Neighborhood, HP_Train, median)
index <- which(is.na(HP_Train$LotFrontage))
  for (i in index) {
    med_frontage = FrontageByNe[FrontageByNe$Neighborhood == HP_Train$Neighborhood[i], 'LotFrontage']
    # then replace the missing value with the median
    HP_Train[i, 'LotFrontage'] = med_frontage[[1]]
  }

# Check again the missing values grouped by column name
  MissingTrain <- sapply(HP_Train, function(x) sum(is.na(x))); MissingTrain[MissingTrain>0]

  
# Doing the same for the test data
  
  # Replace NA with None or NoVariable
  #Alley >>>NoAlley
  levels <- levels(HP_Test$Alley)
  levels[length(levels) + 1] <- "NoAlley"
  HP_Test$Alley <- factor(HP_Test$Alley, levels = levels)
  HP_Test$Alley[is.na(HP_Test$Alley)] <-"NoAlley"
  
  #Pool >>>NoPool
  levels <- levels(HP_Test$PoolQC)
  levels[length(levels) + 1] <- "NoPool"
  HP_Test$PoolQC <- factor(HP_Test$PoolQC, levels = levels)
  HP_Test$PoolQC[is.na(HP_Test$PoolQC)] <-"NoPool" 
  
  #Fence >>>NoFence
  levels <- levels(HP_Test$Fence)
  levels[length(levels) + 1] <- "NoFence"
  HP_Test$Fence <- factor(HP_Test$Fence, levels = levels)
  HP_Test$Fence[is.na(HP_Test$Fence)] <-"NoFence"  
  
  #Fireplace >>>NoFireplace
  levels <- levels(HP_Test$FireplaceQu)
  levels[length(levels) + 1] <- "NoFireplace"
  HP_Test$FireplaceQu <- factor(HP_Test$FireplaceQu, levels = levels)
  HP_Test$FireplaceQu[is.na(HP_Test$FireplaceQu)] <-"NoFireplace" 
  
  #MiscFeature >>>None
  levels <- levels(HP_Test$MiscFeature)
  levels[length(levels) + 1] <- "None"
  HP_Test$MiscFeature <- factor(HP_Test$MiscFeature, levels = levels)
  HP_Test$MiscFeature[is.na(HP_Test$MiscFeature)] <-"None" 
  
  #Garage >>>NoGarage
  levels <- levels(HP_Test$GarageQual)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Test$GarageQual <- factor(HP_Test$GarageQual, levels = levels)
  HP_Test$GarageQual[is.na(HP_Test$GarageQual)] <-"NoGarage"
  
  levels <- levels(HP_Test$GarageType)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Test$GarageType <- factor(HP_Test$GarageType, levels = levels)
  HP_Test$GarageType[is.na(HP_Test$GarageType)] <-"NoGarage"
  
  HP_Test$GarageYrBlt[is.na(HP_Test$GarageYrBlt)] <-median(HP_Test$GarageYrBlt, na.rm = TRUE)
  
  levels <- levels(HP_Test$GarageCond)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Test$GarageCond <- factor(HP_Test$GarageCond, levels = levels)
  HP_Test$GarageCond[is.na(HP_Test$GarageCond)] <-"NoGarage"
  
  levels <- levels(HP_Test$GarageFinish)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Test$GarageFinish <- factor(HP_Test$GarageFinish, levels = levels)
  HP_Test$GarageFinish[is.na(HP_Test$GarageFinish)] <-"NoGarage"  
  
  levels <- levels(HP_Test$GarageArea)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Test$GarageArea <- factor(HP_Test$GarageArea, levels = levels)
  HP_Test$GarageArea[is.na(HP_Test$GarageArea)] <-"NoGarage"
  
  levels <- levels(HP_Test$GarageCars)
  levels[length(levels) + 1] <- "NoGarage"
  HP_Test$GarageCars <- factor(HP_Test$GarageCars, levels = levels)
  HP_Test$GarageCars[is.na(HP_Test$GarageCars)] <-"NoGarage"  
  
  #Basement >>>NoBasement
  levels <- levels(HP_Test$BsmtQual)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Test$BsmtQual <- factor(HP_Test$BsmtQual, levels = levels)
  HP_Test$BsmtQual[is.na(HP_Test$BsmtQual)] <-"NoBasement"
  
  levels <- levels(HP_Test$BsmtFinType1)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Test$BsmtFinType1 <- factor(HP_Test$BsmtFinType1, levels = levels)
  HP_Test$BsmtFinType1[is.na(HP_Test$BsmtFinType1)] <-"NoBasement"
  
  levels <- levels(HP_Test$BsmtFinType2)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Test$BsmtFinType2 <- factor(HP_Test$BsmtFinType1, levels = levels)
  HP_Test$BsmtFinType2[is.na(HP_Test$BsmtFinType2)] <-"NoBasement" 
  
  levels <- levels(HP_Test$BsmtExposure)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Test$BsmtExposure <- factor(HP_Test$BsmtExposure, levels = levels)
  HP_Test$BsmtExposure[is.na(HP_Test$BsmtExposure)] <-"NoBasement"
  
  levels <- levels(HP_Test$BsmtCond)
  levels[length(levels) + 1] <- "NoBasement"
  HP_Test$BsmtCond <- factor(HP_Test$BsmtCond, levels = levels)
  HP_Test$BsmtCond[is.na(HP_Test$BsmtCond)] <-"NoBasement"
  
  levels <- levels(HP_Test$Functional)
  levels[length(levels) + 1] <- "None"
  HP_Test$Functional <- factor(HP_Test$Functional, levels = levels)
  HP_Test$Functional[is.na(HP_Test$Functional)] <-"None"
  
  #  MasVnrType,MasVnrArea,Electrical  >> mode
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  HP_Test$MasVnrType[is.na(HP_Test$MasVnrType)] <-Mode(HP_Test$MasVnrType)
  HP_Test$MasVnrArea[is.na(HP_Test$MasVnrArea)] <-Mode(HP_Test$MasVnrArea)
  HP_Test$Electrical[is.na(HP_Test$Electrical)] <-Mode(HP_Test$Electrical)
  HP_Test$MSZoning[is.na(HP_Test$MSZoning)] <-Mode(HP_Test$MSZoning)
  HP_Test$Utilities[is.na(HP_Test$Utilities)] <-Mode(HP_Test$Utilities)
  HP_Test$Exterior1st[is.na(HP_Test$Exterior1st)] <-Mode(HP_Test$Exterior1st)
  HP_Test$Exterior2nd[is.na(HP_Test$Exterior2nd)] <-Mode(HP_Test$Exterior2nd)
  HP_Test$SaleType[is.na(HP_Test$SaleType)] <-Mode(HP_Test$SaleType)
  HP_Test$KitchenQual[is.na(HP_Test$KitchenQual)] <-Mode(HP_Test$KitchenQual)
  HP_Test$BsmtFinSF1[is.na(HP_Test$BsmtFinSF1)] <- 0
  HP_Test$BsmtFinSF2[is.na(HP_Test$BsmtFinSF2)] <- 0
  HP_Test$BsmtUnfSF[is.na(HP_Test$BsmtUnfSF)] <- 0
  HP_Test$TotalBsmtSF[is.na(HP_Test$TotalBsmtSF)] <- 0
  HP_Test$BsmtFullBath[is.na(HP_Test$BsmtFullBath)] <- 0
  HP_Test$BsmtHalfBath[is.na(HP_Test$BsmtHalfBath)] <- 0
  
  # replace NA in LotFrontage with the median of LotFrontage in the corresponding neighborhood
  # calculate the median Frontage for each neighborhood.
  
  FrontageByNe<-aggregate(LotFrontage ~ Neighborhood, HP_Test, median)
  index <- which(is.na(HP_Test$LotFrontage))
  for (i in index) {
    med_frontage = FrontageByNe[FrontageByNe$Neighborhood == HP_Test$Neighborhood[i], 'LotFrontage']
    # then replace the missing value with the median
    HP_Test[i, 'LotFrontage'] = med_frontage[[1]]
  }
  
  # Check again the missing values grouped by column name
  MissingTest <- sapply(HP_Test, function(x) sum(is.na(x))); MissingTest[MissingTest>0]
  
  
    
# delete the variables with more than 80% missing
  
HP_Train2<-  subset(HP_Train, select=-c(Id,Alley,PoolQC,Fence,MiscFeature))
View(HP_Train2)


# Plot the home price
hist(SalePrice, breaks = 50)
skewness(SalePrice)
kurtosis(SalePrice)


hist(BedroomAbvGr)
max(BedroomAbvGr)
min(BedroomAbvGr)


# check correlation between numeric data.
cor.test(BedroomAbvGr,SalePrice)
plot(YearBuilt,SalePrice)
cor.test(LotArea,SalePrice)
cor.test(HP_Train)
boxplot(LotArea, main="Lot Area")
length(which(LotArea>50000))
summary(LotArea)

#remove outliers (11 observation where Area >50000)
HP1<- HP_Train[which(LotArea<50000),]
boxplot(LotArea, main="Lot Area")
length(which(LotArea>50000))
summary(HP1$LotArea)
boxplot(HP1$LotArea, main="Lot Area")

#remove outliers (11 observation where Area >50000)
HP2<- HP_Train[which(LotArea<16000),]
boxplot(HP2$LotArea, main="Lot Area")
length(which(LotArea>15000))
summary(HP2$LotArea)
boxplot(HP2$LotArea, main="Lot Area")
cor.test(HP2$LotArea,HP2$SalePrice)

cor(HP2)

cor(GrLivArea,SalePrice)
plot(GrLivArea,SalePrice)

cor(TotalBsmtSF,SalePrice)
plot(TotalBsmtSF,SalePrice)

cor(TotalBsmtSF,SalePrice)
plot(TotalBsmtSF,SalePrice)

cor(GarageArea,SalePrice)
plot(GarageArea,SalePrice)
