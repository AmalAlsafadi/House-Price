
###############################################################################################
# CKME 136 Capstone Project                                                                   #
# Ames Housing dataset                                                                        #
###############################################################################################

# install required packages

install.packages("corrplot")
install.packages("party")
install.packages("caret")
install.packages("class")
install.packages("e1071")
install.packages("data.table")
install.packages("dplyr")
install.packages("vcd")
install.packages("mlbench")
install.packages("randomForest")
library(mlbench)
library(randomForest)
library(corrplot)
library(party)
library(caret)
library(class)
library(e1071)
library(data.table)
library(dplyr)
library(vcd)
#####################################################################################

# read train and test dataset files and check data summary
HP_Train <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/train.csv",header=T,sep=",")
HP_Test <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/test.csv",header=T,sep=",")


# Combine both test and train dataset (without SalePrice attribute) to apply consistant changes.
FullData <- rbind(HP_Train[1:80],HP_Test)

# Have a look at the data
View(HP_Train)
View(HP_Test)
View(FullData)

# Check variables data types and summary
str(HP_Train)
str(HP_Test)

summary(HP_Train)
summary(HP_Test)

# print a list of the column names
names(FullData)

# attch FullData dataset to be the current data
attach(FullData)

# check for outliers for numerical attributes .
# relation between attribute: GrLivArea and target attribute: SalePrice in train set
boxplot(HP_Train$GrLivArea, main="GrLivArea")
plot(HP_Train$SalePrice~HP_Train$GrLivArea)

#remove outliers from the full data
length(which(FullData$GrLivArea>4000))
outliers <- c((which(FullData$GrLivArea>4000)))
FullData <- FullData[-outliers,]

outliersT <- c((which(HP_Train$GrLivArea>4000)))
HP_Train <- HP_Train[-outliersT,]
SalePrice<-HP_Train[,81]

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
  HP_Train <- FullData[1:1456,]
  HP_Test <- FullData[1457:2914,]
  
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
  
# seperate numerical and categorical data for both train and test sets
  numFull <- select_if(FullData, is.numeric)
  catFull <- select_if(FullData, is.factor)
  
  numTr <- select_if(HP_Train, is.numeric)
  catTr <- select_if(HP_Train, is.factor)
  
  numTs <- select_if(HP_Test, is.numeric)
  catTs <- select_if(HP_Test, is.factor)
    
# save Id and class attributes for submission
  Id_Train <-HP_Train[,1]
  Id_Test <- HP_Test[,1]
  Label_Train <- HP_Train[,81]
  
# Drop Id for both test and train data
  HP_Train <- HP_Train[,-1]
  HP_Test <- HP_Test[,-1]

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
logS<- log(HP_Train$SalePrice)
hist(log(HP_Train$SalePrice), col="peachpuff", border="black", prob = TRUE)
lines(density(log(HP_Train$SalePrice)), lwd = 2,    col = "chocolate3")

# check correlation between numeric data.
cor_num<- cor(numTr)
cor_num[cor_num>0.5 & cor_num<1]
corrplot(cor_num, method="circle")

attach(HP_Train)
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
HP_Train = subset(HP_Train, select = -c(Electrical,PoolQC,Fence, Heating, RoofMatl, Utilities, Condition2,CentralAir, LandContour, LandSlope,MiscFeature, Alley
                                        , Street, PavedDrive, SaleCondition,SaleType, GarageCond, GarageQual, ExterCond, Condition1,GarageCars, TotRmsAbvGrd, GarageYrBlt, X1stFlrSF, X2ndFlrSF, BsmtFullBath) )

HP_Test = subset(HP_Test, select = -c(Electrical,PoolQC,Fence, Heating, RoofMatl, Utilities, Condition2,CentralAir, LandContour, LandSlope,MiscFeature, Alley
                                        , Street, PavedDrive, SaleCondition,SaleType, GarageCond, GarageQual, ExterCond, Condition1, GarageCars, TotRmsAbvGrd, GarageYrBlt, X1stFlrSF, X2ndFlrSF, BsmtFullBath) )

num_data = subset(num_data, select = -c(GarageCars, TotRmsAbvGrd, GarageYrBlt, X1stFlrSF, X2ndFlrSF, BsmtFullBath))
num_data = subset(num_data, select = -c(OverallCond, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, PoolArea, YrSold, MoSold, ScreenPorch,X3SsnPorch, EnclosedPorch, OpenPorchSF, WoodDeckSF, KitchenAbvGr, HalfBath, FullBath, BsmtHalfBath, LowQualFinSF))
num_data = subset(num_data, select = -c(LotArea, YearRemodAdd, MiscVal, Fireplaces, BedroomAbvGr, MasVnrArea, LotFrontage))


cor_num<- cor(num_data)
cor_num

corrplot(cor_num, method="circle")

print("Find most important features relative to target")
corr = cor.test(num_data,SalePrice)
print(corr)

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

cor(MSZoning,SalePrice)
plot(MSZoning,SalePrice)

chisq.test(cat_data)

index<- c(2:30)

for (i in index){
  s<- chisq.test(cat_data[,1],cat_data[,i])
   print (s$p.value)
}

chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
  for (i in 1:(num-1)) {
    for (j in (i+1):num) {
      m[i,j] = chisq.test(x[,i],x[,j],)$p.value
    }
  }
  return (m)
}
mat = chisqmatrix(cat_data)

dimnames(cat_data)

plot(SaleType)

UpdatedData <- cbind(cat_data, num_data)
View(UpdatedData)
ca<- c(1:24,30)
cat1 <- UpdatedData[,ca]
View(cat1)
modelA <- lm(SalePrice ~ ., cat1)

# example for feature selection
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(cat1[,1:24], cat1[,25], sizes=c(1:24), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results)
