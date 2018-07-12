
###############################################################################################
# CKME 136 Capstone Project                                                                   #
# Ames Housing dataset                                                                        #
###############################################################################################

# install required packages

install.packages("corrplot")      # a graphical display of a correlation matrix, confidence interval
install.packages("party")
install.packages("caret")         # classification and regression training package
install.packages("class")         # various functions for classification, including k-nearest neighbour
install.packages("e1071")         # Explore the Skewness of various features of the dataset
install.packages("data.table")    # Fast aggregation of large data 
install.packages("dplyr")         # data manipulation
install.packages("vcd")           # Visualizing Categorical Data
install.packages("mlbench")       # A collection of artificial and real-world machine learning benchmark problems
install.packages("randomForest")
install.packages("MASS")          # stepwise regression
install.packages("leaps")         # all subsets regression
install.packages("glmnet")        # Lasso and ridge regression
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
#####################################################################################

# read cleaned train and test dataset files (no missing data) and check data summary
HP_Train <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/CleanTrain.csv",header=T,sep=",")
HP_Test <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/CleanTest.csv",header=T,sep=",")

IdTest <- HP_Test[,1]
# remove Id variable from both datasets
HP_Train <- HP_Train[-1]
HP_Train <- HP_Train[-1]

HP_Test <- HP_Test[-1]
HP_Test <- HP_Test[-1]

# Combine both test and train dataset (without SalePrice attribute) to apply consistant changes.
FullData <- rbind(HP_Train[1:79],HP_Test)
SalePrice <- HP_Train[,80]

# Have a look at the data
View(HP_Train)
View(HP_Test)
View(FullData)
sum(is.na(FullData))

#delete categorical variables with no sufficient significance in predicting 
FullData = subset(FullData, select = -c(Electrical,PoolQC,Fence, Heating, RoofMatl, Utilities, Condition2,CentralAir, LandContour, LandSlope,MiscFeature, Alley
                                        , Street, PavedDrive, SaleCondition,SaleType, GarageCond, GarageQual, ExterCond, Condition1,GarageCars, TotRmsAbvGrd, GarageYrBlt, X1stFlrSF, X2ndFlrSF, BsmtFullBath) )

FullData = subset(FullData, select = -c(  OverallCond, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, PoolArea, YrSold, MoSold, ScreenPorch,X3SsnPorch, EnclosedPorch, OpenPorchSF, WoodDeckSF, KitchenAbvGr, HalfBath, FullBath, BsmtHalfBath, LowQualFinSF, LotArea, YearRemodAdd, MiscVal, Fireplaces, BedroomAbvGr, MasVnrArea, LotFrontage))

names(FullData)
# transform MSSubClass into nominal values
FullData$MSSubClass <- as.factor(FullData$MSSubClass)

# This step to measure the skewed numeic features that more than 0.75
classes <- lapply(FullData,function(x) class(x))
numeric_feats <- names(classes[classes=="integer" | classes=="numeric"])
factor_feats <- names(classes[classes=="factor"| classes=="character"])

skewed_feats <- sapply(numeric_feats, function(x) skewness(FullData[[x]]))
skewed_feats <- skewed_feats[abs(skewed_feats) > .75]
skewed_feats

#Take log transformation of features for which skewness more than 0.75

for (x in names(skewed_feats)) {FullData[[x]] <- log(FullData[[x]]+1)}

#Use dummyvars function in caret package for one-hot encoding of categorical variables

dummies <- dummyVars(~., data = FullData, fullRank = TRUE)
fullnonanum <- data.frame(predict(dummies, newdata = FullData))

# Split the data to train and test and add SalePrice to train data


TrainUpdated <-fullnonanum[1:nrow(HP_Train),]
LogSalePrice <- log(SalePrice+1)
TrainUpdated <- cbind(TrainUpdated,SalePrice)
TestUpdated <- fullnonanum[nrow(HP_Train)+1:nrow(HP_Test),]

# save featured data as CSV file
write.csv(TrainUpdated, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TrainUpdatedF.csv")
write.csv(TestUpdated, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TestUpdatedF.csv")
write.csv(fullnonanum, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/FullDataUpdatedF.csv")

#We will split the dataset to 70% of training and 30% of test sets. 
# We want to make sure that the training set and the test set do not have any common data points.
rn_train <- sample(nrow(TrainUpdated), floor(nrow(TrainUpdated)*0.7))
train <- TrainUpdated[rn_train,]
test <- TrainUpdated[-rn_train,]
test <- test[,-274]
testLabel <- test[,274]

# building Linear regression model
modelA <- lm(log(SalePrice+1) ~ ., train)

prediction <- predict(modelA, interval="prediction", newdata =test)

# Let's see the errors and plot them on a histogram. 
errors <- prediction[,"fit"] - log(test$SalePrice)
hist(errors)

#Let's compute the root mean square error and find the percentage of cases with less than 25% error.
rmse <- sqrt(sum((prediction[,"fit"] - log(test$SalePrice))^2)/nrow(test))
rel_change <- 1 - ((log(test$SalePrice) - abs(errors)) / log(test$SalePrice))
pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)

# Let's now use the forward selection algorithm.
#We will start with 'null', which means none of the indepenent variables are selected. 
#We will come up with a selection of independent variables between 'null' and 'full'. 'full' means all the independent variables are included. 

full <- lm(log(SalePrice+1) ~ ., train)
null <- lm(log(SalePrice+1) ~ 1, train)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), direction= "forward", trace=TRUE)
summary(stepF)



  # function to calculate rmse
  
  RMSE <- function(x,y){
    a <- sqrt(sum((log(x)-log(y))^2)/length(y))
    return(a)
  }
  RMSE1 <- RMSE(prediction, test$LogSalePrice)
  
#Another way for linear regression using Cross Validation with 10 folds
lm_model <- train(log(SalePrice)~., data=TrainUpdated, method="lm",metric="RMSE",
                    maximize=FALSE,trControl=trainControl(method = "repeatedcv",number = 10)
  )
lm_model$results

#Write predictions to file
lmpreds <- predict(lm_model,newdata = TestUpdated)

submit <- data.frame(Id=IdTest,SalePrice=exp(lmpreds))
write.csv(submit,file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/House_Price_LinearRegF.csv",row.names=F)
   
# random Forest Model

#using cros validation with5 folds
rf_model <- train(log(SalePrice)~., data=TrainUpdated,method="rf",metric="RMSE",
                  maximize=FALSE,trControl=trainControl(method="repeatedcv",number=5),
                  tuneGrid=expand.grid(mtry = c(5)), importance = T, allowParallel = T, prox = T)
rf_model$results
#Write predictions to file
rfpreds <- predict(rf_model,newdata = TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(rfpreds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/random_forest_predsF.csv",row.names = F)

#using RandomForest function in randomForest Package
modelRF <- randomForest(log(SalePrice+1) ~ ., TrainUpdated)

predictionRF <- predict(modelRF, interval="prediction", newdata =TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(predictionRF)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/random_forestF.csv",row.names = F)

#Laso Regression Model

tr.control <- trainControl(method="repeatedcv", number = 10,repeats = 10)

set.seed(123)
lasso_model <- train(log(SalePrice)~., data=TrainUpdated,method="glmnet",metric="RMSE",
                     maximize=FALSE,trControl=tr.control,
                     tuneGrid=expand.grid(alpha=1,lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001), 0.00075,0.0005,0.0001)))
lasso_model$results
#Write predictions to file
lassopreds <- predict(lasso_model,newdata = TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(lassopreds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/lasso_predsF.csv",row.names = F)



#Ridge Regression Model

tr.control <- trainControl(method="repeatedcv", number = 10,repeats = 10)

lambdas <- seq(1,0,-.001)

set.seed(123)
ridge_model <- train(log(SalePrice+1)~., data=TrainUpdated,method="glmnet",metric="RMSE",
                     maximize=FALSE,trControl=tr.control,
                     tuneGrid=expand.grid(alpha=0,lambda=lambdas))

ridge_model$results

varImp(ridge_model)
#Write predictions to file
ridge_preds <- predict(ridge_model,newdata = TestUpdated)
write.csv(data.frame(Id=IdTest,SalePrice=exp(ridge_preds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/ridge_predsF.csv",row.names = F)


# Work in progress here
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)


param <- list("objective" = "reg:linear", "nthread" = 8, "verbose"=0)

x <- as.matrix(TrainUpdated[,-274])
# Fit the model
xgb.fit = xgboost(param=param, data = x, label = log(SalePrice), nrounds=1500, eta = .01, max_depth = 7, 
                  min_child_weight = 5, scale_pos_weight = 1.0, subsample=0.8) 
xpredict <- predict(xgb.fit, TestUpdated)
