
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
install.packages("DAAG")          # using CVlm
install.packages("lmtest")
install.packages("xgboost")       #xgboost regression
install.packages("ggplot2")
install.packages("caretEnsemble")
install.packages("gridExtra")
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
#####################################################################################

# read cleaned train and test dataset files (no missing data) and check data summary
TrainUpdated <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TrainUpdated.csv",header=T,sep=",")
TestUpdated <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TestUpdated.csv",header=T,sep=",")

# remove Id variable from both datasets
TrainUpdated <- TrainUpdated[-1]

TestUpdated <- TestUpdated[-1]

IdTest <- c(1461:2919)

# Combine both test and train dataset (without SalePrice attribute) to apply consistant changes.
FullData <- rbind(HP_Train[1:79],HP_Test)
SalePrice <- HP_Train[,80]

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
TrainUpdated$SalePrice <- log(TrainUpdated$SalePrice+1)
TestUpdated <- fullnonanum[nrow(HP_Train)+1:nrow(HP_Test),]

# save featured data as CSV file
write.csv(TrainUpdated, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TrainUpdatedF.csv")
write.csv(TestUpdated, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TestUpdatedF.csv")
write.csv(fullnonanum, file = "C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/FullDataUpdatedF.csv")



#linear regression using Cross Validation "k-fold = 5 folds"
# Control the computational nuances of the train function
tr.control <- trainControl(method="repeatedcv", number = 5,repeats = 5)

set.seed(123)
lm_model <- suppressWarnings(train(SalePrice~., data=TrainUpdated, method="lm",metric="RMSE",
                    maximize=FALSE,trControl=tr.control))
print(lm_model)

#Write predictions to file
lmpreds <- predict(lm_model,newdata = TestUpdated)
submit <- data.frame(Id=IdTest,SalePrice=exp(lmpreds))
write.csv(submit,file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/LinearRegF.csv",row.names=F)

#Laso Regression Model

set.seed(123)
lasso_model <- suppressWarnings(train(SalePrice~., data=TrainUpdated,method="glmnet",metric="RMSE",
                maximize=FALSE,trControl=tr.control,
                tuneGrid=expand.grid(alpha=1,
                lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001), 0.00075,0.0005,0.0001))))
print(lasso_model)
min(lasso_model$results$RMSE)
lasso_model$bestTune

ggplot(data=filter(lasso_model$result,RMSE<0.14)) +
  geom_line(aes(x=lambda,y=RMSE))

# Lasso Selection variables
lasso_VarImp <- varImp(lasso_model,scale=F)
lassoImportance <- lasso_VarImp$importance

# count the selected and eliminated features
Selected_var <- length(which(lassoImportance$Overall!=0))
Eliminated_var <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', Selected_var, 'variables in its model, and eliminates', Eliminated_var, 'variables.')

# top 20 important variables
plot(varImp(lasso_model), top=20, main="Lasso model - Variable Importance")

#Write predictions to file
lassopreds <- predict(lasso_model,newdata = TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(lassopreds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/lasso_predsF.csv",row.names = F)


#Ridge Regression Model

set.seed(123)
ridge_model <- suppressWarnings(train(SalePrice~.,data=TrainUpdated,method="glmnet",metric="RMSE",
                     maximize=FALSE,trControl=tr.control,
                     tuneGrid=expand.grid(alpha=0,lambda=seq(1,0,-.001))))
ridge_model$bestTune

mean(ridge_model$resample$RMSE)

ggplot(ridge_model, label="Ridge Model: L2 vs. RMSE")

plot(varImp(ridge_model), top=20, main="Ridg model - Variable Importance")


#Write predictions to file
ridge_preds <- predict(ridge_model,newdata = TestUpdated)
write.csv(data.frame(Id=IdTest,SalePrice=exp(ridge_preds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/ridge_predsF.csv",row.names = F)

# Elastic model

set.seed(123)
# Train glmnet with custom trainControl and tuning: model
ElasticNet <- suppressWarnings(train(SalePrice~.,data=TrainUpdated,  
  tuneGrid = expand.grid(alpha = seq(0,1,0.1), 
                  lambda = seq(0.0001, 1, 10)),
                   method = "glmnet",  trControl = tr.control))
print(ElasticNet)
ElasticNet$bestTune
mean(ElasticNet$resample$RMSE)

ggplot(data=filter(ElasticNet$result,RMSE<0.14)) +
  geom_line(aes(x=alpha,y=RMSE))

#Write predictions to file
enet_preds <- predict(ElasticNet,newdata = TestUpdated)
write.csv(data.frame(Id=IdTest,SalePrice=exp(enet_preds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/ElasticNetF.csv",row.names = F)


# random Forest Model

#using cros validation with 5 folds
set.seed(123)
RandomF_model <- suppressWarnings(train(SalePrice~., data=TrainUpdated,method="rf",metric="RMSE",
                  maximize=FALSE,trControl=tr.control,
                  tuneGrid=expand.grid(mtry = c(5)), importance = T, allowParallel = T, prox = T))
print(RandomF_model)
plot(varImp(RandomF_model), top=20, main="RandomForest model - Variable Importance")
#Write predictions to file
RandomFpreds <- predict(RandomF_model,newdata = TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(RandomFpreds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/random_forestF.csv",row.names = F)

#using RandomForest function in randomForest Package
set.seed(123)
modelRF <- randomForest(SalePrice ~ ., TrainUpdated)

predictionRF <- predict(modelRF, interval="prediction", newdata =TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(predictionRF)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/random_forestP.csv",row.names = F)

#Variable importance

importance    <- importance(modelRF)
varImpPlot(modelRF)

#XGBoost

set.seed(123)
## Model parameters trained using xgb.cv function
xgbModel = xgboost(data = as.matrix(TrainUpdated[,-274]), nfold = 5, label = TrainUpdated$SalePrice, 
                   nrounds = 2200, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", 
                   nthread = 8, eta = 0.01, gamma = 0.0468, max_depth = 6, min_child_weight = 1.7817, 
                   subsample = 0.5213, colsample_bytree = 0.4603)
print(xgbModel)

## Predictions
preds2 <- predict(xgbModel, newdata = as.matrix(TestUpdated))
rmse(Validation$SalePrice, preds2)
write.csv(data.frame(Id=IdTest,SalePrice=exp(preds2)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/xgboost.csv",row.names = F)

# compare models
model_list <- list(lm = lm_model,  lasso=lasso_model, ridge=ridge_model, ElasticN=ElasticNet, rf=RandomF_model)
modelCompare = resamples(model_list)
summary(modelCompare)

par(mfrow=c(1,2))
p1<-bwplot(modelCompare, metric = "RMSE", main="Model Comparation")
p2<-dotplot(modelCompare, metric = "RMSE", main="Model Comparation")
grid.arrange(p1+p2, nrow=1)

sub_avg <- data.frame(Id = IdTest, SalePrice = exp((lmpreds+ridge_preds+enet_preds+RandomFpreds+lassopreds)/5))
write.csv(sub_avg,"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/avg.csv",row.names = F)


set.seed(7)
model_list <- suppressWarnings(caretList(SalePrice~., data=TrainUpdated, trControl=tr.control,
  metric="RMSE", methodList=c("lm", "rf"),
  tuneList=list(
    rf=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    lasso=caretModelSpec(method="glmnet", 
          tuneGrid=expand.grid(alpha=1, lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001), 0.00075,0.0005,0.0001))))))

xyplot(resamples(model_list))
modelCor(resamples(model_list))
caretEnsemble(model_list)




#another code for ensembling

tr.control <- trainControl(
  
  method="cv",
  
  number=7,
  
  savePredictions="final",
  index=createResample(TrainUpdated$SalePrice, 7),  
  
  
  
  allowParallel =TRUE
  
)

xgbTreeGrid <- expand.grid(nrounds = 400, max_depth = seq(2,6,by = 1), eta = 0.1, gamma = 0, colsample_bytree = 1.0,  subsample = 1.0, min_child_weight = 4)

glmnetGridElastic <- expand.grid(.alpha = 0.3, .lambda = 0.009) ## notice the . before the parameter

glmnetGridLasso <- expand.grid(.alpha = 1, .lambda = seq(0.001,0.1,by = 0.001))

glmnetGridRidge <- expand.grid(.alpha = 0, .lambda = seq(0.001,0.1,by = 0.001))



set.seed(123)

modelList <<- caretList(
  
  x = TrainUpdated,
  
  y = TrainUpdated$SalePrice,
  
  trControl=tr.control,
  
  metric="RMSE",
  
  tuneList=list(
    
    glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridElastic), ## Elastic, highly correlated with lasso and ridge regressions
    
    glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridLasso), ## Lasso
    
    glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridRidge) ## Ridge
    
    #svmLinear3= caretModelSpec(method="svmLinear3", tuneLenght = 20) ## SVM 
    
  )
  
)

#weighted ensembling
set.seed(333)

greedyEnsemble <- caretEnsemble(
  
  modelList, 
  
  metric="RMSE",
  
  trControl=trainControl(
    
    number=7, method = "cv"
    
  ))

summary(greedyEnsemble)


library("caTools")
model_preds <- lapply(model_list, predict, newdata=TestUpdated, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedyEnsemble, newdata=TestUpdated, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$Class)


pred_Greedy <- predict(greedyEnsemble, newdata =TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(pred_Greedy)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/ensembling.csv",row.names = F)


set.seed(123)
param <- list("objective" = "reg:linear", "nthread" = 8, "verbose"=0)

x <- as.matrix(TrainUpdated[,-274])
# Fit the model
xgb.fit = xgboost(param=param, data = x, label = TrainUpdated$SalePrice, nrounds=1500, eta = .01, max_depth = 7, 
                  min_child_weight = 5, scale_pos_weight = 1.0, subsample=0.8) 
print(xgb.fit)
xpredict <- predict(xgb.fit, as.matrix(TestUpdated))
write.csv(data.frame(Id=IdTest,SalePrice=exp(xpredict)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/xgb.csv",row.names = F)








modelB <- suppressWarnings(CVlm(data = TrainUpdated, form.lm = formula(SalePrice ~ .), m=5, dots = 
                                  FALSE, seed=29, plotit=TRUE, printit=TRUE))

sqrt(mean((modelB$SalePrice-modelB$Predicted)^2)) 
sqrt(mean((modelB$SalePrice-modelB$cvpred)^2))

mean_squared_error <- attr(modelB, 'ms')
#create the normal probability plot with the qqnorm function, and add the qqline for further comparison.
price.stdres = rstandard(lm_model)
qqnorm(price.stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Old Faithful Eruptions")
qqline(price.stdres)


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
RMSE1 <- RMSE(prediction, testLabel)



#XGBoost has its own function to create matrix for the modeling

dtrain <- xgb.DMatrix(as.matrix(TrainUpdated), label = SalePrice)
dtest <- xgb.DMatrix(as.matrix(TestUpdated))


cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 4, 
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 750,
                        eta = c(0.01,0.005,0.001),
                        max_depth = c(4,6,8),
                        colsample_bytree=c(0,1,10),
                        min_child_weight = 2,
                        subsample=c(0,0.2,0.4,0.6),
                        gamma=0.01)

set.seed(53)

xgb_tune <- train(as.matrix(dtrain),
                  SalePrice, method="xgbTree", 
                  trControl=cv.ctrl, 
                  tuneGrid=xgb.grid, 
                  verbose=T, metric="RMSE", nthread =3)

xgb_params <- list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.01,
  max_depth=8,
  min_child_weight=3,
  alpha=0.3,
  lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.4,
  seed=5,
  silent=TRUE)

bst <- xgb.train(xgb_params,as.matrix(dtrain), nrounds = 10000, early_stopping_rounds = 300, watchlist = list(train=TrainUpdated))

# Functtion to calculate the RMSE value
rmse_eval <- function(y.true, y.pred) {
  mse_eval <- sum((y.true - exp(y.pred)-1)^2) / length(y.true)
  return(sqrt(mse_eval))
}

y_pred.xgb <- predict(bst, TrainUpdated)
rmse_eval(y_true, y_pred.xgb)