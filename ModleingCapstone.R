
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
install.packages("DAAG")
install.packages("lmtest")
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

# Part 2 Modeling
# Now data is ready to apply different models on it

# read cleaned train and test dataset files (no missing data) and check data summary
TrainUpdated <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TrainUpdated.csv",header=T,sep=",")
TestUpdated <-read.csv(file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/TestUpdated.csv",header=T,sep=",")

# remove Id variable from both datasets
TrainUpdated <- TrainUpdated[-1]
TrainUpdated$SalePrice <-log(TrainUpdated$SalePrice+1)
TestUpdated <- TestUpdated[-1]

IdTest <- c(1461:2919)


#linear regression using Cross Validation "k-fold = 5 folds"

# Control the computational nuances of the train function
tr.control <- trainControl(method="repeatedcv", number = 5,repeats = 5)
set.seed(123)
lm_model <- suppressWarnings(train(SalePrice~., data=TrainUpdated, method="lm",metric="RMSE",
                    maximize=FALSE,trControl=tr.control))
print(lm_model)

#Write predictions to file
lmpreds <- predict(lm_model,newdata = TestUpdated)
submit1 <- data.frame(Id=IdTest,SalePrice=exp(lmpreds))
write.csv(submit1,file="C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/LinearReg.csv",row.names=F)

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

write.csv(data.frame(Id=IdTest,SalePrice=exp(lassopreds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/lasso_preds.csv",row.names = F)


#Ridge Regression Model

set.seed(123)
ridge_model <- train(SalePrice~.,data=TrainUpdated,method="glmnet",metric="RMSE",
                     maximize=FALSE,trControl=tr.control,
                     tuneGrid=expand.grid(alpha=0,lambda=seq(1,0,-.001)))
ridge_model$bestTune

mean(ridge_model$resample$RMSE)

ggplot(ridge_model, label="Ridge Model: L2 vs. RMSE")

plot(varImp(ridge_model), top=20, main="Ridg model - Variable Importance")
plot(varImp(ridge_model,top=20))

#Write predictions to file
ridge_preds <- predict(ridge_model,newdata = TestUpdated)
write.csv(data.frame(Id=IdTest,SalePrice=exp(ridge_preds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/ridge_preds24July.csv",row.names = F)

# Elastic model

set.seed(123)
# Train glmnet with custom trainControl and tuning: model
ElasticNet <- train(SalePrice~.,data=TrainUpdated,  
  tuneGrid = expand.grid(alpha = seq(0,1,0.1), 
                  lambda = seq(0.0001, 1, 10)),
                   method = "glmnet",  trControl = tr.control)
print(ElasticNet)
ElasticNet$bestTune
mean(ElasticNet$resample$RMSE)

ggplot(data=filter(ElasticNet$result,RMSE<0.14)) +
  geom_line(aes(x=alpha,y=RMSE))

#Write predictions to file
enet_preds <- predict(ElasticNet,newdata = TestUpdated)
write.csv(data.frame(Id=IdTest,SalePrice=exp(enet_preds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/ElasticNet.csv",row.names = F)


# random Forest Model

#using cros validation with 5 folds
set.seed(123)
RandomF_model <- train(SalePrice~., data=TrainUpdated,method="rf",metric="RMSE",
                  maximize=FALSE,trControl=trainControl(method="repeatedcv",number=5),
                  tuneGrid=expand.grid(mtry = c(5)), importance = T, allowParallel = T, prox = T)
print(RandomF_model)
plot(varImp(RandomF_model), top=20, main="RandomForest model - Variable Importance")
#Write predictions to file
RandomFpreds <- predict(RandomF_model,newdata = TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(RandomFpreds)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/random_forest.csv",row.names = F)

#using RandomForest function in randomForest Package
set.seed(123)
modelRF <- randomForest(SalePrice ~ ., TrainUpdated)

predictionRF <- predict(modelRF, interval="prediction", newdata =TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(predictionRF)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/random_forestP.csv",row.names = F)

#Variable importance

importance    <- importance(modelRF)
varImpPlot(modelRF)


# compare models
model_list <- list(lm = lm_model,  lasso=lasso_model, Ridge=ridge_model, ElasticN=ElasticNet)
resamples = resamples(model_list)
summary(resamples)



# Work in progress here
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)

set.seed(123)
param <- list("objective" = "reg:linear", "nthread" = 8, "verbose"=0)

x <- as.matrix(TrainUpdated[,-274])
# Fit the model
xgb.fit = xgboost(param=param, data = x, label = TrainUpdated$SalePrice, nrounds=1500, eta = .01, max_depth = 7, 
                  min_child_weight = 5, scale_pos_weight = 1.0, subsample=0.8) 
print(xgb.fit)
xpredict <- predict(xgb.fit, as.matrix(TestUpdated))
write.csv(data.frame(Id=IdTest,SalePrice=exp(xpredict)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/xgb.csv",row.names = F)



#XGBoost
install.packages("xgboost")
library(xgboost)
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



#another code for ensembling

tr.control <- trainControl(
  
  method="cv",
  number=7,
  savePredictions="final",
  index=createResample(TrainUpdated$SalePrice, 7),  
  allowParallel =TRUE
  
)

#xgbTreeGrid <- expand.grid(nrounds = 400, max_depth = seq(2,6,by = 1), eta = 0.1, gamma = 0, colsample_bytree = 1.0,  subsample = 1.0, min_child_weight = 4)

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
model_preds <- lapply(modelList, predict, newdata=TestUpdated, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedyEnsemble, newdata=TestUpdated, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$Class)


pred_Greedy <- predict(greedyEnsemble, newdata =TestUpdated)

write.csv(data.frame(Id=IdTest,SalePrice=exp(pred_Greedy)),"C:/Users/alsaf/Desktop/Certificate Change Shool/Capstone/HousePrice/ensembling.csv",row.names = F)
