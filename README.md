## House Prices: Advanced Regression Techniques

 The purpose of this project is predicting the final price of a house based on distinctive features. The dataset is obtained from Kaggle Competition and displays the prices and features of sold residential houses in Ames, Iowa from 2006 to 2010.
 
 Since the predicted value is continuous, Advanced Regression algorithms will be used such as Linear regression, Random Forest, etc. I used R and Weka  to solve this problem.

### Dataset
This project uses the Ames Housing dataset from Kaggle’s competition. Thedataset contains 2920 observationsand 79 explanatory variables(predictors) describing almost every aspect of residential homes in Ames, Iowa. The dataset is divided into equal train and test datasets except that sales price is not included in the test data as this is our target variable.

The dataset contains 36 numerical values and 43 categorical values. The numerical attributes mostly represent features related to either physical space of the property such as square footage, or time related attributes like when the house was built or sold, or amenities related attributes such as “how many rooms or bathrooms?”, or subjective condition and quality attributes. The categorical attributes represent additional details of house features such as the presence or absence of fireplace, pool, utilities, ... etc.

### EDA
- The missing data was we imputed. 
- Some numerical variables have been transformed to categorical. 
- The skewedfeatures have been log transformed.
-T he categorical features transformed to dummy variables.
### Data Modelling
- Linear regression model was used with all variables as our benchmark model. 
- Afterword regularization has been used to improve the prediction. 
- We used Lasso, Ridge, Elasticnet and Random Forest regression models and tuned their parameters to get the best model. 
- we used the features selected during the EDA step and applied the same models. 
- the best score RMSE=12055 was achieved by using Lasso Regression with all variables.

<p align="center">
<img src="/Model%20Comparation.png" alt="drawing" width="400"/>
</p>

