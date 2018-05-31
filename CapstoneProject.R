
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

# convert some attributes to categorical data type.
F=c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20,21)
for(i in F) creditCard[,i]=as.factor(creditCard[,i])
str(creditCard)

# change attribute "Sex and Martial status" to binary attribute. 1 for male, 2 for female
creditCard$Sex<- ifelse(Sex...Marital.Status == 2,2,1) 
hist(creditCard$Sex)





# check relation between sex attribute and credit amount
plot(creditCard$Sex...Marital.Status,creditCard$Creditability)
plot(creditCard$Sex,creditCard$Creditability)

# check for outliers for numerical attributes
# attribute: Age
boxplot(creditCard$Age..years., main='Age')
summary(creditCard$Age..years.)
length(which(creditCard$Age..years.>65))
m1<-c(which(creditCard$Age..years.>65))
m1
creditCard[m1,1]

# attribute: Credit Amount
boxplot(creditCard$Credit.Amount,main='Credit Amount')
summary(creditCard$Credit.Amount)
length(which(creditCard$Credit.Amount>7000))
m2<-c(which(creditCard$Credit.Amount>7000))
m2
sum(creditCard[m2,1]==1)

# attribute: Duration of creditmonth
boxplot(creditCard$Duration.of.Credit..month., main='Duration')
summary(creditCard$Duration.of.Credit..month.)
length(which(creditCard$Duration.of.Credit..month.>40))
m3<-c(which(creditCard$Duration.of.Credit..month.>48))
m3
creditCard[m3,1]


# check correlation between numeric data.
cor.test(Duration.of.Credit..month.,Credit.Amount)
cor.test(Duration.of.Credit..month.,Age..years.)
cor.test(Age..years.,Credit.Amount)


# To calculate correlation for categorical data
?chisq.test
# remove numerical attributes
categ<- creditCard[,-3]
categ<- categ[,-5]
categ<- categ[,-12]
categ<- categ[,-19]
str(categ)

index<- c(2:18)
result<-c(1:17)
for (i in index){
s<- chisq.test(categ[,1],categ[,i])
s$statistic
}
result


# Decision Tree
NewCreditCard <- 
  myvars <- c("Credit.Amount", "Duration.of.Credit..month.", "Sex...Marital.Status")
gc.subset <- creditCard[myvars]

summary(gc.subset)
modelA <- ctree(Creditability ~ ., gc.subset) 


model <- ctree(Creditability ~ ., creditCard) 
print(model)
plot(model)

# splitting the data
train_index <- sample(1:nrow(creditCard), 0.7 * nrow(creditCard))
train.set <- creditCard[train_index,]
test.set <- creditCard[-train_index,]

# run our model on the training set
model1 <- ctree(Creditability ~ ., train.set[,-22]) 
model2 <- ctree(Creditability ~ ., train.set[,-10]) 
model3 <- ctree(Creditability~., categ)
print(model1)
print(model2)
print(model3)
plot(model1)
plot(model2)
plot(model3)
# make our prediction on the test set.
ctree_prediction1 <- predict(model1, test.set[,-22]) 
ctree_prediction2 <- predict(model2, test.set[,-10]) 
# gives the probability for each class
head(ctree_prediction)
# the confusion matrix.
table(ctree_prediction1, test.set$Creditability)
table(ctree_prediction2, test.set$Creditability)

  if(Age..years.<21) {creditCard$NewAge = 1}
  else if (Age..years. <40 &  Age..years.>=21) {creditCard$NewAge = 2}
  else if (Age..years. <54 &  Age..years.>=41) {creditCard$NewAge = 3}
  else  {creditCard$NewAge = 4}

# Clustering
# seperate data with class:Good
GoodClass<- creditCard[which(creditCard$Creditability ==1),]

# remove the credibility column from our training and test datasets.
train.set_new <- train.set[-1]
test.set_new <- test.set[-1]

# store the labels from our training and test datasets.
train_labels <- train.set$Creditability 
test_labels <- test.set$Creditability

# For k=3, let's make our prediction on the test set.
knn_prediction <- knn(train = train.set_new, test = test.set_new, cl= train_labels, k = 3)
# the confusion matrix.
table(x=test_labels, y=knn_prediction, prop.chisq=FALSE)

#clustering kmeans
kmeans<- kmeans(creditCard, 2) 
kmeans
plot(kmeans)
table(creditCard$Creditability,kmeans$cluster)

kmeans1<- kmeans(GoodClass, 2) 
kmeans1
plot(kmeans1)
table(GoodClass$Creditability,kmeans1$cluster)
install.packages("cluster")
library(cluster)
plotcluster(creditCard, kmeans$cluster)

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
mat = chisqmatrix(categ)
mat
