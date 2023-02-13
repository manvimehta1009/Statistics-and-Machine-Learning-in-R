#Reading the csv files
toyota.df <- read.csv("ToyotaCorolla.csv", header=TRUE)
#View(toyota.df)
  
#Converting categorical variables into dummies
xtotal <- model.matrix(~0+Fuel_Type, data=toyota.df)
xtotal <- xtotal[,-3] 
toyota.df <- subset (toyota.df, select = -Fuel_Type)
toyota.df <- cbind(toyota.df,xtotal)
  
#Partitioning data
set.seed(1234)
train.index <- sample(c(1:dim(toyota.df)[1]), dim(toyota.df)[1]*0.6)  
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[-train.index, ]
  
#Normalizing the data 
#Initializing normalized training, validation and complete data to original
train.normal.df <- train.df
valid.normal.df <- valid.df
toyota.normal.df <- toyota.df
library(lattice)
library(ggplot2)
library(caret)
library(forecast)
#install.packages("Metrics")
library(Metrics)
  
  
#normalizing numerical data to be used 
#Vector to be used

vector<-c(3,4,7,8,11,13,16,18,20,24,25,27,29,33,38,39,40)


normal_numdata <- preProcess(toyota.df[,vector],method=c("range"))
normal_numdata

#Partitioning data
train.normal.df[,vector] <- predict(normal_numdata,train.df[,vector])
valid.normal.df[,vector] <- predict(normal_numdata,valid.df[,vector])
toyota.normal.df <- predict(normal_numdata,toyota.df)

#Neural network model
library(neuralnet)
toyota.model.nnw <- neuralnet(Price ~.,  data = train.normal.df[,vector],  linear.output = T, hidden = 2)
plot(toyota.model.nnw)
  
#Validation data-model
toyota.valid.model<- compute(toyota.model.nnw,valid.normal.df[,vector])
toyota.valid.model
  
#Single layer model with 5 nodes
toyota.model.nnw2 <- neuralnet(Price ~.,  data = train.normal.df[,vector],  linear.output = T, hidden=5)
plot(toyota.model.nnw2)
  
#Validation data-model 2
toyota.valid.model2<- compute(toyota.model.nnw2,valid.normal.df[,vector]) 
toyota.valid.model2
  
  
toyota.normal.df <- toyota.normal.df[,vector]
train.normal.df <- train.normal.df[,vector]
valid.normal.df <-valid.normal.df[,vector]
  
#Computing error rate (RMSE) for nnw models 1 & 2

#Model 1- single layer, 2 nodes
predict_train <- compute(toyota.model.nnw, train.normal.df)$net.result
rmse(train.normal.df[,"Price"], predict_train)
  
predict_valid <- compute(toyota.model.nnw, valid.normal.df)$net.result
rmse(valid.normal.df[,"Price"], predict_valid)
  
#Model 2- single layer, 5 nodes
predict_train2 <- compute(toyota.model.nnw2, train.normal.df)$net.result
rmse(train.normal.df[,"Price"], predict_train2)
  
predict_valid2 <- compute(toyota.model.nnw2, valid.normal.df)$net.result
rmse(valid.normal.df[,"Price"], predict_valid2)
  
  
#Regression trees
#Installing packages and libraries
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
  
toyota.df2 <- read.csv("ToyotaCorolla.csv")
options(scipen=999)# to avoid scientific notation
  
#Partitioning data
set.seed(1234)
train.index2 <- sample(c(1:dim(toyota.df2)[1]), dim(toyota.df2)[1]*0.6)  
train.df2 <- toyota.df2[train.index2, ]
valid.df2 <- toyota.df2[-train.index2, ]
  
regression_tree<- rpart(Price ~  Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax +
                     Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player +
                     Powered_Windows + Sport_Model + Tow_Bar, 
                     data = train.df2,method = "anova", minbucket = 1, maxdepth = 30, cp = 0.001)
prp(regression_tree)
t(t(regression_tree$variable.importance))
  
#Comparing RMSE for both 
library(forecast)
accuracy(predict(regression_tree, train.df2), train.df2$Price)
accuracy(predict(regression_tree, valid.df2), valid.df2$Price)
  
#Computing error rate
train_error <-predict(regression_tree, train.df2) -train.df2$Price
valid_error <-predict(regression_tree, valid.df2) -valid.df2$Price
  
#Plotting boxplots
errors <-data.frame(Error =c(train_error, valid_error), Set =c(rep("Training", length(train_error)),rep("Validation", length(valid_error))))
  
boxplot(Error~Set, data=errors, main="RMSE Errors",xlab = "Set", ylab = "Error",
          col="blue",medcol="red",boxlty=0,border="black",
          whisklty=1,staplelwd=4,outpch=13,outcex=1,outcol="green")