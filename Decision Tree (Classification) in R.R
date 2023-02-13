#Problem 9.1

##Install packages and and invoke libraries
install.packages("caret")
install.packages("gains")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(caret); library(gains);library(rpart); library(rpart.plot); library(pROC)

#Reading the data 
mydata.df <- read.csv("eBayAuctions.csv")
str(mydata.df)
View(mydata.df)

#Data Preprocessing
#class(mydata.df$Duration) #to check class of duration variable
#class(mydata.df$Competitive.)
# the competitive variable is also integer. 
#Converting variables into factor/categorical types

mydata.df$Competitive. <- as.factor(mydata.df$Competitive.)
mydata.df$Duration<-as.factor(mydata.df$Duration)
str(mydata.df)


#Partitioning the data
set.seed(1234)

#Partitioning the data into training(60%) and validation(40%)
train.index <- sample(c(1:dim(mydata.df)[1]), dim(mydata.df)[1]*0.6)  
train.df <- mydata.df[train.index, ]
valid.df <- mydata.df[-train.index, ]

#Problem 9.1 (Part a)
set.seed(1234)

default_tree <- rpart(Competitive. ~ ., data = train.df, method = "class")
summary(default_tree)
options(scipen=999)# to avoid scientific notation
prp(default_tree, type = 1, extra = 1, under = TRUE)

set.seed(1234)
full_tree <- rpart(Competitive. ~ ., data = train.df, method = "class", cp=0, minsplit = 2, minbucket = 50,maxdepth=7)
prp(full_tree, type = 1, extra = 1, under = TRUE)
printcp(full_tree)

pruned_tree <- prune(full_tree, cp=0.024528)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

predicted_class <- predict(pruned_tree, valid.df, type = "class")
confusionMatrix(predicted_class, valid.df$Competitive., positive = "1")




#Problem 9.1- Part 2- Using NNW
library(neuralnet)
library(nnet)
library(caret)
library(e1071)

#Variables to be  used
variable_vector <- c("sellerRating", "ClosePrice")
#Partitionign data
set.seed(1234)
train.df.nnw=sample(row.names(mydata.df), dim(mydata.df)[1]*0.6)
valid.df.nnw=setdiff(row.names(mydata.df),train.df.nnw)



train_data <-cbind(mydata.df[train.df.nnw,c(variable_vector)],
                  class.ind(mydata.df[train.df.nnw,]$Duration),
                  class.ind(mydata.df[train.df.nnw,]$Competitive.))
##View(train_data)

names(train_data) <- c(variable_vector, paste("Duration_",c(1,3,5,7,10),sep = ""),paste("Competitive_", c(0,1), sep = ""))
names(train_data)

valid_data <- cbind(mydata.df[valid.df.nnw,c(variable_vector)],class.ind(mydata.df[valid.df.nnw,]$Duration),class.ind(mydata.df[valid.df.nnw,]$Competitive.))
names(valid_data) <- c(variable_vector, paste("Duration_",c(1,3,5,7,10),sep = ""), paste("Competitive_", c(0,1), sep = ""))
names(valid_data)


#Creating the neural network model
nnw_model <- neuralnet(Competitive_0+Competitive_1~sellerRating + ClosePrice + Duration_1 + Duration_3 + Duration_5 +Duration_7 + Duration_10,data = train_data, hidden = 3, stepmax = 1000000)

#Plotting the neural network model
plot(nnw_model, rep = "best")

#Confusion matrix for nnw model
pred_valid <- compute(nnw_model, rep = 1, valid_data)
probabilities <- pred_valid$net.result # get the probabilities
pred_valid1 <- ifelse(probabilities>0.5,1,0) # convert probabilities into classification
vec<-c(valid_data$Competitive_0,valid_data$Competitive_1)
confusionmatrix_nnw <- table(pred_valid1, vec)
confusionmatrix_nnw
