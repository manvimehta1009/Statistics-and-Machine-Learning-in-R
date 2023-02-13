#Problem 5.7

install.packages("caret")
library(caret)

## Creating the table
table<- matrix(c(.03,0,.52,0,.38,0,.82,1,.33,0,.42,0,.55,1,.59,0,.09,0,.21,0,.43,0,.04,0,.08,0,.13,0,.01,0,.79,1,.42,0,.29,0,.08,0,.02,0), ncol=2, byrow=TRUE)
colnames(table)<-c("Propensity of 1","Actual")
table<- as.data.frame(table)
table

#For cutoff 0.25
library(e1071)

##cutoff = 0.25
confusionMatrix(as.factor(ifelse(table$`Propensity of 1`>0.25, '1', '0')), 
                as.factor(table$Actual))

##cutoff = 0.5
confusionMatrix(as.factor(ifelse(table$`Propensity of 1`>0.5, '1', '0')), 
                as.factor(table$Actual))

##cutoff = 0.75
confusionMatrix(as.factor(ifelse(table$`Propensity of 1`>0.75, '1', '0')), 
                as.factor(table$Actual))

#Plotting decile chart
install.packages("gains")
library(gains)

gain <- gains(table$Actual, table$`Propensity of 1`)
barplot(gain$mean.resp / mean(table$Actual), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")


#Problem 6.1

#Working directory has been set
#Reading the file

bostondata<-read.csv("BostonHousing.csv")
View(bostondata)


#Partitioning the data 
#using set.seed(1234) as per instructions 

set.seed(1234)
## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- sample(rownames(bostondata), dim(bostondata)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- bostondata[train.rows, ]
# assign row IDs that are not already in the training set, into validation
valid.rows <- setdiff(rownames(bostondata), train.rows)
valid.data <- bostondata[valid.rows, ]

#6b) Fitting a MLR model
regmodel<-lm(MEDV~CRIM+CHAS+RM,data=bostondata,subset=train.rows)
regmodel
summary(regmodel)
tr.res <- data.frame(train.data$MEDV, regmodel$fitted.values, regmodel$residuals)
##head(tr.res)
#Predicting values of validation data
pred <- predict(regmodel, newdata = valid.data)
vl.res <- data.frame(valid.data$MEDV, pred, residuals =
                       valid.data$MEDV - pred)
head(vl.res)
##pred

#6c)Predicting new values -Q6
newdf<-data.frame(CRIM=0.1,CHAS=0,RM=6)
pred2<-predict(regmodel,newdf)
pred2

library(forecast)
accuracy(pred, valid.data$MEDV)

#6d) i) Exploring relationships between INDUS,NOX and TAX
library(GGally)
ggpairs(bostondata[, c(3, 5, 10)])


#6d) ii) Correlation tableS after removing non-numerical variables
round(cor(bostondata[-c(13,14)]),2)

##

#6d)iii)
#Creating a regression model with remaining variables

regmodel2<-lm(MEDV~.,data=(bostondata[-c(3,10,7,14)]),subset=train.rows)
regmodel2

# use step() to run stepwise regression.
# set directions = to either "backward", "forward", or "both".
boston.step.bw <- step(regmodel2, direction = "backward")
summary(boston.step.bw) 
boston.step.bw.pred <- predict(boston.step.bw, valid.data)
accuracy(boston.step.bw.pred, valid.data$MEDV)

boston.step.fw<-step(regmodel2, direction = "forward")
summary(boston.step.fw) 
boston.step.fw.pred <- predict(boston.step.fw, valid.data)
accuracy(boston.step.fw.pred, valid.data$MEDV)


boston.step.both<-step(regmodel2, direction = "both")
summary(boston.step.both) 
boston.step.both.pred <- predict(boston.step.both, valid.data)
accuracy(boston.step.both.pred, valid.data$MEDV)

comparetest <- data.frame( 
  backwards = c(accuracy(boston.step.bw.pred,valid.data$MEDV)),
  forward =c(accuracy(boston.step.fw.pred, valid.data$MEDV)),
  both = c(accuracy(boston.step.both.pred,valid.data$MEDV))
)
rownames(comparetest) <- c("ME","RMSE","MAE","MPE","MAPE")
comparetest

#Lift charts for all models
actual = valid.data$MEDV
library(gains)

#lift for backwards
gain1 = gains(actual, 
              boston.step.bw.pred,
              group = 10)

plot(c(0, gain1$cume.pct.of.total*sum(actual))~c(0, gain1$cume.obs), type = "l", xlab = "#Cases", ylab = "Cumulative MEDV", main = "Lift Chart for backwards")
segments(0, 0, nrow(valid.data), sum(actual), lty = "dashed", col = "red", lwd = 2)

#Lift chart for forward
gain2 = gains(actual, 
              boston.step.fw.pred,
              group = 10)

plot(c(0, gain2$cume.pct.of.total*sum(actual))~c(0, gain2$cume.obs), type = "l", xlab = "#Cases", ylab = "Cumulative MEDV", main = "Lift Chart for forward")
segments(0, 0, nrow(valid.data), sum(actual), lty = "dashed", col = "red", lwd = 2)

#lift chart for both
gain3 = gains(actual, 
              boston.step.both.pred,
              group = 10)

plot(c(0, gain3$cume.pct.of.total*sum(actual))~c(0, gain3$cume.obs), type = "l", xlab = "#Cases", ylab = "Cumulative MEDV", main = "Lift Chart for both")
segments(0, 0, nrow(valid.data), sum(actual), lty = "dashed", col = "red", lwd = 2)


#Problem 6.4
toyota.df<-read.csv("ToyotaCorolla.csv")

#split data

set.seed(1234)
## partitioning into training (50%), validation (30%), test (20%)
# randomly sample 50% of the row IDs for training
train.rows2 <- sample(rownames(toyota.df), dim(toyota.df)[1]*0.5)
# sample 30% of the row IDs into the validation set, drawing only from records
# not already in the training set
# use setdiff() to find records not already in the training set
valid.rows2 <- sample(setdiff(rownames(toyota.df), train.rows2),
                     dim(toyota.df)[1]*0.3)
# assign the remaining 20% row IDs serve as test
test.rows <- setdiff(rownames(toyota.df), union(train.rows2, valid.rows2))
# create the 3 data frames by collecting all columns from the appropriate rows
train.data2 <- toyota.df[train.rows2, ]
valid.data2 <- toyota.df[valid.rows2, ]
test.data <- toyota.df[test.rows, ]


##View(toyota.df)
toyotareg<-lm(Price~Age_08_04+KM+Fuel_Type+HP+Automatic+Doors+Quarterly_Tax+Mfr_Guarantee+Guarantee_Period+Airco+Automatic_airco+CD_Player+
              Powered_Windows+Sport_Model+Tow_Bar,data=toyota.df,subset=train.rows2)

toyotareg
summary(toyotareg)

#6.4b)
predtoyota <- predict(toyotareg, newdata = valid.data2)
vl.res2 <- data.frame(valid.data2$Price, predtoyota, residuals =
                       valid.data2$Price - predtoyota)
head(vl.res2)
accuracy(predtoyota, valid.data2$Price)


#Problem 10. 1 (a)

banks.df<-read.csv("banks.csv")
View(banks.df)

#setting bins
banks.df$Financial.Condition<-factor(banks.df$Financial.Condition, levels = c(0,1),
                                     labels = c("strong", "weak"))

#splitting data 
set.seed(1234)
train.index <- sample(c(1:dim(banks.df)[1]), dim(banks.df)[1]*0.6)
train.df.banks <- banks.df[train.index, ]
valid.df.banks <- banks.df[-train.index, ]

#running the logistic regression model
glm.fit <- glm(Financial.Condition ~ TotExp.Assets+TotLns.Lses.Assets, data = train.df.banks, family = "binomial")
glm.fit
data.frame(summary(glm.fit)$coefficients, odds = exp(coef(glm.fit)))
summary(glm.fit)

#10.1b
bankpred <- predict(glm.fit,data.frame("TotLns.Lses.Assets" = 0.6,"TotExp.Assets" = 0.11), data = banks.df)
bankpred
odds = exp(bankpred)
odds
Probability = odds / (odds+1)
Probability


#Problem 10.3
#Reading the file
ridingmowers.df <- read.csv("RidingMowers.csv")
View(ridingmowers.df)
ridingmowers.df$X <- NULL
ridingmowers.df$outcome<-ifelse(ridingmowers.df$Ownership=="Owner",1,0)

## Converting the new variable into factor

ridingmowers.df$outcome<-as.factor(as.character(ridingmowers.df$outcome))
str(riding_data)

ridingmowers.df$Ownership <- NULL

## Implementing the Logistic Regression Model

logReg<-glm(outcome~.,data = ridingmowers.df,family = "binomial")
summary(logReg)



#10.3b)
library(ggplot2)
ggplot(ridingmowers.df) + #to access the data frame for the plot
  geom_jitter(aes(x = Income , y = Lot_Size , colour = ridingmowers.df$outcome))+ #Specifications
  ggtitle("Income vs. Lot Size")+ #Main title
  theme(plot.title = element_text(hjust = 0.5,size=12))+ #Centre aligning 
  xlab("Income")+ylab("Lot Size") #Labels for x and y axis


## Predict Probability on Sample Data
prob<-predict(logReg,type = "response")
data.new<-cbind(riding_data,prob)

## Predicting if threshold > 0.6

pred_class<-ifelse(prob>0.6,1,0)
data.new<-cbind(data.new,pred_class)


## Checking the predicting values of outcome if we set the threshold of 60%
## Constructing Contingency Table

table1<-table(data.new$outcome,data.new$pred_class)
table1

pred_class2<-ifelse(prob>0.4,1,0)
data.new<-cbind(data.new,pred_class2)

## Constructing Contingency Table
table1<-table(data.new$outcome,data.new$pred_class2)
table1

