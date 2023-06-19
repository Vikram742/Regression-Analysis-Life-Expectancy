#import and read the dataset
WHO<- read.csv("clipboard", sep = '\t', header = T)
# clean the data
str(WHO)
# change the data type of 2 variables to categorical variables
WHO$Country<- as.factor(WHO$Country)
WHO$Status<- as.factor(WHO$Status)
#check for missing values
sum(is.na(WHO))
colSums(is.na(WHO))
#we drop the rows with missing data
WHO<- na.omit(WHO)
# we drop the columns which we don't want while we run regression analysis
WHO1<- WHO[,-c(1:3)]
# run the regression model
model<- lm(Life.expectancy~., data = WHO1)
options(scipen = 10)
summary(model)
# Multiple R squared is 83% which means the independent variables can explain 83% of the variation
# in the dependent variable. P value is less than 0.05 so it is statistically significant.
# let's plot the model
plot(model, which = 1)


#check for outliers using IQR
resid<- residuals(model)
IQR<- IQR(resid)
IQR
outliers<- resid < quantile(resid,0.25) - 1.5*IQR| resid > quantile(resid,0.75) + 1.5*IQR
outliers_WHO1<- WHO1[outliers,]
#54 outliers have been found. These outliers need to be removed 
myWHO1<- WHO1[!outliers,]

# run regression analysis with new data frame excluding the outliers
options(scipen = 10)
model1<- lm(Life.expectancy~., data = myWHO1)
summary(model1)            
# Multiple R squared is 86.2% which means the independent variables can explain 86.2% of the variation
# in the dependent variable. P value is less than 0.05 so it is statistically significant.
# let's plot the model            
plot(model1, which = 1)

#check for the assumption of multicollinearity
library(car)
vif(model1)
# VIF values should not be more than 5. We have found quite a few variables that have a VIF value 
# higher than 5. Infant.deaths, percentage.expenditure,Under.five.deaths,GDP,thinness1.19,thinness5.9 
# Infant deaths and Under Five deaths have strong collinearity so we drop infant deaths

myWHO2<- myWHO1[,c(1:2,4:19)]

model2<- lm(Life.expectancy~.,data = myWHO2)
summary(model2)
# Multiple R squared is 85.4% which means the independent variables can explain 85.4% of the variation
# in the dependent variable. P value is less than 0.05 so it is statistically significant.
#check for the assumption of multicollinearity 
vif(model2)
# under.five.deaths values has dropped from 211.46 to 2.74 while others have dropped minimally
# we drop thinness 1.19 years as its VIF is more than 5 and there seems to be some as opposed to 
#thinness 5.9 years

myWHO3<- myWHO2[,c(1:14,16:18)]
model3<- lm(Life.expectancy~.,data = myWHO3)
summary(model3)
# Multiple R squared is 85.4% which means the independent variables can explain 85.4% of the variation
# in the dependent variable. P value is less than 0.05 so it is statistically significant.
#check for the assumption of multicollinearity 
vif(model3)
#thinness 5.9 years has dropped from 7.61 to 1.95. While the values of GDP are 13.46, I consider it
# to be an important independent variable. Therefore I will not drop it.

# lets split the data into train data and test data
# set the seed first 
set.seed(1234)
split<- sample(1:nrow(myWHO1), 0.8*nrow(myWHO1))
train<- myWHO1[split,]
test<- myWHO1[-split,]

#train the model using the train data
pred_model<- lm(Life.expectancy~.,data = train)
summary(pred_model)
# Multiple R squared is 86% and p value is less than alpha

# use the train data to predict the test data
predicted<- predict(pred_model, newdata = test)
test$predicted<- predicted

# Find out RMSE and MAPE
library(Metrics)
RMSE_reg<- rmse(test$Life.expectancy,test$predicted)
RMSE_reg
# RMSE_reg is 3.2

mape_reg<- mape(test$Life.expectancy,test$predicted)
mape_reg
# mape_reg is 0.037
(accuracy = 1 - mape_reg)
# Accuracy is 96.20% 

mae_reg<- mae(test$Life.expectancy,test$predicted)
mae_reg
#mae is 2.55 This means that on an average the predicted values deviate about 2.55 years
# from the actual values

#DECISION TREE
#run libraries
library(rpart)
library(rpart.plot)
library(caret)
library(RColorBrewer)
library(rattle)

dtree<- rpart(Life.expectancy~.,data = train, method = "anova")
dtree

plot(dtree)
text(dtree)
fancyRpartPlot(dtree)

mean(train$Life.expectancy)# mean life expectancy is 69.76

tuning_grid1 <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))
control1 <- trainControl(method = "cv", number = 5)
model_lifexp <- train(Life.expectancy~., method = "rpart", data = train, trControl = control1, tuneGrid = tuning_grid1)
print(model_lifexp)

#final value for the model was cp = 0.01 and RMSE is 3.04 and MAE is 2.30 and R squared is 86%

predicted<- predict(model_lifexp, newdata = test, type = "raw")
test$predicted<- predicted

library(Metrics)
rmseTest <- rmse(test$Life.expectancy, test$predicted)
rmseTest # Found to be 3.06
mapeTest <- mape(test$Life.expectancy, test$predicted)
mapeTest # 0.035
(accuracy=1-mapeTest) # 96.45%
maeTest<- mae(test$Life.expectancy, test$predicted)
maeTest
#mae is 2.35. This means that on an average the predicted values deviate about 2.35 years
# from the actual values

## In linear regression, RMSE was 3.2  and in decision tree it is 3.06
## In linear regression, MAPE was 0.037 and in decision tree it is 0.035
## In linear regression, MAE was 2.55 and in decision tree it is 2.35

##Random Forest
library(randomForest)

model <- randomForest(Life.expectancy~., data = train)
print(model)
varImpPlot(model)
#Income composition is the most important followed by adult mortality and the least relevant
# independent variable is Population.

## PREDICT THE MODEL
predicted <- predict(model, newdata = test,type = "response")  # predict life expectancy  
mydata <- data.frame(actuals=test$Life.expectancy, predicteds=predicted)

library(Metrics)
rmseTest <- rmse(mydata$actuals, mydata$predicteds)
rmseTest#1.73

mapeTest <- mape(mydata$actuals, mydata$predicteds)
mapeTest #0.01
(accuracy =1-mapeTest)# 98.27% accuracy

maeTest<- mae(mydata$actuals, mydata$predicteds)
maeTest#1.14 
plot(model)




#Conclusion:
## RMSE: Linear regression: 3.14, decision tree: 3.08 , random forest: 1.73
## MAPE: Linear regression: 0.036, decision tree: 0.034, random forest: 0.01
## MAE: Linear regression: 2.48, decision tree: 2.31 , random forest: 1.14

#random forest
