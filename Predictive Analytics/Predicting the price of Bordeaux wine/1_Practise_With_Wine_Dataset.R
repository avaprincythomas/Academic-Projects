#Practising regression with wine.csv data set

#Getting and setting working directory
getwd()
setwd("E:\\Other Courses\\Analytics Edge\\Chapter - 3 - Linear Regression\\Datasets")
getwd()

#Reading data from wine.csv
wine=read.csv("wine.csv")

#Getting the structure and summary of wine
str(wine)
summary(wine)

#Creating a linear regression model of price vs agst and storing it to the variable model1
model1= lm(Price~AGST , data=wine)

#Viewing the summary of the linear regression model created
summary(model1)

#Viewing the sum of squared errors of the model1
SSE1=sum(model1$residuals^2)
SSE1

#Creating a multiple linear regression model of price vs agst and harvestrain
model2=lm(Price~AGST+HarvestRain, data=wine)
summary(model2)
SSE2=sum(model2$residuals^2)
SSE2

#Creating a multiple linear regression model with all the independent variables
model3=lm(Price~AGST+HarvestRain+WinterRain+Age+FrancePop, data=wine)
summary(model3)
SSE3=sum(model3$residuals^2)
SSE3

#Creating a model eliminating the France Population because it is found to be insignificant
model4=lm(Price~AGST+HarvestRain+WinterRain+Age , data=wine)
summary(model4)
SSE4=sum(model4$residuals^2)
SSE4

#Linear model of Price vs HarvestRain and WinterRain
model5=lm(Price~HarvestRain+WinterRain, data=wine)
summary(model5)
SSE5=sum(model5$residuals^2)
SSE5

#To plot a graph of WinterRain vs Price to visualize the correlation
plot(wine$WinterRain, log(wine$Price), xlab="Winter Rain (mm)", ylab="Logarithm of Price", main="Winter Rain vs log(Price)")

#To plot a graph of Harvest Rain vs AGST to view the correlation between them
plot(wine$HarvestRain, wine$AGST, xlab="Average Rain in (mm)", ylab="AGST(Celsius)")

#To plot a graph of Age and France Population to view the correlation between them
plot(wine$FrancePop, wine$Age, xlab="France Population in number", ylab="Age of wine in years")

#To compute the correlation between age and france population
cor(wine$Age, wine$FrancePop)

#Compute the correlation of all variables in the dataframe wine with each other
cor(wine)

#To see the effect of removing both Age and France Population from the model
model6=lm(Price~HarvestRain+WinterRain+AGST, data=wine)
summary(model6)

#To load the wine_test.csv to R and view the summary and structure of WineTest
WineTest=read.csv("wine_test.csv")
str(WineTest)
summary(WineTest)

#Predicting the values of price in WineTest using model4
PredictTest=predict(model4, newdata=WineTest)
PredictTest
str(WineTest)

#To compute the sum of squared errors in for the model prediction using the test data set
Prediction_SSE=sum((WineTest$Price-PredictTest)^2)
Prediction_SSE

#To compute the total sum of squares for the model prediction using winetest
Prediction_SST=sum((WineTest$Price-mean(wine$Price))^2)
Prediction_SST

#To compute the R-squared value for the prediction model
Prediction_R_Squared=1-(Prediction_SSE/Prediction_SST)
Prediction_R_Squared
