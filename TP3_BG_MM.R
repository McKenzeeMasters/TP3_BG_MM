
library(tidyverse)
dataset=read.csv('Clean_Dataset.csv')

# Exploration of data set
head(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)


# Missing value
colSums(is.na(dataset))
# No missing values

# Drop flight and X columns
dataset = subset(dataset, select = -X)
dataset = subset(dataset, select = -flight)
glimpse(dataset)


# Multiple Linear Regression
library(caTools)
set.seed(100)

# 80% training, 20% testing data split
split=sample.split(dataset$price, SplitRatio = 0.8) 

training_set=subset(dataset,split=TRUE)
test_set=subset(dataset, split=FALSE)


# Multiple linear regression training
Regressor =lm(formula=price~ ., training_set)
summary(Regressor)
#5.268e+04 + -1.164e+02(airlineAirAsia) + 1.589e+03(airlineGO_FIRST) + 1.991e+03(airlineIndigo) + 2.178e+03(airlineSpiceJet) + 3.955e+03(airlineVistara) + -6.748e+01(source_cityChennai) + -1.406e+03(source_cityDelhi) + -1.679e+03(source_cityHyderabad) + 1.584e+03(source_cityKolkata) + -2.119e+02(source_cityMumbai) + 8.357e+02(departure_timeEarly_Morning) + 7.338e+02(departure_timeEvening) + 1.694e+03(departure_timeLate_Night) + 8.563e+02(departure_timeMorning) + 6.901e+02(departure_timeNight) + 2.105e+03(stopstwo_or_more) + -7.586e03(stopszero) + -7.720e+02(arrival_timeEarly_Morning) + 9.247e+02(arrival_timeEvening) + 9.533e+02(arrival_timeLate_Night) + 4.766e+02(arrival_timeMorning) + 1.143e+03(arrival_timeNight) + -2.198e+02(destination_cityChennai) + -1.554e+03(destination_cityDelhi) + -1.720e+03(destination_cityHyderabad) + 1.377e+03(destination_cityKolkata) + -2.877e+01(destination_cityMumbai) + -4.492e+04(classEconomy) + 4.257e(duraction) + -1.310e+02(days_left)
#source_cityChennai and destination_cityMumbai are not statistically significant

# Mean square error
summ=summary(Regressor)
MSE=(mean(summ$residuals^2)) 
paste('Mean Square Error:', MSE)

paste('RMSE:', (MSE)^0.5)

# R-Squared
paste('R-Squared:', summary(Regressor)$r.squared)

# Testing Set Prediction
y_pred=predict(Regressor, newdata = test_set)
data=data.frame(test_set$price, y_pred)
head(data)


# Validation
new=read_csv("Clean_Dataset.csv")

colSums(is.na(new))

glimpse(new)
x=new[c(1:9)]
glimpse(x)

data.frame(new[c(10)], predict(Regressor, newdata = x))
data.frame(new[c(10)])


# 60% training, 40% testing data split
dataset3=read.csv('Clean_Dataset.csv')
dataset3 = subset(dataset3, select = -X)
dataset3 = subset(dataset3, select = -flight)
glimpse(dataset3)

split=sample.split(dataset3$price, SplitRatio = 0.6) 

training_set=subset(dataset3,split=TRUE)
test_set=subset(dataset3, split=FALSE)


# Multiple linear regression training
Regressor =lm(formula=price~ ., training_set)
summary(Regressor)
#5.268e+04 + -1.164e+02(airlineAirAsia) + 1.589e+03(airlineGO_FIRST) + 1.991e+03(airlineIndigo) + 2.178e+03(airlineSpiceJet) + 3.955e+03(airlineVistara) + -6.748e+01(source_cityChennai) + -1.406e+03(source_cityDelhi) + -1.679e+03(source_cityHyderabad) + 1.584e+03(source_cityKolkata) + -2.119e+02(source_cityMumbai) + 8.357e+02(departure_timeEarly_Morning) + 7.338e+02(departure_timeEvening) + 1.694e+03(departure_timeLate_Night) + 8.563e+02(departure_timeMorning) + 6.901e+02(departure_timeNight) + 2.105e+03(stopstwo_or_more) + -7.586e03(stopszero) + -7.720e+02(arrival_timeEarly_Morning) + 9.247e+02(arrival_timeEvening) + 9.533e+02(arrival_timeLate_Night) + 4.766e+02(arrival_timeMorning) + 1.143e+03(arrival_timeNight) + -2.198e+02(destination_cityChennai) + -1.554e+03(destination_cityDelhi) + -1.720e+03(destination_cityHyderabad) + 1.377e+03(destination_cityKolkata) + -2.877e+01(destination_cityMumbai) + -4.492e+04(classEconomy) + 4.257e(duraction) + -1.310e+02(days_left)
#source_cityChennai and destination_cityMumbai are not statistically significant

# Mean square error
summ=summary(Regressor)
MSE=(mean(summ$residuals^2)) 
paste('Mean Square Error:', MSE)

paste('RMSE:', (MSE)^0.5)

# R-Squared
paste('R-Squared:', summary(Regressor)$r.squared)

# Testing Set Prediction
y_pred=predict(Regressor, newdata = test_set)
data2=data.frame(test_set$price, y_pred)
head(data2)


# Validation
new=read_csv("Clean_Dataset.csv")

colSums(is.na(new))

glimpse(new)
x=new[c(1:9)]
glimpse(x)

data.frame(new[c(10)], predict(Regressor, newdata = x))
data.frame(new[c(10)])


# 70% training, 30% testing data split
dataset2=read.csv('Clean_Dataset.csv')
dataset2 = subset(dataset2, select = -X)
dataset2 = subset(dataset2, select = -flight)
glimpse(dataset2)

split=sample.split(dataset2$price, SplitRatio = 0.7) 

training_set=subset(dataset2,split=TRUE)
test_set=subset(dataset2, split=FALSE)


# Multiple linear regression training
Regressor =lm(formula=price~ ., training_set)
summary(Regressor)
#5.268e+04 + -1.164e+02(airlineAirAsia) + 1.589e+03(airlineGO_FIRST) + 1.991e+03(airlineIndigo) + 2.178e+03(airlineSpiceJet) + 3.955e+03(airlineVistara) + -6.748e+01(source_cityChennai) + -1.406e+03(source_cityDelhi) + -1.679e+03(source_cityHyderabad) + 1.584e+03(source_cityKolkata) + -2.119e+02(source_cityMumbai) + 8.357e+02(departure_timeEarly_Morning) + 7.338e+02(departure_timeEvening) + 1.694e+03(departure_timeLate_Night) + 8.563e+02(departure_timeMorning) + 6.901e+02(departure_timeNight) + 2.105e+03(stopstwo_or_more) + -7.586e03(stopszero) + -7.720e+02(arrival_timeEarly_Morning) + 9.247e+02(arrival_timeEvening) + 9.533e+02(arrival_timeLate_Night) + 4.766e+02(arrival_timeMorning) + 1.143e+03(arrival_timeNight) + -2.198e+02(destination_cityChennai) + -1.554e+03(destination_cityDelhi) + -1.720e+03(destination_cityHyderabad) + 1.377e+03(destination_cityKolkata) + -2.877e+01(destination_cityMumbai) + -4.492e+04(classEconomy) + 4.257e(duraction) + -1.310e+02(days_left)
#source_cityChennai and destination_cityMumbai are not statistically significant

# Mean square error
summ=summary(Regressor)
MSE=(mean(summ$residuals^2)) 
paste('Mean Square Error:', MSE)



# R-Squared
paste('R-Squared:', summary(Regressor)$r.squared)

# Testing Set Prediction
y_pred=predict(Regressor, newdata = test_set)
data2=data.frame(test_set$price, y_pred)
head(data2)


# Validation
new=read_csv("Clean_Dataset.csv")

colSums(is.na(new))

glimpse(new)
x=new[c(1:9)]
glimpse(x)

data.frame(new[c(10)], predict(Regressor, newdata = x))
data.frame(new[c(10)])
