library(readr)
library(tidyverse)

#This is meant to supplement the R Markdown document by showing just the code I used, with light documentation.

#Question 1

#reading statedata CSV and creating a summary
StateData <- read_csv("StateData.csv")
summary(StateData)

#creating a scatterplot of statedata and creating a map
plot(StateData$Longitude, StateData$Latitude, 
     main="United States", xlab = "Longitude", ylab = "Latitude")

#splitting high school graduation by region, and then applying mean over the vector
regiongrad <-  split(StateData$HighSchoolGrad, StateData$Region)
sapply(regiongrad, mean)

#creating a boxplot based on murder by region
regionmurder <- split(StateData$Murder, StateData$Region)
boxplot(regionmurder, xlab = "Region", ylab = "Murder Rate per 100k")

#creating a linear model, summarizing it, and then getting only the coefficients
LifeExpPredict = lm(LifeExp ~ Population + Income + Illiteracy + Murder + HighSchoolGrad + Frost + Area, data = StateData)
summary(LifeExpPredict)
summary(LifeExpPredict)$coefficient

#plotting life expectancy vs income, with straight line to help see relationship
plot(StateData$Income, StateData$LifeExp, main = "Life Expectancy vs Income", xlab = "Income", ylab = "Life Expectancy") 
abline(lm(StateData$LifeExp ~ StateData$Income), col="red")

#revising the model based on previous R-squared and p-values
revismodel = lm(StateData$LifeExp ~ StateData$Murder + StateData$HighSchoolGrad + StateData$Population + StateData$Frost)
summary(revismodel)
summary(revismodel)$coefficients

#creating a vector out of the prediction of the revised linear model
#data framing that vector to be used later
#creating a coordinates data frame out of the latitude and longitude data we have
#binding the two data frames together
#Printing the result, with the state number, its coordinates, and its expected life expectancy.
predict_vector <- predict(revismodel)
vector_frame <- data.frame(predict_vector)
coordinates <- data.frame(StateData$Latitude, StateData$Longitude)
est_lifexp <- cbind(coordinates, vector_frame)
print(est_lifexp %>% arrange(desc(vector_frame)))

#Question 2

#reading CSV
climate <- read_csv("ClimateChange.csv")

#splitting data into training set and testing set via subset
train_data <- subset(climate, Year <= 2006)
test_data <- subset(climate, Year > 2006)

#creating linear model based on training data
climatemodel = lm(Temp ~ CFC.11 + CFC.12 + CO2 + N2O + CH4 + Aerosols + TSI + MEI, data = train_data)

#summarizing the model
summary(climatemodel)
summary(climatemodel)$coefficients

#looking at correlations between variables
cor(train_data)

#revising model based on question
revised_climate = lm(Temp ~ N2O + MEI + TSI + Aerosols, data = train_data)
summary(revised_climate)
summary(revised_climate)$coefficients

#making test set predictions
test_climate = lm(Temp ~ N2O + MEI + TSI + Aerosols, data = test_data)
summary(test_climate)
predict(test_climate)


#Question 3

#reading CSV
elantra <- read_csv("Elantra.csv")

#splitting data by subset
etrain_data <- subset(elantra, Year <= 2012)
etest_data <- subset(elantra, Year > 2012)

#creating linear model based on training data
elantramodel = lm(ElantraSales ~ Unemployment + Queries + CPI.Energy + CPI.All, data = etrain_data)

#summarizing model
summary(elantramodel)
summary(elantramodel)$coefficients

#Using month as a variable in new model
monthmodel = lm(ElantraSales ~ Month + Unemployment + Queries + CPI.Energy + CPI.All, data = etrain_data)

#summarizing month model
summary(monthmodel)
summary(monthmodel)$coefficients

#factorizing the month variable and using it in the model
etrain_data$factormonth <- as.factor(etrain_data$Month)

emonthmodel = lm(ElantraSales ~ factormonth + Unemployment + Queries + CPI.Energy + CPI.All, data = etrain_data)

summary(emonthmodel)
summary(emonthmodel)$coefficients

#Attempting to use factorized months with test data and in prediction
etest2013 <- subset(etest_data, Year < 2014)

testsales = lm(ElantraSales ~ Unemployment + Queries + CPI.Energy + CPI.All + as.factor(Month), data = etest_data)

testsales2013 = lm(ElantraSales ~ Unemployment + Queries + CPI.Energy + CPI.All + as.factor(Month), data = etest2013)

summary(testsales2013)
summary(testsales)
predict(testsales)
