options(digits=3, show.signif.stars=FALSE)

energy.df1<-read.csv("energydata_complete.csv", header = TRUE)
View(energy.df1)
str(energy.df1)

#converting integer to numeric
energy.df1$Appliances<-as.numeric(energy.df1$Appliances)
str(energy.df1)
energy.df1$lights<-as.numeric(energy.df1$lights)
str(energy.df1)

#Data exploration
 # deleting date column and re order the columns
library(dplyr)
energy.df1=subset(energy.df1, select = -c(1))
 # re ordering the columns of the dataframe
mydata1 = select(energy.df1, Appliances, lights, rv1, rv2, T1, T2, T3, T4, T5, T6, T7, T8, T9, RH_1, RH_2, RH_3, RH_4, RH_5, RH_6, RH_7, RH_8, RH_9, T_out, Tdewpoint, RH_out, Press_mm_hg, Windspeed, Visibility)

# deleting lights column
mydata1 = subset(mydata1, select = -c(2))
summary(mydata1)

# creating  new dataframes from mydata1
temperatures = mydata1[, c(4:12)]

humidity = mydata1[, c(13:21)]

weather = mydata1[, c(22:27)]

random_var = mydata1[(2:3)]

appliances = mydata1[1]

# calculating the descriptive statistics
summary(temperatures)
summary(humidity)
summary(weather)
summary(appliances)
summary(random_var)

#Data visualization
 #Scatter plots
pairs(temperatures, main=" Scatter plot of Temperatures")
pairs(humidity, main= "Scatterplot of Relative Humidity")
pairs(weather, main= "Scatter plot of Weather")
plot(T_out~T6, data = mydata1, main = "T_out vs T6")
plot(T7~T9, data = mydata1, main = "T7 vs T9")
plot(Appliances~Windspeed, data = mydata1, main = "Scatterplot of Appliances vs windspeed")
 
  #Histograms
hist(Appliances)
hist(Windspeed)
hist(Visibility)
hist(Tdewpoint)
hist(Press_mm_hg)
hist(RH_1)
hist(RH_2)
hist(RH_3)
hist(RH_4)
hist(RH_5)
hist(RH_6)
hist(RH_7)
hist(RH_8)
hist(RH_9)
hist(RH_out)
hist(T1)
hist(T2)
hist(T3)
hist(T4)
hist(T5)
hist(T6)
hist(T7)
hist(T8)
hist(T9)
hist(T_out)
# Data preprocessing
#removing RV1, RV2 and visibility
mydata1 = subset(mydata1, select = -c(2,3))
mydata1 = subset(mydata1, select = -c(25))
View(mydata1)

#Dataset splitting into training and testing data
indexes = sample(1:nrow(mydata1), size = 0.7*nrow(mydata1))
mydata1.train = mydata1[indexes,]
mydata1.test = mydata1[-indexes,]

#Data standardization
train.df<-mydata1.train 
m<-apply(train.df,2,mean)
s<-apply(train.df,2,sd)
z<-scale(train.df,m,s)  
View(train.df)

test.df<-mydata1.test
x<-apply(test.df,2,mean)  
su<-apply(test.df,2,sd)  
w<-scale(test.df,x,su)
View(test.df)
#Here train.df and test.df are the scaled/normalized training and testing data set respectively

# Removing T_6 and T_9 from the scaled training and testing data
train.df = subset(train.df, select = -c(7,10)) # normalized train data
View(train.df)

test.df = subset(test.df, select = -c(7,10)) #normalized test data
View(test.df)

#Multiple linear regression
model1<-lm(Appliances~T1+T2+T3+T4+T5+T7+T8+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Tdewpoint+RH_out+Press_mm_hg+Windspeed, data = train.df)
summary(model1)

model2<-lm(Appliances~T2+T3+T5+T7+T8+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_8+RH_9+Windspeed, data = train.df)
summary(model2)

model3<-lm(Appliances~T2+T3+T5+T7+T8+RH_1+RH_2+RH_3+RH_4+RH_5+RH_8+RH_9+Windspeed, data = train.df)
summary(model3)

#RMSE for Multiple linear regression
lmhat2<- predict.lm(model1, data = test.df)
mse00 = mean((lmhat2 - test.df$Appliances)^2)
RMSE00 = sqrt(mse00)
lmhat1<- predict.lm(model2, data = test.df)
mse000 = mean((lmhat1 - test.df$Appliances)^2)
RMSE000 = sqrt(mse000)

lmhat<-predict.lm(model3, data = test.df)
mse0 = mean((lmhat - test.df$Appliances)^2)
RMSE0 = sqrt(mse0)

# Random forest
library(randomForest)
set.seed(101)
attach(train.df)

#model1
appliance.rf=randomForest(Appliances~., data=train.df)
appliance.rf
yhat.rf = predict(appliance.rf, data=test.df)
mse =mean((yhat.rf - test.df$Appliances)^2)
rmse = sqrt(mse)
importance(appliance.rf)

#model2
appliance.rf2 =randomForest(Appliances~., data = train.df, mtry = 3, importance = TRUE)
appliance.rf2
importance(appliance.rf2)
yhat.rf2 = predict(appliance.rf2, data = test.df)
mse1 = mean((yhat.rf2 - test.df$Appliances)^2)
rmse1 = sqrt(mse)

#model3
appliance.rf3 =randomForest(Appliances~RH_1+RH_3+RH_8, data = train.df)
appliance.rf3
importance(appliance.rf3)
yhat.rf3 = predict(appliance.rf3, data = test.df)
mse2 = mean((yhat.rf3 - test.df$Appliances)^2)
rmse2 = sqrt(mse2)
coefficients(appliance.rf3)


