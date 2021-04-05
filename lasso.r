options(digits=3, show.signif.stars=FALSE)

energy.df<-read.csv("energydata_complete.csv", header = TRUE)

str(energy.df)

#converting integer to numeric
energy.df$Appliances<-as.numeric(energy.df$Appliances)
str(energy.df)
energy.df1$lights<-as.numeric(energy.df1$lights)
str(energy.df)

# deleting date column and re order the columns
energy.df=subset(energy.df, select = -c(1))
View(energy.df)

mydata = select(energy.df, Appliances, lights, rv1, rv2, T1, T2, T3, T4, T5, T6, T7, T8, T9, RH_1, RH_2, RH_3, RH_4, RH_5, RH_6, RH_7, RH_8, RH_9, T_out, Tdewpoint, RH_out, Press_mm_hg, Windspeed, Visibility)
View(mydata)

# deleting light column
mydata = subset(mydata, select = -c(2))

#removing RV1, RV2 and visibility
mydata = subset(mydata, select = -c(2,3))
mydata = subset(mydata, select = -c(25))

# Removing T_6 and T_9
mydata = subset(mydata, select = -c(7,10)) 

#Data standardization
 
p<-apply(mydata,2,mean)
q<-apply(mydata,2,sd)
r<-scale(mydata,p,q)  
View(mydata)
str(mydata)

# Lasso Regression
library(glmnet)
attach

# splitting mydata1 into train and test for LASSO REGRESSION
grid = 10^seq(10, -2, length = 100)
g = model.matrix(Appliances~., mydata) [,-1]
h = mydata$Appliances
set.seed(1)
lasso_train = sample(1:nrow(g), nrow(g)/2)
lasso_test = (-lasso_train)
h.test = h[lasso_test]

#fitting Lasso regression model
lasso.mod1 = glmnet(g[lasso_train,], h[lasso_train], alpha = 1, lambda = grid)
plot(lasso.mod1)

set.seed(1)
cv.out = cv.glmnet(g[lasso_train,], h[lasso_train], alpha = 1)
plot(cv.out)

bestlam = cv.out$lambda.min

lasso.pred = predict(lasso.mod1, s=bestlam, newx = g[lasso_test ,])
mean((lasso.pred-h.test)^2)
sqrt(mean((lasso.pred-h.test)^2))

out = glmnet(g,h,alpha = 1, lambda = grid)
lasso.coe = predict(out, type = "coefficients", s=bestlam)[1:22 ,]
lasso.coe

lasso.coe[lasso.coe!=0]


