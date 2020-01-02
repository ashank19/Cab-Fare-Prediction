# CLearing the RAM

rm(list=ls())

# Setting the working directory

setwd('D:/Data Science/Scripts/Edwisor/Projects/Project 2')

# Checking for above operation
getwd()

# List of packages to be imported 

x=c("tidyverse","ggplot2","corrgram","DMwR","caret","randomForest",
    "unbalanced","c50","dummies","e1071","Information","MASS","rpart"
    ,"gbm","ROSE","usdm","readxl")

# Installing the packages in the list

install.packages(x)

lapply(x,require,character.only=TRUE)

rm(x)

install.packages("readxl")

install.packages("rpart")

# Loading libraries related to the packages

library(readxl)
library(ggplot2)
library(corrgram)
library(DMwR)
library(caret)
library(randomForest)
library(unbalanced)
library(c50)
library(dummies)
library(e1071)
library(Information)
library(MASS)
library(rpart)
library(gbm)
library(ROSE)

library(usdm)
library(tidyverse)

# Reading the data from the csv file

df=read.csv("train_cab.csv",header=T)
df1=read.csv("test.csv",header=T)

train=df
test=df1

# Getting the datatype of each variable

str(df)

str(df1)

# Cleaning the data i.e. removing all absurd values from it.

# Removing all those records which have pickup_datetime = 43 as it was clear from 
# python notebook that this value is not in appropriate so dropping it as datetime
# cannot be predicted.

df = df[-which(df$pickup_datetime == 43 ),]


# Extracting information such as month, year, day and hour of the day from pickup_datetime
# and storing them into a new column with appropriate names.

df$pickup_date = as.Date(as.character(df$pickup_datetime))
df$pickup_weekday = as.factor(format(df$pickup_date,"%u"))# Monday = 1
df$pickup_mnth = as.factor(format(df$pickup_date,"%m"))
df$pickup_yr = as.factor(format(df$pickup_date,"%Y"))
df$pickup_time =strftime(df$pickup_datetime, format = "%H:%M:%S")
df$pickup_hour= format(as.POSIXct(strptime(df$pickup_time,"%H:%M:%S",tz="")) ,format = "%H")

# Similarly extracting month, year, day and hour of the day from pickup_datetime from testing
# set.

df1$pickup_date = as.Date(as.character(df1$pickup_datetime))
df1$pickup_weekday = as.factor(format(df1$pickup_date,"%u"))# Monday = 1
df1$pickup_mnth = as.factor(format(df1$pickup_date,"%m"))
df1$pickup_yr = as.factor(format(df1$pickup_date,"%Y"))
df1$pickup_time =strftime(df1$pickup_datetime, format = "%H:%M:%S")
df1$pickup_hour= format(as.POSIXct(strptime(df1$pickup_time,"%H:%M:%S",tz="")) ,format = "%H")

# Summary so as to get overview of newly added columns

summary(df)

# Changing the data type of fare amount to numeric type.

df$fare_amount=as.numeric(df$fare_amount)

# Checking the summary of fare amount

summary(df$fare_amount)

# Checking the summary of passenger count

summary(df$passenger_count)

str(df)

# Resetting the row index

row.names(df)=NULL


summary(df1$passenger_count)

# Removing all those records which have passenger count greater than 6 as more than six
# passengers are not possible in a cab ride.

df=df[-which(df$passenger_count > 6 ),]

# Removing all those records which have zero passenger count as fare without a passenger is
# not possible.

df=df[-which(df$passenger_count == 0 ),]

# Checking for any discrepency in the data such that the latitude remains within -90 to +90 and
# longitude between -180 to +180 and dropping any such absurd value.

print(paste('pickup_longitude above 180=',nrow(df[which(df$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(df[which(df$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(df[which(df$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(df[which(df$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(df[which(df$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(df[which(df$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(df[which(df$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(df[which(df$dropoff_latitude > 90 ),])))

# Dropping all those records having pickup latitude greater than 90

df = df[-which(df$pickup_latitude > 90),]

# Similarly checking in testing data for any discrepancy in the latitudes and longitude 
# in the testing data

print(paste('pickup_longitude above 180=',nrow(df1[which(df1$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(df1[which(df1$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(df1[which(df1$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(df1[which(df1$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(df1[which(df1$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(df1[which(df1$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(df1[which(df1$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(df1[which(df1$dropoff_latitude > 90 ),])))

train=df

str(df)

# Missing Value Analysis

missing_val=data.frame(apply(df,2,function(x){sum(is.na(x))}))

missing_val$columns=row.names(missing_val)

row.names(missing_val)=NULL

names(missing_val)[1]='Missing_percentage'

missing_val$Missing_percentage=(missing_val$Missing_percentage/nrow(df))*100

# Missing value for test data

missing_val1=data.frame(apply(df1,2,function(x){sum(is.na(x))}))

missing_val1$columns=row.names(missing_val1)

row.names(missing_val1)=NULL

names(missing_val1)[1]='Missing_percentage'

missing_val1$Missing_percentage=(missing_val1$Missing_percentage/nrow(df))*100

# Plot of missing value percentage of some variables in the dataset. 

missing_val=missing_val[,c(2,1)]

missing_val=missing_val[order(-missing_val$Missing_percentage),]

row.names(missing_val) <- NULL

ggplot(data = missing_val[1:4,], aes(x= columns, y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Variables")+
  ggtitle("Missing data percentage") + theme_bw()

# Here I have used two methods of imputation mean and median as knn package was not compatible for my R version for each type of column. 
# For categorical variables I have first converted them to numeric type then imputed their values using the mean and median methods (depending upon which method
# imputes value closest to the actual value),then rounded them off to nearest integer and converted them to factor type.

# Imputing missing value for passenger count column

z=df
z[1,7]=NA
z$passenger_count[is.na(z$passenger_count)]=mean(z$passenger_count,na.rm=T)
p=z[1,7]
z=df
z[1,7]=NA
z$passenger_count[is.na(z$passenger_count)]=median(z$passenger_count,na.rm=T)
q=z[1,7]

# As the value of q is closer to actual value than of p so using median method.

df$passenger_count[is.na(df$passenger_count)]=median(df$passenger_count,na.rm=T)

sum(is.na(df$passenger_count))

# Checking the passenger count column.

summary(df$passenger_count)

# Removing the record containing fraction as passenger count

df=df[-which(df$passenger_count == 0.12 ),]

# Checking the passenger count variable

summary(df$passenger_count)

# Rounding off all the imputed values in passenger count as it should be an integer not fraction.

df$passenger_count=round(df$passenger_count)
df$passenger_count=as.integer(format(df$passenger_count))

# Creating list of continuous variables

con_var=c('pickup_latitude','pickup_longitude','dropoff_latitude','dropoff_longitude',
          'fare_amount')


# Outlier Analysis

# Creating boxplot for all continuous variables to check for outliers.

for (i in 1:length(con_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (con_var[i]), x = "fare_amount"), data = subset(df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=con_var[i],x="fare_amount")+
           ggtitle(paste("Box plot of fare_amount for",con_var[i])))
}

# ## Plotting plots together

gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,ncol=1)

# Columns having outliers
# pickup_latitude,pickup_longitude,dropoff_latitude,dropoff_longitude

# Here I have removed outliers using flooring and capping method i.e. replaced the outliers beyond (q75+1.5*iqr) with the same value
# and values below (q25-1.5*iqr) with the same value, where q75,q25 and iqr are 75th percentile,25th percentile and inter-quartile range of the given continuous variable.

# Removing outliers from pickup_latitude variable

t=df

q25=quantile(t$pickup_latitude,c(.25))
q75=quantile(t$pickup_latitude,c(.75))
iqr=q75-q25


for (i in 1:nrow(t)){
  if(t[i,"pickup_latitude"] > (q75+(iqr)*1.5)){
    t[i,"pickup_latitude"]=q75+(iqr)*1.5
    
  }
  else if(t[i,"pickup_latitude"] < (q25-(iqr)*1.5)){
    t[i,"pickup_latitude"]=q25-(iqr)*1.5
  }
}

# Removing outliers from pickup_longitude variable

q25=quantile(t$pickup_longitude,c(.25))
q75=quantile(t$pickup_longitude,c(.75))
iqr=q75-q25
for (i in 1:nrow(t)){
  if(t[i,"pickup_longitude"] > q75+(iqr)*1.5){
    t[i,"pickup_longitude"]=q75+(iqr)*1.5
  }
  else if(t[i,"pickup_longitude"] < q25-(iqr)*1.5){
    t[i,"pickup_longitude"]=q25-(iqr)*1.5
  }
  else{next}
}

# Removing outliers from dropoff_longitude variable

q25=quantile(t$dropoff_longitude,c(.25))
q75=quantile(t$dropoff_longitude,c(.75))
iqr=q75-q25
for (i in 1:nrow(t)){
  if(t[i,"dropoff_longitude"] > q75+(iqr)*1.5){
    t[i,"dropoff_longitude"]=q75+(iqr)*1.5
  }
  else if(t[i,"dropoff_longitude"] < q25-(iqr)*1.5){
    t[i,"dropoff_longitude"]=q25-(iqr)*1.5
  }
  else{next}
}

# Removing outliers from dropoff_latitude variable

q25=quantile(t$dropoff_latitude,c(.25))
q75=quantile(t$dropoff_latitude,c(.75))
iqr=q75-q25
for (i in 1:nrow(t)){
  if(t[i,"dropoff_latitude"] > q75+(iqr)*1.5){
    t[i,"dropoff_latitude"]=q75+(iqr)*1.5
  }
  else if(t[i,"dropoff_latitude"] < q25-(iqr)*1.5){
    t[i,"dropoff_latitude"]=q25-(iqr)*1.5
  }
  else{next}
}

df=t

# Creating boxplot for all continuous variables in testing set to check for outliers.

con_var=c('pickup_latitude','pickup_longitude','dropoff_latitude','dropoff_longitude')

for (i in 1:length(con_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (con_var[i]), x = "dropoff_longitude"), data = subset(df1))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=con_var[i],x="dropoff_longitude")+
           ggtitle(paste("Box plot of testing set for",con_var[i])))
}

# Plotting the boxplots
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)

# Removing outliers from pickup_latitude variable

t=df1

q25=quantile(t$pickup_latitude,c(.25))
q75=quantile(t$pickup_latitude,c(.75))
iqr=q75-q25


for (i in 1:nrow(t)){
  if(t[i,"pickup_latitude"] > (q75+(iqr)*1.5)){
    t[i,"pickup_latitude"]=q75+(iqr)*1.5
    
  }
  else if(t[i,"pickup_latitude"] < (q25-(iqr)*1.5)){
    t[i,"pickup_latitude"]=q25-(iqr)*1.5
  }
}

# Removing outliers from pickup_longitude variable

q25=quantile(t$pickup_longitude,c(.25))
q75=quantile(t$pickup_longitude,c(.75))
iqr=q75-q25
for (i in 1:nrow(t)){
  if(t[i,"pickup_longitude"] > q75+(iqr)*1.5){
    t[i,"pickup_longitude"]=q75+(iqr)*1.5
  }
  else if(t[i,"pickup_longitude"] < q25-(iqr)*1.5){
    t[i,"pickup_longitude"]=q25-(iqr)*1.5
  }
  else{next}
}

# Removing outliers from dropoff_longitude variable

q25=quantile(t$dropoff_longitude,c(.25))
q75=quantile(t$dropoff_longitude,c(.75))
iqr=q75-q25
for (i in 1:nrow(t)){
  if(t[i,"dropoff_longitude"] > q75+(iqr)*1.5){
    t[i,"dropoff_longitude"]=q75+(iqr)*1.5
  }
  else if(t[i,"dropoff_longitude"] < q25-(iqr)*1.5){
    t[i,"dropoff_longitude"]=q25-(iqr)*1.5
  }
  else{next}
}

# Removing outliers from dropoff_latitude variable

q25=quantile(t$dropoff_latitude,c(.25))
q75=quantile(t$dropoff_latitude,c(.75))
iqr=q75-q25
for (i in 1:nrow(t)){
  if(t[i,"dropoff_latitude"] > q75+(iqr)*1.5){
    t[i,"dropoff_latitude"]=q75+(iqr)*1.5
  }
  else if(t[i,"dropoff_latitude"] < q25-(iqr)*1.5){
    t[i,"dropoff_latitude"]=q25-(iqr)*1.5
  }
  else{next}
}

df1=t

# Calculating the distance travelled using haversine formula

deg_to_rad = function(deg){
  (deg * pi) / 180
}
haversine = function(long1,lat1,long2,lat2){
  #long1rad = deg_to_rad(long1)
  phi1 = deg_to_rad(lat1)
  #long2rad = deg_to_rad(long2)
  phi2 = deg_to_rad(lat2)
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371e3
  R * c / 1000 #1000 is used to convert to meters
}

# Using haversine formula to calculate distance for both training and testing set

df$distance = haversine(df$pickup_longitude,df$pickup_latitude,df$dropoff_longitude,df$dropoff_latitude)

df1$distance = haversine(df1$pickup_longitude,df1$pickup_latitude,df1$dropoff_longitude,df1$dropoff_latitude)

# We will remove the variables which were used to feature engineer new variables
df = subset(df,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
df1 = subset(df1,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

# Adding new feature depending on pickup_hour

df$pickup_hour=as.integer(df$pickup_hour)

df1$pickup_hour=as.integer(df1$pickup_hour)

df$Time_of_day[df$pickup_hour>=3 & df$pickup_hour<=9]='Morning'
df$Time_of_day[df$pickup_hour>9 & df$pickup_hour<=15]='Day'
df$Time_of_day[df$pickup_hour>15 & df$pickup_hour<=21]='Evening'
df$Time_of_day[df$pickup_hour>21 | df$pickup_hour<3]='Night'

df1$Time_of_day[df1$pickup_hour>=3 & df1$pickup_hour<=9]='Morning'
df1$Time_of_day[df1$pickup_hour>9 & df1$pickup_hour<=15]='Day'
df1$Time_of_day[df1$pickup_hour>15 & df1$pickup_hour<=21]='Evening'
df1$Time_of_day[df1$pickup_hour>21 | df1$pickup_hour<3]='Night'

str(df)

# Feature selection using R

# Correlation Plot of continuous variables so as to observe correlation if any among independent variables. 

numeric_index = sapply(df,is.numeric)

numeric_data = df[,numeric_index]

cnames = colnames(numeric_data)

corrgram(df[,cnames], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

df$Time_of_day[df$Time_of_day=='Morning']=1
df$Time_of_day[df$Time_of_day=='Day']=2
df$Time_of_day[df$Time_of_day=='Evening']=3
df$Time_of_day[df$Time_of_day=='Night']=4

df1$Time_of_day[df1$Time_of_day=='Morning']=1
df1$Time_of_day[df1$Time_of_day=='Day']=2
df1$Time_of_day[df1$Time_of_day=='Evening']=3
df1$Time_of_day[df1$Time_of_day=='Night']=4

df$Time_of_day=as.factor(df$Time_of_day)
df1$Time_of_day=as.factor(df1$Time_of_day)

#ANOVA for categorical variables with target numeric variable

#aov_results = aov(fare_amount ~ Time_of_day * pickup_weekday * pickup_mnth * pickup_yr,data 
# = train)

aov_results = aov(fare_amount ~ Time_of_day + pickup_weekday + pickup_mnth + pickup_yr,data = df)

summary(aov_results)

# pickup_weekday has p value greater than 0.05 

df = subset(df,select=-pickup_weekday)

#removing from test set

df1 = subset(df1,select=-pickup_weekday)

# histogram for distance

hist(df$distance,xlab='Distance in meters',
     ylab='Frequency',main='Distribution of distance',
     xlim=c(0,15),ylim=c(0,5000),col='grey',border = 'black')

# From the histogram it is clear that the distance variable has right skewed data,
# thus we need to normalise it.

df[,'distance'] = (df[,'distance'] - min(df[,'distance']))/(max(df[,'distance'])-min(df[,'distance']))

df1[,'distance'] = (df1[,'distance'] - min(df1[,'distance']))/(max(df1[,'distance'])-min(df1[,'distance']))  

# updating categorical columns list
cat_var=c('Time_of_day','pickup_yr','pickup_mnth')

# Adding dummies for categorical variables. As this package will add dummies for all factor type variables
 
k=df[,c('Time_of_day','pickup_yr','pickup_mnth','distance','fare_amount','passenger_count')]

library(dummies)

k= dummy.data.frame(k, sep = "_",all=F)

k$distance=df$distance
k$passenger_count=df$passenger_count
k$fare_amount=df$fare_amount


k1=df1[,c('Time_of_day','pickup_yr','pickup_mnth','distance','passenger_count')]

k1= dummy.data.frame(k1, sep = "_",all=F)

k1$distance=df1$distance
k1$passenger_count=df1$passenger_count

# Dropping one dummy from each categorical variable as they are linearly independent

X=subset(k,select= -c(Time_of_day_1,pickup_yr_2009,pickup_mnth_01))

X1=subset(k1,select=-c(Time_of_day_1,pickup_yr_2009,pickup_mnth_01))

set.seed(123)

# As our taget variable is continuous so using random sampling method rather than
# stratified or systematic sampling

# Splitting the dataset into training and testing set  in 80:20 ratio.

train.index=sample(nrow(X),0.8*nrow(X),replace=F)

#Training set
X_train = X[train.index,]

# Validation set
X_test = X[-train.index,]

# Resetting the index of training and testing set.
row.names(X_train)=NULL

row.names(X_test)=NULL

#Decision tree for regression

library(rpart)

library(MASS)

# Training the model
fit=rpart(fare_amount ~ .,data=X_train,method='anova')

# Predicting the output on training and testing set.
pred_DT_train=predict(fit,X_train[,-23])

pred_DT_test=predict(fit,X_test[,-23])

pred_DT_testdata=predict(fit,X1)

test$fare_amount_DT=pred_DT_testdata
# Summary of Decision Tree model

summary(fit)

# Writing rules to disk

write(capture.output(summary(fit)), "Rules.txt")

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$fare_amount)

#  Total sum of squares
ss_total <- sum((X_test$fare_amount - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_DT_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$fare_amount - pred_DT_test)^2)

#  R2 Score
r2 <- 1 - (ss_residuals / ss_total)

# r2 = 0.324

# Calculating the error metric for this model

library(DMwR)

regr.eval(X_train[,23],pred_DT_train,stats='rmse')
# RMSE = 140.175

regr.eval(X_test[,23],pred_DT_test,stats='rmse')
# RMSE = 141.6259


# Random Forest regressor Model

library(randomForest)

# Training the model
fit_RF=randomForest(fare_amount ~ .,data=X_train,importance=TRUE)


# Predicting the output on testing and training set.
pred_RF_train=predict(fit_RF,X_train[,-23])

pred_RF_test=predict(fit_RF,X_test[,-23])

pred_RF_testdata=predict(fit_RF,X1)

test$fare_amount_RF=pred_RF_testdata

# Evaluating the error metrics.
regr.eval(X_train[,23],pred_RF_train,stats='rmse')

# RMSE = 96.716

regr.eval(X_test[,23],pred_RF_test,stats='rmse')

# RMSE = 140.1306

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$fare_amount)

#  Total sum of squares
ss_total <- sum((X_test$fare_amount - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_RF_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$fare_amount - pred_RF_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

# r2 = 0.3391

# Linear regression model

# checking multicollinearity

library(usdm)

# Checking the variance inflation factor of all variables
vif(X[,-23])

vifcor(X[,-23],th=0.8)

fit_LR=lm(fare_amount ~ .,data=X_train)

# Predicting for training data
pred_LR_train = predict(fit_LR, X_train[,-23])

# Predicting for testing data
pred_LR_test = predict(fit_LR,X_test[,-23])

pred_LR_testdata=predict(fit_LR,X1)

test$fare_amount_LR=pred_LR_testdata

regr.eval(X_train[,23],pred_LR_train,stats='rmse')

# RMSE = 158.8126

regr.eval(X_test[,23],pred_LR_test,stats='rmse')

# RMSE = 158.168

summary(fit_LR)
# R-squared= 0.1501
# Adj R-squared= 0.1486

# As per above model summary dropping all the variables having p-values greater than 0.05 
# and building the model again.


X=subset(k,select= c(pickup_yr_2012,pickup_yr_2013,pickup_yr_2014,pickup_yr_2015,pickup_mnth_05,
                     pickup_mnth_09,pickup_mnth_10,pickup_mnth_11,pickup_mnth_12,
                     distance,fare_amount))


vifcor(X[,-11],th=0.8)


train.index=sample(nrow(X),0.8*nrow(X),replace=F)

X_train = X[train.index,]

X_test = X[-train.index,]

row.names(X_train)=NULL

row.names(X_test)=NULL

fit_LR=lm(fare_amount ~ .,data=X_train)

pred_LR_train = predict(fit_LR, X_train[,-11])

pred_LR_test = predict(fit_LR,X_test[,-11])

regr.eval(X_train[,11],pred_LR_train,stats='rmse')

# RMSE = 159.053

regr.eval(X_test[,11],pred_LR_test,stats='rmse')

# RMSE = 157.3267

summary(fit_LR)

# R squared = 0.1485
# Adj R squared = 0.1478
# On comparing the rmse of above two linear models it is found that rmse remains constant
# and r squared is also same
# thus switching to original dataset which was used for the first linear model.

# Dropping one dummy column of each categorical variable.

X=subset(k,select= -c(Time_of_day_1,pickup_yr_2009,pickup_mnth_01))

# As our taget variable is continuous so using random sampling method rather than
# stratified or systematic sampling

# Splitting the dataset into training and testing set  in 80:20 ratio.

train.index=sample(nrow(X),0.8*nrow(X),replace=F)

# Training set
X_train = X[train.index,]

# Testing set
X_test = X[-train.index,]

# Resetting the index of training and testing set.
row.names(X_train)=NULL

row.names(X_test)=NULL

# SVM model for regression

library(e1071)

sum(is.na(df))

#Regression with SVM
fit_svm = svm(fare_amount ~ .,data=X_train)

#Predict using SVM regression
pred_svm_train = predict(fit_svm, X_train[,-23])

pred_svm_test=predict(fit_svm, X_test[,-23])

pred_svm_testdata=predict(fit_svm,X1)

test$fare_amount_SVM=pred_svm_testdata

regr.eval(X_train[,23],pred_svm_train,stats='rmse')

# RMSE = 146.79

regr.eval(X_test[,23],pred_svm_test,stats='rmse')

# RMSE = 152.947

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$fare_amount)

#  Total sum of squares
ss_total <- sum((X_test$fare_amount - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_svm_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$fare_amount - pred_svm_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

# r2 = 0.2136

# Model for XGBoost

set.seed(123)

library(gbm)

#Develop Model on training data
fit_XGB = gbm(fare_amount~., data = X_train, n.trees = 1000, interaction.depth = 2)

#Lets predict for training data
pred_XGB_train = predict(fit_XGB, X_train[,-23], n.trees = 1000)

#Lets predict for testing data
pred_XGB_test = predict(fit_XGB, X_test[,-23], n.trees = 1000)

pred_XGB_testdata=predict(fit_XGB,X1,n.trees=1000)

test$fare_amount_XGB=pred_XGB_testdata

regr.eval(X_train[,23],pred_XGB_train,stats='rmse')

# RMSE = 132.17

regr.eval(X_test[,23],pred_XGB_test,stats='rmse')

# RMSE = 135.43

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(X_test$fare_amount)

#  Total sum of squares
ss_total <- sum((X_test$fare_amount - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_XGB_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((X_test$fare_amount - pred_XGB_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

# r2 = 0.3834

str(test)

# Writing the predictions into a csv file

write.csv(test,'R_predictions.csv',row.names=F,col.names=T)