# Cab-Fare-Prediction
A cab rental start-up company has successfully run the pilot project and now want to launch your cab service across the country. They have collected the historical data from pilot project and now have a requirement to apply analytics for fare prediction.I have designed a system that predicts the fare amount for a cab ride in the city.

> Here I have assessed the dataset first and cleaned all absurd values/records such as fare amount,passenger count in decimal,
fare amount is zero,passenger count is more than six,absurd date time value etc.

> Missing value analysis:-
Here I have first counte the null values present in any variable including the dependent variable. If the null values are more than
30 percent then it won’t be wise to impute or drop it. Otherwise we can either impute the null values or simply drop that record from
the dataset. Here I have imputed the missing values using the mean,median and KNN method. Here we have first replaced a known value with null in the dataset and stored it in a variable. Next we have imputed the value using both the methods mean and median. After imputing the missing values using both methods each of the imputed value is compared with the actual value stored originally and the method which imputes the value closest to actual value is used for imputing missing values of that particular variable. In python this procedure has been performed using user defined function and loops, and in R it showing memory error so only user defined function has been used and no loops.After performing the missing value analysis the data is cross checked so that no null values remain leftover.

> Moving on to EDA I have used pie-charts for every categorical variable and histograms for continuous variables
to see their distribution in the dataset

> Outliers from the data has been removed using flooring and capping method.
In this method we compare each value of a variable with two values

Min = q25-1.5*iqr
Max = q75+1.5*iqr
 
Where q25 = 25th percentile of that variable
             
             q75= 75th percentile of that variable
 
            iqr = inter-quartile range of that variable.

If a value exceeds Max then it is replaced with Max similarly when a value is less than Min it is replaced with min.
Thus outliers are removed from every variable.

> Feature Engineering

>Here we have added new columns in the dataset. From the pickup_datetime column we have extracted date, year, month, hour and day and created a column containing information regarding each of them. From the hour column we have created a column called time of day. Here we clubbed the hours in the following way

	3am to 9am as Morning
	9am to 3pm as Day
	3pm to 9pm as Evening
	9pm to 3am as Night

> Thereafter we have added a new column called distance travelled by the cab for a ride sung the haversine formula.
The haversine formula determines the great-circle distance between two points on a sphere given their longitudes and latitudes. Important in navigation, it is a special case of a more general formula in spherical trigonometry, the law of haversines, that relates the sides and angles of spherical triangles.

There after we have removed all the latitude and longitude from both test and training set.

> Again exploratory data analysis has been performed on cleaned data using multivariate plots
 both in R and Python.

> After data cleaning the continuous variables have been normalised as every continuous variable has different range which may convey
some wrong information regarding some variable.


> After data cleaning the continuous variables have been normalised as every continuous variable has different range which may convey
some wrong information regarding some variable.

> After feature scaling correlation plot has been used to check for multicollinearity in continuous variables and anova has been used for categorical variables to check if all the variables are significant.Thereafter dummies has been added for categorical variables and one dummy from each category has been removed to avoid any linear dependency.

> After adding dummies machine learning model has been built using different algorithms such as decision tree,random forest,
support vector machines,linear regression and Xgboost regression in both R and python. 

> RMSE for differet algorithms has been computed on training and testing set along with r-squared. 

> After this hyper parameters of Decision tree,random forests (runtime execution used to expire while tuning svr and Xgboost regression) have been tuned using Random Search CV So again models have been built using the same algorithms along computing their 
RMSE on training and testing data with R squared.The highest R squared value achieved was 0.723 using XGboost.

>A quite interesting fact is visible here that there’s huge difference in RMSE and R squared among same models in R and Python (untuned algorithms). It may be due to the fact that R was executed on my local machine and python on Google colab. It clearly reveals the hardware dependency of Machine learning models.










