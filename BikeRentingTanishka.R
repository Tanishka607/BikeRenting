rm (list = ls()) # Cleaing the evironment
setwd('C:/Users/tanis/Desktop/Data_Science/Edwisor/Project1_BikeRenting/BikeRentingTanishka') #setting directory
getwd()

#Reading the data
data = read.csv('day.csv')

#knowing structure of our data
str(data) #We have 731 obs. of 16 variables

#Changing the data types of categorical variables
data$season= factor(data$season)
data$yr = factor(data$yr)
data$mnth = factor(data$mnth)
data$holiday = factor(data$holiday)
data$weekday = factor(data$weekday)
data$workingday = factor(data$workingday)
data$weathersit = factor(data$weathersit)


############################DATA PRE-POCESSING########################
######################################################################


#1.****MISSING VLUE ANALYIS****
#sum(is.na(data)) = 0
sapply(data, function(x) sum(is.na(x))) # We don't have any missing value


#2.****VISUALIZATION****
install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(scales)
par(mar=c(3,3,1,1))
par(mfrow=c(3,2))


#A)Univariate
#variable quantity
#par("mar") - set it to par(mar= c(1,1,1,1)) to avoid margin error for plots
#par("mar")
plot(data$temp, main="ScatterPlot of temp")
plot(data$atemp, main="ScatterPlot of atemp")
plot(data$hum, main="ScatterPlot of hum")
plot(data$windspeed, main="ScatterPlot of windspeed")
plot(data$casual, main="ScatterPlot of casual")
plot(data$registered, main="ScatterPlot of registered")
plot(data$cnt, main="ScatterPlot of cnt")

#B)bi-variant 
#B.1)categorical variables vs target variable
plot(cnt ~ season , data = data, main = 'season')# we see least rentals are in season 1 and most in season 3
plot(cnt ~ yr, data= data, main = 'yr')# #we see rental are high in 2012, this tells the rental is increasing with time
plot(cnt ~ mnth, data= data, main = 'mnth')#we see rental high from march to oct
plot(cnt ~ holiday, data= data, main = 'holiday')#we see rental high in weekdays
plot(cnt ~ weekday , data = data, main = 'weekday')#not much difference 
ggplot(data , aes_string(x=data$workingday)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("Working day") + ylab("Count") + ggtitle("Working day distribution") + theme(text = element_text(size = 15))
#bikes are rented more on working days
plot(cnt ~ weathersit , data = data, main = 'weathersit')#we see rental are high with clear weather and low with rainy


#B.2)continuous variables vs target variable 
reg1 = lm(cnt ~ temp, data = data)
with(data ,plot(temp, cnt, main = 'temp'))
abline(reg1) #rental counts increase with increase in temperature

reg2 = lm(cnt ~ atemp , data = data)
with (data, plot(atemp, cnt, main = 'atemp'))
abline(reg2)

reg3 = lm(cnt ~ hum, data = data)
with(data ,plot(hum, cnt, main = 'hum'))
abline(reg3) #rental count decrease with increase in humidity

reg4 = lm(cnt ~ windspeed , data = data)
with (data, plot(windspeed, cnt, main = 'windspeed'))
abline(reg4)

reg5 = lm(cnt ~ casual, data = data)
with(data ,plot(casual,cnt,main = 'casual'))
abline(reg5) 

reg6 = lm(cnt ~ registered , data = data)
with (data, plot(registered, cnt, main = 'registered'))
abline(reg6)


#3.****OUTLIER ANALYSIS****
numeric_index = sapply(data, is.numeric) # creating numerical value index
numeric_data = data[,numeric_index] # storing numeric data
cnames = colnames(numeric_data) #storing numeric data column names

#Creating box-plot to analyze outliers
for (i in 1:length(cnames)){
  assign(paste0("gn", i), ggplot(aes_string(y = cnames[i], x = "cnt"), data = subset(data)) +
           stat_boxplot(geom = "errorbar", width = 0.5) + 
           geom_boxplot(outlier.colour = "red", fill = "blue", outlier.shape = 18, outlier.size = 1, notch = FALSE) + 
           theme(legend.position = "bottom") + labs(y = cnames[i], x="count") + ggtitle(paste("Boxplot of count for", cnames[i])))
}
gridExtra::grid.arrange(gn2, gn3, gn4,gn5, gn6, gn7, ncol = 3, nrow = 3) # excludif gn1 as that is unique for each observation

#replace outliers with NA 
for(i in cnames) {
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i]) $out]
  
  print(length(val))
  data[,i][data[,i] %in% val] = NA
}

#imputing NA values
data$hum[is.na(data$hum)] = mean(data$hum,na.rm = T) #closest value was from mean
data$casual[is.na(data$casual)] = data$cnt - data$registered # as cnt  geg + casul 
data$windspeed[is.na(data$windspeed)] = mean(data$windspeed, na.rm = T)#closest value was from mean

# creating copy of imputed data 
copy1 = data


#4.****FEATURE SELECTION****
install.packages("corrgram") # for correlation graph
library(corrgram)

#A. Correlation check on continuous variable
round(cor(numeric_data),2) #Correlation tablecolumn wise
corrgram(data[, numeric_index], order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "correlation plot") # temp & ateamp, cnt=registered+casual
#temp and atemp are strongle correlated


#B. Anova test on categorical variable 
#creating sunset randomly for anova 
data = subset(data, select=-c(instant, atemp, casual, registered, dteday))

anova_test=aov(cnt~season + yr + mnth + holiday + weekday + workingday + weathersit, data = data)
summary(anova_test)

data = subset(data, select=-c(holiday, workingday))
data

#Multicollinearity test
install.packages("usdm")
library(usdm)

vifcor(data[,c(6,7,8)])


#5.****FEATURE SCALING****

hist(data$temp)
hist(data$hum)
hist(data$windspeed)
hist(data$cnt)


#Scaling categorical variable with dummies
install.packages("dummies") #for scaling
library(dummies)

#dummy.data.frame()
data_new = dummy.data.frame(data, sep = '_')
data_new

#data['cnt'] = (data$cnt-min(data$cnt))/(max(data$cnt)-min(data$cnt))#no need of scaling target variable.


############################MODELING########################
############################################################

#Sampling
#1. Non scaled data
set.seed(101)
train_index = sample(1:nrow(data), 0.8*nrow(data))
data_train = data[train_index,] 
data_test = data[-train_index,]

#2. Scaled data
#set.seed(102) 
train_scaled = sample(1:nrow(data_new), 0.8*nrow(data_new))
data_train_scaled = data_new[train_scaled,] 
data_test_scaled = data_new[-train_scaled,]

#Function to calculate MAPE
mape = function(actual, predict){
  mean(abs((actual-predict)/actual))*100
}


##1. *******************Decision tree*************************
#*************************************************************

par(mar=c(1,1,1,1))
par(mfrow=c(1,1))
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

#Model
set.seed(101)
model_DT = rpart(cnt~. , data = data_train, method = "anova")
summary(model_DT)
plt = rpart.plot(model_DT, type = 5, digits = 2, fallen.leaves = TRUE)
#?rpart.plot

#Predictions
DT_Predict = predict(model_DT, data_test[,-9])
plot(data_test$cnt, DT_Predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'DT model')

#Evaluation statistics
install.packages("caret")
library(caret)
postResample(DT_Predict, data_test$cnt)#R-sq = 0.74
mape(data_test$cnt, DT_Predict) #25.98



##2. *******************Random forest*************************
#*************************************************************

install.packages("randomForest")
library(randomForest)
library(inTrees)

#Model
set.seed(101)
model_RF = randomForest(cnt ~. , data_train, importance = TRUE, ntree = 500)
model_RF

#Error plotting
plot(model_RF) #my error i decreasing with higher number of trees

#Predict test data using RF model
RF_predict = predict(model_RF, data_test[,-9])
plot(data_test$cnt, RF_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'RF model')

#Evaluation statistics
postResample(RF_predict, data_test$cnt)#R-sq = 0.84
mape(data_test$cnt, RF_predict) #20.64

varImpPlot(model_RF) #Check the importance of variables in our RT model 



##3. *******************Linear regression*************************
#*****************************************************************

#Model
set.seed(101)
model_LR = lm(cnt ~. , data = data_train_scaled)
summary(model_LR)

#Predictions
LR_predict = predict(model_LR, data_test_scaled[,-32])
plot(data_test_scaled$cnt, LR_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'LR model') 
LR_predict

#Evaluation statistics
postResample(LR_predict, data_test_scaled$cnt)#R-sq = 0.82
mape(data_test_scaled$cnt, LR_predict) #18.29

#*****************************************************************
#*****************************************************************

# ***************Compare all three models*************************
plot(data_test$cnt, DT_Predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'DT model')
plot(data_test$cnt, RF_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'RF model')
plot(data_test_scaled$cnt, LR_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'LR model') 

#As per the above calculation Random forest is best suite for our Prediction.


