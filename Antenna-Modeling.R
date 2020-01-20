install.packages('caret')
install.packages('e1071')
library(caret)   # Create Data Partition function
library(e1071)
#Reading csv file and pre-processing 
data_antenna <- read.csv("BDA_Project_Final.csv")
View(data_antenna)
str(data_antenna)
f <- data_antenna$g1
db <- gsub("mm","",f)
g1 <- as.numeric(db)
db2 <- gsub("mm","",data_antenna$ringx)
ringx <- as.numeric(db2)
db3 <- gsub("mm","",data_antenna$t)
t <- as.numeric(db3)
db4 <- gsub("mm","",data_antenna$y1)
y1 <- as.numeric(db4)
freq <- data_antenna$F
gain <- data_antenna$Target
final_data <- data.frame(g1, ringx, t, y1, freq, gain)

#split the data into training and testing sets
intrain<- createDataPartition(final_data$g1,p=0.7,list=FALSE)
intrain

training_antenna <- final_data[intrain,]
View(training_antenna)
testing_antenna <- final_data[-intrain,]
View(testing_antenna)

dim(training_antenna)
dim(testing_antenna)
#Linear regression

model <- lm(gain ~ g1 + ringx + t + y1 + freq, training_antenna)

# Forecasted gain
forecast.gain <- predict(model,data=testing_antenna)

# R Square of a Regression Model

summary(model)$r.squared

summary(model)$adj.r.squared

# Square of Error
squaredError.reg <- (forecast.gain-testing_antenna$gain)^2

MSE.Regression <- mean(squaredError.reg)
MSE.Regression
Accuracy.Regression <- 100 - MSE.Regression
Accuracy.Regression

#SVM

#Fit a model. The function syntax is very similar to lm function

model_svm <- svm(gain ~ g1 + ringx + t + y1 + freq , training_antenna)

#Use the predictions on the data

pred.svm <- predict(model_svm, testing_antenna)

# Square of Error - Support Vector Machine
squaredError.svm <- (pred.svm - testing_antenna$gain)^2

MSE.SVM <- mean(squaredError.svm)
MSE.SVM
Accuracy.SVM <- 100 - MSE.SVM
Accuracy.SVM

#K-NN

# fit model
fit <- knnreg(training_antenna[,1:5], training_antenna[,6], k=3)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, testing_antenna[,1:5])
# summarize accuracy
mse <- mean((testing_antenna$gain - predictions)^2)
print(mse)
Accuracy.KNN <- 100 - mse
Accuracy.KNN
