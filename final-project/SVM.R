
#Using the required libraries
library(caret)
library(e1071)
setwd('A:\\rcode\\')

# Reading necessary file
review <- read.csv('Amazon_Reviews.csv')
review[,7]
review[,7] = as.numeric(review[,7])-1
review[,7]
result<-review[,7]
data <- read.csv('data_sparse.csv')
data <- cbind(data,result)
trainData <- data[1:3000,]
testData <- data[3001:5000,]

#linear <- tune(svm, result~., data = trainData, kernel = "linear", 
#ranges = list(gamma = 10^(-1:1), cost = 10^(.5:1)))
#summary(linear)

#building model using Linear kernal since it has less error
model = svm(result ~ ., kernel = "radial", cost = 4, 
type="C-classification", gamma =0.125, data = trainData, scale = F)
model

#finding accuracy of the model
predictions <-  predict(model, testData)
plot(predictions)
confusionMatrix(testData[,580], predictions)

#Displaying the actual versus predicted values in a matrix on test data.
table(testData[,580], predictions) 

#writing it to csv file
write.csv(predictions,'SVM.csv')




