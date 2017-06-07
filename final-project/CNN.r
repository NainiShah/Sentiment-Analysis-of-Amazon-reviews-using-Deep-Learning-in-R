library(mxnet)
library(mlbench)

setwd('C:\\Users\\avina\\Desktop\\prudential data')

review <- read.csv('Amazon_Reviews.csv')
review <- review[,3:7]
review
review[,5] = as.numeric(review[,5])-1

trainData <- review[1:2000,]
testData <- review[2000:5000,]

train.x <- data.matrix(trainData[,1:5])
train.x

train.y <- trainData[,5]
test.x <- data.matrix(testData[,])
test.y <- testData[,5]

mx.set.seed(0)
model <- mx.mlp(train.x, train.y , hidden_node=10, out_node=2,
                 learning.rate=0.07)


preds =t(predict(model,test.x))
colnames(preds) <- c('0','1')
preds

write.csv(preds,'CNN.csv')




