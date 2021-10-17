winedata = read.csv('C:\\Users\\Picard\\Dropbox\\Github\\CS3002_003_Classification\\winedata3.csv', sep=",")

wineclass = winedata[,1]
winevalues = winedata[,-1]

#set up training set
wineclassTrain = wineclass[1:100]
winevaluesTrain = winedata[1:100,]

#and test set
wineclassTest = wineclass[100:178]
winevaluesTest = winevalues[100:178,]

#install.packages("rpart")
library(rpart)
fit <- rpart(wineclassTrain~., method="class", data=winevaluesTrain)

plot(fit, uniform=TRUE, main="Decision Treee for WineData")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
