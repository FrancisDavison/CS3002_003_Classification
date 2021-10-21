winedata = read.csv('/home/defiant/Dropbox/Github/CS3002_03_Classification/winedata3.csv', sep=",") #reads csv file into R,values seperated by commas

Wineclass = winedata[,1]
winevalues = winedata[,-1]

#set up training set
wineclassTrain = wineclass[1:100]
winevaluesTrain = winedata[1:100,]

#and test set
wineclassTest = wineclass[100:178]
winevaluesTest = winevalues[100:178,]

#install.packages("rpart") #only needs to be installed once so can be left commented most of the time
library(rpart)
fit <- rpart(wineclassTrain~., method="class", data=winevaluesTrain)

plot(fit, uniform=TRUE, main="Decision Treee for WineData3")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

treepred <- predict(fit, winevaluesTest, type = 'class')
n = length(wineclassTest) #the number of test cases
ncorrect = sum(treepred==wineclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)
