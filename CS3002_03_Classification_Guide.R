winedata = read.csv('C:\\Users\\Picard\\Dropbox\\Github\\CS3002_03_Classification\\winedata.csv', sep=",")

wineclass = winedata[,1]
winevalues = winedata[,-1]

#set up a training set
wineclassTrain = wineclass[1:100]
winevaluesTrain = winevalues[1:100,]

#and testset
wineclassTest = wineclass[100:178]
winevaluesTest = winevalues[100:178,]

#Decision Trees


#Build decision tree with Rpart
#install.packages("") #not needed unless Rpart has not been installed on current System
library(rpart) #Imports Rpart library
fit <- rpart(wineclassTrain~., method="class", data=winevaluesTrain)

#Plot decision tree
plot(fit, uniform=TRUE, main="Decision Tree for WineData3")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Calculate predictions for each testcase in test set
treepred <-predict(fit, winevaluesTest, type = 'class')

#compare prefections to actual test case values to get accuracy
n = length(wineclassTest) #the number of test cases
ncorrect = sum(treepred==wineclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

# results as confusion matrix
table_mat = table(wineclassTest, treepred)
print(table_mat)

#prune decision tree
pfit<- prune(fit, cp=0.1)
plot(pfit, uniform=TRUE, main="Pruned Decision Tree for WineData3")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


#KNN

#import class library
library(class)
#generate predicted classes
knn3pred = knn(winevaluesTrain, winevaluesTest, wineclassTrain, k=3)

#calculate accuracy
n = length(wineclassTest) #the number of test cases
ncorrect = sum(knn3pred==wineclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)
