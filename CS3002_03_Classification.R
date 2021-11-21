#Read in data and create randomised data set
seedsData = read.csv('C:\\Users\\Picard\\Dropbox\\Github\\CS3002_03_Classification\\seeds_data.csv', sep=",")
seeds_rand=seedsData[sample(209,209),] #randomises the dataset to allow training

#creating seedsclass and seedsvalues dataframes
seedsclass = seeds_rand[,1]
seedsvalues = seeds_rand[,-1]

#set up a training set
seedsclassTrain = seedsclass[1:120]
seedsvaluesTrain = seedsvalues[1:120,]

#and testset
seedsclassTest = seedsclass[120:208]
seedsvaluesTest = seedsvalues[120:208,]

#DECISION TREE

#Build decision tree with Rpart
#install.packages("rpart") #not needed unless Rpart has not been installed on current System
library(rpart) #Imports Rpart library
fit <- rpart(seedsclassTrain~., method="class", data=seedsvaluesTrain)

#Plot decision tree
plot(fit, uniform=TRUE, main="Decision Tree for SeedsData")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Calculate predictions for each testcase in test set
treepred <-predict(fit, seedsvaluesTest, type = 'class')

#compare predections to actual test case values to get accuracy
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(treepred==seedsclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

# results as confusion matrix
table_mat = table(seedsclassTest, treepred)
print(table_mat)

#prune decision tree
pfit<- prune(fit, cp=0.1)
plot(pfit, uniform=TRUE, main="Pruned Decision Tree for SeedsData")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


#KNN

#import class library
library(class)
#generate predicted classes
knn3pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=3)

#calculate accuracy
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(knn3pred==seedsclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)