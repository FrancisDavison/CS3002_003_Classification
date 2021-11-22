#Set Working Directory
#setwd("C:\\Users\\Picard\\Dropbox\\Github\\CS3002_03_Classification") #Set Working Directory Desktop
setwd("C:\\Users\\Janeway\\Dropbox\\Github\\CS3002_03_Classification") #Set Working Directory Laptop

#Read in data and create randomised data set
seedsData = read.csv('.\\seeds_data.csv', sep=",") #CSV Read
seeds_rand=seedsData[sample(150,150),] #randomises the dataset to allow training

#creating seedsclass and seedsvalues dataframes
seedsclass = seeds_rand[,1] #Selects Class values from Column 1 of seeds_rand
seedsvalues = seeds_rand[,-1] #sELECTS seed values from seeds_rand

#set up a training set
seedsclassTrain = seedsclass[1:84] #Selects Class Training data from Seedsclass
seedsvaluesTrain = seedsvalues[1:84,] #Selects Values Training data from seedsvalues

#and testset
seedsclassTest = seedsclass[84:150] #Selects class test data from unused part of seedsclass
seedsvaluesTest = seedsvalues[84:150,] #Selects values test data from unused part of seedsvlaues

min=0
max=1
interval=0.01

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

#IS THIS PART CORRECT?!?!?!?!?!?!?!?!?!?!?!?!!?!?!?!?!?!?!?!?!?!?!?!?!!??! Hopefuly correct for Q2?
#prune decision tree
pfit<- prune(fit, cp=0.1)
plot(pfit, uniform=TRUE, main="Pruned Decision Tree for SeedsData")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#Try with a for loop and just chance the value of CP from 0 to 1 with increments of 0.01

for(i in min:max, 1)
{
  
}

#printcp(fit, digits=getOption("digits")-2)
#(pfit, minline=TRUE, lty=3, col=1, upper=c("none"))


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

