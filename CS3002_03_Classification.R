#Set Working Directory
if((Sys.info()["nodename"])=="JANEWAY")
{
  setwd("C:\\Users\\Janeway\\Dropbox\\Github\\CS3002_03_Classification")
  print("Working Directory set to C:\\Users\\Janeway\\Dropbox\\Github\\CS3002_03_Classification")
}
if((Sys.info()["nodename"])=="PICARD")
{
  setwd("C:\\Users\\Picard\\Dropbox\\Github\\CS3002_03_Classification")
  print("Working directory set to C:\\Users\\Picard\\Dropbox\\Github\\CS3002_03_Classification")
}

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
seedsvaluesTest = seedsvalues[84:150,] #Selects values test data from unused part of seedsvalues

prune_temp <- data.frame()
prune_store <- data.frame()
pt_accuracy_store <- data.frame()
knn_accuracy_store <- data.frame()
knn_pt_compare <- data.frame()
knn_pt_temp_compare <- data.frame()

library(rpart.plot) #Imports Rpart library
library(class)

#DECISION TREE

#Build decision tree with Rpart

fit <- rpart(seedsclassTrain~., method="class", data=seedsvaluesTrain)

#Plot decision tree
rpart.plot(fit, uniform=TRUE, main="Decision Tree for SeedsData")
#text(fit, use.n=TRUE, all=TRUE, cex=.8)

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

m_accuracy=0

for(i in seq(from=0.1,to=1,by=0.1))
{
  fit_temp=fit
  pfit_temp<- prune(fit_temp, cp=i)
  #plot(pfit_temp, uniform=TRUE, main="Pruned Decision Tree for SeedsData")
  #text(pfit_temp, use.n=TRUE, all=TRUE, cex=.8)
  
  p_treepred <- predict(pfit_temp, seedsvaluesTest, type='class')
  p_ncorrect=sum(p_treepred==seedsclassTest)
  p_accuracy=p_ncorrect/n
  pt_accuracy_store=rbind(pt_accuracy_store,p_accuracy)
  output=data.frame(i,p_accuracy)
  print(output)
  prune_store <- rbind(prune_store, output)
  if(p_accuracy>m_accuracy)
  {
    m_accuracy=p_accuracy
    best_tree=pfit_temp
  }
}
plot(prune_store)
rpart.plot(best_tree)















for(i in seq(from=1,to=10,by=1))
{
  knn3pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=i)
  n = length(seedsclassTest) #the number of test cases
  ncorrect = sum(knn3pred==seedsclassTest) #the number of correctly predicted
  knn_accuracy=ncorrect/n
  print(knn_accuracy)
  knn_accuracy_store <- rbind(knn_accuracy_store, knn_accuracy)
}

knn_pt_compare <- cbind(knn_accuracy_store,pt_accuracy_store)
colnames(knn_pt_compare) <- c("knn_accuracy","pt_accuracy")
plot(knn_pt_compare)