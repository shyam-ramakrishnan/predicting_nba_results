#Read data from CSV and store in Data Frame
prc <- read.csv("post73_updated.csv",stringsAsFactors = FALSE)
str(prc) #check if structured

#Remove inessential columns - only keep features, classifier column
KeepCols <- c("pca1","pca2","pca3","pca4","Playoffs")
prc <- prc[,KeepCols]
prc

#Rename Classifier column data
table(prc$Playoffs)
prc$Playoffs <- factor(prc$Playoffs, levels = c("1", "0"), labels = c("Make it to Playoffs", "Don't"))
table(prc$Playoffs)

#r=832, k=29

#Normalize all data in Data Frame
normalize <- function(x){
  return( (x - min(x)) / ( max(x) - min(x)) )
}

prc_n <- as.data.frame(lapply(prc[1:4], normalize))
summary(prc_n$shots.attemptec)

#Setting Training and Testing data
prc_train <- prc_n[1:802,]
prc_test <- prc_n[803:832,]

#Including Classifier labels
prc_train_labels <- prc[1:802, 5]
prc_test_labels <- prc[803:832, 5]

#Install and use package 'class' for knn
#install.packages("class")
library(class)

#Apply knn using k=29 and store in prc_test_pred
prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=29)

#Check if values in prc_test_pred matches with prc_test_labels
#install.packages("gmodels")
library(gmodels)

CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq = FALSE)

