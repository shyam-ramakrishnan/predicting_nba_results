#Read data from CSV and store in Data Frame
prc <- read.csv("46-73_updated.csv",stringsAsFactors = FALSE)
str(prc) #check if structured

#Remove inessential columns - only keep features, classifier column
KeepCols <- c("pca1","pca2","Champion")
prc <- prc[,KeepCols]
prc

#Rename Classifier column data
table(prc$Champion)
prc$Champion <- factor(prc$Champion, levels = c("1", "0"), labels = c("Champions", "Not Champions"))
table(prc$Champion)

#Number of instances=356, optimal value taken k=19

#Normalize all data in Data Frame
normalize <- function(x){
  return( (x - min(x)) / ( max(x) - min(x)) )
}

prc_n <- as.data.frame(lapply(prc[1:2], normalize))
summary(prc_n$shots.attempted)

#Setting Training and Testing data
prc_train <- prc_n[1:328,]
prc_test <- prc_n[329:355,]

#Including Classifier labels
prc_train_labels <- prc[1:328, 3]
prc_test_labels <- prc[329:355, 3]

#Install and use package 'class' for knn
#install.packages("class")
library(class)

#Apply knn using k=19 and store in prc_test_pred
prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=19)

#Check if values in prc_test_pred matches with prc_test_labels
#install.packages("gmodels")
library(gmodels)

CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq = FALSE)

