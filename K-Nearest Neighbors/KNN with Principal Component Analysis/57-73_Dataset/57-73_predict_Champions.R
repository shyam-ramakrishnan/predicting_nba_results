#Read data from CSV and store in Data Frame
prc <- read.csv("57-73_updated.csv",stringsAsFactors = FALSE)
str(prc) #check if structured

#Remove inessential columns - only keep features, classifier column
KeepCols <- c("pca1","pca2","Champion")
prc <- prc[,KeepCols]
prc

#Rename Classifier column data
table(prc$Champion)
prc$Champion <- factor(prc$Champion, levels = c("1", "0"), labels = c("Champions", "Not Champions"))
table(prc$Champion)

#r=256, k=16

#Normalize all data in Data Frame
normalize <- function(x){
  return( (x - min(x)) / ( max(x) - min(x)) )
}

prc_n <- as.data.frame(lapply(prc[1:2], normalize))
summary(prc_n$shots.attempted)

#Setting Training and Testing data
prc_train <- prc_n[1:216,]
prc_test <- prc_n[217:243,]

#Including Classifier labels
prc_train_labels <- prc[1:216, 3]
prc_test_labels <- prc[217:243, 3]

#Install and use package 'class' for knn
#install.packages("class")
library(class)

#Apply knn using k=16 and store in prc_test_pred
prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=16)

#Check if values in prc_test_pred matches with prc_test_labels
#install.packages("gmodels")
library(gmodels)

CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq = FALSE)

