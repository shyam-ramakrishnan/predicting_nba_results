#Read data from CSV and store in Data Frame
prc <- read.csv("57-73_updated.csv",stringsAsFactors = FALSE)
str(prc) #check if structured

#Remove inessential columns - only keep features, classifier column
KeepCols <- c("year","shots.made","shots.attempted","free.throws.made",
              "free.throws.attempted","assists","fouls.committed",
              "points.scored","points.allowed",
              "Over.500")
prc <- prc[,KeepCols]
prc

#See Classifier column data
table(prc$Over.500)
prc$Over.500 <- factor(prc$Over.500, levels = c("1","0"), labels = c("More than 50% Wins", "Less than 50% WIns"))
table(prc$Over.500)

#r=251, k=16

#Normalize all data in Data Frame
normalize <- function(x){
  return( (x - min(x)) / ( max(x) - min(x)) )
}

prc_n <- as.data.frame(lapply(prc[2:9], normalize))
summary(prc_n$shots.attempted)

zero = as.integer(0)
one = as.integer(0)
two = as.integer(0)
three = as.integer(0)

for (i in 1957:1972)
{
  prc_n <- cbind(prc_n,prc$year)
  prc_test = prc_n[prc_n$year == i,]
  prc_train = prc_n[prc_n$year != i,]
  prc_test = prc_test[,1:8]
  prc_train = prc_train[,1:8]
  prc_n = prc_n[,1:8]
  prc = prc[,2:10]}

#Setting Training and Testing data
prc_train <- prc_n[1:216,]
prc_test <- prc_n[217:243,]

#Including Classifier labels
prc_train_labels <- prc[1:216, 9]
prc_test_labels <- prc[217:243, 9]


#Install and use package 'class' for knn
#install.packages("class")
library(class)

#Apply knn using k=19 and store in prc_test_pred
prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=19)

#Check if values in prc_test_pred matches with prc_test_labels
#install.packages("gmodels")
library(gmodels)

y <- CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq = FALSE)

if(ncol(y$t) == 2){
  zero = (y$t[2,2]/27)*251
  one = (y$t[2,1]/27)*251
  two = (y$t[1,2]/27)*251
  three = (y$t[1,1]/27)*251}else{
    one = (y$t[2,1]/27)*251
    three = (y$t[1,1]/27)*251
  }

zero 
one
two 
three
