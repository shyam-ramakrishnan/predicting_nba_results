#Read data from CSV and store in Data Frame
prc <- read.csv("post73_updated.csv",stringsAsFactors = FALSE)
str(prc) #check if structured

#Remove inessential columns - only keep features, classifier column
KeepCols <- c("year","shots.made","shots.attemptec","free.throws.made",
              "free.throws.attempted","offensive.rebounds","defensive.rebounds","total.rebounds","assists",
              "fouls.committed","steals","turnovers","blocks","total.points","made.shots.allowed","shot.attempts.allowed",
              "made.free.throws.against","free.throw.attempts.allowed","offensive.rebounds.allowed","defensive.rebounds.allowed",
              "total.rebounds.allowed","assists.allowed","fouls.against","steals.against","turnovers.against","blocks.against",
              "X3.pointers.allowed","points.allowed","Over.500")
prc <- prc[,KeepCols]
prc

#Rename Classifier column data
table(prc$Over.500)
prc$Over.500 <- factor(prc$Over.500, levels = c("1", "0"), labels = c("More than 50% wins", "Less than 50% wins"))
table(prc$Over.500)

#r=832, k=29

#Normalize all data in Data Frame
normalize <- function(x){
  return( (x - min(x)) / ( max(x) - min(x)) )
}

prc_n <- as.data.frame(lapply(prc[2:28], normalize))
summary(prc_n$shots.attemptec)

zero = as.integer(0)
one = as.integer(0)
two = as.integer(0)
three = as.integer(0)

for (i in 1973:2004)
{
  prc_n <- cbind(prc_n,prc$year)
  prc_test = prc_n[prc_n$year == i,]
  prc_train = prc_n[prc_n$year != i,]
  prc_test = prc_test[,1:27]
  prc_train = prc_train[,1:27]
  prc_n = prc_n[,1:27]
  prc = prc[,2:29]}


#Setting Training and Testing data
prc_train <- prc_n[1:802,]
prc_test <- prc_n[803:832,]

#Including Over.500 column which was left out in normalized data
prc_train_labels <- prc[1:802, 28]
prc_test_labels <- prc[803:832, 28]

#Install and use package 'class' for knn
#install.packages("class")
library(class)

#Apply knn using k=29 and store in prc_test_pred
prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=29)

#Check if values in prc_test_pred matches with prc_test_labels
#install.packages("gmodels")
library(gmodels)

y <- CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq = FALSE)

if(ncol(y$t) == 2){
  zero = (y$t[2,2]/30)*832
  one = (y$t[2,1]/30)*832
  two = (y$t[1,2]/30)*832
  three = (y$t[1,1]/30)*832}else{
    one = (y$t[2,1]/30)*832
    three = (y$t[1,1]/30)*832
  }

zero 
one
two 
three