library(rpart)
library(gmodels)

#for teams winning over 50% games
data <- read.csv("57-72.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1946:1972)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Over.500 ~ shots.made.100 + shots.attempted.100 + free.throws.made.100 + free.throws.attempted.100 + assists.100 + fouls.commited.100 + points.scored.100 + points.allowed.100,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),15]
y <- CrossTable(x=data2,y=result,prop.chisq=FALSE)
if(ncol(y$t) == 2){
TP = zero + y$t[2,2]
FN = one + y$t[2,1]
FP = two + y$t[1,2]
TN = three + y$t[1,1]}
else{
  TP = one + y$t[2,1]
  TN = three + y$t[1,1]
}
}
TP 
FN
FP 
TN

#for teams who qualify to the playoffs
data <- read.csv("57-72.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1946:1972)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Playoffs ~ shots.made.100 + shots.attempted.100 + free.throws.made.100 + free.throws.attempted.100 + assists.100 + fouls.commited.100 + points.scored.100 + points.allowed.100,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),26]
y <- CrossTable(x=data2,y=result,prop.chisq=FALSE)
if(ncol(y$t) == 2){
TP = zero + y$t[2,2]
FN = one + y$t[2,1]
FP = two + y$t[1,2]
TN = three + y$t[1,1]}
else{
  TP = one + y$t[2,1]
  TN = three + y$t[1,1]
}
}
TP 
FN
FP 
TN

#for teams who win in the playoffs
data <- read.csv("57-72.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1946:1972)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Playoff.Win ~ shots.made.100 + shots.attempted.100 + free.throws.made.100 + free.throws.attempted.100 + assists.100 + fouls.commited.100 + points.scored.100 + points.allowed.100,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),27]
y <- CrossTable(x=data2,y=result,prop.chisq=FALSE)
if(ncol(y$t) == 2){
TP = zero + y$t[2,2]
FN = one + y$t[2,1]
FP = two + y$t[1,2]
TN = three + y$t[1,1]}
else{
  TP = one + y$t[2,1]
  TN = three + y$t[1,1]
}
}
TP 
FN
FP 
TN

#for top 2 teams
data <- read.csv("57-72.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1946:1972)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Final.2 ~ shots.made.100 + shots.attempted.100 + free.throws.made.100 + free.throws.attempted.100 + assists.100 + fouls.commited.100 + points.scored.100 + points.allowed.100,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),28]
y <- CrossTable(x=data2,y=result,prop.chisq=FALSE)
if(ncol(y$t) == 2){
TP = zero + y$t[2,2]
FN = one + y$t[2,1]
FP = two + y$t[1,2]
TN = three + y$t[1,1]}
else{
  TP = one + y$t[2,1]
  TN = three + y$t[1,1]
}
}
TP 
FN
FP 
TN


#for Champion
data <- read.csv("57-72.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1946:1972)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Champion ~ shots.made.100 + shots.attempted.100 + free.throws.made.100 + free.throws.attempted.100 + assists.100 + fouls.commited.100 + points.scored.100 + points.allowed.100,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),16]
y <- CrossTable(x=data2,y=result,prop.chisq=FALSE)
if(ncol(y$t) == 2){
TP = zero + y$t[2,2]
FN = one + y$t[2,1]
FP = two + y$t[1,2]
TN = three + y$t[1,1]}
else{
  TP = one + y$t[2,1]
  TN = three + y$t[1,1]
}
}
TP 
FN
FP 
TN
