library(rpart)
library(gmodels)

#for teams winning over 50% games
data <- read.csv("73-04.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1973:2004)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Over.500 ~ shots.made + shots.attempted + free.throws.made + free.throws.attempted + offensive.rebounds + defensive.rebounds + assists + fouls.commited + steals + turnovers +  blocks + total.points + made.shots.allowed +  shot.attempts.allowed + made.free.throws.against + free.throw.attempts.allowed + offensive.rebounds.allowed + defensive.rebounds.allowed + assists.allowed + fouls.against + steals.against + turnovers.against + blocks.against + points.allowed + pace,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),35]
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
data <- read.csv("73-04.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1973:2004)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Playoffs ~ shots.made + shots.attempted + free.throws.made + free.throws.attempted + offensive.rebounds + defensive.rebounds + assists + fouls.commited + steals + turnovers +  blocks + total.points + made.shots.allowed +  shot.attempts.allowed + made.free.throws.against + free.throw.attempts.allowed + offensive.rebounds.allowed + defensive.rebounds.allowed + assists.allowed + fouls.against + steals.against + turnovers.against + blocks.against + points.allowed + pace,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),37]
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
data <- read.csv("73-04.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1973:2004)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Playoff.Win ~ shots.made + shots.attempted + free.throws.made + free.throws.attempted + offensive.rebounds + defensive.rebounds + assists + fouls.commited + steals + turnovers +  blocks + total.points + made.shots.allowed +  shot.attempts.allowed + made.free.throws.against + free.throw.attempts.allowed + offensive.rebounds.allowed + defensive.rebounds.allowed + assists.allowed + fouls.against + steals.against + turnovers.against + blocks.against + points.allowed + pace,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),38]
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
data <- read.csv("73-04.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1973:2004)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Final.2 ~ shots.made + shots.attempted + free.throws.made + free.throws.attempted + offensive.rebounds + defensive.rebounds + assists + fouls.commited + steals + turnovers +  blocks + total.points + made.shots.allowed +  shot.attempts.allowed + made.free.throws.against + free.throw.attempts.allowed + offensive.rebounds.allowed + defensive.rebounds.allowed + assists.allowed + fouls.against + steals.against + turnovers.against + blocks.against + points.allowed + pace,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),39]
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
data <- read.csv("73-04.csv", header=TRUE);
TP = as.integer(0)
FN = as.integer(0)
FP = as.integer(0)
TN = as.integer(0)
for (i in 1973:2004)
{testing = data[data$year == i,]
training = data[data$year != i,]
tree <- rpart(Champion ~ shots.made + shots.attempted + free.throws.made + free.throws.attempted + offensive.rebounds + defensive.rebounds + assists + fouls.commited + steals + turnovers +  blocks + total.points + made.shots.allowed +  shot.attempts.allowed + made.free.throws.against + free.throw.attempts.allowed + offensive.rebounds.allowed + defensive.rebounds.allowed + assists.allowed + fouls.against + steals.against + turnovers.against + blocks.against + points.allowed + pace,training)
result<-predict(tree,testing,type="class")
data2 <- testing[1:nrow(testing),36]
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
