##Install the package MASS if not already installed
#install.packages("MASS")
require(MASS)

#Read the csv file and select all attributes and measures of goodness from dataset
prcs <- read.csv("post73_updated.csv",stringsAsFactors = FALSE)
KeepCols <- c("shots.made","shots.attemptec","free.throws.made",
              "free.throws.attempted","offensive.rebounds","defensive.rebounds","total.rebounds","assists",
              "fouls.committed","steals","turnovers","blocks","total.points","made.shots.allowed","shot.attempts.allowed",
              "made.free.throws.against","free.throw.attempts.allowed","offensive.rebounds.allowed","defensive.rebounds.allowed",
              "total.rebounds.allowed","assists.allowed","fouls.against","steals.against","turnovers.against","blocks.against",
              "X3.pointers.allowed","points.allowed","Over.500","Champion","Playoffs","Playoff.Win","Final.2")
prcs <- prcs[,KeepCols]
prcs[1:10,]

#Apply feature selection on all attribuets wrt Champion goodness measure
#Similarly, apply feature selection for Over.500, Champion, Playoffs, Playoff.Win, Final.2 goodness measures too

prc.mod1 = lm(Champion ~ shots.made + shots.attemptec + free.throws.made + free.throws.attempted + offensive.rebounds 
              + defensive.rebounds + total.rebounds + assists + fouls.committed + steals + turnovers + blocks + total.points 
              + made.shots.allowed + shot.attempts.allowed + made.free.throws.against + free.throw.attempts.allowed + offensive.rebounds.allowed
              + defensive.rebounds.allowed + total.rebounds.allowed + assists.allowed + fouls.against + steals.against + turnovers.against 
              + blocks.against + X3.pointers.allowed + points.allowed, data = prcs)


summary(prc.mod1)

dropterm(prc.mod1, test = "F")

#A table containing features and their F-Values are derived. Here the most significant features are selected.
#After running dropterm for all measures of goodness, we derive:
#Top features:  1.total.points 2.assists 3.shot.attempts.allowed 4.free.throws.made 5.made.shots.allowed

