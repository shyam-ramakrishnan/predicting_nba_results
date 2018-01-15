##Install the package MASS if not already installed
#install.packages("MASS")
require(MASS)

#Read the csv file and select all attributes and measures of goodness from dataset
prcs <- read.csv("46-73_updated.csv",stringsAsFactors = FALSE)
KeepCols <- c("shots.made","shots.attempted","free.throws.made",
              "free.throws.attempted","assists","fouls.committed",
              "points.scored","points.allowed","points.scored","points.allowed",
              "Over.500","Champion","Playoffs","Playoff.Win","Final.2")
prcs <- prcs[,KeepCols]
prcs[1:10,]

#Apply feature selection on all attribuets wrt Playoff.Win goodness measure
#Similarly, apply feature selection for Over.500, Champion, Playoffs, Playoff.Win, Final.2 goodness measures too
prc.mod1 = lm(Playoff.Win ~ shots.made + shots.attempted + free.throws.made
              + free.throws.attempted + assists + fouls.committed
              + points.scored + points.allowed + points.scored + points.allowed, data = prcs)

summary(prc.mod1)

dropterm(prc.mod1, test = "F")

#A table containing features and their F-Values are derived. Here the most significant features are selected.
#After running dropterm for all measures of goodness, we derive:
#Top features - 1. points.allowed 2. points.scored 3. shots.attempted 4. shots.made 5. free.throws.made



