##########_____Mutualism/Antagonism Synthesis Review_____#########


library(dplyr)
library(operators)


#Analysis of screeners returned databases:
#KJM
MA.fullrec.screendatA.pt1.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt1.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt1.done)
nrow(subset(MA.fullrec.screendatA.pt1.done, decision == "include")) #31/82 included, 37.80488% inclusion rate
summary(subset(MA.fullrec.screendatA.pt1.done, decision == "include"))

#UE
MA.fullrec.screendatA.pt2.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt2.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt2.done)
nrow(subset(MA.fullrec.screendatA.pt2.done, decision == "include")) #TO BE COMPLETED
summary(subset(MA.fullrec.screendatA.pt2.done, decision == "include"))

#JW
MA.fullrec.screendatA.pt3.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt3.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt3.done)
nrow(subset(MA.fullrec.screendatA.pt3.done, decision == "include")) #63/82 included, 76.82927% inclusion rate
summary(subset(MA.fullrec.screendatA.pt3.done, decision == "include"))

#CM
MA.fullrec.screendatA.pt4.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt4.done.csv", sep = ";", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt4.done)
nrow(subset(MA.fullrec.screendatA.pt4.done, decision == "include")) #39/82 included, 47.56098% inclusion rate
summary(subset(MA.fullrec.screendatA.pt4.done, decision == "include"))

#CF
MA.fullrec.screendatA.pt5.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt5.done.csv", sep = ";", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt5.done)
nrow(subset(MA.fullrec.screendatA.pt5.done, decision == "include")) #40/83 included, 48.19277% inclusion rate
summary(subset(MA.fullrec.screendatA.pt5.done, decision == "include"))

#PT
MA.fullrec.screendatB.pt1.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatB.pt1.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatB.pt1.done)
nrow(subset(MA.fullrec.screendatB.pt1.done, decision == "include")) #74/137 studies included, 54.0146% inclusion rate
summary(subset(MA.fullrec.screendatB.pt1.done, decision == "include"))

#MW
MA.fullrec.screendatB.pt2.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatB.pt2.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatB.pt2.done)
nrow(subset(MA.fullrec.screendatB.pt2.done, decision == "include")) #42/137 studies included, 30.65693% inclusion rate
summary(subset(MA.fullrec.screendatB.pt2.done, decision == "include"))

#NM
MA.fullrec.screendatB.pt3.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatB.pt3.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatB.pt3.done)
nrow(subset(MA.fullrec.screendatB.pt3.done, decision == "include")) #78/137 studies included, 56.93431% inclusion rate
summary(subset(MA.fullrec.screendatB.pt3.done, decision == "include"))


#Conflict Identification
MA.fullrec.screendatA.pt2.done$notes <- ""
MA.fullrec.screendatA.pt3.done$notes <- ""
MA.fullrec.screendatA.pt5.done$notes <- ""
screendatA.done <- rbind(MA.fullrec.screendatA.pt1.done,MA.fullrec.screendatA.pt2.done,MA.fullrec.screendatA.pt3.done,MA.fullrec.screendatA.pt4.done,MA.fullrec.screendatA.pt5.done)
labels(screendatA.done)

MA.fullrec.screendatB.pt2.done$notes <- ""
MA.fullrec.screendatB.pt3.done$notes <- ""
screendatB.done <- rbind(MA.fullrec.screendatB.pt1.done,MA.fullrec.screendatB.pt2.done,MA.fullrec.screendatB.pt3.done)
labels(screendatB.done)

conflict.identification <- merge(screendatA.done, screendatB.done, by = "abstract.id", all.x = TRUE)
labels(conflict.identification)
summary(conflict.identification)

#Excluded records
excluded.B <- subset(conflict.identification, decision.y == "exclude")
excluded.both <- subset(excluded.B, decision.x == "exclude")
nrow(excluded.both) #110 records excluded so far

#Included records
included.B <- subset(conflict.identification, decision.y == "include")
included.both <- subset(included.B, decision.x == "include")
nrow(included.both) #108 records excluded so far

#Conflicting decisions
conflict.1 <- subset(excluded.B, decision.x != "exclude")
conflict.2 <- subset(included.B, decision.x != "include")

conflicts <- rbind(conflict.1, conflict.2)
nrow(conflicts) #111 conflicts

write.csv(conflicts, "MA.conflicts.csv")

