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
MA.fullrec.screendatA.pt5.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt5.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt5.done)
nrow(subset(MA.fullrec.screendatA.pt5.done, decision == "include")) #TO BE COMPLETED
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


#A Screeners
MA.fullrec.screendatA.pt2.done$notes <- ""
MA.fullrec.screendatA.pt3.done$notes <- ""
MA.fullrec.screendatA.pt5.done$notes <- ""


screendatA.done <- rbind(MA.fullrec.screendatA.pt1.done,MA.fullrec.screendatA.pt2.done,MA.fullrec.screendatA.pt3.done,MA.fullrec.screendatA.pt4.done,MA.fullrec.screendatA.pt5.done)
screendatA.done <- rename(screendatA.done, decisionA = decision)
screendatA.done <- rename(screendatA.done, screener.A = screener.id)
screendatA.done <- rename(screendatA.done, scaleA = scale)
screendatA.done <- rename(screendatA.done, topicA = topic)
screendatA.done <- rename(screendatA.done, notesA = notes)
labels(screendatA.done)

#B Screeners
MA.fullrec.screendatB.pt2.done$notes <- ""
MA.fullrec.screendatB.pt3.done$notes <- ""

screendatB.done <- rbind(MA.fullrec.screendatB.pt1.done,MA.fullrec.screendatB.pt2.done,MA.fullrec.screendatB.pt3.done)
screendatB.done <- rename(screendatB.done, decisionB = decision)
screendatB.done <- rename(screendatB.done, screener.B = screener.id)
screendatB.done <- rename(screendatB.done, scaleB = scale)
screendatB.done <- rename(screendatB.done, topicB = topic)
screendatB.done <- rename(screendatB.done, notesB = notes)
labels(screendatB.done)

screendatA.done.reduced <- select(screendatA.done, "abstract.id", "screener.A", "decisionA", "scaleA", "topicA", "notesA")

conflict.identification <- merge(screendatA.done.reduced, screendatB.done, by = "abstract.id", all.x = TRUE)
write.csv(conflict.identification, "conflicts.csv")
