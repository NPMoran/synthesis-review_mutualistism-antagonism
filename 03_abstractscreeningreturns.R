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


#Conflict analysis

screendatB.done <- rbind(MA.fullrec.screendatB.pt1.done,MA.fullrec.screendatB.pt2.done,MA.fullrec.screendatB.pt3.done)

MA.fullrec.screendatB.pt2.done$notes <- ""
MA.fullrec.screendatB.pt3.done$notes <- ""

screendatB.done <- rbind(MA.fullrec.screendatB.pt1.done,MA.fullrec.screendatB.pt2.done,MA.fullrec.screendatB.pt3.done)
summary(screendatB.done) #Screeners B found 194 potentially relevant studies
