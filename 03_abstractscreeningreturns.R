##########_____Mutualism/Antagonism Synthesis Review_____#########


library(dplyr)
library(operators)


#Analysis of screeners returned databases:
#KJM
MA.fullrec.screendatA.pt1.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt1.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt1.done)
KJM.included<-subset(MA.fullrec.screendatA.pt1.done, decision == "include")
nrow(KJM.included) #31/82 included, 37.80488% inclusion rate
summary(KJM.included)

#UE


#JW


#CM
MA.fullrec.screendatA.pt4.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt4.done.csv", sep = ";", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt4.done)
CM.included<-subset(MA.fullrec.screendatA.pt4.done, decision == "include")
nrow(CM.included) #39/82 included, 47.56098% inclusion rate
summary(CM.included)

#CF


#PT


#MW
MA.fullrec.screendatB.pt2.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatB.pt2.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatB.pt2.done)
MW.included<-subset(MA.fullrec.screendatB.pt2.done, decision == "include")
nrow(MW.included) #42/137 studies included, 30.65693% inclusion rate
summary(MW.included)

#NM