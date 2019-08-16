##########_____Mutualism/Antagonism Synthesis Review_____#########


library(dplyr)
library(operators)


#Analysis of screeners returned databases:
#KJM
MA.fullrec.screendatA.pt1.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt1.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt1.done)
KJM.included<-subset(MA.fullrec.screendatA.pt1.done, decision == "include")
summary(KJM.included)
KJM.included$topic
