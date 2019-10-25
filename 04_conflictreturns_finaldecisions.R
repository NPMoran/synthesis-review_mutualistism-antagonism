##########_____Mutualism/Antagonism Synthesis Review_____#########


library(dplyr); library(operators)


#Analysis of returned conflict resolution databases ----
#KJM
#MA.fullrec.screendatA.pt1.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt1.done.csv", strip.white = TRUE)
#nrow(subset(MA.fullrec.screendatA.pt1.done, decision == "include")) #

#UE
MA.fullrec.conflictres.UE.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.UE.done.csv", strip.white = TRUE)
MA.fullrec.conflictres.UE.done.no <- subset(MA.fullrec.conflictres.UE.done, decision == "no"); MA.fullrec.conflictres.UE.done.no$decision <- "exclude" #
MA.fullrec.conflictres.UE.done.maybe <- subset(MA.fullrec.conflictres.UE.done, decision == "maybe"); MA.fullrec.conflictres.UE.done.maybe$decision <- "include"
MA.fullrec.conflictres.UE.done.yes <- subset(MA.fullrec.conflictres.UE.done, decision == "yes"); MA.fullrec.conflictres.UE.done.yes$decision <- "include"
MA.fullrec.conflictres.UE.done <- rbind(MA.fullrec.conflictres.UE.done.no, MA.fullrec.conflictres.UE.done.maybe, MA.fullrec.conflictres.UE.done.yes)
nrow(MA.fullrec.conflictres.UE.done)
nrow(subset(MA.fullrec.conflictres.UE.done, decision == "include")) #6 more included

##JW
#MA.fullrec.screendatA.pt3.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt3.done.csv", strip.white = TRUE)
#nrow(subset(MA.fullrec.screendatA.pt3.done, decision == "include")) #

##CM
#MA.fullrec.screendatA.pt4.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt4.done.csv", sep = ";", strip.white = TRUE)
#nrow(subset(MA.fullrec.screendatA.pt4.done, decision == "include")) #

##CF
MA.fullrec.conflictres.CF.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.CF.done.csv")
nrow(subset(MA.fullrec.conflictres.CF.done, decision == "include")) #9 more included
#
##PT
MA.fullrec.conflictres.PT.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.PT.done.csv", strip.white = TRUE)
nrow(subset(MA.fullrec.conflictres.PT.done, decision == "include")) #5 more included
#
##MW
MA.fullrec.conflictres.MW.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.MW.done.csv", strip.white = TRUE)
nrow(subset(MA.fullrec.conflictres.MW.done, decision == "include")) #6 more included

##NM
#MA.fullrec.screendatB.pt3.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatB.pt3.done.csv", strip.white = TRUE)
#nrow(subset(MA.fullrec.screendatB.pt3.done, decision == "include")) #



#Final abstract decision ----



