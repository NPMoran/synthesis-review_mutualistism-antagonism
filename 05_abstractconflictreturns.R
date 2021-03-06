##########_____Mutualism/Antagonism Synthesis Review_____#########
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Title: Shifts between cooperation and antagonism driven by individual variation: A systematic synthesis review 


#5. PROCESSING CONFLICT RESOLUTIONS AND PRODUCING FINAL ABSTRACT DECISION DATABASE


library(dplyr); library(operators)


#Analysis of returned conflict resolution databases ----
#KJB
MA.fullrec.conflictres.KJB.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.KJB.done.csv", strip.white = TRUE)
nrow(subset(MA.fullrec.conflictres.KJB.done, decision == "include")) #8 more included

#UE
MA.fullrec.conflictres.UE.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.UE.done.csv", strip.white = TRUE)
MA.fullrec.conflictres.UE.done.no <- subset(MA.fullrec.conflictres.UE.done, decision == "no"); MA.fullrec.conflictres.UE.done.no$decision <- "exclude" #
MA.fullrec.conflictres.UE.done.maybe <- subset(MA.fullrec.conflictres.UE.done, decision == "maybe"); MA.fullrec.conflictres.UE.done.maybe$decision <- "include"
MA.fullrec.conflictres.UE.done.yes <- subset(MA.fullrec.conflictres.UE.done, decision == "yes"); MA.fullrec.conflictres.UE.done.yes$decision <- "include"
MA.fullrec.conflictres.UE.done <- rbind(MA.fullrec.conflictres.UE.done.no, MA.fullrec.conflictres.UE.done.maybe, MA.fullrec.conflictres.UE.done.yes)
nrow(MA.fullrec.conflictres.UE.done)
nrow(subset(MA.fullrec.conflictres.UE.done, decision == "include")) #6 more included

#JW
MA.fullrec.conflictres.JW.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.JW.done.csv", strip.white = TRUE)
nrow(subset(MA.fullrec.conflictres.JW.done, decision == "include")) #12 more included

#CM
MA.fullrec.conflictres.CM.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.CM.done.csv")
nrow(subset(MA.fullrec.conflictres.CM.done, decision == "include")) #9 more included

#CF
MA.fullrec.conflictres.CF.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.CF.done.csv")
nrow(subset(MA.fullrec.conflictres.CF.done, decision == "include")) #9 more included

#PT
MA.fullrec.conflictres.PT.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.PT.done.csv", strip.white = TRUE)
nrow(subset(MA.fullrec.conflictres.PT.done, decision == "include")) #5 more included

#MW
MA.fullrec.conflictres.MW.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.MW.done.csv", strip.white = TRUE)
nrow(subset(MA.fullrec.conflictres.MW.done, decision == "include")) #6 more included

#NM
MA.fullrec.conflictres.NM.done <- read.csv("./MA_screening_returns/MA.fullrec.conflictres.NM.done.csv", strip.white = TRUE)
nrow(subset(MA.fullrec.conflictres.NM.done, decision == "include")) #9 more included


#Final abstract decision ---- 
MA.fullrec.conflictres.CF.done$notes <- "" #to account for screeners who added their own notes column
MA.fullrec.conflictres.JW.done$notes <- ""
MA.fullrec.conflictres.CM.done$notes <- ""
MA.fullrec.conflictres.PT.done$notes <- ""
MA.fullrec.conflictres.MW.done$notes <- ""
MA.fullrec.conflictres.NM.done$notes <- ""

screendatconflicts.done <- rbind(MA.fullrec.conflictres.KJB.done,
                                 MA.fullrec.conflictres.UE.done,
                                 MA.fullrec.conflictres.JW.done,
                                 MA.fullrec.conflictres.CM.done,
                                 MA.fullrec.conflictres.CF.done,
                                 MA.fullrec.conflictres.PT.done,
                                 MA.fullrec.conflictres.MW.done,
                                 MA.fullrec.conflictres.NM.done)
nrow(screendatconflicts.done) #140 conflicts resolved
nrow(subset(screendatconflicts.done, decision == "include")) #64 now included, 45.71429% inclusion rate

conflict.identification <- read.csv("MA.abstractdecision_first.csv")
MA.abstractscreeningfull <- merge(conflict.identification, screendatconflicts.done, by = "abstract.id", all.x = TRUE)

abstractsincluded1 <- subset(MA.abstractscreeningfull, decision.x == "include")
abstractsincluded1 <- subset(abstractsincluded1, decision.y == "include")
abstractsincluded2 <- subset(MA.abstractscreeningfull, decision == "include")
abstractsincluded <- rbind(abstractsincluded1, abstractsincluded2)
labels(abstractsincluded)
abstractsincluded <- select(abstractsincluded, -c(X, title.x, abstract.x, title.y, abstract.y, title, abstract))
#renaming variables by screening stage. 
abstractsincluded <- rename(abstractsincluded, screener.id.A = screener.id.x)
abstractsincluded <- rename(abstractsincluded, decision.A = decision.x)
abstractsincluded <- rename(abstractsincluded, scale.A = scale.x)
abstractsincluded <- rename(abstractsincluded, topic.A = topic.x)
abstractsincluded <- rename(abstractsincluded, notes.A = notes.x)
abstractsincluded <- rename(abstractsincluded, screener.id.B = screener.id.y)
abstractsincluded <- rename(abstractsincluded, decision.B = decision.y)
abstractsincluded <- rename(abstractsincluded, scale.B = scale.y)
abstractsincluded <- rename(abstractsincluded, topic.B = topic.y)
abstractsincluded <- rename(abstractsincluded, notes.B = notes.y)
abstractsincluded <- rename(abstractsincluded, screener.id.C = screener.id)
abstractsincluded <- rename(abstractsincluded, decision.C = decision)
abstractsincluded <- rename(abstractsincluded, scale.C = scale)
abstractsincluded <- rename(abstractsincluded, topic.C = topic)
abstractsincluded <- rename(abstractsincluded, notes.C = notes)

MA.fullrec.final <- read.csv("MA.fullrec.final.csv")
labels(MA.fullrec.final)
MA.fullrec.final <- select(MA.fullrec.final, -c(X, label, n_duplicates))

abstractsincluded <- merge(MA.fullrec.final, abstractsincluded, by = "abstract.id", all.x = FALSE)
labels(abstractsincluded)
nrow(abstractsincluded)

write.csv(MA.abstractscreeningfull, "MA.abstractdecision_final.csv")
write.csv(abstractsincluded, "MA.abstractdecision_included.csv")


#Note: Database of final abstract decisions is also available as MA.abstractdecision_final_MASTER.xlsx, https://osf.io/9kfpc/ under 'OSF Storage/Screening Databases'


