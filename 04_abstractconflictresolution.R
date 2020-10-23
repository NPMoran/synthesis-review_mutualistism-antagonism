##########_____Mutualism/Antagonism Synthesis Review_____#########
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Title: Movement between cooperation and antagonism driven by individual variation: A systematic synthesis review 


#4. PRODUCING DATABASES FOR RESOLUTION OF CONFLICT BY THIRD REVIEWER


library(dplyr); library(operators)


#Conflict resolution allocations ----
conflicts <- read.csv("MA.abstractconflicts.csv", strip.white = TRUE)

#Due to the high amount of conflicts, these were randomly allocated conflicts to a third reviewer to make a final decision
#For 8 screener; 4x 18 records, 4x 17 records.
labels(conflicts)
conflicts$title <- conflicts$title.y
conflicts$abstract <- conflicts$abstract.y
conflicts.allocation <- select(conflicts, -c(X, title.x, abstract.x, decision.x, scale.x, topic.x, notes.x, title.y, abstract.y, decision.y, scale.y, topic.y, notes.y)) #cleaning up old variables
summary(as.factor(conflicts.allocation$screener.id.x)); summary(as.factor(conflicts.allocation$screener.id.y))
#Screeners involved in most conflicts: PT(48) NM(47) MW(45) JW(42) UE(29) CF(26) KJB(22) CM(21)

set.seed(8688) #set.seed was set as a value that allocated all abstracts to screeners that had not previously screened that abstract

#Conflict resolution allocation for PT
conflicts.allocation.noPT <- subset(conflicts.allocation, screener.id.y != "PT")
conflicts.allocation.wiPT <- subset(conflicts.allocation, screener.id.y == "PT")
nrow(conflicts.allocation.noPT); nrow(conflicts.allocation.wiPT)
MA.fullrec.conflictres.PT <- sample_n(conflicts.allocation.noPT, 18, replace = FALSE, weight = NULL)
conflicts.allocation <- subset(conflicts.reduced, abstract.id %!in% MA.fullrec.conflictres.PT$abstract.id)
nrow(conflicts.allocation)

#Conflict resolution allocation for NM
conflicts.allocation.noNM <- subset(conflicts.allocation, screener.id.y != "NM")
conflicts.allocation.wiNM <- subset(conflicts.allocation, screener.id.y == "NM")
nrow(conflicts.allocation.noNM); nrow(conflicts.allocation.wiNM)
MA.fullrec.conflictres.NM <- sample_n(conflicts.allocation.noNM, 18, replace = FALSE, weight = NULL)
conflicts.allocation <- subset(conflicts.allocation, abstract.id %!in% MA.fullrec.conflictres.NM$abstract.id)
nrow(conflicts.allocation)

#Conflict resolution allocation for MW
conflicts.allocation.noMW <- subset(conflicts.allocation, screener.id.y != "MW")
conflicts.allocation.wiMW <- subset(conflicts.allocation, screener.id.y == "MW")
nrow(conflicts.allocation.noMW); nrow(conflicts.allocation.wiMW)
MA.fullrec.conflictres.MW <- sample_n(conflicts.allocation.noMW, 18, replace = FALSE, weight = NULL)
conflicts.allocation <- subset(conflicts.allocation, abstract.id %!in% MA.fullrec.conflictres.MW$abstract.id)
nrow(conflicts.allocation)

#Conflict resolution allocation for JW
conflicts.allocation.noJW <- subset(conflicts.allocation, screener.id.x != "JW")
conflicts.allocation.wiJW <- subset(conflicts.allocation, screener.id.x == "JW")
nrow(conflicts.allocation.noJW); nrow(conflicts.allocation.wiJW)
MA.fullrec.conflictres.JW <- sample_n(conflicts.allocation.noJW, 18, replace = FALSE, weight = NULL)
conflicts.allocation <- subset(conflicts.allocation, abstract.id %!in% MA.fullrec.conflictres.JW$abstract.id)
nrow(conflicts.allocation)

#Conflict resolution allocation for UE
conflicts.allocation.noUE <- subset(conflicts.allocation, screener.id.x != "UE")
conflicts.allocation.wiUE <- subset(conflicts.allocation, screener.id.x == "UE")
nrow(conflicts.allocation.noUE); nrow(conflicts.allocation.wiUE)
MA.fullrec.conflictres.UE <- sample_n(conflicts.allocation.noUE, 17, replace = FALSE, weight = NULL)
conflicts.allocation <- subset(conflicts.allocation, abstract.id %!in% MA.fullrec.conflictres.UE$abstract.id)
nrow(conflicts.allocation)

#Conflict resolution allocation for CF
conflicts.allocation.noCF <- subset(conflicts.allocation, screener.id.x != "CF")
conflicts.allocation.wiCF <- subset(conflicts.allocation, screener.id.x == "CF")
nrow(conflicts.allocation.noCF); nrow(conflicts.allocation.wiCF)
MA.fullrec.conflictres.CF <- sample_n(conflicts.allocation.noCF, 17, replace = FALSE, weight = NULL)
conflicts.allocation <- subset(conflicts.allocation, abstract.id %!in% MA.fullrec.conflictres.CF$abstract.id)
nrow(conflicts.allocation)

#Conflict resolution allocation for KJB
conflicts.allocation.noKJB <- subset(conflicts.allocation, screener.id.x != "KJB")
conflicts.allocation.wiKJB <- subset(conflicts.allocation, screener.id.x == "KJB")
nrow(conflicts.allocation.noKJB); nrow(conflicts.allocation.wiKJB)
MA.fullrec.conflictres.KJB <- sample_n(conflicts.allocation.noKJB, 17, replace = FALSE, weight = NULL)
conflicts.allocation <- subset(conflicts.allocation, abstract.id %!in% MA.fullrec.conflictres.KJB$abstract.id)
nrow(conflicts.allocation)

#Conflict resolution allocation for CM
nrow(subset(conflicts.allocation, screener.id.x != "CM"))
MA.fullrec.conflictres.CM <- conflicts.allocation
conflicts.allocation <- subset(conflicts.allocation, abstract.id %!in% MA.fullrec.conflictres.CM$abstract.id)
nrow(conflicts.allocation) #all conflicts allocated to a independent screener


#checking the allocation worked
nCow(MA.fullrec.conflictres.PT); nrow(MA.fullrec.conflictres.NM); nrow(MA.fullrec.conflictres.MW); nrow(MA.fullrec.conflictres.JW)
nrow(MA.fullrec.conflictres.UE); nrow(MA.fullrec.conflictres.CF); nrow(MA.fullrec.conflictres.KJB); nrow(MA.fullrec.conflictres.CM)
check <- rbind(MA.fullrec.conflictres.PT, MA.fullrec.conflictres.NM, MA.fullrec.conflictres.MW, MA.fullrec.conflictres.JW, MA.fullrec.conflictres.UE, MA.fullrec.conflictres.CF, MA.fullrec.conflictres.KJB, MA.fullrec.conflictres.CM)
setdiff(check, conflicts.reduced)
intersect(check, conflicts.reduced) #all abstracts allocated, no duplicates 


#Formatting datasets for third screener ----
labels(MA.fullrec.conflictres.PT)
MA.fullrec.conflictres.PT <- select(MA.fullrec.conflictres.PT, -c(screener.id.x, screener.id.y))
MA.fullrec.conflictres.NM <- select(MA.fullrec.conflictres.NM, -c(screener.id.x, screener.id.y))
MA.fullrec.conflictres.MW <- select(MA.fullrec.conflictres.MW, -c(screener.id.x, screener.id.y))
MA.fullrec.conflictres.JW <- select(MA.fullrec.conflictres.JW, -c(screener.id.x, screener.id.y))
MA.fullrec.conflictres.UE <- select(MA.fullrec.conflictres.UE, -c(screener.id.x, screener.id.y))
MA.fullrec.conflictres.CF <- select(MA.fullrec.conflictres.CF, -c(screener.id.x, screener.id.y))
MA.fullrec.conflictres.KJB <- select(MA.fullrec.conflictres.KJB, -c(screener.id.x, screener.id.y))
MA.fullrec.conflictres.CM <- select(MA.fullrec.conflictres.CM, -c(screener.id.x, screener.id.y))
MA.fullrec.conflictres.PT$screener.id <- "PT"
MA.fullrec.conflictres.NM$screener.id <- "NM"
MA.fullrec.conflictres.MW$screener.id <- "MW"
MA.fullrec.conflictres.JW$screener.id <- "JW"
MA.fullrec.conflictres.UE$screener.id <- "UE"
MA.fullrec.conflictres.CF$screener.id <- "CF"
MA.fullrec.conflictres.KJB$screener.id <- "KJB"
MA.fullrec.conflictres.CM$screener.id <- "CM"
MA.fullrec.conflictres.PT$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.conflictres.NM$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.conflictres.MW$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.conflictres.JW$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.conflictres.UE$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.conflictres.CF$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.conflictres.KJB$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.conflictres.CM$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.conflictres.PT$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.conflictres.NM$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.conflictres.MW$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.conflictres.JW$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.conflictres.UE$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.conflictres.CF$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.conflictres.KJB$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.conflictres.CM$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.conflictres.PT$"topic area (e.g. predator-prey, parasite-host, male-female, theoretical/modelling, review)" <- ""
MA.fullrec.conflictres.NM$"topic area (e.g. predator-prey, parasite-host, male-female, theoretical/modelling, review)" <- ""
MA.fullrec.conflictres.MW$"topic area (e.g. predator-prey, parasite-host, male-female, theoretical/modelling, review)" <- ""
MA.fullrec.conflictres.JW$"topic area (e.g. predator-prey, parasite-host, male-female, theoretical/modelling, review)" <- ""
MA.fullrec.conflictres.UE$"topic area (e.g. predator-prey, parasite-host, male-female, theoretical/modelling, review)" <- ""
MA.fullrec.conflictres.CF$"topic area (e.g. predator-prey, parasite-host, male-female, theoretical/modelling, review)" <- ""
MA.fullrec.conflictres.KJB$"topic area (e.g. predator-prey, parasite-host, male-female, theoretical/modelling, review)" <- ""
MA.fullrec.conflictres.CM$"topic area (e.g. predator-prey, parasite-host, male-female, theoretical/modelling, review)" <- ""

write.csv(MA.fullrec.conflictres.PT, "./MA_screening_files/MA.fullrec.conflictres.PT.csv", row.names=FALSE)
write.csv(MA.fullrec.conflictres.NM, "./MA_screening_files/MA.fullrec.conflictres.NM.csv", row.names=FALSE)
write.csv(MA.fullrec.conflictres.MW, "./MA_screening_files/MA.fullrec.conflictres.MW.csv", row.names=FALSE)
write.csv(MA.fullrec.conflictres.JW, "./MA_screening_files/MA.fullrec.conflictres.JW.csv", row.names=FALSE)
write.csv(MA.fullrec.conflictres.UE, "./MA_screening_files/MA.fullrec.conflictres.UE.csv", row.names=FALSE)
write.csv(MA.fullrec.conflictres.CF, "./MA_screening_files/MA.fullrec.conflictres.CF.csv", row.names=FALSE)
write.csv(MA.fullrec.conflictres.KJB, "./MA_screening_files/MA.fullrec.conflictres.KJB.csv", row.names=FALSE)
write.csv(MA.fullrec.conflictres.CM, "./MA_screening_files/MA.fullrec.conflictres.CM.csv", row.names=FALSE)
