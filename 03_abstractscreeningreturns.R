##########_____Mutualism/Antagonism Synthesis Review_____#########
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Title: Shifts between cooperation and antagonism driven by individual variation: A systematic synthesis review 


#3. PROCESSING RESPONSES TO ABSTRACT SCREENING


library(dplyr); library(operators)


#Analysis of screeners returned databases ----
#KJM
MA.fullrec.screendatA.pt1.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt1.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt1.done)
nrow(subset(MA.fullrec.screendatA.pt1.done, decision == "include")) #31/82 included, 37.80488% inclusion rate
summary(subset(MA.fullrec.screendatA.pt1.done, decision == "include"))

#UE
MA.fullrec.screendatA.pt2.done <- read.csv("./MA_screening_returns/MA.fullrec.screendatA.pt2.done.csv", strip.white = TRUE)
summary(MA.fullrec.screendatA.pt2.done) #
MA.fullrec.screendatA.pt2.done <- transform(MA.fullrec.screendatA.pt2.done, topic=paste(topic.area..e.g..predator.prey..parasite.host..male.female..theoretical.modelling..review., observation.experiment..modelling..theory..review)) #screener created an additional topic area variable, so combining these to create a new topic variable
MA.fullrec.screendatA.pt2.done <- select(MA.fullrec.screendatA.pt2.done, -c(topic.area..e.g..predator.prey..parasite.host..male.female..theoretical.modelling..review., observation.experiment..modelling..theory..review)) #cleaning up old variables
MA.fullrec.screendatA.pt2.done.no <- subset(MA.fullrec.screendatA.pt2.done, decision == "no"); MA.fullrec.screendatA.pt2.done.no$decision <- "exclude" #screener input 4 levels of decisions, agreed to classify maybe and yes as include, no and rather nots as exclude
MA.fullrec.screendatA.pt2.done.rathernot <- subset(MA.fullrec.screendatA.pt2.done, decision == "rather not"); MA.fullrec.screendatA.pt2.done.rathernot$decision <- "exclude"
MA.fullrec.screendatA.pt2.done.maybe <- subset(MA.fullrec.screendatA.pt2.done, decision == "maybe"); MA.fullrec.screendatA.pt2.done.maybe$decision <- "include"
MA.fullrec.screendatA.pt2.done.yes <- subset(MA.fullrec.screendatA.pt2.done, decision == "yes"); MA.fullrec.screendatA.pt2.done.yes$decision <- "include"
MA.fullrec.screendatA.pt2.done <- rbind(MA.fullrec.screendatA.pt2.done.no, MA.fullrec.screendatA.pt2.done.rathernot,MA.fullrec.screendatA.pt2.done.maybe, MA.fullrec.screendatA.pt2.done.yes)
MA.fullrec.screendatA.pt2.done.include <- subset(MA.fullrec.screendatA.pt2.done, decision == c("no", "rather"))
nrow(MA.fullrec.screendatA.pt2.done)
nrow(subset(MA.fullrec.screendatA.pt2.done, decision == "include")) #29/82 included, 35.36585% inclusion rate
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


#Conflict Identification ----
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
nrow(excluded.both) #143 records excluded so far

#Included records
included.B <- subset(conflict.identification, decision.y == "include")
included.both <- subset(included.B, decision.x == "include")
nrow(included.both) #128 records included so far

#Conflicting decisions
conflict.1 <- subset(excluded.B, decision.x != "exclude")
conflict.2 <- subset(included.B, decision.x != "include")

conflicts <- rbind(conflict.1, conflict.2)
nrow(conflicts) #140 conflicts

write.csv(conflicts, "MA.abstractconflicts.csv")
write.csv(conflict.identification, "MA.abstractdecision_first.csv")