##########_____Mutualism/Antagonism Synthesis Review_____#########


library(dplyr); library(operators)


#Production of files for individual screeners ----

#importing full database
MA.fullrec.final <- read.csv("MA.fullrec.deduplicated.manual.csv", strip.white = TRUE)
nrow(MA.fullrec.final)

#adding an abstract id for each record
MA.fullrec.final <- transform(MA.fullrec.final, abstract.id=paste("MA",1:411))
MA.fullrec.final$abstract.id <- gsub(" ", "", MA.fullrec.final$abstract.id) 

#Adding Columns for screeners to complete
MA.fullrec.final$screener.id <- ""
MA.fullrec.final$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.final$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.final$"topic area (e.g. predator-prey, parasite-host, male-female, theoretical/modelling, review)" <- ""
write.csv(MA.fullrec.final, "MA.fullrec.final.csv")

#for full double screening, the final deduplicated database is randomly split into 5 random sections twice for 10 total screeners
MA.fullrec.screendatA <- MA.fullrec.final 
MA.fullrec.screendatB <- MA.fullrec.final

#to split MA.fullrec.screendatA into 5 sections (4x 82, 1 x 83) 
set.seed(253)
MA.fullrec.screendatA.pt1 <- sample_n(MA.fullrec.screendatA, 82, replace = FALSE, weight = NULL)
MA.fullrec.screendatA <- subset(MA.fullrec.screendatA, abstract.id %!in% MA.fullrec.screendatA.pt1$abstract.id)
nrow(MA.fullrec.screendatA)

MA.fullrec.screendatA.pt2 <- sample_n(MA.fullrec.screendatA, 82, replace = FALSE, weight = NULL)
MA.fullrec.screendatA <- subset(MA.fullrec.screendatA, abstract.id %!in% MA.fullrec.screendatA.pt2$abstract.id)
nrow(MA.fullrec.screendatA)

MA.fullrec.screendatA.pt3 <- sample_n(MA.fullrec.screendatA, 82, replace = FALSE, weight = NULL)
MA.fullrec.screendatA <- subset(MA.fullrec.screendatA, abstract.id %!in% MA.fullrec.screendatA.pt3$abstract.id)
nrow(MA.fullrec.screendatA)

MA.fullrec.screendatA.pt4 <- sample_n(MA.fullrec.screendatA, 82, replace = FALSE, weight = NULL)
MA.fullrec.screendatA <- subset(MA.fullrec.screendatA, abstract.id %!in% MA.fullrec.screendatA.pt4$abstract.id)
nrow(MA.fullrec.screendatA)

MA.fullrec.screendatA.pt5 <- MA.fullrec.screendatA

#to split MA.fullrec.screendatB into 3 sections (3x 137) 
set.seed(23)
MA.fullrec.screendatB.pt1 <- sample_n(MA.fullrec.screendatB, 137, replace = FALSE, weight = NULL)
MA.fullrec.screendatB <- subset(MA.fullrec.screendatB, abstract.id %!in% MA.fullrec.screendatB.pt1$abstract.id)
nrow(MA.fullrec.screendatB)

MA.fullrec.screendatB.pt2 <- sample_n(MA.fullrec.screendatB, 137, replace = FALSE, weight = NULL)
MA.fullrec.screendatB <- subset(MA.fullrec.screendatB, abstract.id %!in% MA.fullrec.screendatB.pt2$abstract.id)
nrow(MA.fullrec.screendatB)

MA.fullrec.screendatB.pt3 <- MA.fullrec.screendatB


#removing all identifying information excluding the tital and abstract
labels(MA.fullrec.screendatA.pt1)
MA.fullrec.screendatA.pt1.reduced <- select(MA.fullrec.screendatA.pt1, -c(label, author, journal, volume, number, pages, year, doi, n_duplicates))
MA.fullrec.screendatA.pt2.reduced <- select(MA.fullrec.screendatA.pt2, -c(label, author, journal, volume, number, pages, year, doi, n_duplicates))
MA.fullrec.screendatA.pt3.reduced <- select(MA.fullrec.screendatA.pt3, -c(label, author, journal, volume, number, pages, year, doi, n_duplicates))
MA.fullrec.screendatA.pt4.reduced <- select(MA.fullrec.screendatA.pt4, -c(label, author, journal, volume, number, pages, year, doi, n_duplicates))
MA.fullrec.screendatA.pt5.reduced <- select(MA.fullrec.screendatA.pt5, -c(label, author, journal, volume, number, pages, year, doi, n_duplicates))
MA.fullrec.screendatB.pt1.reduced <- select(MA.fullrec.screendatB.pt1, -c(label, author, journal, volume, number, pages, year, doi, n_duplicates))
MA.fullrec.screendatB.pt2.reduced <- select(MA.fullrec.screendatB.pt2, -c(label, author, journal, volume, number, pages, year, doi, n_duplicates))
MA.fullrec.screendatB.pt3.reduced <- select(MA.fullrec.screendatB.pt3, -c(label, author, journal, volume, number, pages, year, doi, n_duplicates))

#Allocation of screeners ----
#Koen Johannes Benthem (KJB), MA.fullrec.screendatA.pt1.reduced
MA.fullrec.screendatA.pt1.reduced$screener.id <- "KJB"
#Uli Ernst (UE), MA.fullrec.screendatA.pt2.reduced
MA.fullrec.screendatA.pt2.reduced$screener.id <- "UE"
#Jamie Winternitz (JW), MA.fullrec.screendatA.pt3.reduced
MA.fullrec.screendatA.pt3.reduced$screener.id <- "JW"
#Caroline Muller (CM), MA.fullrec.screendatA.pt4.reduced
MA.fullrec.screendatA.pt4.reduced$screener.id <- "CM"
#Claudia Fricke (CF), MA.fullrec.screendatA.pt5.reduced
MA.fullrec.screendatA.pt5.reduced$screener.id <- "CF"
#Pete Trimmer (PT), MA.fullrec.screendatB.pt1.reduced
MA.fullrec.screendatB.pt1.reduced$screener.id <- "PT"
#Meike Wittmann (MW), MA.fullrec.screendatA.pt4.reduced
MA.fullrec.screendatB.pt2.reduced$screener.id <- "MW"
#Nicholas Moran (NM), MA.fullrec.screendatB.pt5.reduced
MA.fullrec.screendatB.pt3.reduced$screener.id <- "NM"



write.csv(MA.fullrec.screendatA.pt1.reduced, "./MA_screening_files/MA.fullrec.screendatA.pt1.csv", row.names=FALSE)
write.csv(MA.fullrec.screendatA.pt2.reduced, "./MA_screening_files/MA.fullrec.screendatA.pt2.csv", row.names=FALSE)
write.csv(MA.fullrec.screendatA.pt3.reduced, "./MA_screening_files/MA.fullrec.screendatA.pt3.csv", row.names=FALSE)
write.csv(MA.fullrec.screendatA.pt4.reduced, "./MA_screening_files/MA.fullrec.screendatA.pt4.csv", row.names=FALSE)
write.csv(MA.fullrec.screendatA.pt5.reduced, "./MA_screening_files/MA.fullrec.screendatA.pt5.csv", row.names=FALSE)
write.csv(MA.fullrec.screendatB.pt1.reduced, "./MA_screening_files/MA.fullrec.screendatB.pt1.csv", row.names=FALSE)
write.csv(MA.fullrec.screendatB.pt2.reduced, "./MA_screening_files/MA.fullrec.screendatB.pt2.csv", row.names=FALSE)
write.csv(MA.fullrec.screendatB.pt3.reduced, "./MA_screening_files/MA.fullrec.screendatB.pt3.csv", row.names=FALSE)

