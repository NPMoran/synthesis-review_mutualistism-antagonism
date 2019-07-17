##########_____Mutualism/Antagonism Synthesis Review_____#########


library(revtools)
library(dplyr)
library(operators)


#Databases: Web of Science Core Collection (http://www.webofknowledge.com); Scopus (https://www.scopus.com/)


#Wos Search ----
#TS= ("*mutualis*" OR "cooperati*" OR "interdependenc*" OR "symbio*") AND TS=("antagonis*" OR "competi*" OR ("host*" AND "parasit*") OR ("predator*" AND "prey") OR "conflict") AND TS=(("intraspecific" OR "within-species" OR "individual*" OR "agent*" OR "organism*" OR "animal*") NEAR/5 ("varia*" OR "divers*" OR "difference*"))
#Refined by: WEB OF SCIENCE CATEGORIES: ( ECOLOGY OR EVOLUTIONARY BIOLOGY OR ZOOLOGY OR BEHAVIORAL SCIENCES OR BIOLOGY )
#Timespan: All years. Indexes: SCI-EXPANDED, SSCI, A&HCI, ESCI.

WoS <- read_bibliography("wos.bib")
WoS.df <- as.data.frame(WoS)
reducing.fields <- c("label","title","author","journal","issn", "volume","number","pages","year","doi","abstract")
WoS.df.reduced <- WoS.df[,reducing.fields]
summary(WoS.df.reduced)

#265 Results


#Scopus Search ----
#( TITLE-ABS-KEY ("*mutualis*" OR "cooperati*" OR "interdependenc*" OR "symbio*") AND 
#  TITLE-ABS-KEY ("antagonis*" OR "competi*" OR ("host*" AND "parasit*") OR ("predator*" AND "prey") OR "conflict") AND 
#  TITLE-ABS-KEY (("intraspecific" OR "within-species" OR "individual*" OR "agent*" OR "organism*" OR "animal*") W/5 ("varia*" OR "divers*" OR "difference*"))
Scopus <- read_bibliography("scopus.bib")
Scopus.df <- as.data.frame(Scopus)
Scopus.df.reduced <- Scopus.df[,reducing.fields]
summary(Scopus.df.reduced)

#... Results


#Deduplication + Export ----
MA.fullrec <- rbind(WoS.df.reduced, Scopus.df.reduced)
summary(MA.fullrec)
MA.fullrec.duplicates <- find_duplicates(data = MA.fullrec,
                                     match_variable = "title",
                                     group_variable = NULL,
                                     match_function = "fuzzdist",
                                     method = "fuzz_m_ratio",
                                     remove_punctuation = T,
                                     threshold = 0.1) 
MA.fullrec.deduplicated.auto <- extract_unique_references(MA.fullrec, MA.fullrec.duplicates)
nrow(MA.fullrec.deduplicated.auto); summary(MA.fullrec.deduplicated.auto)

#gives ... results (number of deduplicates removed)

#exporting csv for manual deduplication
write.csv(MA.fullrec.deduplicated.auto,"MA.fullrec.deduplicated.auto.csv", row.names=FALSE)

#gives ... results (number of additional duplicates removed)


#Production of files for individual screeners:
#importing full database
MA.fullrec.final <- read.csv("MA.fullrec.deduplicated.final.csv", strip.white = TRUE)

#adding an abstract id for each record
MA.fullrec.final <- transform(MA.fullrec.final, AbstractID=paste("MA",1:265))
MA.fullrec.final$AbstractID <- gsub(" ", "", MA.fullrec.final$AbstractID) 

MA.fullrec.final$"screener initials" <- ""
MA.fullrec.final$"include or exclude (note: exclude only if the abstract is unrelated to our topic, if you are unsure mark as include)" <- ""
MA.fullrec.final$"ecological scale (e.g. interspecific or intraspecific)" <- ""
MA.fullrec.final$"topic area (e.g. predator-prey, parasite-host, male-female)" <- ""

#for full double screening, the final deduplicated database is randomly split into 5 random sections twice for 10 total screeners
MA.fullrec.screendatA <- MA.fullrec.final 
MA.fullrec.screendatB <- MA.fullrec.final

#to split  MA.fullrec.screendatA into 5 equal sections of 53 (UPDATE WITH THE FULL LIST)
set.seed(253)
MA.fullrec.screendatA.pt1 <- sample_n(MA.fullrec.screendatA, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatA <- subset(MA.fullrec.screendatA, AbstractID %!in% MA.fullrec.screendatA.pt1$AbstractID)
nrow(MA.fullrec.screendatA)

MA.fullrec.screendatA.pt2 <- sample_n(MA.fullrec.screendatA, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatA <- subset(MA.fullrec.screendatA, AbstractID %!in% MA.fullrec.screendatA.pt2$AbstractID)
nrow(MA.fullrec.screendatA)

MA.fullrec.screendatA.pt3 <- sample_n(MA.fullrec.screendatA, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatA <- subset(MA.fullrec.screendatA, AbstractID %!in% MA.fullrec.screendatA.pt3$AbstractID)
nrow(MA.fullrec.screendatA)

MA.fullrec.screendatA.pt4 <- sample_n(MA.fullrec.screendatA, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatA <- subset(MA.fullrec.screendatA, AbstractID %!in% MA.fullrec.screendatA.pt4$AbstractID)
nrow(MA.fullrec.screendatA)

MA.fullrec.screendatA.pt5 <- sample_n(MA.fullrec.screendatA, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatA <- subset(MA.fullrec.screendatA, AbstractID %!in% MA.fullrec.screendatA.pt5$AbstractID)
nrow(MA.fullrec.screendatA)

#to split  MA.fullrec.screendatB into 5 equal sections of 53 (UPDATE WITH THE FULL LIST)
set.seed(23)
MA.fullrec.screendatB.pt1 <- sample_n(MA.fullrec.screendatB, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatB <- subset(MA.fullrec.screendatB, AbstractID %!in% MA.fullrec.screendatB.pt1$AbstractID)
nrow(MA.fullrec.screendatB)

MA.fullrec.screendatB.pt2 <- sample_n(MA.fullrec.screendatB, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatB <- subset(MA.fullrec.screendatB, AbstractID %!in% MA.fullrec.screendatB.pt2$AbstractID)
nrow(MA.fullrec.screendatB)

MA.fullrec.screendatB.pt3 <- sample_n(MA.fullrec.screendatB, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatB <- subset(MA.fullrec.screendatB, AbstractID %!in% MA.fullrec.screendatB.pt3$AbstractID)
nrow(MA.fullrec.screendatB)

MA.fullrec.screendatB.pt4 <- sample_n(MA.fullrec.screendatB, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatB <- subset(MA.fullrec.screendatB, AbstractID %!in% MA.fullrec.screendatB.pt4$AbstractID)
nrow(MA.fullrec.screendatB)

MA.fullrec.screendatB.pt5 <- sample_n(MA.fullrec.screendatB, 53, replace = FALSE, weight = NULL)
MA.fullrec.screendatB <- subset(MA.fullrec.screendatB, AbstractID %!in% MA.fullrec.screendatB.pt5$AbstractID)
nrow(MA.fullrec.screendatB)

write.csv(MA.fullrec.screendatA.pt1, "MA.fullrec.screendatA.pt1.csv")
write.csv(MA.fullrec.screendatA.pt2, "MA.fullrec.screendatA.pt2.csv")
write.csv(MA.fullrec.screendatA.pt3, "MA.fullrec.screendatA.pt3.csv")
write.csv(MA.fullrec.screendatA.pt4, "MA.fullrec.screendatA.pt4.csv")
write.csv(MA.fullrec.screendatA.pt5, "MA.fullrec.screendatA.pt5.csv")
write.csv(MA.fullrec.screendatB.pt1, "MA.fullrec.screendatB.pt1.csv")
write.csv(MA.fullrec.screendatB.pt2, "MA.fullrec.screendatB.pt2.csv")
write.csv(MA.fullrec.screendatB.pt3, "MA.fullrec.screendatB.pt3.csv")
write.csv(MA.fullrec.screendatB.pt4, "MA.fullrec.screendatB.pt4.csv")
write.csv(MA.fullrec.screendatB.pt5, "MA.fullrec.screendatB.pt5.csv")
