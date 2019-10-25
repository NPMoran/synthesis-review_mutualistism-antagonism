##########_____Mutualism/Antagonism Synthesis Review_____#########


#library(devtools)
#install_github("mjwestgate/revtools")
#install.packages("revtools")
#library(xtable)
#library(slam)
library(dplyr); library(operators); library(revtools)


#Databases: Web of Science Core Collection (http://www.webofknowledge.com); Scopus (https://www.scopus.com/)


#Wos Search 18072019----
#TS= ("*mutualis*" OR "cooperati*" OR "interdependenc*" OR "symbio*") AND TS=("antagonis*" OR "competi*" OR ("host*" AND "parasit*") OR ("predator*" AND "prey") OR "conflict") AND TS=(("intraspecific" OR "within-species" OR "individual*" OR "agent*" OR "organism*" OR "animal*") NEAR/5 ("varia*" OR "divers*" OR "difference*"))
#Refined by: WEB OF SCIENCE CATEGORIES: ( ECOLOGY OR EVOLUTIONARY BIOLOGY OR ZOOLOGY OR BEHAVIORAL SCIENCES OR BIOLOGY )
#Timespan: All years. Indexes: SCI-EXPANDED, SSCI, A&HCI, ESCI.

WoS <- read_bibliography("wos.bib")
WoS.df <- as.data.frame(WoS)
reducing.fields <- c("label","title","author","journal", "volume","number","pages","year","doi","abstract")
WoS.df.reduced <- WoS.df[,reducing.fields]
summary(WoS.df.reduced)

#265 Results


#Scopus Search 18072019----
#( TITLE-ABS-KEY ("*mutualis*" OR "cooperati*" OR "interdependenc*" OR "symbio*") AND 
#  TITLE-ABS-KEY ("antagonis*" OR "competi*" OR ("host*" AND "parasit*") OR ("predator*" AND "prey") OR "conflict") AND 
#  TITLE-ABS-KEY (("intraspecific" OR "within-species" OR "individual*" OR "agent*" OR "organism*" OR "animal*") W/5 ("varia*" OR "divers*" OR "difference*"))
#Refined to Scopus Subject Area "Agricultural and Biological Sciences".

Scopus <- read_bibliography("scopus.bib")
Scopus.df <- as.data.frame(Scopus)
Scopus.df.reduced <- Scopus.df[,reducing.fields]
summary(Scopus.df.reduced)

#290 Results


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

#gives 415 results (140 duplicates removed)

#exporting csv of deduplicated database for manual deduplication
write.csv(MA.fullrec.deduplicated.auto,"MA.fullrec.deduplicated.auto.csv", row.names=FALSE)

#exporting csv of pre-auto duplication database to manually check if find_duplicates function is operating correctly
write.csv(MA.fullrec,"MA.fullrec.csv", row.names=FALSE) # function is operating well, all duplicates checked were real duplicates

#additional duplicates removed from file
#Competition versus cooperation: success of individuals foraging alone and in groups	Ranta, E. and Rita, H. and Lindstrom, American Naturalist K.10.1086/285528
#Are there general principles of signal design?	Dawkins, M.S.	Philosophical Transactions - Royal Society of London, B 10.1098/rstb.1993.0065
#Viral quasispecies profiles as the result of the interplay of competition cooperation	Arbiza, Juan and Mirazo, Santiago and Fort, Hugo	BMC EVOLUTIONARY BIOLOGY 10.1186/1471-2148-10-137
#On the morphology of Balaenophilus manatorum (Ortz, Lalana, and Torres) (Copepoda: Harpacticoida) from sea turtles of the Mexican pacific with notes on intraspecific variation	Surez-Morales, E. and Lazo-Wasem, E.A.	Comparative Parasitology 10.1654/4351.1


#gives 411 results (number of additional duplicates removed)