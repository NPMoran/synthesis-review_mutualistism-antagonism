##########_____Mutualism/Antagonism Synthesis Review_____#########
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#         Elina Takola for Bibliomtric analysis (Friedrich Schiller University Jena)
#
# Title: Movement between cooperation and antagonism driven by individual variation: A systematic synthesis review 


#7. PRODUCING VISUALISATIONS AND TABLES FOR SUPPLEMENTARY MATERIALS


Sys.setenv(LANG = "en")
library(tidyverse); library(ggplot2)
install.packages("bibliometrix", dependencies=TRUE)
library(bibliometrix)


#(Fig S4a,S4b) Temporal trends in the scale of included studies ----
#https://ggplot2.tidyverse.org/reference/geom_density.html
fulltextinclusions <- read.csv("MA.finalinclusions.csv", strip.white = TRUE)
labels(fulltextinclusions)
nrow(fulltextinclusions) #total number of included studies
summary(fulltextinclusions$year)


#producing dataframe for ggplot
Fig.time <- NULL
Fig.time$Scale <- fulltextinclusions$Scale_Processed
Fig.time$year <- fulltextinclusions$year
Fig.time <- as.data.frame(Fig.time)
Fig.time$Scale <- ordered(Fig.time$Scale, levels = c("Interspecific", "Inter- and Intraspecific", "Intraspecific"))


Fig.time1 <- ggplot(Fig.time, aes(x=year, stat(count)))+
  geom_density(adjust = 0.8, color="black", fill="seashell2") +
  labs(x = "Year of publication", y ="Number of publications")+
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=10, vjust = 2),
        axis.title.x  = element_text(size=10, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  scale_x_continuous(limits = c(1960, 2020), expand = c(0, 0), breaks=c(1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020)) +
  scale_y_continuous(limits = c(0,7), expand = c(0, 0), breaks=c(0:7))
Fig.time1

Fig.time2 <- ggplot(Fig.time, aes(x=year, stat(count), fill=Scale)) +
  geom_density(position = "stack", adjust = 0.8) +
  labs(x = "Year of publication", y ="Number of publications") +
  scale_x_continuous(limits = c(1960, 2020), expand = c(0, 0), breaks=c(1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020)) +
  scale_y_continuous(limits = c(0,7), expand = c(0, 0), breaks=c(0:7)) +
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=10, vjust = 2),
        axis.title.x  = element_text(size=10, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = c(0.15,0.7),
        legend.title = element_blank()) +
  scale_fill_manual(values=c("seashell2","white","red"))
Fig.time2

ggsave("Visualisations/Fig.time1.jpg", width = 18, height = 7, units = "cm", Fig.time1, dpi = 600)
ggsave("Visualisations/Fig.time2.jpg", width = 18, height = 7, units = "cm", Fig.time2, dpi = 600)



##(Fig S4c) Temporal trends in the interaction type of included studies ----
summary(as.factor(fulltextinclusions$InteractionType_Processed))

#duplicating studies that consider mutiple interaction types to to include them in both categories,
CompCoop <- rbind(subset(fulltextinclusions, InteractionType_Processed == "Competition-cooperation"),
                  subset(fulltextinclusions, InteractionType_Processed == "Male-female; Competition-cooperation"),
                  subset(fulltextinclusions, InteractionType_Processed == "Male-female; Parent-offspring; Competition-cooperation"),
                  subset(fulltextinclusions, InteractionType_Processed == "Parent-offspring; Competition-cooperation"))
CompCoop$InteractionTypeDuplicated <- "Competition-cooperation" 
ConsRes <- rbind(subset(fulltextinclusions, InteractionType_Processed == "Consumer-resource/plant-animal"),
                 subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont; Consumer-resource/plant-animal"))
ConsRes$InteractionTypeDuplicated <- "Consumer-resource/plant-animal" 
HostSym <- rbind(subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont"),
                 subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont; Consumer-resource/plant-animal"),
                 subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont; Male-female; Parent-offspring"))
HostSym$InteractionTypeDuplicated <- "Host-symbiont" 
MalFem <- rbind(subset(fulltextinclusions, InteractionType_Processed == "Male-female"),
                subset(fulltextinclusions, InteractionType_Processed == "Male-female; Competition-cooperation"),
                subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont; Male-female; Parent-offspring"),
                subset(fulltextinclusions, InteractionType_Processed == "Male-female; Parent-offspring"),
                subset(fulltextinclusions, InteractionType_Processed == "Male-female; Parent-offspring; Competition-cooperation"))
MalFem$InteractionTypeDuplicated <- "Male-female" 
ParOff <- rbind(subset(fulltextinclusions, InteractionType_Processed == "Parent-offspring; Competition-cooperation"),
                subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont; Male-female; Parent-offspring"),
                subset(fulltextinclusions, InteractionType_Processed == "Male-female; Parent-offspring"),
                subset(fulltextinclusions, InteractionType_Processed == "Male-female; Parent-offspring; Competition-cooperation"))
ParOff$InteractionTypeDuplicated <- "Parent-offspring" 
nrow(ConsRes)

fulltextinclusions_stacked <- rbind(CompCoop,
                        ConsRes,
                        HostSym,
                        MalFem, 
                        ParOff)
nrow(fulltextinclusions_stacked)

Fig.time <- NULL
Fig.time$Type <- fulltextinclusions_stacked$InteractionTypeDuplicated
Fig.time$year <- fulltextinclusions_stacked$year
Fig.time <- as.data.frame(Fig.time)
Fig.time$Type <- ordered(Fig.time$Type, levels = c("Consumer-resource/plant-animal", "Host-symbiont", "Competition-cooperation", "Male-female", "Parent-offspring"))


Fig.time3 <- ggplot(Fig.time, aes(x=year, stat(count), fill=Type)) +
  geom_density(position = "stack", adjust = 0.8) +
  labs(x = "Year of publication", y ="Number of publications") +
  scale_x_continuous(limits = c(1960, 2020), expand = c(0, 0), breaks=c(1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020)) +
  scale_y_continuous(limits = c(0,8), expand = c(0, 0), breaks=c(0:8)) +
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=10, vjust = 2),
        axis.title.x  = element_text(size=10, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = c(0.3,0.66),
        legend.title = element_blank()) +
  scale_fill_manual(values=c("seashell2","lightgrey","white","red","black"))
Fig.time3

ggsave("Visualisations/Fig.time3.jpg", width = 18, height = 7, units = "cm", Fig.time3, dpi = 600)


##(Fig S3a) Scale composition of studies by study type ----
#producing dataframe for ggplot
fulltextinclusions.expobs <- subset(fulltextinclusions, StudyType_Processed != "Modelling/theory")
fulltextinclusions.expobs <- subset(fulltextinclusions.expobs, StudyType_Processed != "Review")
fulltextinclusions.expobs <- subset(fulltextinclusions.expobs, StudyType_Processed != "Review, Modelling/theory")
nrow(fulltextinclusions.expobs)
summary(as.factor(fulltextinclusions.expobs$Scale_Processed))
ExpObs_scale <- NULL
ExpObs_scale$StudyType <- "Exp/obs"
ExpObs_scale$Scale <- c('Interspecific', 'Inter- and Intraspecific','Intraspecific')
ExpObs_scale$Npapers <- c(16,2,29)
ExpObs_scale$Percentpapers <- (ExpObs_scale$Npapers)/47*100
ExpObs_scale <- as.data.frame(ExpObs_scale)

fulltextinclusions.review <- subset(fulltextinclusions, StudyType_Processed != "Experimental")
fulltextinclusions.review <- subset(fulltextinclusions.review, StudyType_Processed != "Experimental; Modelling/theory")
fulltextinclusions.review <- subset(fulltextinclusions.review, StudyType_Processed != "Modelling/theory")
fulltextinclusions.review <- subset(fulltextinclusions.review, StudyType_Processed != "Observational")
fulltextinclusions.review <- subset(fulltextinclusions.review, StudyType_Processed != "Observational; Experimental")
fulltextinclusions.review <- subset(fulltextinclusions.review, StudyType_Processed != "Observational; Experimental; Modelling/theory")
fulltextinclusions.review <- subset(fulltextinclusions.review, StudyType_Processed != "Observational; Modelling/theory")
nrow(fulltextinclusions.review)
summary(as.factor(fulltextinclusions.review$Scale_Processed))
Rev_scale <- NULL
Rev_scale$StudyType <- "Review" 
Rev_scale$Scale <- c('Interspecific', 'Inter- and Intraspecific','Intraspecific')
Rev_scale$Npapers <- c(5,3,11) 
Rev_scale$Percentpapers <- (Rev_scale$Npapers)/19*100
Rev_scale <- as.data.frame(Rev_scale)

fulltextinclusions.model <- subset(fulltextinclusions, StudyType_Processed != "Experimental")
fulltextinclusions.model <- subset(fulltextinclusions.model, StudyType_Processed != "Observational; Experimental")
fulltextinclusions.model <- subset(fulltextinclusions.model, StudyType_Processed != "Review")
fulltextinclusions.model <- subset(fulltextinclusions.model, StudyType_Processed != "Observational")
nrow(fulltextinclusions.model)
summary(as.factor(fulltextinclusions.model$Scale_Processed))
Mod_scale <- NULL
Mod_scale$StudyType <- "Modelling/theory" 
Mod_scale$Scale<- c('Interspecific', 'Inter- and Intraspecific','Intraspecific')
Mod_scale$Npapers <- c(5,3,26) 
Mod_scale$Percentpapers <- (Mod_scale$Npapers)/34*100
Mod_scale <- as.data.frame(Mod_scale)

Fig_scale <- rbind(ExpObs_scale, Rev_scale, Mod_scale)
Fig_scale$Scale <- ordered(Fig_scale$Scale, levels =c('Interspecific', 'Inter- and Intraspecific','Intraspecific'))
Fig_scale$StudyType <- ordered(Fig_scale$StudyType, levels = c("Exp/obs","Review","Modelling/theory"))

Fig.comp1 <- ggplot(Fig_scale, aes(fill=Scale, y=Percentpapers, x=StudyType)) + 
  geom_bar(position="Stack", stat="identity",  colour="black") +
  scale_fill_manual(values=c("seashell2","white","red")) +
  labs(x = "Study type", y ="Percentage of included publications") +
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=10, vjust = 2),
        axis.title.x  = element_text(size=10, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.title = element_blank())
Fig.comp1

ggsave("Visualisations/Fig.comp1.jpg", width = 14.5, height = 9, units = "cm", Fig.comp1, dpi = 600)


##(Fig S3b) Topic composition of studies by study type ----
#producing dataframe for ggplot using stacked dataset from above
summary(as.factor(fulltextinclusions_stacked$StudyType_Processed))

fulltextinclusions_stacked.expobs <- subset(fulltextinclusions_stacked, StudyType_Processed != "Modelling/theory")
fulltextinclusions_stacked.expobs <- subset(fulltextinclusions_stacked.expobs, StudyType_Processed != "Review")
fulltextinclusions_stacked.expobs <- subset(fulltextinclusions_stacked.expobs, StudyType_Processed != "Review, Modelling/theory")
nrow(fulltextinclusions_stacked.expobs)
summary(as.factor(fulltextinclusions_stacked.expobs$InteractionTypeDuplicated))
ExpObs_topics <- NULL
ExpObs_topics$StudyType <- "Exp/obs" 
ExpObs_topics$Topic <- c('Consumer-resource/plant-animal','Host-symbiont','Competition-cooperation','Male-female', 'Parent-offspring')
ExpObs_topics$Npapers <- c(4,11,28,11,2) 
ExpObs_topics$Percentpapers <- ((ExpObs_topics$Npapers)/47)*100
ExpObs_topics <- as.data.frame(ExpObs_topics)

fulltextinclusions_stacked.review <- subset(fulltextinclusions_stacked, StudyType_Processed != "Experimental")
fulltextinclusions_stacked.review <- subset(fulltextinclusions_stacked.review, StudyType_Processed != "Experimental; Modelling/theory")
fulltextinclusions_stacked.review <- subset(fulltextinclusions_stacked.review, StudyType_Processed != "Modelling/theory")
fulltextinclusions_stacked.review <- subset(fulltextinclusions_stacked.review, StudyType_Processed != "Observational")
fulltextinclusions_stacked.review <- subset(fulltextinclusions_stacked.review, StudyType_Processed != "Observational; Experimental")
fulltextinclusions_stacked.review <- subset(fulltextinclusions_stacked.review, StudyType_Processed != "Observational; Experimental; Modelling/theory")
fulltextinclusions_stacked.review <- subset(fulltextinclusions_stacked.review, StudyType_Processed != "Observational; Modelling/theory")
nrow(fulltextinclusions_stacked.review)
summary(as.factor(fulltextinclusions_stacked.review$InteractionTypeDuplicated))
Rev_topics <- NULL
Rev_topics$StudyType <- "Review" 
Rev_topics$Topic <- c('Consumer-resource/plant-animal','Host-symbiont','Competition-cooperation','Male-female', 'Parent-offspring')
Rev_topics$Npapers <- c(4,5,11,6,3) 
Rev_topics$Percentpapers <- ((Rev_topics$Npapers)/19)*100
Rev_topics <- as.data.frame(Rev_topics)

fulltextinclusions_stacked.model <- subset(fulltextinclusions_stacked, StudyType_Processed != "Experimental")
fulltextinclusions_stacked.model <- subset(fulltextinclusions_stacked.model, StudyType_Processed != "Observational; Experimental")
fulltextinclusions_stacked.model <- subset(fulltextinclusions_stacked.model, StudyType_Processed != "Review")
fulltextinclusions_stacked.model <- subset(fulltextinclusions_stacked.model, StudyType_Processed != "Observational")
nrow(fulltextinclusions_stacked.model)
summary(as.factor(fulltextinclusions_stacked.model$InteractionTypeDuplicated))
Mod_topics <- NULL
Mod_topics$StudyType <- "Modelling/theory" 
Mod_topics$Topic <- c('Consumer-resource/plant-animal','Host-symbiont','Competition-cooperation','Male-female', 'Parent-offspring')
Mod_topics$Npapers <- c(3,2,28,5,1) 
Mod_topics$Percentpapers <- ((Mod_topics$Npapers)/34)*100
Mod_topics <- as.data.frame(Mod_topics)

Fig_topics <- rbind(ExpObs_topics, Rev_topics, Mod_topics)
Fig_topics$Topic <- ordered(Fig_topics$Topic, levels = c('Consumer-resource/plant-animal','Host-symbiont','Competition-cooperation','Male-female', 'Parent-offspring'))
Fig_topics$StudyType <- ordered(Fig_topics$StudyType, levels = c("Exp/obs","Review","Modelling/theory"))


Fig.comp2 <- ggplot(Fig_topics, aes(fill=Topic, y=Percentpapers, x=StudyType)) + 
  geom_bar(position="stack", stat="identity",  colour="black") +
  scale_fill_manual(values=c("seashell2","lightgrey","white","red","black")) +
  labs(x = "Study type", y ="Percentage of included publications") +
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=10, vjust = 2),
        axis.title.x  = element_text(size=10, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.title = element_blank())
Fig.comp2

ggsave("Visualisations/Fig.comp2.jpg", width = 16, height = 9, units = "cm", Fig.comp2, dpi = 600)


##Bibliometric analysis ----
#library(bibliometrix); library(VennDiagram)
#
#
##Importing datasets
##WOS: 79 records from included articles (79/95, 83%)
#file <- "wos.included.csv"
#WOS <- convert2df(file, dbsource = "wos", format = "csv")
#head(WOS["TC"])
#
##WOS: 73 records from included articles (73/95, 77%%)
#file <- "scopus.included.bib"
#SCO <- convert2df(file, dbsource = "scopus", format = "bibtex")
#head(SCO["TC"])
#
##2 articles missing from both databases:
##- Mutualisms as consumer-resource interactions	Holland, J. N., Ness, J. H., Boyle, A. L., & Bronstein, J. L. (book chapter)
##- Social rank modulates how environmental quality influences cooperation and conflict within animal societies.	Liu, M., Chen, B. F., Rubenstein, D. R., & Shen, S. F. (recent paper, not indexed at time of analysis)
#
#
###Create Venn Diagram to check overlap between the two databases
#install.packages("venn")
#library(VennDiagram)
##Size of each set
#nrow(CompCoop) #63 Competition-cooperation
#nrow(ConsRes) #11 Consumer-resource/plant-animal
#nrow(HostSym) #18 Host-symbiont
#nrow(MalFem ) #19 Male-female
#nrow(ParOff) #5 Parent-offspring
##Size off overlaps
#summary(as.factor(fulltextinclusions$InteractionType_Processed))
##5 Host-symbiont; Consumer-resource/plant-animal
##1 Host-symbiont; Male-female; Parent-offspring
##8 Male-female; Competition-cooperation
##1 Male-female; Parent-offspring
##2 Male-female; Parent-offspring; Competition-cooperation
##1 Parent-offspring; Competition-cooperation
#
#
#### Isolate titles to use for comparison
##Scopus <- SCO$TI
##WoS <- WOS$TI
##name <- "Fig_venn"
#### Create the diagram according to similarity of titles
##v <- venn.diagram(x = (list("Scopus" = Scopus, "WoS" = WoS)),
##                  fill = c("seashell2","lightgrey"),
##                  alpha = 0.4, 
##                  filename=name, height = 2400, width = 2800, resolution =
##                  600, imagetype = "png",
##                  fontfamily = "sans",
##                  sub.fontfamily = "sans")
### The Venn diagram is automatically saved in the working directory
#
#MER <- mergeDbSources (SCO, WOS, remove.duplicated = TRUE)
##57 duplicates removed, 2 duplicates not removed
#
#
## An example of a classical keyword co-occurrences network
#NetMatrix <- biblioNetwork(MER, analysis = "co-occurrences", network = "keywords", sep = ";")
#netstat <- networkStat(NetMatrix)
#
## Co-citation network
#NetMatrix <- biblioNetwork(MER, analysis = "co-citation", network = "references", sep = ",")
#net=networkPlot(NetMatrix, n = 20, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
#
#
## Create keyword co-occurrences network
#NetMatrix2 <- biblioNetwork(MER, analysis = "co-occurrences", network = "keywords", sep = ";")
#net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = NULL, type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
#
## Conceptual Structure using keywords (method="CA")
#CS <- conceptualStructure(WOS,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)
#
## Create a historical citation network
#options(width=130)
#histResults <- histNetwork(MER, min.citations = 1, sep = ";")
### 
### WOS DB:
#
## Plot a historical co-citation network
#net <- histPlot(histResults, n=15, size = 10, labelsize=5)
#
#
#
#


#(Table S1, S2, S3) Summary Tables  ----
fulltextinclusions <- read.csv("MA.finalinclusions.csv", strip.white = TRUE)
table1 <- subset(fulltextinclusions, StudyType_Processed != "Modelling/theory")
table1 <- subset(table1, StudyType_Processed != "Review, Modelling/theory")
table1 <- subset(table1, StudyType_Processed != "Review")
nrow(table1)
table1$Ref_Processed <- paste(table1$short_citation, table1$abstract.id, sep = " ")
table1$TypeScale_Processed <- paste(table1$InteractionType_Processed, table1$Scale_Processed, sep = " (")
table1$TypeScale_Processed <- paste(table1$TypeScale_Processed, ")", sep = "")

table1 <- select(table1, -c(used.in.main.text., sort.id, WOS, SCO, abstract.id, row.id, FulltextReviewer.initial, title, author, short_citation, StudyType, StudyType_Processed, 
TaskforceAllocation, Scale, What.is.the.scale.of.the.shift, Scale_Processed, FulltextTopicIdentifier, InteractionType, 
InteractionType_Processed, journal, volume, number, pages, year, doi, Timestamp, FulltextReviewer, SpeciesInteracting, 
TraitDescription, SpeciesTraitVariation, GeneticNotes, 
Is.there.a.shift.in.the.interaction, What.is.the.direction.of.shift, What.factors.are.driving.that.shift, 
What.is.the.ecological.impact.of.the.interaction.and.shift.of.that.interaction, Is.there.a.connection.to.the.NC3, 
Reviewer.notes, Reviewer.outstanding.concerns, ReviewerDecision, 
ReviewerDecisionReason, ReviewerDecision.checked.by, FinalDecision, FinalDecisionReason, FinalDecisionNotes))

write.csv(table1, "~/synthesis-review_mutualistism-antagonism/Visualisations/Table1.csv")


table2a <- subset(fulltextinclusions, StudyType_Processed == "Review")
table2b <- subset(fulltextinclusions, StudyType_Processed == "Review, Modelling/theory")
table2 <- rbind(table2a,table2b)
labels(table2)
table2$Ref_Processed <- paste(table2$short_citation, table2$abstract.id, sep = " ")
table2$TypeScale_Processed <- paste(table2$InteractionType_Processed, table2$Scale_Processed, sep = " (")
table2$TypeScale_Processed <- paste(table2$TypeScale_Processed, ")", sep = "")

table2 <- select(table2, -c(used.in.main.text., sort.id, WOS, SCO, abstract.id, row.id, FulltextReviewer.initial, Species_Processed, author, short_citation, StudyType, StudyType_Processed, 
                            TaskforceAllocation, Scale, What.is.the.scale.of.the.shift, Scale_Processed, FulltextTopicIdentifier, InteractionType, 
                            InteractionType_Processed, journal, volume, number, pages, year, doi, Timestamp, FulltextReviewer, SpeciesInteracting, 
                            Trait_Processed, TraitDescription, SpeciesTraitVariation, GeneticNotes, 
                            Is.there.a.shift.in.the.interaction, What.is.the.direction.of.shift, What.factors.are.driving.that.shift, 
                            What.is.the.ecological.impact.of.the.interaction.and.shift.of.that.interaction, Is.there.a.connection.to.the.NC3, 
                            Reviewer.notes, Reviewer.outstanding.concerns, ReviewerDecision, 
                            ReviewerDecisionReason, ReviewerDecision.checked.by, FinalDecision, FinalDecisionReason, FinalDecisionNotes))

write.csv(table2, "~/synthesis-review_mutualistism-antagonism/Visualisations/Table2.csv")


table3 <- read.csv("MA.finalinclusions_modelsonly.csv", strip.white = TRUE)
labels(table3)
table3$Ref_Processed <- paste(table3$short_citation, table3$abstract.id, sep = " ")
table3$TypeScale_Processed <- paste(table3$InteractionType_Processed, table3$Scale_Processed, sep = " (")
table3$TypeScale_Processed <- paste(table3$TypeScale_Processed, ")", sep = "")

table3 <- select(table3, -c(sort.id,	used.in.main.text.,	WOS,	SCO, Trait_Processed, year, abstract.id, FulltextReviewer.initial, Species_Processed, author, short_citation, StudyType, StudyType_Processed, 
                            TaskforceAllocation, Scale, What.is.the.scale.of.the.shift, Scale_Processed, FulltextTopicIdentifier, InteractionType, 
                            InteractionType_Processed, journal, volume, number, pages, doi, Timestamp, FulltextReviewer, SpeciesInteracting, 
                            TraitDescription, SpeciesTraitVariation, GeneticNotes,
                            Is.there.a.shift.in.the.interaction, What.is.the.direction.of.shift, What.factors.are.driving.that.shift, 
                            What.is.the.ecological.impact.of.the.interaction.and.shift.of.that.interaction, Is.there.a.connection.to.the.NC3, 
                            Reviewer.notes, Reviewer.outstanding.concerns, ReviewerDecision, 
                            ReviewerDecisionReason, ReviewerDecision.checked.by, FinalDecision, FinalDecisionReason, FinalDecisionNotes))

write.csv(table3, "~/synthesis-review_mutualistism-antagonism/Visualisations/Table3.csv")

