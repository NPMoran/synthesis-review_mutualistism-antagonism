##########_____Mutualism/Antagonism Synthesis Review_____#########


Sys.setenv(LANG = "en")
library(ggplot2); library(tidyverse); library(bibliometrix)

####TO DO LIST
#Bibliometrix ----
#Visualising the Scales of interactions ----
#Visualising the Types of interations used ----
#Visualising the relevant traits ----


#Time x interaction scale (included fulltexts) ----
#https://ggplot2.tidyverse.org/reference/geom_density.html
fulltextinclusions <- read.csv("MA.finalinclusions.csv", strip.white = TRUE)
labels(fulltextinclusions)
nrow(fulltextinclusions) #total number of included studies
summary(fulltextinclusions$year)

Fig.time <- NULL
Fig.time$Scale <- fulltextinclusions$Scale_Processed
Fig.time$year <- fulltextinclusions$year
Fig.time <- as.data.frame(Fig.time)
Fig.time$Scale <- factor(Fig.time$Scale,levels(Fig.time$Scale)[c(3,1,2)])

Fig.time1 <- ggplot(Fig.time, aes(x=year, stat(count)))+
  geom_density(adjust = 0.8, color="black", fill="seashell2") +
  labs(x = "Year of publication", y ="Number of publications")+
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=10, vjust = 2),
        axis.title.x  = element_text(size=10, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  scale_x_continuous(limits = c(1985, 2019), expand = c(0, 0), breaks=c(1985,1990,1995,2000,2005,2010,2015,2019)) +
  scale_y_continuous(limits = c(0,7), expand = c(0, 0), breaks=c(0:7))
Fig.time1

Fig.time2 <- ggplot(Fig.time, aes(x=year, stat(count), fill=Scale)) +
  geom_density(position = "stack", adjust = 0.8) +
  labs(x = "Year of publication", y ="Number of publications") +
  scale_x_continuous(limits = c(1985, 2019), expand = c(0, 0), breaks=c(1985,1990,1995,2000,2005,2010,2015,2019)) +
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

ggsave("Visualisations/Fig.time1.jpg", width = 12, height = 7, units = "cm", Fig.time1, dpi = 600)
ggsave("Visualisations/Fig.time2.jpg", width = 12, height = 7, units = "cm", Fig.time2, dpi = 600)


#Time x interaction type (included fulltexts) ----
labels(fulltextinclusions)
summary(fulltextinclusions$InteractionType_Processed)


#Duplicating studies that include mutiple interaction types to could them in both,
CompCoop <- rbind(subset(fulltextinclusions, InteractionType_Processed == "Competition-cooperation"),
                  subset(fulltextinclusions, InteractionType_Processed == "Consumer-resource/plant-animal; Competition-cooperation"),
                  subset(fulltextinclusions, InteractionType_Processed == "Male-female; Competition-cooperation"),
                  subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont; Competition-cooperation"),
                  subset(fulltextinclusions, InteractionType_Processed == "Male-female; Parent-offspring; Competition-cooperation"),
                  subset(fulltextinclusions, InteractionType_Processed == "Parent-offspring; Competition-cooperation"))
CompCoop$InteractionTypeDuplicated <- "Competition-cooperation" 
ConsRes <- rbind(subset(fulltextinclusions, InteractionType_Processed == "Consumer-resource/plant-animal"),
                 subset(fulltextinclusions, InteractionType_Processed == "Consumer-resource/plant-animal; Competition-cooperation"),
                 subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont; Consumer-resource/plant-animal"))
ConsRes$InteractionTypeDuplicated <- "Consumer-resource/plant-animal" 
HostSym <- rbind(subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont"),
                 subset(fulltextinclusions, InteractionType_Processed == "Host-symbiont; Competition-cooperation"),
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
NonSpec <- subset(fulltextinclusions, InteractionType_Processed == "Non-specific")
NonSpec$InteractionTypeDuplicated <- "Non Specific" 
nrow(ConsRes)

interactionfig <- rbind(CompCoop,
                        ConsRes,
                        HostSym,
                        MalFem, 
                        ParOff)

Fig.time <- NULL
Fig.time$Type <- interactionfig$InteractionTypeDuplicated
Fig.time$year <- interactionfig$year
Fig.time <- as.data.frame(Fig.time)
Fig.time$Type <- factor(Fig.time$Type,levels(Fig.time$Type)[c(1,2,3,4,5)])


Fig.time3 <- ggplot(Fig.time, aes(x=year, stat(count), fill=Type)) +
  geom_density(position = "stack", adjust = 0.8) +
  labs(x = "Year of publication", y ="Number of publications") +
  scale_x_continuous(limits = c(1985, 2019), expand = c(0, 0), breaks=c(1985,1990,1995,2000,2005,2010,2015,2019)) +
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

ggsave("Visualisations/Fig.time3.jpg", width = 12, height = 7, units = "cm", Fig.time3, dpi = 600)


#Summary Tables ----
fulltextinclusions <- read.csv("MA.finalinclusions.csv", strip.white = TRUE)
table1 <- subset(fulltextinclusions, StudyType_Processed != "Modelling/theory")
table1 <- subset(table1, StudyType_Processed != "Review")
nrow(table1)
table1$Ref_Processed <- paste(table1$short_citation, table1$abstract.id, sep = " ")
table1$TypeScale_Processed <- paste(table1$InteractionType_Processed, table1$Scale_Processed, sep = " (")
table1$TypeScale_Processed <- paste(table1$TypeScale_Processed, ")", sep = "")

table1 <- select(table1, -c(abstract.id, row.id, FulltextReviewer.initial, title, author, short_citation, StudyType, StudyType_Processed, 
TaskforceAllocation, Scale, What.is.the.scale.of.the.shift, Scale_Processed, FulltextTopicIdentifier, InteractionType, 
InteractionType_Processed, journal, volume, number, pages, year, doi, Timestamp, FulltextReviewer, SpeciesInteracting, 
TraitDescription, SpeciesTraitVariation, GeneticNotes, 
Is.there.a.shift.in.the.interaction, What.is.the.direction.of.shift, What.factors.are.driving.that.shift, 
What.is.the.ecological.impact.of.the.interaction.and.shift.of.that.interaction, Is.there.a.connection.to.the.NC3, 
Reviewer.notes, Reviewer.outstanding.concerns, ReviewerDecision, 
ReviewerDecisionReason, ReviewerDecision.checked.by, FinalDecision, FinalDecisionReason, FinalDecisionNotes))

write.csv(table1, "~/synthesis-review_mutualistism-antagonism/Visualisations/Table1.csv")


table2 <- subset(fulltextinclusions, StudyType_Processed == "Review")
labels(table2)
table2$Ref_Processed <- paste(table2$short_citation, table2$abstract.id, sep = " ")
table2$TypeScale_Processed <- paste(table2$InteractionType_Processed, table2$Scale_Processed, sep = " (")
table2$TypeScale_Processed <- paste(table2$TypeScale_Processed, ")", sep = "")

table2 <- select(table2, -c(abstract.id, row.id, FulltextReviewer.initial, Species_Processed, author, short_citation, StudyType, StudyType_Processed, 
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

table3 <- select(table3, -c(abstract.id, row.id, FulltextReviewer.initial, Species_Processed, author, short_citation, StudyType, StudyType_Processed, 
                            TaskforceAllocation, Scale, What.is.the.scale.of.the.shift, Scale_Processed, FulltextTopicIdentifier, InteractionType, 
                            InteractionType_Processed, journal, volume, number, pages, doi, Timestamp, FulltextReviewer, SpeciesInteracting, 
                            TraitDescription, SpeciesTraitVariation, GeneticNotes, title,
                            Is.there.a.shift.in.the.interaction, What.is.the.direction.of.shift, What.factors.are.driving.that.shift, 
                            What.is.the.ecological.impact.of.the.interaction.and.shift.of.that.interaction, Is.there.a.connection.to.the.NC3, 
                            Reviewer.notes, Reviewer.outstanding.concerns, ReviewerDecision, 
                            ReviewerDecisionReason, ReviewerDecision.checked.by, FinalDecision, FinalDecisionReason, FinalDecisionNotes))

write.csv(table3, "~/synthesis-review_mutualistism-antagonism/Visualisations/Table3.csv")


#install.packages(c("gt","rlang","dplyr"))
#library(gt)
#library(rlang)
#library(dplyr)
#tabledata <- read.csv("MA.fulltextscreening.finaldecisions.csv")
#tabledata <- subset(tabledata, FinalDecision == "Include")
#labels(tabledata)
#summary(tabledata$StudyType)
#tabledata1_expA <- subset(tabledata, StudyType == "Experimental")
#tabledata1_expB <- subset(tabledata, StudyType == "Observational")
#tabledata1_exp <- rbind(tabledata1_expA, tabledata1_expB)
#tabledata1_exp <- select(tabledata1_exp, -c(row.id,TaskforceAllocation,FulltextTopicIdentifier,
#                                            FulltextReviewer.initial,volume,number,pages,Timestamp,
#                                            FulltextReviewer,GeneticNotes,What.is.the.scale.of.the.shift,
#                                            What.is.the.direction.of.shift,What.factors.are.driving.that.shift,
#                                            What.is.the.ecological.impact.of.the.interaction.and.shift.of.that.interaction,
#                                            Is.there.a.connection.to.the.NC3,Reviewer.notes,
#                                            Reviewer.outstanding.concerns,ReviewerDecision,ReviewerDecisionReason,
#                                            ReviewerDecision.checked.by,FinalDecision,FinalDecisionReason,FinalDecisionNotes))
#table1 <- tabledata1_exp %>% gt()




#Word cloud_included titles (from abstract screening) ----
#Code based on: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
library("tm")# for text mining
library("SnowballC")# for text stemming
library("wordcloud")# word-cloud generator 
library("RColorBrewer")# color palettes

#Importing and formatting the files
text <- readLines("MA-includedtitles.txt")
docs <- Corpus(VectorSource(text))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|") 
docs <- tm_map(docs, content_transformer(tolower)) #to convert the text to lower case
docs <- tm_map(docs, removeNumbers) #to remove numbers
docs <- tm_map(docs, removeWords, stopwords("english"))# to remove english common stopwords
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) # To remove your own words, specify your stopwords as a character vector
docs <- tm_map(docs, removePunctuation) #to remove punctuations
docs <- tm_map(docs, stripWhitespace) #to remove extra white spaces
# docs <- tm_map(docs, stemDocument) # Text stemming #Only use if you want to reduce words to their root

#finding frequency of words (within each title)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Drawing word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Finding the most frequent terms and the association between them
findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "variation", corlimit = 0.3)
findAssocs(dtm, terms = "individual", corlimit = 0.3)


#Word cloud_included abstracts (from abstract screening) text ----
#Importing and formatting the files
text <- readLines("MA-includedabstracts.txt")
docs <- Corpus(VectorSource(text))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|") 
docs <- tm_map(docs, content_transformer(tolower)) #to convert the text to lower case
docs <- tm_map(docs, removeNumbers) #to remove numbers
docs <- tm_map(docs, removeWords, stopwords("english"))# to remove english common stopwords
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) # To remove your own words, specify your stopwords as a character vector
docs <- tm_map(docs, removePunctuation) #to remove punctuations
docs <- tm_map(docs, stripWhitespace) #to remove extra white spaces
# docs <- tm_map(docs, stemDocument) # Text stemming #Only use if you want to reduce words to their root

#finding frequency of words 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Drawing word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, scale=c(4,.5), min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Finding the most frequent terms and the association between them
findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "variation", corlimit = 0.3)
findAssocs(dtm, terms = "individual", corlimit = 0.3)

