##########_____Mutualism/Antagonism Synthesis Review_____#########


Sys.setenv(LANG = "en")
library(ggplot2); library(tidyverse)
#https://ggplot2.tidyverse.org/reference/geom_density.html


#Time x interaction scale (included fulltexts) ----
fulltextdecisions <- read.csv("MA.fulltextscreening.finaldecisionssimplified.csv", strip.white = TRUE)
labels(fulltextdecisions)
fulltextincluded <- subset(fulltextdecisions, FinalDecision == "Include")
nrow(fulltextincluded) #total number of included studies


labels(fulltextincluded)
summary(fulltextincluded$year)

Fig.time <- NULL
Fig.time$Scale <- fulltextincluded$Scale
Fig.time$year <- fulltextincluded$year
Fig.time <- as.data.frame(Fig.time)
Fig.time$Scale <- factor(Fig.time$Scale,levels(Fig.time$Scale)[c(3,2,1)])

Fig.time1 <- ggplot(Fig.time, aes(x=year, stat(count)))+
  geom_density(color="darkblue", fill="lightblue") +
  labs(x = "Year of publication", y ="Number of publications")+
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=14, vjust = 0.1),
        axis.title.x  = element_text(size=14, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  scale_x_continuous(limits = c(1985, 2019), expand = c(0, 0), breaks=c(1985,1990,1995,2000,2005,2010,2015,2019)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

Fig.time2 <- ggplot(Fig.time, aes(x=year, stat(count), fill=Scale)) +
  geom_density(position = "stack", adjust = 0.8) +
  labs(x = "Year of publication", y ="Number of publications") +
  scale_x_continuous(limits = c(1985, 2019), expand = c(0, 0), breaks=c(1985,1990,1995,2000,2005,2010,2015,2019)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=14, vjust = 0.1),
        axis.title.x  = element_text(size=14, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = c(0.15,0.7),
        legend.title = element_blank()) +
  scale_fill_manual(values=c("seashell2","white","red"))
Fig.time2

ggsave("Visualisations/Fig.time1.jpg", width = 16, height = 8, units = "cm", Fig.time1, dpi = 600)
ggsave("Visualisations/Fig.time2.jpg", width = 16, height = 8, units = "cm", Fig.time2, dpi = 600)


#Time x interaction type (included fulltexts) ----
labels(fulltextincluded)
summary(fulltextincluded$InteractionType)

Fig.time <- NULL
Fig.time$Type <- fulltextincluded$InteractionType
Fig.time$year <- fulltextincluded$year
Fig.time <- as.data.frame(Fig.time)

Fig.time3 <- ggplot(Fig.time, aes(x=year, stat(count), fill=Type)) +
  geom_density(position = "stack", adjust = 0.8) +
  labs(x = "Year of publication", y ="Number of publications") +
  scale_x_continuous(limits = c(1985, 2019), expand = c(0, 0), breaks=c(1985,1990,1995,2000,2005,2010,2015,2019)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=14, vjust = 0.1),
        axis.title.x  = element_text(size=14, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = c(0.20,0.7),
        legend.title = element_blank()) +
  scale_fill_manual(values=c("seashell2","lightgrey","white","red","black"))

ggsave("Visualisations/Fig.time3.jpg", width = 16, height = 8, units = "cm", Fig.time3, dpi = 600)


#Summary Tables ----
#install.packages(c("gt","rlang","dplyr"))
library(gt)
library(rlang)
library(dplyr)
tabledata <- read.csv("MA.fulltextscreening.finaldecisions.csv")
tabledata <- subset(tabledata, FinalDecision == "Include")
labels(tabledata)
summary(tabledata$StudyType)
tabledata1_expA <- subset(tabledata, StudyType == "Experimental")
tabledata1_expB <- subset(tabledata, StudyType == "Observational")
tabledata1_exp <- rbind(tabledata1_expA, tabledata1_expB)
tabledata1_exp <- select(tabledata1_exp, -c(row.id,TaskforceAllocation,FulltextTopicIdentifier,
                                            FulltextReviewer.initial,volume,number,pages,Timestamp,
                                            FulltextReviewer,GeneticNotes,What.is.the.scale.of.the.shift,
                                            What.is.the.direction.of.shift,What.factors.are.driving.that.shift,
                                            What.is.the.ecological.impact.of.the.interaction.and.shift.of.that.interaction,
                                            Is.there.a.connection.to.the.NC3,Reviewer.notes,
                                            Reviewer.outstanding.concerns,ReviewerDecision,ReviewerDecisionReason,
                                            ReviewerDecision.checked.by,FinalDecision,FinalDecisionReason,FinalDecisionNotes))
table1 <- tabledata1_exp %>% gt()




#Visualising the Scales of interactions ----



#Visualising the Types of interations used ----



#Visualising the relevant traits ----



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

