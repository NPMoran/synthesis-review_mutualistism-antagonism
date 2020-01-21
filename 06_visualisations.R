##########_____Mutualism/Antagonism Synthesis Review_____#########
Sys.setenv(LANG = "en")
library(ggplot2); library(tidyverse)

#Word cloud_abstract titles ----
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Importing and formatting the files
text <- readLines("MA-includedtitles.txt")
docs <- Corpus(VectorSource(text))
inspect(docs)


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming #Only use if you want to reduce words to their root
# docs <- tm_map(docs, stemDocument) 

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


#Word cloud_abstract text ----
#Importing and formatting the files
text <- readLines("MA-includedabstracts.txt")
docs <- Corpus(VectorSource(text))
inspect(docs)


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming #Only use if you want to reduce words to their root
# docs <- tm_map(docs, stemDocument) 

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


#Time x interaction (included) ----
abstractsincluded <- read.csv("MA.abstractdecision_included.csv", strip.white = TRUE)
fulltextincluded <- read.csv("MA.fulltextdecision_included.csv", strip.white = TRUE)
fulltextincluded <- merge(fulltextincluded, abstractsincluded, by = "abstract.id", all.x = FALSE)

labels(fulltextincluded)
summary(fulltextincluded$year)
fulltextincluded <- rename(fulltextincluded, Scale = scale) #renaming scale to avoid confustion with the function

Fig.time <- NULL
Fig.time$Scale <- fulltextincluded$Scale
Fig.time$year <- fulltextincluded$year
Fig.time <- as.data.frame(Fig.time)

Fig.time1 <- ggplot(Fig.time, aes(x=year, stat(count)))+
  geom_density(color="darkblue", fill="lightblue") +
  labs(x = "Year of publication", y ="Number of publications")+
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=14, vjust = 0.1),
        axis.title.x  = element_text(size=14, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  xlim(1989, 2019)

Fig.time2 <- ggplot(Fig.time, aes(x=year, stat(count), fill=Scale)) +
  geom_density(position = "stack") +
  labs(x = "Year of publication", y ="Number of publications") +
  xlim(1989, 2019) +  
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=14, vjust = 0.1),
        axis.title.x  = element_text(size=14, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = c(0.15,0.7),
        legend.title = element_blank()) +
  scale_fill_manual(values=c("seashell2","seagreen2","lightblue"))

ggsave("Visualisations/Fig.time1.jpg", width = 16, height = 8, units = "cm", Fig.time1, dpi = 600)
ggsave("Visualisations/Fig.time2.jpg", width = 16, height = 8, units = "cm", Fig.time2, dpi = 600)


#Same but including maybes
abstractsincluded <- read.csv("MA.abstractdecision_included.csv", strip.white = TRUE)
fulltextincluded <- read.csv("MA.fulltextdecision_maybeincluded.csv", strip.white = TRUE)
fulltextincluded <- merge(fulltextincluded, abstractsincluded, by = "abstract.id", all.x = FALSE)

labels(fulltextincluded)
summary(fulltextincluded$year)
fulltextincluded <- rename(fulltextincluded, Scale = scale) #renaming scale to avoid confustion with the function

Fig.time <- NULL
Fig.time$Scale <- fulltextincluded$Scale
Fig.time$year <- fulltextincluded$year
Fig.time <- as.data.frame(Fig.time)

Fig.time1a <- ggplot(Fig.time, aes(x=year, stat(count)))+
  geom_density(color="darkblue", fill="lightblue") +
  labs(x = "Year of publication", y ="Number of publications")+
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=14, vjust = 0.1),
        axis.title.x  = element_text(size=14, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  xlim(1989, 2019)

Fig.time2a <- ggplot(Fig.time, aes(x=year, stat(count), fill=Scale)) +
  geom_density(position = "stack") +
  labs(x = "Year of publication", y ="Number of publications") +
  xlim(1989, 2019) + 
  theme(axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        axis.title.y  = element_text(size=14, vjust = 0.1),
        axis.title.x  = element_text(size=14, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = c(0.15,0.7),
        legend.title = element_blank()) +
  scale_fill_manual(values=c("seashell2","seagreen2","lightblue"))


#Visualising the Scales of interactions ----

#Visualising the Types of interations used ----

#Visualising the relevant traits ----

#Time x scale ----


