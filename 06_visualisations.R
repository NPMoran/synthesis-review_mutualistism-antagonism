##########_____Mutualism/Antagonism Synthesis Review_____#########

install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Creating a word cloud from included abstract titles ----
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


#Creating a word cloud from included abstracts ----
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
