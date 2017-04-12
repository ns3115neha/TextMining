# Global setups

library(tm)
library(wordcloud)
library(Rstem)
library(syuzhet)
library(lda)
library(topicmodels)
library(cluster)
library(ggplot2)
library(Rgraphviz)
library(ggfortify)
library(stringr)  # To clean Tweeter data for @ and other charachters 
library(ape)
library(igraph)
library(networkD3)
library(visNetwork)
library(linkcomm)
library(twitteR)
library(RCurl)
library(stringr)
library(plotrix)


#setwd("C:/Users/neha.sharma/Desktop/RModels/ShinyApplication")
#data1 <- read.csv("dataforshiny.csv", stringsAsFactors = FALSE)
#data1 <- data1[,"Full.Text"]


####  WORD CLOUD , TOPIC MODELLING , SENTIMENT ANALYSIS , Word Frequency( Most frequent words), Correlation Analysis  #########
###############################################################################################################################


# Grab text data

r_stats_text_corpus <- Corpus(VectorSource(data1))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, stripWhitespace)
r_stats_text_corpus <- tm_map(r_stats_text_corpus,stemDocument)

##Topic Moddeling##
tdm <- TermDocumentMatrix(r_stats_text_corpus,control = list(wordLenghts = c(1,Inf)))
tdm2 <- removeSparseTerms(tdm, sparse = 0.994)
#tdm <- TermDocumentMatrix(r_stats_text_corpus)
dtm <- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm, 1, sum)
dtm.new <- dtm[rowTotals > 0, ]
lda <- LDA(dtm.new, k=6)
term <- terms(lda,4)
term
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
topic <- topics(lda, 1)
topics  <- data.frame(topic)
topics_new <- cbind("Topics" = rownames(topics$topics))



#Inspecting the Frequent Words

freq.terms <- findFreqTerms(tdm, lowfreq = 15)
#length(freq.terms)#530 words
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 15)
df <- data.frame(term = names(term.freq), freq = term.freq)
df <- df[order(-df$freq),c(1,2)]  ##Sorted in Descending Order


##Sentiment Analysis'####
data1 <- as.character(data1)
data_raw_text<-get_nrc_sentiment(data1)
sum_data_raw_text <- data.frame(colSums(data_raw_text[1:10]))
names(sum_data_raw_text)[1] <- "count"
sum_data_raw_text <- cbind("sentiment" = rownames(sum_data_raw_text), sum_data_raw_text)
rownames(sum_data_raw_text) <- NULL
senti_data_analysis <- sum_data_raw_text[1:10,]



###Hierachial Clustering 

tdm2 <- removeSparseTerms(tdm, sparse = 0.994) #Only 7 Terms Remain at sparse= 0.95 , ok value 0.99 ##check for the sparse 
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D2")
groups <- cutree(fit, k=5)
hcd = as.dendrogram(fit)



####          Social Network Analysis /Word Network  ##########
###############################################################
tdm <- TermDocumentMatrix(r_stats_text_corpus,control = list(wordLenghts = c(1,Inf)))
idx <- which(dimnames(tdm)$Terms == "call")  ##change the terms to be searched 
tdm2 <- removeSparseTerms(tdm, sparse = 0.994)
m2 <- as.matrix(tdm2)
m2[m2>=1] <- 1
m2 <- m2 %*% t(m2)  ##Adjaceny Matrix
g <- graph.adjacency(m2, weighted=T, mode = "undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
#V(g)$degree <- degree(g)
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)











