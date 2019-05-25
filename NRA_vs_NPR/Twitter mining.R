# Jason Dean
# Feb 26, 2017
# This script pulls 2000 tweets containing either #NRA or #NPR from Twitter and performs sentiment analysis.
# More info at my website:  jasontdean.com

library(knitr)
library(twitteR)
library("ROAuth")

consumer_key <- 'your key'
consumer_secret <- 'your secret'
access_token <- 'your token'
access_secret <- 'your secret'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

nra <- searchTwitter("#NRA", n=2000, lang='en')
npr <- searchTwitter("#NPR", n=2000, lang='en')
sunshine <- searchTwitter("#sunshine", n=2000, lang='en')

library(tm)
library(wordcloud)
library(RColorBrewer)

# extract text from tweets
nra.text = sapply(nra, function(x) x$getText())
npr.text = sapply(npr, function(x) x$getText())
sunshine.text = sapply(sunshine, function(x) x$getText())

# remove non-ascii characters and convert to lowercase
nra.text <- iconv(nra.text, "latin1", "ASCII", sub="")
nra.text <- tolower(nra.text)

npr.text <- iconv(npr.text, "latin1", "ASCII", sub="")
npr.text <- tolower(npr.text)

sunshine.text <- iconv(sunshine.text, "latin1", "ASCII", sub="")
sunshine.text <- tolower(sunshine.text)

# remove 'http'
nra.text <- gsub('http.* *', '', nra.text)
npr.text <- gsub('http.* *', '',npr.text)
sunshine.text <- gsub('http.* *', '',sunshine.text)

# create a Corpus
nra.corp <- Corpus(VectorSource(nra.text))
npr.corp <- Corpus(VectorSource(npr.text))
sunshine.corp <- Corpus(VectorSource(sunshine.text))

nra.data = TermDocumentMatrix(nra.corp, control = list(stemming = TRUE, removePunctuation = TRUE, stopwords = c("the", "nra", stopwords("english")),removeNumbers = TRUE, stripWhitespace = TRUE))

npr.data = TermDocumentMatrix(npr.corp, control = list(stemming = TRUE, removePunctuation = TRUE, stopwords = c("the", "npr", stopwords("english")),removeNumbers = TRUE, stripWhitespace = TRUE))

sunshine.data = TermDocumentMatrix(sunshine.corp, control = list(stemming = TRUE, removePunctuation = TRUE, stopwords = c("the", "sunshine", "sunshin", stopwords("english")),removeNumbers = TRUE, stripWhitespace = TRUE))

# NRA wordcloud
nra.matrix <- as.matrix(nra.data)
nra.word_freqs = sort(rowSums(nra.matrix), decreasing=TRUE)
nra.df <- data.frame(word=names(nra.word_freqs), freq=nra.word_freqs)

wordcloud(nra.df$word, nra.df$freq, scale=c(5,0.5), random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

#  NPR wordcloud
npr.matrix <- as.matrix(npr.data)
npr.word_freqs = sort(rowSums(npr.matrix), decreasing=TRUE)
npr.df <- data.frame(word=names(npr.word_freqs), freq=npr.word_freqs)

wordcloud(npr.df$word, npr.df$freq, scale=c(5,0.5), random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

#  sunshine wordcloud
sunshine.matrix <- as.matrix(sunshine.data)
sunshine.word_freqs = sort(rowSums(sunshine.matrix), decreasing=TRUE)
sunshine.df <- data.frame(word=names(sunshine.word_freqs), freq=sunshine.word_freqs)

wordcloud(sunshine.df$word, sunshine.df$freq, scale=c(5,0.5), random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

# Word Frequency Analysis and Association

# NRA
kable(head(as.data.frame(nra.word_freqs, 5)), format="html", align = 'c')
# NPR
kable(head(as.data.frame(npr.word_freqs, 5)), format="html", align = 'c')
# sunshine
kable(head(as.data.frame(sunshine.word_freqs, 5)), format="html", align = 'c')

nra.america <- findAssocs(nra.data, 'america', 0.25)
npr.america <- findAssocs(npr.data, 'america', 0.25)
sunshine.america <- findAssocs(sunshine.data, 'america', 0.25)

# NRA
kable(head(as.data.frame(nra.america)), format="html", align = 'c')

# NPR
kable(head(as.data.frame(npr.america)), format="html", align = 'c')

# sunshine
kable(head(as.data.frame(sunshine.america)), format="html", align = 'c')

nra.love <- findAssocs(nra.data, 'love', 0.30)
npr.love <- findAssocs(npr.data, 'love', 0.30)
sunshine.love <- findAssocs(sunshine.data, 'love', 0.30)

# NRA
kable(head(as.data.frame(nra.love)), format="html", align = 'c')

# NPR
kable(head(as.data.frame(npr.love)), format="html", align = 'c')

# sunshine
kable(head(as.data.frame(sunshine.love)), format="html", align = 'c')

nra.trump <- findAssocs(nra.data, 'trump', 0.2)
npr.trump <- findAssocs(npr.data, 'trump', 0.2)
sunshine.trump <- findAssocs(sunshine.data, 'trump', 0.2)

# NRA
kable(head(as.data.frame(nra.trump)), format="html", align = 'c')

# NPR
kable(head(as.data.frame(npr.trump)), format="html", align = 'c')

# sunshine
kable(head(as.data.frame(sunshine.trump)), format="html", align = 'c')

library(dplyr)

# NRA
nra2 <- as.data.frame(nra.matrix)
# calculate the standard deviations of each word across tweets
nra.stdev <- as.numeric(apply(nra2, 1, sd))
nra2$stdev <- nra.stdev
# filter out words that have a standard deviation equal to zero
nra2 <- nra2 %>% filter(stdev>0)
nra2 <- nra2[,-2001]

# NPR
npr2 <- as.data.frame(npr.matrix)
# calculate the standard deviations of each word across tweets
npr.stdev <- as.numeric(apply(npr2, 1, sd))
npr2$stdev <- npr.stdev
# filter out words that have a standard deviation equal to zero
npr2 <- npr2 %>% filter(stdev>0)
npr2 <- npr2[,-2001]

# NRA
nra.corr <- cor(nra2)
nra.corr[is.na(nra.corr)] <- 0
nra.corr[nra.corr == 1] <- 0
# find the highest correlation coefficient 
nra.max <- as.matrix(nra.corr[as.numeric(which(nra.corr > 0.95 & nra.corr < 0.99))])
nra.max <- sort(nra.max, decreasing = TRUE)
nra.maximum <- nra.max[1]
# find where this maximum occurs in the correlation matrix
nra.loc <- which(nra.corr == nra.maximum, arr.ind = TRUE)
# and last find what words this correlation coeffient is calculated from
nra.words <- row.names(nra.matrix)
nra.top2 <- c(nra.words[nra.loc[1,1]], nra.words[nra.loc[1,2]])

# NPR
npr.corr <- cor(npr2)
npr.corr[is.na(npr.corr)] <- 0
npr.corr[npr.corr == 1] <- 0
# find the highest correlation coefficient 
npr.max <- as.matrix(npr.corr[as.numeric(which(npr.corr > 0.95 & npr.corr < 0.99))])
npr.max <- sort(npr.max, decreasing = TRUE)
npr.maximum <- npr.max[1]
# find where this maximum occurs in the correlation matrix
npr.loc <- which(npr.corr == npr.maximum, arr.ind = TRUE)
# and last find what words this correlation coeffient is calculated from
npr.words <- row.names(npr.matrix)
npr.top2 <- c(npr.words[npr.loc[1,1]], npr.words[npr.loc[1,2]])

# "Top two associated words in the NRA tweet data set""
nra.top2

# "Top two associated words in the NPR tweet data set""
npr.top2

# Sentiment Analysis

# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon

positive <- readLines("positive-words.txt")
negative <- readLines("negative-words.txt")

nra.df$positive <- match(nra.df$word, positive)
npr.df$positive <- match(npr.df$word, positive)
sunshine.df$positive <- match(sunshine.df$word, positive)

nra.df$negative <- match(nra.df$word, negative)
npr.df$negative <- match(npr.df$word, negative)
sunshine.df$negative <- match(sunshine.df$word, negative)

nra.df[is.na(nra.df)] <- 0
nra.df$positive[nra.df$positive != 0] <- 1
nra.df$negative[nra.df$negative != 0] <- 1

npr.df[is.na(npr.df)] <- 0
npr.df$positive[npr.df$positive != 0] <- 1
npr.df$negative[npr.df$negative != 0] <- 1

sunshine.df[is.na(sunshine.df)] <- 0
sunshine.df$positive[sunshine.df$positive != 0] <- 1
sunshine.df$negative[sunshine.df$negative != 0] <- 1

library(ggplot2)
library(reshape2)

nra.positive <- sum((nra.df$positive*nra.df$freq))/sum(nra.df$freq)
nra.negative <- sum((nra.df$negative*nra.df$freq))/sum(nra.df$freq)

npr.positive <- sum((npr.df$positive*npr.df$freq))/sum(npr.df$freq)
npr.negative <- sum((npr.df$negative*npr.df$freq))/sum(npr.df$freq)

sunshine.positive <- sum((sunshine.df$positive*sunshine.df$freq))/sum(sunshine.df$freq)
sunshine.negative <- sum((sunshine.df$negative*sunshine.df$freq))/sum(sunshine.df$freq)

# format the data for plotting
nra.sents <- data.frame(positive = nra.positive, negative = nra.negative)
npr.sents <- data.frame(positive = npr.positive, negative = npr.negative)
sunshine.sents <- data.frame(positive = sunshine.positive, negative = sunshine.negative)
sentiments <- rbind(nra.sents, npr.sents, sunshine.sents)
names <- c("#NRA", "#NPR", "#sunshine")
sentiments$tweets <- names
#row.names(sentiments) <- names
sentiments.m <- melt(sentiments)
colnames <- c("tweets", "sentiment", "fraction")
colnames(sentiments.m) <- colnames

# plot the data
ggplot(data=sentiments.m, aes(tweets, fraction, fill=sentiment)) + geom_bar(stat='identity') + ylab("fraction of tweets") + xlab("") + theme_bw()

sentiments$ratio <- sentiments$positive/sentiments$negative
head(sentiments[,3:4])
