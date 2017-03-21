library(tm)
library(ngram)
library(readr)
library(RWeka)
library(qdap)
library(data.table)
library(stringr)
library(dplyr)

##Downloading data from HC Corpora
loc <- "~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/"
setwd(loc)
loc_blogs <- "~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/en_US.blogs.txt"
loc_twitter <- "~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/en_US.twitter.txt"
loc_news <- "~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/en_US.news.txt"
loc <- "~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/"
blogs <- readLines(file(loc_blogs))
close(file(loc_blogs))
twitter <- readLines(file(loc_twitter))
close(file(loc_twitter))
news <- readLines(file(loc_news))
close(file(loc_news))


##Sampling 60% of the data for use in the lookup algorithm 
set.seed(123)

blogs_inTrain <- sample(1:length(blogs), size=0.6*length(blogs))
blogs_train <- blogs[blogs_inTrain]
blogs_test <- blogs[-blogs_inTrain]

news_inTrain <- sample(1:length(news), size=0.6*length(news))
news_train <- news[news_inTrain]
news_test <- news[-news_inTrain]

twitter_inTrain <- sample(1:length(twitter), size=0.6*length(twitter))
twitter_train <- twitter[twitter_inTrain]
twitter_test <- twitter[-twitter_inTrain]

##Writing samples to disk

setwd(loc)
if (!dir.exists("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset") == TRUE) {
        dir.create("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset")
}
setwd("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset")
write.csv(blogs_train, file = "en_us.blogs_training.csv")
write.csv(news_train, file = "en_us.news_training.csv")
write.csv(twitter_train, file = "en_us.twitter_training.csv")
remove(blogs, twitter, news)


##Cleaning the Data

setwd("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset")
corpus <- Corpus(DirSource("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset"))

##Remove Numbers, Punctuation, and superfluous whitespace, change all words to lowercase
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)

##Removing profanity according to list.

profanity <- readLines(file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"))
close(file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"))
corpus_clean <- tm_map(corpus, removeWords, profanity)

##Write cleaned data corpus to disk

corpus_clean <- unlist(corpus_clean)
corpus <- unlist(corpus)
write.csv(corpus, file = "cleaned_training_corpus.csv")

##Read in clean data data and split into two chunks for easier processing

setwd("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset")

corpus <- read.csv("cleaned_training_corpus.csv", colClasses = c("NULL", NA), header = TRUE)
corpus <- as.vector(corpus$x)

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
corpus_chunk <- chunk2(corpus, 2)
corpus1 <- corpus_chunk[1]
corpus2 <- corpus_chunk[2]

corpus2 <- unlist(corpus2)
corpus1 <- unlist(corpus1)

##Tokenize the corpus into n-grams, get n-gram frequency tables, clean tables, save all ngrams with frequency more than 4 to disk

if (!dir.exists("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/ngram_tables") == TRUE) {
        dir.create("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/ngram_tables")
}
setwd("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/ngram_tables")

##Create 2gram table
bigram <- ngram_asweka(toString(corpus), min = 2, max = 2, sep = " ")
bigram_table <- data.table(sort(table(bigram), decreasing = TRUE))
bigram_table_split <- data.table(str_split_fixed(bigram_table$bigram, " ", 2), bigram_table$N)
colnames(bigram_table_split) <- c("Word1", "LastWord", "N")
bigram_table <- filter(bigram_table_split, N >= 5)
setkey(bigram_table, "Word1", "LastWord")
bigram_table <- filter(bigram_table, grepl("[[:alpha:]]", Word1))
bigram_table <- filter(bigram_table, grepl("[[:alpha:]]", LastWord))
bigram_table <- filter(bigram_table, !grepl(",", Word1))
bigram_table <- filter(bigram_table, !grepl(",", LastWord))

write.csv(bigram_table, file = "2gram_table.csv")

##Create and save trigram table (trigram table uses the corpus in two different chunks)

trigram <- ngram_asweka(toString(corpus1), min = 3, max = 3, sep = " ")
trigram2 <- ngram_asweka(toString(corpus2), min = 3, max = 3, sep = " ")
trigram_table1 <- table(trigram)
trigram_table2 <- table(trigram2)
trigram_table1_short <- data.table(trigram_table1[trigram_table1>3])
trigram_table2_short <- data.table(trigram_table2[trigram_table2>3])
colnames(trigram_table2_short) <- c("trigram", "N")
trigram_table_short <- rbind(trigram_table1_short, trigram_table2_short)

trigram_table <- data.table(str_split_fixed(trigram_table_short$trigram, " ", 3), trigram_table_short$N)
colnames(trigram_table) <- c("Word1", "Word2", "LastWord", "N")

trigram_table <- filter(trigram_table, grepl("[[:alpha:]]", Word1))
trigram_table <- filter(trigram_table, grepl("[[:alpha:]]", Word2))
trigram_table <- filter(trigram_table, grepl("[[:alpha:]]", LastWord))

trigram_table <- filter(trigram_table, !grepl(",", Word1))
trigram_table <- filter(trigram_table, !grepl(",", Word2))
trigram_table <- filter(trigram_table, !grepl(",", LastWord))

trigram_table <- data.table(filter(trigram_table, N >= 5))

setkey(trigram_table, "Word1", "Word2", "LastWord")
write.csv(trigram_table, file = "3gram_table.csv")

##Create and save fourgram table

fourgram <- ngram_asweka(toString(corpus), min = 4, max = 4, sep = " ")
fourgram_table <- data.table(sort(table(fourgram), decreasing = TRUE))
colnames(fourgram_table) <- c("Phrase", "N")
fourgram_table_short <- data.table(filter(fourgram_table, N >= 4))
fourgram_table <- data.table(str_split_fixed(fourgram_table_short$Phrase, " ", 4), fourgram_table_short$N)
colnames(fourgram_table) <- c("Word1", "Word2", "Word3", "LastWord", "N")
setkey(fourgram_table, "Word1", "Word2", "Word3", "LastWord")

fourgram_table <- filter(fourgram_table, grepl("[:alpha:]", Word1))
fourgram_table <- filter(fourgram_table, grepl("[:alpha:]", Word2))
fourgram_table_smaller <- filter(fourgram_table, grepl("[:alpha:]", Word3))
fourgram_table_smaller <- filter(fourgram_table, grepl("[:alpha:]", LastWord))

fourgram_table <- filter(fourgram_table, !grepl(",", Word1))
fourgram_table <- filter(fourgram_table, !grepl(",", Word2))
fourgram_table <- filter(fourgram_table, !grepl(",", Word3))
fourgram_table <- filter(fourgram_table, !grepl(",", LastWord))

write.csv(fourgram_table, file = "4gram_table.csv")

##Create and save fivegram table
fivegram <- ngram_asweka(toString(corpus), min = 5, max = 5, sep = " ")
fivegram_table <- data.table(sort(table(fivegram), decreasing = TRUE))
fivegram_table <- table(fivegram)
fivegram_table_short <- fivegram_table[fivegram_table>3]
fivegram_table_short <- data.table(fivegram_table_short)
fivegram_table <- data.table(str_split_fixed(fivegram_table_short$fivegram, " ", 5), fivegram_table_short$N)
colnames(fivegram_table) <- c("Word1", "Word2", "Word3", "Word4", "LastWord", "N")

fivegram_table <- filter(fivegram_table, grepl("[[:alpha:]]", Word1))
fivegram_table <- filter(fivegram_table, grepl("[[:alpha:]]", Word2))
fivegram_table <- filter(fivegram_table, grepl("[[:alpha:]]", Word3))
fivegram_table <- filter(fivegram_table, grepl("[[:alpha:]]", Word4))
fivegram_table <- filter(fivegram_table, grepl("[[:alpha:]]", LastWord))

fivegram_table <- filter(fivegram_table, !grepl(",", Word1))
fivegram_table <- filter(fivegram_table, !grepl(",", Word2))
fivegram_table <- filter(fivegram_table, !grepl(",", Word3))
fivegram_table <- filter(fivegram_table, !grepl(",", Word4))
fivegram_table <- filter(fivegram_table, !grepl(",", LastWord))

setkey(fivegram_table_split, "Word1", "Word2", "Word3", "Word4", "LastWord")
write.csv(fivegram_table, file = "5gram_table.csv")

##The following code loads the n-gram tables and turns them into the data.table format.
setwd("~/Dropbox/Data_Science/Swiftkey_Project/Shiny_App/Next_Word_Predictor/data")

fivegram_table <- data.table(read.csv("5gram_table.csv"))
setkey(fivegram_table, "Word1", "Word2", "Word3", "Word4", "LastWord")

fourgram_table <- data.table(read.csv("4gram_table.csv"))
setkey(fourgram_table, "Word1", "Word2", "Word3", "LastWord")

trigram_table <- data.table(read.csv("3gram_table.csv"))
setkey(trigram_table, "Word1", "Word2", "LastWord")

bigram_table <- data.table(read.csv("2gram_table.csv"))
setkey(bigram_table, "Word1", "LastWord")


fivegram_table_nocomma <- filter(fivegram_table, !grepl(",", Word1))


