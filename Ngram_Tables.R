library(tm)
library(ngram)
library(readr)
library(RWeka)
library(dtplyr)
library(caret)
library(qdap)
library(tm)
library(ngram)
library(readr)
library(RWeka)
library(data.table)
library(stringr)

##Downloading data
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


##Sampling the data (This time 60%)
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

setwd(loc)
if (!dir.exists("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset") == TRUE) {
        dir.create("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset")
}
setwd("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset")
write.csv(blogs_train, file = "en_us.blogs_training.csv")
write.csv(news_train, file = "en_us.news_training.csv")
write.csv(twitter_train, file = "en_us.twitter_training.csv")

setwd(loc)
if (!dir.exists("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/testset") == TRUE) {
        dir.create("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/testset")
}
setwd("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/testset")
write.csv(blogs_test, file = "en_us.blogs_test.csv")
write.csv(news_test, file = "en_us.news_test.csv")
write.csv(twitter_test, file = "en_us.twitter_test.csv")

remove(blogs, twitter, news)


##Cleaning Data
setwd("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset")
corpus <- Corpus(DirSource("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset"))

##corpus_clean <- tm_map(corpus, content_transformer(strip))

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)

profanity <- readLines(file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"))
close(file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"))
corpus_clean <- tm_map(corpus, removeWords, profanity)

##Tokenize ngrams

corpus_clean <- unlist(corpus_clean)
corpus <- unlist(corpus)
write.csv(corpus, file = "cleaned_training_corpus.csv")

##Can start from here
library(tm)
library(ngram)
library(readr)
library(RWeka)
library(data.table)
library(stringr)
library(dplyr)
setwd("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/trainingset")

corpus <- read.csv("cleaned_training_corpus.csv", colClasses = c("NULL", NA), header = TRUE)
corpus <- as.vector(corpus$x)

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
corpus_chunk <- chunk2(corpus, 2)
corpus1 <- corpus_chunk[1]
corpus2 <- corpus_chunk[2]

corpus2 <- unlist(corpus2)
corpus1 <- unlist(corpus1)

##Tokenizing, creating ngram tables sorted by frequency
if (!dir.exists("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/ngram_tables") == TRUE) {
        dir.create("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/ngram_tables")
}
setwd("~/Dropbox/Data_Science/Swiftkey_Project/final/en_US/ngram_tables")

##Create 2gram table
bigram <- ngram_asweka(toString(corpus), min = 2, max = 2, sep = " ")
bigram_table <- data.table(sort(table(bigram), decreasing = TRUE))
bigram_table_split <- data.table(str_split_fixed(bigram_table$bigram, " ", 2), bigram_table$N)
colnames(bigram_table_split) <- c("Word1", "LastWord", "N")
bigram_table_split <- filter(bigram_table_split, N >= 4)
setkey(bigram_table_split, "Word1", "LastWord")


bigram_table <- filter(bigram_table, !(Word1 == "," | LastWord == ","))
bigram_table <- filter(bigram_table, !(Word1 == "\u0095" | LastWord == "\u0095"))
bigram_table <- filter(bigram_table, !(Word1 == "\u0096" | LastWord == "\u0096"))
bigram_table <- filter(bigram_table, !(Word1 == "\u0097" | LastWord == "\u0097"))
bigram_table <- filter(bigram_table, !(Word1 == "\u0094" | LastWord == "\u0094"))
bigram_table <- filter(bigram_table, !(Word1 == "¾" | LastWord == "¾"))
bigram_table <- filter(bigram_table, !(Word1 == "¼" | LastWord == "¼"))
bigram_table <- filter(bigram_table, !(Word1 == "½" | LastWord == "½"))



bigram_table <- filter(bigram_table, grepl("[[:alpha:]]", Word1))
bigram_table <- filter(bigram_table, grepl("[[:alpha:]]", LastWord))

bigram_table <- filter(bigram_table, !grepl(",", Word1))
bigram_table <- filter(bigram_table, !grepl(",", LastWord))

bigram_table <- filter(bigram_table, N>=5)

write.csv(bigram_table, file = "2gram_table.csv")

##Create and save trigram table

trigram <- ngram_asweka(toString(corpus1), min = 3, max = 3, sep = " ")
trigram2 <- ngram_asweka(toString(corpus2), min = 3, max = 3, sep = " ")
trigram_table1 <- table(trigram)
trigram_table2 <- table(trigram2)
trigram_table1_short <- data.table(trigram_table1[trigram_table1>3])
trigram_table2_short <- data.table(trigram_table2[trigram_table2>3])
colnames(trigram_table2_short) <- c("trigram", "N")
trigram_table_short <- rbind(trigram_table1_short, trigram_table2_short)

trigram_table_split <- data.table(str_split_fixed(trigram_table_short$trigram, " ", 3), trigram_table_short$N)
colnames(trigram_table_split) <- c("Word1", "Word2", "LastWord", "N")

trigram_table <- filter(trigram_table_split, !(Word1 == "," | Word2 == "," | LastWord == ","))
trigram_table <- filter(trigram_table, !(Word1 == "\u0095" | Word2 == "\u0095" | LastWord == "\u0095"))
trigram_table <- filter(trigram_table, !(Word1 == "\u0096" | Word2 == "\u0096" | LastWord == "\u0096"))
trigram_table <- filter(trigram_table, !(Word1 == "\u0097" | Word2 == "\u0097" | LastWord == "\u0097"))
trigram_table <- filter(trigram_table, !(Word1 == "¾" | Word2 == "¾" | LastWord == "¾"))
trigram_table <- filter(trigram_table, !(Word1 == "¼" | Word2 == "¼" | LastWord == "¼"))
trigram_table <- filter(trigram_table, !(Word1 == "½" | Word2 == "½" | LastWord == "½"))

trigram_table <- filter(trigram_table, grepl("[[:alpha:]]", Word1))
trigram_table <- filter(trigram_table, grepl("[[:alpha:]]", Word2))
trigram_table <- filter(trigram_table, grepl("[[:alpha:]]", LastWord))

trigram_table <- filter(trigram_table, !grepl(",", Word1))
trigram_table <- filter(trigram_table, !grepl(",", Word2))
trigram_table <- filter(trigram_table, !grepl(",", LastWord))

trigram_table <- data.table(filter(trigram_table, N >= 5))

trigram_table_split <- data.table(filter(trigram_table_split, N >= 4))
setkey(trigram_table, "Word1", "Word2", "LastWord")
write.csv(trigram_table, file = "3gram_table.csv")

##Create and save fourgram table

fourgram <- ngram_asweka(toString(corpus), min = 4, max = 4, sep = " ")
fourgram_table <- data.table(sort(table(fourgram), decreasing = TRUE))
colnames(fourgram_table) <- c("Phrase", "N")
fourgram_table_short <- data.table(filter(fourgram_table, N >= 4))
fourgram_table_split <- data.table(str_split_fixed(fourgram_table_short$Phrase, " ", 4), fourgram_table_short$N)
colnames(fourgram_table_split) <- c("Word1", "Word2", "Word3", "LastWord", "N")
setkey(fourgram_table_split, "Word1", "Word2", "Word3", "LastWord")

fourgram_table <- filter(fourgram_table, !(Word1 == "," | Word2 == "," | Word3 == "," | LastWord == ","))
fourgram_table <- filter(fourgram_table, !(Word1 == "\u0095" | Word2 == "\u0095" | Word3 == "\u0095" | LastWord == "\u0095"))
fourgram_table <- filter(fourgram_table, !(Word1 == "\u0096" | Word2 == "\u0096" | Word3 == "\u0096" | LastWord == "\u0096"))
fourgram_table <- filter(fourgram_table, !(Word1 == "\u0097" | Word2 == "\u0097" | Word3 == "\u0097" | LastWord == "\u0097"))
fourgram_table <- filter(fourgram_table, !(Word1 == "¾" | Word2 == "¾" | Word3 == "¾" | LastWord == "¾"))
fourgram_table <- filter(fourgram_table, !(Word1 == "¼" | Word2 == "¼" | Word3 == "¾" | LastWord == "¼"))
fourgram_table <- filter(fourgram_table, !(Word1 == "½" | Word2 == "½" | Word3 == "¾" | LastWord == "½"))

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
fivegram_table_split <- data.table(str_split_fixed(fivegram_table_short$fivegram, " ", 5), fivegram_table_short$N)
colnames(fivegram_table_split) <- c("Word1", "Word2", "Word3", "Word4", "LastWord", "N")

fivegram_table <- filter(fivegram_table, !(Word1 == "," | Word2 == "," | Word3 == "," | Word4 == "," | LastWord == ","))
fivegram_table <- filter(fivegram_table, !(Word1 == "\u0095" | Word2 == "\u0095" | Word3 == "\u0095" | Word4 == "\u0095" | LastWord == "\u0095"))
fivegram_table <- filter(fivegram_table, !(Word1 == "\u0096" | Word2 == "\u0096" | Word3 == "\u0096" | Word4 == "\u0096"| LastWord == "\u0096"))
fivegram_table <- filter(fivegram_table, !(Word1 == "\u0097" | Word2 == "\u0097" | Word3 == "\u0097" | Word4 == "\u0097"| LastWord == "\u0097"))
fivegram_table <- filter(fivegram_table, !(Word1 == "¾" | Word2 == "¾" | Word3 == "¾" | Word4 == "¾"| LastWord == "¾"))
fivegram_table <- filter(fivegram_table, !(Word1 == "¼" | Word2 == "¼" | Word3 == "¾" | Word4 == "¾"| LastWord == "¼"))
fivegram_table <- filter(fivegram_table, !(Word1 == "½" | Word2 == "½" | Word3 == "¾" | Word4 == "¾"| LastWord == "½"))

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



##Create 5-gram table
fivegram_table_split <- data.table(str_split_fixed(fivegram_table$fivegram, " ", 5), fivegram_table$N)
colnames(fivegram_table_split) <- c("Word1", "Word2", "Word3", "Word4", "LastWord", "N")
fivegram_table_split <- data.table(filter(fivegram_table_split, N >= 2))
setkey(fivegram_table_split, "Word1", "Word2", "Word3", "Word4", "LastWord")


##Write load function - turn into data.table

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


##Lookup Function - Stupid Backoff

discount <- 0.5
predict <- function(input) {
        if (!class(input) == "character") {print("Please enter only words")} else {
        split <- str_split(input, " ")
        split <- unlist(split)
        look4 <- unlist(str_split(word(input, start = -4, end = -1), " ", 4))
        look3 <- unlist(str_split(word(input, start = -3, end = -1), " ", 3))
        look2 <- unlist(str_split(word(input, start = -2, end = -1), " ", 2))
        look1 <- unlist(word(input, -1))
        results <- data.frame()
        ##Looking for 5-gram from 4-gram input, in 5-gram table
        if (length(split) >= 4) {
                fivegram_results <- fivegram_table[.(look4[1], look4[2], look4[3], look4[4])]
                fivegram_total <- sum(fivegram_results$N)
                fivegram_results <- select(arrange(mutate(fivegram_results, scores = N/fivegram_total), -scores), LastWord, scores)
                results <- rbind(results, fivegram_results)
                if (nrow(results) >=5) {print(results[1:5,])}
                        else {
                        ##Looking for 4-gram
                        ##delete NAs
                        fourgram_results <- fourgram_table[.(look3[1], look3[2], look3[3])]
                        fourgram_total <- sum(fourgram_results$N)
                        fourgram_results <- select(arrange(mutate(fourgram_results, scores = discount*N/fourgram_total), -scores), LastWord, scores)
                        ##delete duplicates 
                        fourgram_results <- filter(fourgram_results, !(fourgram_results$LastWord %in% results$LastWord))
                        results <- results[!anyNA(results)]
                        results <- rbind(results, fourgram_results)
                        results <- arrange(results, -scores) 
                        if (nrow(results) >= 5) {print(results[1:5,])
                        } else {
                                ##Looking for trigram
                                trigram_results <- trigram_table[.(look2[1], look2[2])]
                                trigram_total <- sum(trigram_results$N)
                                trigram_results <- select(arrange(mutate(trigram_results, scores = discount*discount*N/trigram_total), -scores), LastWord, scores)
                                ##delete duplicates
                                trigram_results <- filter(trigram_results, !(trigram_results$LastWord %in% results$LastWord))
                                results <- results[!anyNA(results)]
                                results <- rbind(results, trigram_results)
                                
                                if (nrow(results) >= 5) {print(results[1:5,])
                                } else {
                                        ##Looking for bigram
                                        bigram_results <- bigram_table[.(look1[1])]
                                        bigram_total <- sum(bigram_results$N)
                                        bigram_results <- select(arrange(mutate(bigram_results, scores = discount*discount*discount*N/bigram_total), -scores), LastWord, scores)
                                        bigram_results <- filter(bigram_results, !(bigram_results$LastWord %in% results$LastWord))
                                        results <- results[!anyNA(results)]
                                        results <- rbind(results, bigram_results)   
                                        print(results[1:5,])
                                } 
                        }
                        }
        } else if (length(split) == 3){
                ##Looking for 4gram from 3gram input, in 4gram table
                        fourgram_results <- fourgram_table[.(look3[1], look3[2], look3[3])]
                        fourgram_total <- sum(fourgram_results$N)
                        fourgram_results <- select(arrange(mutate(fourgram_results, scores = discount*N/fourgram_total), -scores), LastWord, scores)
                        ##delete duplicates 
                        results <- results[!anyNA(results)]
                        results <- rbind(results, fourgram_results)
                        results <- arrange(results, -scores) 
                        if (nrow(results) >= 5) {print(results[1:5,])
                        } else {
                                ##Looking for trigram
                                trigram_results <- trigram_table[.(look2[1], look2[2])]
                                trigram_total <- sum(trigram_results$N)
                                trigram_results <- select(arrange(mutate(trigram_results, scores = discount*discount*N/trigram_total), -scores), LastWord, scores)
                                ##delete duplicates
                                trigram_results <- filter(trigram_results, !(trigram_results$LastWord %in% results$LastWord))
                                results <- results[!anyNA(results)]
                                results <- rbind(results, trigram_results)
                                if (nrow(results) >= 5) {print(results[1:5,])
                                } else {
                                        ##Looking for bigram
                                        bigram_results <- bigram_table[.(look1[1])]
                                        bigram_total <- sum(bigram_results$N)
                                        bigram_results <- select(arrange(mutate(bigram_results, scores = discount*discount*discount*N/bigram_total), -scores), LastWord, scores)
                                        bigram_results <- filter(bigram_results, !(bigram_results$LastWord %in% results$LastWord))
                                        results <- results[!anyNA(results)]
                                        result <- rbind(results, bigram_results)   
                                        print(result[1:5,])
                                } 
                        }
                }
                 else if (length(split) == 2) {
                        ##Looking for 3gram from 2gram input, in 3gram table
                         ##Looking for trigram
                         trigram_results <- trigram_table[.(look2[1], look2[2])]
                         trigram_total <- sum(trigram_results$N)
                         trigram_results <- select(arrange(mutate(trigram_results, scores = discount*discount*N/trigram_total), -scores), LastWord, scores)
                         ##delete duplicates
                         trigram_results <- filter(trigram_results, !(trigram_results$LastWord %in% results$LastWord))
                         results <- results[!anyNA(results)]
                         results <- rbind(results, trigram_results)
                         if (nrow(results) >= 5) {print(results[1:5,])
                         } else {
                                 ##Looking for bigram
                                 bigram_results <- bigram_table[.(look2[1])]
                                 bigram_total <- sum(bigram_results$N)
                                 bigram_results <- select(arrange(mutate(bigram_results, scores = discount*discount*discount*N/bigram_total), -scores), LastWord, scores)
                                 bigram_results <- filter(bigram_results, !(bigram_results$LastWord %in% results$LastWord))
                                 results <- results[!anyNA(results)]
                                 result <- rbind(results, bigram_results)   
                                 print(result[1:5,])
                         } 
                 } 
                         else if (length(split == 1)) {
                                ##Looking for 2gram from 1gram input, in 2gram table
                                bigram_results <- bigram_table[.(look1[1])]
                                bigram_total <- sum(bigram_results$N)
                                bigram_results <- select(arrange(mutate(bigram_results, scores = discount*discount*discount*N/bigram_total), -scores), LastWord, scores)
                                bigram_results <- filter(bigram_results, !(bigram_results$LastWord %in% results$LastWord)
                                result <- rbind(results, bigram_results)   
                                print(result[1:5,])
                                }
        }
}
}
