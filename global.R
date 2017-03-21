##global.R loads all required packages and data for the app

library(shiny)
library(dplyr)
library(data.table)
library(stringr)
library(wordcloud)
library(tm)
library(qdap)
library(stats)

fivegram_table <- data.table(read.csv("data/5gram_table.csv"))
setkey(fivegram_table, "Word1", "Word2", "Word3", "Word4", "LastWord")

fourgram_table <- data.table(read.csv("data/4gram_table.csv"))
setkey(fourgram_table, "Word1", "Word2", "Word3", "LastWord")

trigram_table <- data.table(read.csv("data/3gram_table.csv"))
setkey(trigram_table, "Word1", "Word2", "LastWord")

bigram_table <- data.table(read.csv("data/2gram_table.csv"))
setkey(bigram_table, "Word1", "LastWord")
