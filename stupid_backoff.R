

predict <- function(input) {
        
        if (!class(input) == "character") {output <- "Please enter only words"} else {
                discount <- 0.5
                ##Clean Up Input
                input <- gsub("[[:punct:]]", "", input)
                input <- gsub("\\s+"," ",input)
                input <- gsub(" $","", input)
                input <- tolower(input)
                
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
                        if (nrow(results) >=5) {output <- results[1:5,]}
                        else {
                                ##Looking for 4-gram
                                ##delete NAs
                                fourgram_results <- fourgram_table[.(look3[1], look3[2], look3[3])]
                                fourgram_total <- sum(fourgram_results$N)
                                fourgram_results <- select(arrange(mutate(fourgram_results, scores = discount*N/fourgram_total), -scores), LastWord, scores)
                                ##delete duplicates 
                                fourgram_results <- filter(fourgram_results, !(fourgram_results$LastWord %in% results$LastWord))
                                results <- results[complete.cases(results), ]
                                results <- rbind(results, fourgram_results)
                                results <- arrange(results, -scores) 
                                if (nrow(results) >= 5) {output <- results[1:5,]
                                } else {
                                        ##Looking for trigram
                                        trigram_results <- trigram_table[.(look2[1], look2[2])]
                                        trigram_total <- sum(trigram_results$N)
                                        trigram_results <- select(arrange(mutate(trigram_results, scores = discount*discount*N/trigram_total), -scores), LastWord, scores)
                                        ##delete duplicates
                                        trigram_results <- filter(trigram_results, !(trigram_results$LastWord %in% results$LastWord))
                                        results <- results[complete.cases(results), ]
                                        results <- rbind(results, trigram_results)
                                        
                                        if (nrow(results) >= 5) {output <- results[1:5,]
                                        } else {
                                                ##Looking for bigram
                                                bigram_results <- bigram_table[.(look1[1])]
                                                bigram_total <- sum(bigram_results$N)
                                                bigram_results <- select(arrange(mutate(bigram_results, scores = discount*discount*discount*N/bigram_total), -scores), LastWord, scores)
                                                bigram_results <- filter(bigram_results, !(bigram_results$LastWord %in% results$LastWord))
                                                results <- results[complete.cases(results), ]
                                                results <- rbind(results, bigram_results)   
                                                output <- results[1:5,]
                                        } 
                                }
                        }
                } else if (length(split) == 3){
                        ##Looking for 4gram from 3gram input, in 4gram table
                        fourgram_results <- fourgram_table[.(look3[1], look3[2], look3[3])]
                        fourgram_total <- sum(fourgram_results$N)
                        fourgram_results <- select(arrange(mutate(fourgram_results, scores = discount*N/fourgram_total), -scores), LastWord, scores)
                        ##delete duplicates
                        results <- results[complete.cases(results), ]
                        ##Select only last word and score
                        results <- rbind(results, fourgram_results)
                        results <- arrange(results, -scores) 
                        if (nrow(results) >= 5) {output <- results[1:5,]
                        } else {
                                ##Looking for trigram
                                trigram_results <- trigram_table[.(look2[1], look2[2])]
                                trigram_total <- sum(trigram_results$N)
                                trigram_results <- select(arrange(mutate(trigram_results, scores = discount*discount*N/trigram_total), -scores), LastWord, scores)
                                ##delete duplicates
                                trigram_results <- filter(trigram_results, !(trigram_results$LastWord %in% results$LastWord))
                                results <- results[complete.cases(results), ]
                                results <- rbind(results, trigram_results)
                                if (nrow(results) >= 5) {output <- results[1:5,]
                                } else {
                                        ##Looking for bigram
                                        bigram_results <- bigram_table[.(look1[1])]
                                        bigram_total <- sum(bigram_results$N)
                                        bigram_results <- select(arrange(mutate(bigram_results, scores = discount*discount*discount*N/bigram_total), -scores), LastWord, scores)
                                        bigram_results <- filter(bigram_results, !(bigram_results$LastWord %in% results$LastWord))
                                        results <- results[complete.cases(results), ]
                                        results <- rbind(results, bigram_results)   
                                        output <- results[1:5,]
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
                        ##trigram_results <- filter(trigram_results, !(trigram_results$LastWord %in% results$LastWord))
                        results <- results[complete.cases(results), ]
                        results <- rbind(results, trigram_results)
                        if (nrow(results) >= 5) {output <- results[1:5,]
                        } else {
                                ##Looking for bigram
                                bigram_results <- bigram_table[.(look2[1])]
                                bigram_total <- sum(bigram_results$N)
                                bigram_results <- select(arrange(mutate(bigram_results, scores = discount*discount*discount*N/bigram_total), -scores), LastWord, scores)
                                bigram_results <- filter(bigram_results, !(bigram_results$LastWord %in% results$LastWord))
                                results <- results[complete.cases(results), ]
                                results <- rbind(results, bigram_results)   
                                output <- results[1:5,]
                        } 
                } 
                else if (length(split == 1)) {
                        ##Looking for 2gram from 1gram input, in 2gram table
                        bigram_results <- bigram_table[.(look1[1])]
                        bigram_total <- sum(bigram_results$N)
                        bigram_results <- select(arrange(mutate(bigram_results, scores = discount*discount*discount*N/bigram_total), -scores), LastWord, scores)
                        bigram_results <- filter(bigram_results, !(bigram_results$LastWord %in% results$LastWord))
                        results <- rbind(results, bigram_results)   
                        output <- results[1:5,]
                }
        }
        return(output)
}
