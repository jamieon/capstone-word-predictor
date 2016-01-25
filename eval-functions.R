require(dplyr, warn.conflicts = FALSE)
require(data.table, warn.conflicts = FALSE)
require(hashr)

# load("models\\training-dictionary.RData")
# load("models\\training-bigram-model.RData")
# load("models\\training-trigram-model.RData")
# load("models\\training-quadgram-model.RData")


DLookup <- function(x) {
  # Looks up a word from training.dictionary, which must be loaded
  #
  # Args:
  #   x: A hash of a word in dictionary 
  #
  # Returns:
  #   The corresponding word
  
  training.dictionary[data.table(x), word]
}


InterpolateModels <- function(hash.trigram) {
  # Predicts a single word from a vector of three words, interpolated from the
  # bigram, trigram and quadgram models 
  #
  # Args:
  #   tokens: A vector of three words 
  #
  # Returns:
  #   A single word
  
  # These coefficients have been tuned
  lambda.trigram <- 0.8
  lambda.quadgram <- 0.6

  row.bigram <- unlist(bigram.model[word.1 == hash.trigram[3],
                                    2:11, with = FALSE])
  row.trigram <- unlist(trigram.model[word.1 == hash.trigram[3] &
                                       word.2 == hash.trigram[2], 
                                      3:12, with = FALSE])
  row.quadgram <- unlist(quadgram.model[word.1 == hash.trigram[3] & 
                                         word.2 == hash.trigram[2] &
                                         word.3 == hash.trigram[1],
                                        4:13, with = FALSE])
  if (length(row.quadgram) == 0){
    row.quadgram = rep(0, 10)
  }
  if (length(row.trigram) == 0){
    row.trigram = rep(0, 10)
  }

  matrix.bigram <- matrix(row.bigram, ncol = 2) 
  matrix.trigram <- matrix(row.trigram, ncol = 2) 
  matrix.quadgram <- matrix(row.quadgram, ncol = 2) 
  
  
  # Combine into a single data.frame ------------------------------------------
  answer <- data.frame(word = character(15), quadgram = numeric(15), 
                   trigram = numeric(15), bigram = numeric(15),
                   stringsAsFactors = FALSE) 
  answer$word[1:5] <- DLookup(matrix.bigram[, 1])
  answer$word[6:10] <- DLookup(matrix.trigram[, 1])
  answer$word[11:15] <- DLookup(matrix.quadgram[, 1])
  answer$bigram[1:5] <- matrix.bigram[, 2]
  answer$trigram[6:10] <- matrix.trigram[, 2]
  answer$quadgram[11:15] <- matrix.quadgram[, 2]
  
  answer <- answer %>%
    group_by(word) %>%
    summarise(quadgram = max(quadgram),
              trigram = max(trigram),
              bigram = max(bigram)) %>%
    filter(!is.na(word))

   
  # Calculate the interpolated probability ------------------------------------
  if(any(answer$quadgram != 0)) {
    answer$interpolated <- lambda.quadgram * answer$quadgram +
      (1 - lambda.quadgram) * lambda.trigram * answer$trigram +
      (1 - lambda.quadgram) * (1 - lambda.trigram) * answer$bigram
  } else {
    if(any(answer$trigram != 0)) {
      answer$interpolated <- lambda.trigram * answer$trigram + 
        (1 - lambda.trigram) * answer$bigram
    } else {
      answer$interpolated <- answer$bigram
    }
  }
  answer <- answer[order(answer$interpolated, decreasing = TRUE), ]
  answer$interpolated <- round(answer$interpolated, 3)

  answer
}


# Decode ngram models ---------------------------------------------------------
# t <- trigram.model[!is.na(answer_5)]
# t2 <- t[1:1000]
# sea <- mutate(sea,
#              word.1 = DLookup(word.1), 
#              word.2 = DLookup(word.2), 
#              word.3 = DLookup(word.3), 
#              answer_1 = DLookup(answer_1), 
#              answer_2 = DLookup(answer_2), 
#              answer_3 = DLookup(answer_3), 
#              answer_4 = DLookup(answer_4), 
#              answer_5 = DLookup(answer_5)) 
# 
# v <- val.results[!is.na(answer_5)]
# v2 <- v[1:1000]
# v3 <- mutate(v2,
#              word.1 = DLookup(word.1), 
#              word.2 = DLookup(word.2), 
#              word.3 = DLookup(word.3), 
#              answer_1 = DLookup(answer_1), 
#              answer_2 = DLookup(answer_2), 
#              answer_3 = DLookup(answer_3), 
#              answer_4 = DLookup(answer_4), 
#              answer_5 = DLookup(answer_5),
#              outcome = DLookup(outcome))