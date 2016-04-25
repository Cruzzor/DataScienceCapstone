library(dplyr)
library(tm)
library(quanteda)

full_sample <- gsub("[^[:alnum:]' ]", " ", full_sample) %>% 
  removeNumbers() %>%
  tolower() %>%
  removeWords(naughtywords) %>%
  stripWhitespace()

#saveRDS(full_sample, file = "fullsample.Rda")

#corp <- corpus(full_sample)

#test <- "this is a test sentence for testing my new model."
#tokens <- tokenize(test,removePunct = TRUE, simplify = TRUE)

full_sample <- readRDS("fullsample.Rda")
set.seed(42)
full_sample <- sample(full_sample, size = length(full_sample) * 0.01, replace = FALSE)

tokens <- tokenize(paste(full_sample, collapse = ''), removePunct = FALSE)
unigrams <- quanteda::ngrams(tokens, n = 1)
unigrams <- unique(unigrams[[1]])
df <- tbl_df(data.frame(unigrams = unlist(unigrams)))
df <- count(df, unigrams) %>% filter(n > 1)


# Build CV-Set
set.seed(301)
twitter_sample <- sample(twitter, size = length(twitter) * 0.001, replace = FALSE)
blogs_sample <- sample(blogs, size = length(blogs) * 0.001, replace = FALSE)
news_sample <- sample(news, size = length(news) * 0.001, replace = FALSE)
full_sample <- c(twitter_sample, blogs_sample, news_sample)

## Hier: pre-processing wie oben
tokens <- tokenize(paste(full_sample, collapse = ''), removePunct = FALSE)
trigrams <- quanteda::ngrams(tokens, n = 3)
cv <- tbl_df(data.frame(trigrams = trigrams[[1]]))
cv$trigrams <- as.character(cv$trigrams)

cv <-separate(cv, col = trigrams, c("First", "Second", "Third"), sep = "_")
cv$predictedCorrectly = FALSE

# readRDS("cv_sample.Rda")

cv$predictedCorrectly <- FALSE
system.time(for (i in 1:5000) {
  print(i)
  first <- as.character(cv[i, 'First'])
  second <- as.character(cv[i, 'Second'])
  third <- as.character(cv[i, 'Third'])
  print(paste("Predicting:", first, second, third, sep = " "))
  prediction <- predict(paste(first, second, sep = " "))
  print(prediction)
  if (third %in% prediction) {
    cv[i, 'predictedCorrectly'] <- TRUE
  }
})
sum <- sum(cv$predictedCorrectly[1:5000])
print("----------")
print(paste("RESULT: Correct = ", sum, "; Incorrect = ", 5000 - sum, "; Percentage: ", sum/5000, sep = ""))




