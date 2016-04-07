library(dplyr)
library(tm)
library(quanteda)

#full_sample <- gsub("[^[:alnum:]' ]", " ", full_sample) %>% 
#  removeNumbers() %>%
#  tolower() %>%
#  removeWords(naughtywords) %>%
#  stripWhitespace()

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


# Create DB
db <- dbConnect(SQLite(), dbname = "Test")
dbSendQuery(conn = db, 
            "CREATE TABLE unigram (
            id INTEGER NOT NULL,
            gram TEXT,
            PRIMARY KEY (id))")

# Write unigrams to table
for (i in 1:nrow(df)) {
  print(i)
  value <- df$unigrams[i]
  value <- gsub("'", "''", value) # Escape apostrophes
  dbSendQuery(conn = db, paste("INSERT INTO unigram (gram) VALUES ('", value, "')", sep = ""))
}


