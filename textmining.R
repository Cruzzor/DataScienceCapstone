library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)

cname <- file.path("C:/Users/mbergman/Documents/Privat/Coursera/Capstone/Project", "texts")

corp <- Corpus(DirSource(cname))

corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, content_transformer(tolower))

# corp <- tm_map(corp, removeWords, stopwords("english"))
