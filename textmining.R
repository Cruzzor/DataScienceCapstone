library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)
library(tm) # For text mining
library(SnowballC) # For Stemming

############################
###### Preprocessing #######
############################

cname <- file.path("C:/Users/mbergman/Documents/Privat/Coursera/Capstone/DataScienceCapstone", "texts")
corp <- Corpus(DirSource(cname))

corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
naughtywords <- scan("en", what = "String")
corp <- tm_map(corp, removeWords, naughtywords)
#corp <- tm_map(corp, stemDocument)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, stripWhitespace)



############################
####### Exploration ########
############################
dtm <- DocumentTermMatrix(corp)
