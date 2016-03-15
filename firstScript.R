library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)

blogs <- readLines("./en_US/en_US.blogs.txt", encoding = "UTF-8")
news <- readLines("./en_US/en_US.news.txt", encoding = "UTF-8")
twitter <- readLines("./en_US/en_US.twitter.txt", encoding = "UTF-8")

data <- readLines("./en_US/en_US.blogs.txt", encoding = "UTF-8", n = 50)

## What is the length of the longest line seen in any of the three en_US data sets? 
max(nchar(blogs))
max(nchar(news))
max(nchar(twitter))

## In the en_US twitter data set, if you divide the number of lines where the word "love" 
## (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
love <- grepl("love", twitter)
hate <- grepl("hate", twitter)
sum(love) / sum(hate)
