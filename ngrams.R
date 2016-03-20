library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)
library(tm) # For text mining
library(SnowballC) # For Stemming
library(slam) # For operations on DTM
library(ggplot2)
library(dplyr)

############################
###### Preprocessing #######
############################

twitter <- readLines("./en_US/en_US.twitter.txt", encoding = "UTF-8")
blogs <- readLines("./en_US/en_US.blogs.txt", encoding = "UTF-8")
news <- readLines("./en_US/en_US.news.txt", encoding = "UTF-8")
naughtywords <- scan("en", what = "String")


oneList <- list()
oneList_rel <- list()

twoList <- list()
twoList_rel <- list()

threeList <- list()
threeList_rel <- list()

for (i in 1:100){
set.seed(42+i^2)
print(paste("Run ", i))
twitter_sample <- sample(twitter, size = length(twitter) * 0.002, replace = FALSE)
blogs_sample <- sample(blogs, size = length(blogs) * 0.002, replace = FALSE)
news_sample <- sample(news, size = length(news) * 0.002, replace = FALSE)
full_sample <- c(twitter_sample, blogs_sample, news_sample)

full_sample <- gsub("[^[:alnum:]' ]", " ", full_sample)
corp <- Corpus(VectorSource(full_sample))
#corp <- tm_map(corp, removeWords, stopwords("english"))
#corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeWords, naughtywords)
#dict <- corp
#corp <- tm_map(corp, stemDocument)
#corp <- tm_map(corp, stemCompletion, dictionary = dict)
corp <- tm_map(corp, stripWhitespace)


############################
####### Exploration ########
############################

bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " \r\n\t"))
system.time(dtm <- DocumentTermMatrix(corp, control = list(tokenize = bigramTokenizer)))

sums <- col_sums(dtm)
nbigrams <- sum(sums)
sums <- subset(sums, sums >= 2)
sums_rel <- sums / nbigrams
df <- data.frame(as.table(sums))
df_rel <- data.frame(as.table(sums_rel))
twoList[[i]] <- df
twoList_rel[[i]] <- df_rel

print("-- Bigrams done.")
#---------------------
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " \r\n\t"))
system.time(dtm <- DocumentTermMatrix(corp, control = list(tokenize = bigramTokenizer)))

sums <- col_sums(dtm)
ntrigrams <- sum(sums)
sums <- subset(sums, sums >= 2)
sums_rel <- sums / ntrigrams
df <- data.frame(as.table(sums))
df_rel <- data.frame(as.table(sums_rel))
threeList[[i]] <- df
threeList_rel[[i]] <- df_rel

print("-- Trigrams done.")
#---------------------
system.time(dtm <- DocumentTermMatrix(corp))
wordFreq <- findFreqTerms(dtm)

sums <- col_sums(dtm)
nwords <- sum(sums)
sums <- subset(sums, sums >= 2)
sums_rel <- sums / nwords
df <- data.frame(as.table(sums))
df_rel <- data.frame(as.table(sums_rel))
oneList[[i]] <- df
oneList_rel[[i]] <- df_rel

print("-- Words done.")
}




tmp <- do.call("rbind",oneList)
tmp <- tmp %>% group_by(Var1) %>% summarize(Freq = mean(Freq)) %>% arrange(desc(Freq))

tmp2 <- do.call("rbind",oneList_rel)
tmp2 <- tmp2 %>% group_by(Var1) %>% summarize(Freq = mean(Freq)) %>% arrange(desc(Freq))

tmp3 <- do.call("rbind",twoList_rel)
tmp3 <- tmp3 %>% group_by(Var1) %>% summarize(Freq = mean(Freq)) %>% arrange(desc(Freq))

tmp4 <- do.call("rbind",threeList_rel)
tmp4 <- tmp4 %>% group_by(Var1) %>% summarize(Freq = mean(Freq)) %>% arrange(desc(Freq))

tmp5 <- do.call("rbind",threeList)
tmp5 <- tmp5 %>% group_by(Var1) %>% summarize(Freq = mean(Freq)) %>% arrange(desc(Freq))

g <- ggplot(tmp[1:50, ], aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity") + coord_flip()
g

g <- ggplot(tmp2[1:50, ], aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity") + coord_flip()
g

g <- ggplot(tmp3[1:50, ], aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity") + coord_flip()
g

g <- ggplot(tmp4[1:50, ], aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity") + coord_flip()
g

g <- ggplot(tmp5[1:50, ], aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity") + coord_flip()
g
