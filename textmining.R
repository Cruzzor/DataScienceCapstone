library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)
library(tm) # For text mining
library(SnowballC) # For Stemming
library(ggplot2)

############################
###### Preprocessing #######
############################

#cname <- file.path("C:/Users/mbergman/Documents/Privat/Coursera/Capstone/DataScienceCapstone", "texts")
#cname <- file.path("C:/Users/mbergman/Documents/Privat/Coursera/Capstone/DataScienceCapstone", "en_US")
#corp <- Corpus(DirSource(cname))

twitter <- readLines("./en_US/en_US.twitter.txt", encoding = "UTF-8")
blogs <- readLines("./en_US/en_US.blogs.txt", encoding = "UTF-8")
news <- readLines("./en_US/en_US.news.txt", encoding = "UTF-8")

myList <- list()
myList_rel <- list()

for (i in 1:20){
set.seed(42+i^2)
twitter_sample <- sample(twitter, size = length(twitter) * 0.001, replace = FALSE)
blogs_sample <- sample(blogs, size = length(blogs) * 0.001, replace = FALSE)
news_sample <- sample(news, size = length(news) * 0.001, replace = FALSE)
full_sample <- c(twitter_sample, blogs_sample, news_sample)

full_sample <- gsub("[^[:alnum:]///' ]", " ", full_sample)
corp <- Corpus(VectorSource(full_sample))
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, content_transformer(tolower))
naughtywords <- scan("en", what = "String")
corp <- tm_map(corp, removeWords, naughtywords)
#dict <- corp
#corp <- tm_map(corp, stemDocument)
#corp <- tm_map(corp, stemCompletion, dictionary = dict)
corp <- tm_map(corp, stripWhitespace)



############################
####### Exploration ########
############################
dtm <- DocumentTermMatrix(corp)
wordFreq <- findFreqTerms(dtm)

# Find count
matObj <- as.matrix(dtm)
sums <- colSums(matObj)
nwords <- sum(sums)
nwords
sums <- subset(sums, sums >= 2)
sums_rel <- sums / nwords
df <- data.frame(as.table(sums)) %>% arrange(desc(Freq))
df_rel <- data.frame(as.table(sums_rel)) %>% arrange(desc(Freq))
myList[[i]] <- df
myList_rel[[i]] <- df_rel
}

tmp <- do.call("rbind",myList)
tmp <- tmp %>% group_by(Var1) %>% summarize(Freq = mean(Freq)) %>% arrange(desc(Freq))

tmp2 <- do.call("rbind",myList_rel)
tmp2 <- tmp2 %>% group_by(Var1) %>% summarize(Freq = mean(Freq)) %>% arrange(desc(Freq))


g <- ggplot(tmp[1:50, ], aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity") + coord_flip()
g

g <- ggplot(tmp2[1:50, ], aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity") + coord_flip()
g

