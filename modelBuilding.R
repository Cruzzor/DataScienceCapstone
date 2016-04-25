library(dplyr)
library(tidyr)

one <- readRDS(file = "1grams.Rda")
two <- readRDS(file = "2grams.Rda")
three <- readRDS(file = "3grams.Rda")
four <- readRDS(file = "4grams.Rda")

one <- readRDS(file = "one_02.Rda")
two <- readRDS(file = "two_02.Rda")
three <- readRDS(file = "three_02.Rda")
four <- readRDS(file = "four_02.Rda")


sep_one <- separate(two, col = Var1, c("First", "Second"), sep = " ") %>% 
  group_by(First) %>% 
  arrange(desc(Freq)) %>% 
  top_n(3)

sep_two <- separate(three, col = Var1, c("First", "Second", "Third"), sep = " ") %>% 
  unite(col = "First", sep = " ", First, Second) %>% rename(Second = Third) %>%  group_by(First) %>% arrange(desc(Freq)) %>% top_n(3)



computeGT <- function(c, k, freqTbl) {
  if (c <= k) {
    gt <- (c + 1) * (as.numeric(filter(freqTbl, freq == c + 1)[2] / filter(freqTbl, freq == c)[2]))
    kdiscount <- (k + 1) * (as.numeric(filter(freqTbl, freq == k + 1)[2] / filter(freqTbl, freq == 1)[2]))
    (gt - c * kdiscount) / (1 - kdiscount)
  }
  else {
    c
  }
}

compute3gramAlpha <- function(first, second, third) {
  # Beta
  fourgrams <- filter(four, First == first & Second == second & Third == third)
  sum_fourgramsProb <- sum(fourgrams$GTProb)
  beta <- 1 - sum_fourgramsProb
  
  threegrams <- filter(three, First == second & Second == third & Third %in% fourgrams$Fourth)
  sum_threegramsProb <- sum(threegrams$GTProb)
  alpha <- beta / (1 - sum_threegramsProb)
  
  # Return
  alpha
}

compute2gramAlpha <- function(first, second) {
  # Beta
  threegrams <- filter(three, First == first & Second == second)
  sum_threegramsProb <- sum(threegrams$GTProb)
  beta <- 1 - sum_threegramsProb
  
  twograms <- filter(two, First == second & Second %in% threegrams$Third)
  sum_twogramsProb <- sum(twograms$GTProb)
  alpha <- beta / (1 - sum_twogramsProb)
  
  # Return
  alpha
}


compute1gramAlpha <- function(word) {
  # Beta
  twograms <- filter(two, First == word)
  sum_twogramsProb <- sum(twograms$GTProb)
  beta <- 1 - sum_twogramsProb
  
  # Alpha
  unigrams <- filter(one, Word %in% twograms$Second)
  sum_unigramsProb <- sum(unigrams$GTProb)
  alpha <- beta / (1 - sum_unigramsProb)
  
  # Return
  alpha
}

# Compute good-turing discounts
ncounts_one <- data.frame(freq = one$Freq) %>% group_by(freq) %>% summarise(count = n())
ncounts_two <- data.frame(freq = two$Freq) %>% group_by(freq) %>% summarise(count = n())
ncounts_three <- data.frame(freq = three$Freq) %>% group_by(freq) %>% summarise(count = n())
ncounts_four <- data.frame(freq = four$Freq) %>% group_by(freq) %>% summarise(count = n())

one <- filter(one, Freq > 1)
two <- filter(two, Freq > 1)
three <- filter(three, Freq > 1)
four <- filter(four, Freq > 1)

one <- one %>% rowwise() %>% mutate(GTCounts = computeGT(Freq, 5, ncounts_one))
two <- two %>% rowwise() %>% mutate(GTCounts = computeGT(Freq, 5, ncounts_two))
three <- three %>% rowwise() %>% mutate(GTCounts = computeGT(Freq, 5, ncounts_three))
four <- four %>% rowwise() %>% mutate(GTCounts = computeGT(Freq, 5, ncounts_four))

one <- readRDS("1grams_GT.Rda")
two <- readRDS("2grams_GT.Rda")
three <- readRDS("3grams_GT.Rda")
four <- readRDS("4grams_GT.Rda")

n <- sum(one$Freq)
one <- mutate(one, GTProb = GTCounts / n) %>% rename(Word = Var1)
one$Word <- as.character(one$Word)

n <- sum(two$Freq)
two <- mutate(two, GTProb = GTCounts / n) %>% 
  separate(col = Var1, c("First", "Second"), sep = " ")

n <- sum(three$Freq)
three <- mutate(three, GTProb = GTCounts / n) %>% 
  separate(col = Var1, c("First", "Second", "Third"), sep = " ")

n <- sum(four$Freq)
four <- mutate(four, GTProb = GTCounts / n) %>% 
  separate(col = Var1, c("First", "Second", "Third", "Fourth"), sep = " ") %>%
  mutate(Ngram = paste(First, Second, Third, Fourth, sep = " "))

system.time(for (i in 1:nrow(three)) {
  first <- as.character(three[i, 1])
  second <- as.character(three[i, 2])
  third <- as.character(three[i, 3])
  alpha <- compute3gramAlpha(first, second, third)
  three$Alpha[i] <- alpha
})

system.time(for (i in 1:nrow(two)) {
  first <- as.character(two[i,1])
  second <- as.character(two[i,2])
  alpha <- compute2gramAlpha(first, second)
  two$Alpha[i] <- alpha
})

system.time(for (i in 1:nrow(one)) {
  word <- as.character(one[i,1])
  alpha <- compute1gramAlpha(word)
  one$Alpha[i] <- alpha
  print(i)
})

one <- readRDS("1grams_GT_Alpha.Rda")
two <- readRDS("2grams_GT_Alpha.Rda")
three <- readRDS("3grams_GT_Alpha.Rda")
