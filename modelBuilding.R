library(dplyr)
library(tidyr)

one <- readRDS(file = "1grams.Rda")
two <- readRDS(file = "2grams.Rda")
three <- readRDS(file = "3grams.Rda")

one <- readRDS(file = "one_02.Rda")
two <- readRDS(file = "two_02.Rda")
three <- readRDS(file = "three_02.Rda")
four <- readRDS(file = "four_02.Rda")

sep_one <- separate(two, col = Var1, c("First", "Second"), sep = " ") %>% 
  group_by(First) %>% arrange(desc(Freq)) %>% top_n(3)

sep_two <- separate(three, col = Var1, c("First", "Second", "Third"), sep = " ") %>% 
  unite(col = "First", sep = " ", First, Second) %>% rename(Second = Third) %>%  group_by(First) %>% arrange(desc(Freq)) %>% top_n(3)


computeGT <- function(c, k, ncounts) {
  if (c < k) {
    (c + 1) * (as.numeric(filter(ncounts, freq == c + 1)[2] / filter(ncounts, freq == c)[2]))
  }
  else {
    c
  }
}

computeGT <- function(c, k) {
  if (c <= k) {
    gt <- (c + 1) * (as.numeric(filter(ncounts_one, freq == c + 1)[2] / filter(ncounts_one, freq == c)[2]))
    kdiscount <- (k + 1) * (as.numeric(filter(ncounts_one, freq == k + 1)[2] / filter(ncounts_one, freq == 1)[2]))
    (gt - c * kdiscount) / (1 - kdiscount)
  }
  else {
    c
  }
}

# Compute good-turing discounts
ncounts_one <- data.frame(freq = one$Freq) %>% group_by(freq) %>% summarise(count = n())
ncounts_two <- data.frame(freq = two$Freq) %>% group_by(freq) %>% summarise(count = n())
ncounts_three <- data.frame(freq = three$Freq) %>% group_by(freq) %>% summarise(count = n())

test <- one %>% rowwise() %>% mutate(GTCounts = computeGT(Freq, 5))
