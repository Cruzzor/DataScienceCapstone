library(dplyr)


one <- readRDS(file = "1grams.Rda")
two <- readRDS(file = "2grams.Rda")
three <- readRDS(file = "3grams.Rda")

sep_one <- separate(two, col = Var1, c("First", "Second"), sep = " ") %>% 
  group_by(First) %>% arrange(desc(Freq)) %>% top_n(3)
