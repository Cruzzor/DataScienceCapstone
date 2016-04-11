nwords <- function(x) {round(sum(sapply(strsplit(x, " "), length)))}

predict_one <- function(x) {
  x <- strsplit(x, " ")[[1]][nwords(x)] %>% tolower()
  filter <- sep_one %>% filter(First == x)
  return(filter$Second)
}


##############################################

predict <- function(x) {
  split <- strsplit(x, " ")
  nwords <- nwords(x)
  first <- split[[1]][nwords - 1]
  second <- split[[1]][nwords]
  
  # Tri-gram
  trigrams <- filter(three, First == first & Second == second) %>% top_n(3, GTProb)
  if (nrow(trigrams) > 0) {
    return(trigrams$Third)
  }
  else {
    # Two-grams
    twograms <- filter(two, First == second) %>% mutate(newProb = GTProb * Alpha) %>% top_n(3, newProb)
    if (nrow(twograms) > 0) {
      return(twograms$Second)
    }
    else {
      # Return most frequent single words
      unigrams <- mutate(one, newProb = GTProb * Alpha) %>% arrange(desc(newProb)) %>% top_n(3, newProb)
      return(unigrams$Word)
    }
  }
  
  #OPTIMIERUNGEN: Berechne vorher alpha*P; Behalte nur die besten drei von allen
}