nwords <- function(x) {round(sum(sapply(strsplit(x, " "), length)))}

predict_one <- function(x) {
  x <- strsplit(x, " ")[[1]][nwords(x)] %>% tolower()
  filter <- sep_one %>% filter(First == x)
  return(filter$Second)
}