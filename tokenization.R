library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)

data <- readLines("./en_US/en_US.blogs.txt", encoding = "UTF-8", n = 100)
data <- as.String(data)

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
data_annotations <- annotate(data, list(word_ann, sent_ann))
data_doc <- AnnotatedPlainTextDocument(data, data_annotations)

# sents(data_doc)
# words(data_doc)