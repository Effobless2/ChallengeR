install.packages("tm");

library(tm);

# src <- DirSource("training2016", recursive = TRUE);
# corpus <- SimpleCorpus(src, control = list(language = "en"));
#
# content(corpus[[1]]);
#
splash <- function(x){
    res <- NULL
    for (i in x) res <- paste(res, i)
    res
}

removeScript <- function(t){
    #decoupage de la chaine en utilisant "<split"
    #sp=strsplit(splash(t), "<script")
    sp <- strsplit(t, "<script")
    #pour chaque partie du split, le debut (jusqu'a </script> est supprime
    vec <- sapply(sp[[1]], gsub, pattern=".*</script>", replace=" ")
    #les elements du split nottoyes sont concatenes
    PlainTextDocument(splash(vec))
}

removeBalises <- function(x){
    t1 <- gsub("<[^>]*>", " ", x)
    #suppression des occurrences multiples d'espaces (ou de tabulations)
    PlainTextDocument(gsub("[ \t]+"," ",t1))
}
#
# clean_corpus <- function(corpus){
#   corpus <- tm_map(corpus, content_transformer(tolower));
#   corpus <- tm_map(corpus, content_transformer(splash));
#   corpus <- tm_map(corpus, content_transformer(removeScript));
#   corpus <- tm_map(corpus, content_transformer(removeBalises));
#   corpus <- tm_map(corpus, removeWords, stopwords("en"));
#   corpus <- tm_map(corpus, removePunctuation);
#   corpus <- tm_map(corpus, removeNumbers);
# }
#
# cleaned_corpus <- clean_corpus(corpus);
#
# dtm <- DocumentTermMatrix(cleaned_corpus);
acc<-VCorpus(DirSource("./training2016", encoding="latin1",recursive = TRUE))
acc<-tm_map(acc,content_transformer(tolower))
acc<-tm_map(acc,content_transformer(splash))
acc<-tm_map(acc,content_transformer(removeScript))
acc<-tm_map(acc,content_transformer(removeBalises))
acc<-tm_map(acc,removeNumbers)
acc<-tm_map(acc,removeWords,words=stopwords('en'))
acc<-tm_map(acc,stemDocument)
acc<-tm_map(acc,removePunctuation)
