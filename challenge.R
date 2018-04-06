install.packages("tm");
install.packages("SnowballC");

library(tm)
library(NLP)
library(SnowballC)

splash=function(x){
    res=NULL
    for (i in x) res=paste(res, i)
    res
}

removeScript=function(t){
    sp=strsplit(t, "<script")
    vec=sapply(sp[[1]], gsub, pattern=".*</script>", replace=" ")
    PlainTextDocument(splash(vec))
}

removeBalises=function(x){
    t1=gsub("<[^>]*>", " ", x)
    PlainTextDocument(gsub("[ \t]+"," ",t1))
}

cleanCorpus <- function(path){
corp<-VCorpus(DirSource(path, encoding="latin1", recursive=TRUE))
corp<-tm_map(corp,content_transformer(tolower))
corp<-tm_map(corp,content_transformer(splash))
corp<-tm_map(corp,content_transformer(removeScript))
corp<-tm_map(corp,content_transformer(removeBalises))
corp<-tm_map(corp,removeNumbers)
corp<-tm_map(corp,removeWords,words=stopwords('en'))
corp<-tm_map(corp,stemDocument)
corp<-tm_map(corp,removePunctuation)


mat<-DocumentTermMatrix(corp,control=list())
vocab<-findFreqTerms(mat,lowfreq=20)

mat500<-mat[,vocab]

mat <- as.matrix(mat500)
mat <- cbind(mat, classe = c(rep(1,150), rep(2,150), rep(3,150), rep(4,150), rep(5,150), rep(6,150), rep(7,150)));

header = head(mat)

return (mat);

}

listeClasses <- vec("accueil", "blog", "commerce", "FAQ", "home", "liste", "recherche");

mat <- cleanCorpus("./training2016/")
write.csv(mat, file = "MyData.csv")
save(mat, file = "MyData.rda")

# classes <- 1
# train <- cbind(mat,classes)
