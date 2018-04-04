# install.packages("tm");
install.packages("SnowballC")

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
corp<-VCorpus(DirSource(path, recursive=TRUE))
corp<-tm_map(corp,content_transformer(tolower))
corp<-tm_map(corp,content_transformer(splash))
corp<-tm_map(corp,content_transformer(removeScript))
corp<-tm_map(corp,content_transformer(removeBalises))
corp<-tm_map(corp,removeNumbers)
corp<-tm_map(corp,removeWords,words=stopwords('en'))
corp<-tm_map(corp,stemDocument)
corp<-tm_map(corp,removePunctuation)

# blog<-VCorpus(DirSource("./training2016/blog"))
# blog<-tm_map(blog,content_transformer(tolower))
# blog<-tm_map(blog,content_transformer(splash))
# blog<-tm_map(blog,content_transformer(removeScript))
# blog<-tm_map(blog,content_transformer(removeBalises))
# blog<-tm_map(blog,removeNumbers)
# blog<-tm_map(blog,removeWords,words=stopwords('en'))
# blog<-tm_map(blog,stemDocument)
# blog<-tm_map(blog,removePunctuation)



mat<-DocumentTermMatrix(corp,control=list())
vocab<-findFreqTerms(mat,lowfreq=100)
mat500<-mat[,vocab]

mat <- as.matrix(mat500)
}

mat <- cleanCorpus("./training2016")
# classes <- 1
# train <- cbind(mat,classes)
