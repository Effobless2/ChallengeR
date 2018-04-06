distance <- function(x,y){
  sqrt(sum(y-x)^2)
}

#4
dist.voisins <- function(vec, data){
  apply(data[,-ncol(data)], 1, distance, vec)
}

#5
kppv <- function(vec, k, data){
  tab <- dist.voisins(vec,data)
  head(order(tab), n=k)
}

#6
classer <- function(vec, k, data){
  ppv <- kppv(vec, k, data)
  classes <- table(data[, ncol(data)][ppv])
  names(classes)[(which.max(classes))]
}

#7
verifClasse <-function(data, k){
  classes <- data[, ncol(data)]
  predictions <- sapply(seq_len(nrow(data)), function(i){
    v <- data[i, -ncol(data)]
    classer(v, k, data[-i,])
  })
  sum(classes != predictions)
}

#8
graphErr <- function(data){
  res <- sapply(seq(30), function(i) verifClasse(data, i))
  return (sum(res));
}
