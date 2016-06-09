library("igraph")
library("hash")
library("MASS")
library("ppls")
dir <- dirname(sys.frame(1)$ofile)
setwd(dir)

movie2actor <- function(movie_rating, actNum, label){ #label-- 1: mean, 2: max, 3: median, 4:tfidf
  switch (label,
          mean = mean(movie_rating),
          max = max(movie_rating),
          median = median(movie_rating),
          tfidf = tfidf(movie_rating, actNum)
  )
}

tfidf <- function(movie_rating, actNum){
  tf_idf =  c()
  tf =  1/length(movie_rating)
  for(i in 1:length(movie_rating)){
    idf =  log(actorNum/actNum[i])
    tf_idf =  append(tf_idf, tf*idf)
  }
  score = sum(normalize.vector(tf_idf)**2 * movie_rating)
  return(score)
}

g_b <- read.graph("../project_2_data/bgraph.txt", format="ncol", directed=FALSE)
neighbor_b <- list()
for(i in 1:3){
  id1 <- (1:vcount(g))[V(g)$movieName == movie_interest[i]]
  id2 <- (1:vcount(g_b))[V(g_b)$name == toString(as.numeric(V(g)[id1]$name) + actorNum)]
  tmp<-ego(g_b,1,V(g_b)[id2])
  neighbor_b[[i]]<-tmp[[1]][2:length(tmp[[1]])]
}

actorRating <- list(c(), c(), c())
for(i in 1:3){
  for(j in 1:length(neighbor_b[[i]])){
    tmp <- ego(g_b, 1, V(g_b)[neighbor_b[[i]][j]])
    tmp_rating <- c()
    tmp_num <- c()
    for(k in 2:length(tmp[[1]])){
      id3 <- (1:vcount(g))[V(g)$name == toString(as.numeric(V(g_b)[tmp[[1]][k]]$name)-actorNum)]
      tmp_rating <- append(tmp_rating, as.numeric(rating[[id3]]))
      tmp_num <- append(tmp_num, ego_size(g_b, 1, V(g_b)[id3])[1])
    }
    if(length(tmp_rating) > 0)  actorRating[[i]] <- append(actorRating[[i]], movie2actor(tmp_rating, tmp_num, "tfidf"))
  }
}

