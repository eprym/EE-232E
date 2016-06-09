library("igraph")
library("hash")
library("MASS")
dir <- dirname(sys.frame(1)$ofile)
setwd(dir)

File_pagerank <- file("../project_2_data/pageScore.txt",open="r")
pagerank <- readLines(File_pagerank, encoding="latin1")
pr = list()
pagerank <- strsplit(pagerank, "\t")
h <- hash()

for (i in 1:length(pagerank)){
  h[[pagerank[[i]][3]]] <- as.numeric(pagerank[[i]][2])
}
print(length(h))
close(File_pagerank)
print("Done for pagerank")

a_rank <- list()
for(i in 1:length(actorList)){
  k<-0
  actorRank_tmp<-c()
  for(j in 1:length(actorList[[i]]))
  {
    r<-h[[actorList[[i]][j]]]
    actorRank_tmp<-append(actorRank_tmp,r)
  }
  if(length(actorRank_tmp) != 0)  actorRank_tmp<-sort(actorRank_tmp,decreasing=TRUE)
  a_rank[[i]]<-actorRank_tmp[1:5]
}
print("Done for actor rank")

File_rating <- file("../project_2_data/movie_rating.txt",open="r")
movie_rating <- readLines(File_rating, encoding="latin1")
movie_rating <- strsplit(movie_rating, "\t\t")
rating <- list()

for (i in 1:length(movie_rating)){
  nodeId2 = (1:vcount(g))[V(g)$movieName==movie_rating[[i]][1]]
  if(length(nodeId2) != 0 && length(actorList[[nodeId2]]) !=0)  rating[[nodeId2]] <- movie_rating[[i]][2]
}

print("Done for rating")

inTopDirect<-list()
for(i in 1:vcount(g))
{
  tmpDirect<-rep(0,101)
  if(V(g)[i]$Director %in% top_director)
  {
    tmpDirect[1]<-1
    j<-which(top_director==V(g)[i]$Director)
    tmpDirect[j]<-1
  }
  inTopDirect[[i]]<-tmpDirect
}
print("Done for boolean vector")
data = apply( cbind( a_rank, inTopDirect, rating ) , 1 , unlist )
write.matrix(as.matrix(data), "../project_2_data.txt")
save.image("/home/ubuntu/EE232/project_2_data/p7_8.RData")
