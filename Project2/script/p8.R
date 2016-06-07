library("igraph")
library("hash")
dir <- dirname(sys.frame(1)$ofile)
setwd(dir)


File_top<-file("../project_2_data/top_100_director.txt",open="r")
top_director = list()
top<-readLines(File_top, encoding="latin1")
top <- strsplit(top,"\t\t")

for (i in 1:length(top)){
  top_director[[i]] <- top[[i]][1]
}
top_director <- unique(top_director)[1:100]
close(File_top)
print("Done for top director")

File_movie_actor <- file("../project_2_data/movie_list.txt",open="r")
movie_actor <- readLines(File_movie_actor, encoding="latin1")
movie_actor <- strsplit(movie_actor,"\t\t")
actorList<-list()
for(i in 1:length(movie_actor))
{
  actorlist<-movie_actor[[i]][3:length(movie_actor[[i]])]
  nodeId<-(1:vcount(g))[V(g)$name==movie_actor[[i]][2]]
  if(length(nodeId) != 0) actorList[[nodeId]]<-actorlist
}
close(File_movie_actor)
print("Done for movie_actor")

File_pagerank <- file("../project_2_data/pageScore.txt",open="r")
pagerank <- readLines(File_pagerank, encoding="latin1")
pagerank <- strsplit(pagerank, "\t")
h <- hash()

for (i in 1:length(pagerank)){
  h[[pagerank[[i]][3]]] <- pagerank[[i]][2]
}
print(length(h))
close(File_pagerank)
print("Done for pagerank\n")

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

for (i in 1:length(rating)){
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
