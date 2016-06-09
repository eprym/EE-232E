library("igraph")
library("hash")
library("MASS")
dir <- dirname(sys.frame(1)$ofile)
setwd(dir)

file_actor = "../project_2_data/merge_actor.txt"
actor <- readLines(file_actor, encoding="latin1")
actor <- strsplit(actor, "\t\t")
actorNum <- 244301
moviename_newid <- hash()
for(i in 1:vcount(g)){
  moviename_newid[[V(g)[i]$movieName]] <- as.numeric(V(g)[i]$name) + actorNum
}
print("Done for map movie name to new id")

bgraph <- list()
count <- 1
for(i in 1:length(actor)){
  for(j in 2:length(actor[[i]])){
    if(actor[[i]][j] != "" ){
      bgraph[[count]] <- c(actor[[i]][1], moviename_newid[[actor[[i]][j]]])
      count <- count+1
    }
  }
}
x = lapply(bgraph, write, "../project_2_data/bgraph.txt", append=T, ncolumns=2)
