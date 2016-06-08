library("igraph")
library("hash")
library("MASS")
dir <- dirname(sys.frame(1)$ofile)
setwd(dir)
movie_interest <- c("Batman v Superman: Dawn of Justice (2016)", "Mission: Impossible - Rogue Nation (2015)",
                    "Minions (2015)")

a_rank_interest <- list()
inTopDirect_interest <- list()
for(i in 1:3){
  nodeId <- (1:vcount(g))[V(g)$movieName==movie_interest[i]]
  a_rank_interest[[i]] <- a_rank[[nodeId]]
  inTopDirect_interest[[i]] <- inTopDirect[[nodeId]]
}
data_interest <- apply( cbind( a_rank_interest, inTopDirect_interest) , 1, unlist )
write.matrix(t(as.matrix(data_interest)), "../project_2_data/data_interest.txt")