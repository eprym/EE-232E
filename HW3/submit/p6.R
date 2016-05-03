library("igraph")
library("netrw")
g <- read.graph("sorted_directed_net.txt", format="ncol", directed=TRUE)
cl <- clusters(g, mode="strong")
gcc_idx = which.max(cl$csize)
non_gcc_idx = (1:vcount(g))[cl$membership != gcc_idx]
gcc = delete.vertices(g, non_gcc_idx)
gcc_ud <-  as.undirected(gcc, mode="collapse", edge.attr.comb = list(weight="prod"))
E(gcc_ud)$weight <- sqrt(E(gcc_ud)$weight)
fgc <-  fastgreedy.community(gcc_ud)

m <- matrix(0, vcount(gcc), length(sizes(fgc)))
M <- matrix(0, vcount(gcc), length(sizes(fgc)))

for(i in 1:vcount(gcc)){
  m[i, fgc$membership[i]] = 1
}
number_vertex <- 30

for(i in 1:vcount(gcc)){
  pagerank = netrw(gcc, walker.num=100, damping=0.85, start.node=i)
  v = sort(pagerank$ave.visit.prob, index.return = TRUE, decreasing = TRUE)
  for(j in 1:number_vertex){
    M[i,] = M[i,] + v$x[j] * m[v$ix[j],] 
  }
}

threshold <- 0.024
filter_first = matrix(0, 1, dim(M)[1])
filter_second = matrix(0, 1, dim(M)[1])
sorted_M = matrix(0, dim(M)[1], dim(M)[2])
for(i in 1:dim(M)[1]){
  sorted_M[i,] = sort(M[i,], decreasing=TRUE)
  filter_first[1, i] = sorted_M[i,1]
  filter_second[1, i] = sorted_M[i,2]
}

png(filename="dist_firstlarge.png")
hist(filter_first, xlab="first largest value", ylab="Frequency", main=NULL)
dev.off()

png(filename="dist_secondlarge.png")
hist(filter_second, xlab="Second largest value", ylab="Frequency", main = NULL)
dev.off()

multi_node_idx <-  list()
idx <- 1
for(i in 1:dim(sorted_M)[1]){
  sorted_M[i,] <- replace(sorted_M[i,], sorted_M[i,] < threshold, 0)
  if(sorted_M[i,2] > 0){
    multi_node_idx[[idx]] <- i
    idx <- idx+1
  }
}
print(multi_node_idx)