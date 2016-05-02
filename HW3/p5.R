library("igraph")
g <- read.graph("sorted_directed_net.txt", format="ncol", directed=TRUE)
cl <- clusters(g)
gcc_idx = which.max(cl$csize)
non_gcc_idx = (1:vcount(g))[cl$membership != gcc_idx]
gcc = delete.vertices(g, non_gcc_idx)
gcc_ud <-  as.undirected(gcc, mode="collapse", edge.attr.comb = list(weight="prod"))
E(gcc_ud)$weight <- sqrt(E(gcc_ud)$weight)
fgc <-  fastgreedy.community(gcc_ud)
max_idx <-  which.max(sizes(fgc))
non_max_idx <-  (1:vcount(gcc_ud))[fgc$membership != max_idx]
maxFastCom <- delete.vertices(gcc_ud, non_max_idx)
maxSubCom <- fastgreedy.community(maxFastCom)

over_100_idx <-  which(sizes(maxSubCom)>100)
mod <-  rep(0, length(over_100_idx))
for(i in 1:length(over_100_idx)){
  non_idx <-  (1:vcount(maxFastCom))[maxSubCom$membership != over_100_idx[i]]
  fastCom <- delete.vertices(maxFastCom, non_idx)
  subCom <- fastgreedy.community(fastCom)
  mod[i] <- modularity(subCom)
  imgname = sprintf("sub_com_%d.png", i)
  png(filename=imgname)
  plot(subCom, fastCom, vertex.label=NA)
  dev.off()
}
print(mod)