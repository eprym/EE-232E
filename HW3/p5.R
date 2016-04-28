library("igraph")
g <- read.graph("sorted_directed_net.txt", format="ncol", directed=TRUE)
cl <- clusters(g, mode="strong")
gcc_idx = which.max(cl$csize)
non_gcc_idx = (1:vcount(g))[cl$membership != gcc_idx]
gcc = delete.vertices(g, non_gcc_idx)
gcc_ud <-  as.undirected(gcc, mode="collapse", edge.attr.comb = list(weight="prod"))
E(gcc_ud)$weight <- sqrt(E(gcc_ud)$weight)
fgc <-  fastgreedy.community(gcc_ud)

over_100_idx <-  which(sizes(fgc)>100)
mod <-  rep(0, length(over_100_idx))
for(i in 1:length(over_100_idx)){
  non_idx <-  (1:vcount(gcc_ud))[fgc$membership != over_100_idx[i]]
  fastCom <- delete.vertices(gcc_ud, non_idx)
  subCom <- fastgreedy.community(fastCom)
  mod[i] <- modularity(subCom)
  imgname = sprintf("sub_com_%d.png", i)
  png(filename=imgname)
  plot(subCom, fastCom, vertex.label=NA)
  dev.off()
}
print(mod)