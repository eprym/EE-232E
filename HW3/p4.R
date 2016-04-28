library("igraph")
g <- read.graph("sorted_directed_net.txt", format="ncol", directed=TRUE)
cl <- clusters(g, mode="strong")
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
modularity(maxSubCom)
png(filename="large_sub_com.png")
plot(maxSubCom, maxFastCom, vertex.label=NA)
dev.off()

