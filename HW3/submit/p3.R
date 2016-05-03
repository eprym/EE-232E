library("igraph")
g <- read.graph("sorted_directed_net.txt", format="ncol", directed=TRUE)
cl <- clusters(g)
gcc_idx = which.max(cl$csize)
non_gcc_idx = (1:vcount(g))[cl$membership != gcc_idx]
gcc = delete.vertices(g, non_gcc_idx)

#option 1
gcc_ud <-  as.undirected(gcc, mode="each")
lpc <-  label.propagation.community(gcc_ud)
sizes(lpc)
modularity(lpc)
png(filename="lpc_opt1.png")
plot(lpc, gcc_ud, vertex.label=NA)
dev.off()

#option 2
gcc_ud <-  as.undirected(gcc, mode="collapse", edge.attr.comb = list(weight="prod"))
E(gcc_ud)$weight <- sqrt(E(gcc_ud)$weight)
lpc <- label.propagation.community((gcc_ud))
sizes(lpc)
modularity(lpc)
png(filename="lpc_opt2.png")
plot(lpc, gcc_ud, vertex.label=NA)
dev.off()

fgc <- fastgreedy.community(gcc_ud)
sizes(fgc)
modularity(fgc)
png(filename="fgc_opt2.png")
plot(fgc, gcc_ud, vertex.label=NA)
dev.off()


