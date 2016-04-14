
library(igraph)
#decompose the graph into separate components with
g <- barabasi.game(1000,directed=FALSE)
graphs <- decompose.graph(g)
#assign some vertex ids as vertex attributes so that one can keep track of which vertex #is n which component
V(g)$label <- seq(vcount(g))
#plot the largest component--GCC
largest <- which.max(sapply(graphs, vcount))
plot(graphs[[largest]])