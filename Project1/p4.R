library("igraph")

fb_graph = read.graph("facebook_combined.txt", format = "ncol", directed = FALSE)
personalNetworks <- make_ego_graph(fb_graph, 1, nodes = V(fb_graph))
coreNodes <-  which(ego_size(fb_graph, 1) > 200)
node_to_choose <- coreNodes[1]
pn <- personalNetworks[[node_to_choose]]
pn_orig <- pn

node_to_delete = V(pn)[V(pn)$name == V(fb_graph)[node_to_choose]$name]
#pn <- delete.vertices(pn, node_to_delete)
nodesize <- rep(4, vcount(pn))
nodesize[V(pn)$name == V(fb_graph)[node_to_choose]$name] = 7
V(pn)$color = "blue"
V(pn)[V(pn)$name == V(fb_graph)[node_to_choose]$name]$color = "black"
png(filename = "p4_1.png")
plot(pn, vertex.size = nodesize, vertex.label = NA)
dev.off()

fgc = fastgreedy.community(pn)
size_fgc <- sizes(fgc)
mod_fgc <- modularity(fgc)
V(pn)$color <- rainbow(length(size_fgc))[fgc$membership]
png(filename = "p4_2_fgc.png")
plot(pn, vertex.size = nodesize, vertex.label = NA)
dev.off()

fgc_orig = fastgreedy.community(pn_orig)
size_fgc_orig <- sizes(fgc_orig)
mod_fgc_orig <- modularity(fgc_orig)
print(mod_fgc)
print(mod_fgc_orig)

ebc <- edge.betweenness.community(pn)
size_ebc <- sizes(ebc)
mod_ebc <- modularity(ebc)
V(pn)$color <- rainbow(length(size_ebc))[ebc$membership]
png(filename = "p4_2_ebc.png")
plot(pn, vertex.size = nodesize, vertex.label = NA)
dev.off()

ebc_orig <- edge.betweenness.community(pn_orig)
size_ebc_orig <- sizes(ebc_orig)
mod_ebc_orig <- modularity(ebc_orig)
print(mod_ebc)
print(mod_ebc_orig)

ifc <- infomap.community(pn)
size_ifc <- sizes(ifc)
mod_ifc <- modularity(ifc)
V(pn)$color <- rainbow(length(size_ifc))[ifc$membership]
png(filename = "p4_2_ifc.png")
plot(pn, vertex.size = nodesize, vertex.label = NA)
dev.off()

ifc_orig <- infomap.community(pn_orig)
size_ifc_orig <- sizes(ifc_orig)
mod_ifc_orig <- modularity(ifc)
print(mod_ifc)
print(mod_ifc_orig)

png(filename = "density.png")
plot(density(fgc$membership), main = "", col = 'red', lwd=3)
lines(density(ebc$membership), main = "", col = 'green', lwd = 3)
lines(density(ifc$membership), main = "", col = 'blue', lwd = 3)
legend("topright", c("fast-greedy", "edge-betweenness", "infomap"), lwd=3, col = c("red", "green", "blue"))
dev.off()

png(filename = "density_compare_fgc.png")
plot(density(fgc_orig$membership), main = "", col = 'red', lwd=3)
lines(density(fgc$membership), main = "", col = 'green', lwd = 3)
legend("topright", c("Before deletion", "After deletion"), lwd=3, col = c("red", "green"))
dev.off()

png(filename = "density_compare_ebc.png")
plot(density(ebc_orig$membership), main = "", col = 'red', lwd=3)
lines(density(ebc$membership), main = "", col = 'green', lwd = 3)
legend("topright", c("Before deletion", "After deletion"), lwd=3, col = c("red", "green"))
dev.off()

png(filename = "density_compare_ifc.png")
plot(density(ifc_orig$membership), main = "", col = 'red', lwd=3)
lines(density(ifc$membership), main = "", col = 'green', lwd = 3)
legend("topright", c("Before deletion", "After deletion"), lwd=3, col = c("red", "green"))
dev.off()



