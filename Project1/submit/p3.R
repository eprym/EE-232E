library(igraph)
g <- read.graph("/Users/bairongjing/Documents/ucla\ /16_spring/232/facebook_combined.txt", format="ncol", directed=F)
personalNetwork <- graph.neighborhood(g,1,nodes=V(g))
# find the core nodes
coreNodes <- which(neighborhood.size(g,1)>201) 
numOfcoreNodes <- length(coreNodes)
# find the average degree of the core node
avecoreNodeDegree <- mean(degree(g)[coreNodes])
node_to_choose <- coreNodes[1]
# create the personal network of the randomly choose core node
pn<-personalNetwork[[node_to_choose]]
nodeSize <- rep(4,vcount(pn))
nodeSize[V(pn)$name==V(g)[node_to_choose]$name]=7
V(pn)$color="blue"
V(pn)[V(pn)$name==V(g)[node_to_choose]$name]$color="black"
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/3_1.png")
plot(pn,vertex.size=nodeSize,vertex.label=NA)
dev.off()

#fast greedy commmunity
fg_c <-fastgreedy.community(pn)
size_fgc<-sizes(fg_c)
mod_f <- modularity(pn,fg_c$membership)
V(pn)$color <-rainbow(length(size_fgc))[fg_c$membership]
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/3_2.png")
plot(pn,vertex.size=nodeSize,vertex.label=NA,layout=layout.lgl)
dev.off()
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/3_3.png")
hist(fg_c$membership,col="red",xlab="Community Number",ylab="Numbers of Nodes in a Community",main = "")
dev.off()

#edge betweennness community
ed_c <- edge.betweenness.community(pn)
size_edc<-sizes(ed_c)
mod_e <- modularity(pn,ed_c$membership)
V(pn)$color <-rainbow(length(size_edc))[ed_c$membership]
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/3_4.png")
plot(pn,vertex.size=nodeSize,vertex.label=NA,layout=layout.lgl,vertex.color=ed_c$membership)
dev.off()
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/3_5.png")
hist(ed_c$membership,col="green",xlab="Community Number",ylab="Numbers of Nodes in a Community",main = "")
dev.off()

#infomap community
in_c <- infomap.community(pn)
size_inc <-sizes(in_c)
mod_i <- modularity(pn,in_c$membership)
V(pn)$color <-rainbow(length(size_inc))[in_c$membership]
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/3_6.png")
plot(pn,vertex.size=nodeSize,vertex.label=NA,layout=layout.lgl,vertex.color=in_c$membership)
dev.off()
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/3_7.png")
hist(in_c$membership,col="blue",xlab="Community Number",ylab="Numbers of Nodes in a Community",main = "")
dev.off()
