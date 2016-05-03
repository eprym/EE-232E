library("igraph")
g<-read.graph("/Users/mysteryjoe/Desktop/ucla/ee232/hw3/sorted_directed_net.txt",format="ncol",directed=TRUE)


####Q1####
ifconnect=is.connected(g)
print(ifconnect)
cl<-clusters(g)
gccID<-which.max(cl$csize)
nonGccID <-(1:vcount(g))[cl$membership != gccID]
gcc<-delete.vertices(g,nonGccID)
print(cl$csize)
print(nonGccID)

####Q2####
inD<-degree.distribution(gcc,mode="in")
outD<-degree.distribution(gcc,mode="out")
png(filename="/Users/mysteryjoe/Desktop/ucla/ee232/hw3/p2_in.png")
plot(inD,type="l",main="Indegree Distribution")
dev.off()
png(filename="/Users/mysteryjoe/Desktop/ucla/ee232/hw3/p2_out.png")
plot(outD,type="l",main="Outdegree Distribution")
dev.off()