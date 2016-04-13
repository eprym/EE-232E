library(igraph)
g <- barabasi.game(1000,power=-3,directed=FALSE)
d<-degree.distribution(g)
png(filename="/Users/mysteryjoe/Desktop/ucla/ee232/")
plot(d, type="o")
diameter(g)
dev.off()

count<-0
for(i in 1:100){
	if(is.connected(g))
	{
	count<-count+1
	}
}
k<-count/100

cl<- count/100
gccIndex = which.max(cl$csize)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g, nonGccNodes)

# find communities using fastgreedy algorithm
fg <- fastgreedy.community(gcc)
modularity(gcc,membership(fg))

##create a undirected network with a fat-tailed degree distribution with 10000 nodes
g <- barabasi.game(10000,power=-3,directed = FALSE)
fg <- fastgreedy.community(g)
modularity(fg)