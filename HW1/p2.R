library(igraph)
#a
#create a undirected network with a fat-tailed degree distribution
g <- barabasi.game(1000,directed=FALSE)
d<-degree.distribution(g)
#png(filename="/Users/mysteryjoe/Desktop/ucla/ee232/figure1.png")
plot(d, type="o")
#diameter(g)
dev.off()
diameters<-0
count<-0
for(i in 1:100){
  g <- barabasi.game(1000,directed=FALSE)
  diameters<-diameters+diameter(g)
	if(is.connected(g))
	{
	count<-count+1
	}
}
#k<-count
print(count)
print(diameters/100)
graphs <- decompose.graph(g)
#assign some vertex ids as vertex attributes so that one can keep track of which vertex #is n which component
V(g)$label <- seq(vcount(g))
#plot the largest component--GCC
largest <- which.max(sapply(graphs, vcount))
plot(graphs[[largest]])
#b
# find the giant connected component
cl <- clusters(g)
gccIndex = which.max(cl$csize)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g, nonGccNodes)
#c
g <- barabasi.game(10000,directed=FALSE)
cl = clusters(g)
gccIndex = which.max(cl$csize)
#print(gccIndex+1)
nonGccNodes = (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g,nonGccNodes)
struct = fastgreedy.community(gcc)
print(struct)
mod = modularity(struct)

print("modularity=")
print(modularity(gcc,membership(struct)))

##create a undirected network with a fat-tailed degree distribution with 10000 nodes
cl = clusters(g)
gccIndex = which.max(cl$csize)
#print(gccIndex+1)
nonGccNodes = (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g,nonGccNodes)
struct = fastgreedy.community(gcc)
print(struct)
mod = modularity(struct)
print("modularity=")
print(modularity(gcc,membership(struct)))
#d
g <- barabasi.game(10000,power=-3, directed = FALSE)
degreesVector <- degree(g)
for (p in 1:10000){
  #randomly choose node i
  i<-sample(1:10000,1)
  b<-sample(1:degreesVector[i],1)
  #randomly choose the neighborhood node of i
  j<-neighborhood(g,1,i)[[1]][b+1]
  if(p==1){
    deg<-degreesVector[j]
    }else{
      deg<-c(deg,degreesVector[j])
    }
}
hs <- hist(deg, breaks=seq(-0.5, by=1 , length.out=max(deg)+2))
#degree distribution of node j 
dd <- data.frame(x=hs$mids, y=hs$density)
#png(filename="/Users/mysteryjoe/Desktop/ucla/ee232/figure2.png")
plot(dd , type="o")
dev.off()

