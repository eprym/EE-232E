
library(igraph)
g<-barabasi.game(1000, directed = FALSE)
ran<-sample(1:1000, 1)
nran<-neighborhood(g, 1,ran)
size<-neighborhood.size(g, 1,ran)-1
ran2<-sample(1:size, 1)
nei<-c(nran[[1]][ran2+1])
d<-degree(g, nei)
for (i in 1:1999) {
  g<-barabasi.game(1000, directed = FALSE)
  ran<-sample(1:1000, 1)
  nran<-neighborhood(g, 1,ran)
  size<-neighborhood.size(g, 1,ran)-1
  ran2<-sample(1:size, 1)
  nei<-c(nran[[1]][ran2+1])
  temp<-degree(g, nei)
  d<-c(d,temp)
}
h <- hist(d, breaks=seq(-0.5, by=1 , length.out=max(d)+2))
pl <- data.frame(x=h$mids, y=h$density)
plot(pl,log="xy", main = "Degree Distribution with 1000 nodes", xlab = "Degree", ylab = "Density")

x<-h$mids
y<-2.3*x^(-2)
lines(x,y,col="red")