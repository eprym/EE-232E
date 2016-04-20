
library(igraph)
g1<- barabasi.game(1000,directed=FALSE)
fc <- fastgreedy.community(g1)
m<- sizes(fc)
for(i in 1:99){
  g1<- barabasi.game(1000,directed=FALSE)
  fc <- fastgreedy.community(g1)
  temp<- sizes(fc)
  m<-rbind(m,temp)}
h <- hist(m, breaks=seq(-0.5, by=1 , length.out=max(m)+2))
pl <- data.frame(x=h$mids, y=h$density)
plot(pl,log="y",type="p",xlab="size of community",ylab="density",main="size of community")