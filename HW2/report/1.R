library ("igraph")
library ("netrw")
p <- 0.01
nodeNum <- 1000
#a create a random networks with 1000nodes and p = 0.01
g <- random.graph.game(nodeNum, p, directed=FALSE);
d1<-degree.distribution(g)
png(filename="C:/Users/rjbai/Desktop/232/Q1a_1_1.png")
plot(d1, type="o")
dev.off()
#b:measure the average distance of the walker
step <- 100
walkNum <-nodeNum
avg <- numeric()
sdv<- numeric()
for(t in 1:step){  
  
  rw <- netrw(g,walker.num = walkNum,
              damping=1,weights=NULL,T=t,
              output.walk.path=TRUE)
  shortestDistance<-numeric()
  for(w in 1:walkNum)
  {
    #calculate the shortest paths from starting point to the t step node
    tmp <- get.shortest.paths(g,from=rw$walk.path[1,w],to=rw$walk.path[t,w])
    shortestDistance <-c(shortestDistance,length(tmp$vpath[[1]])-1)

  }
  avg <- c(avg,mean(shortestDistance))
  sdv <- c(sdv,sd(shortestDistance))      	
}
png(filename="C:/Users/rjbai/Desktop/232/Q1b_1.png")
plot(1:step,avg,type="o",xlab="Steps t",ylab="Average Distance")
dev.off()
png(filename="C:/Users/rjbai/Desktop/232/Q1b_2.png")
plot(1:step,sdv,type="o",xlab="Steps t",ylab="Standard Deviation")
dev.off()

#e:the degree distribution of the end node of the random walk
degreeVector <-numeric()
for(w in 1:walkNum)
{
  degreeVector <- c(degreeVector,degree(g,rw$walk.path[step,w]))
}
png(filename="C:/Users/rjbai/Desktop/232/Q1e_1.png")
hs<-hist(degreeVector,breaks=seq(-0.5, by=1 , length.out=max(degreeVector)+2))
degreeVector<-data.frame(x=hs$mids, y=hs$density)
#plot(degreeVector,type='o')
dev.off()
diameter_1 = diameter(g)
print(diameter_1)
