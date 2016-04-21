library ("igraph")
library ("netrw")
#1: network with 100 nodes
nodeNum <- 100
g <- barabasi.game(nodeNum, power = -3, directed=FALSE);
#measure the average distance of the walker
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
png(filename="C:/Users/rjbai/Desktop/232/Q2d_1_100.png")
plot(1:step,avg,type="o",xlab="Steps t",ylab="Average Distance")
dev.off()
png(filename="C:/Users/rjbai/Desktop/232/Q2d_2_100.png")
plot(1:step,sdv,type="o",xlab="Steps t",ylab="Standard Deviation")
dev.off()
print (diameter(g))

#2:network with 10000nodes
nodeNum2 <- 10000
g2 <- barabasi.game(nodeNum2, power = -3, directed=FALSE);
#measure the average distance of the walker
step <- 100
walkNum2 <-10000
avg <- numeric()
sdv<- numeric()
for(t in 1:step){

  rw <- netrw(g2,walker.num = walkNum2,
              damping=1,weights=NULL,T=t,
              output.walk.path=TRUE)
  shortestDistance<-numeric()
  for(w in 1:walkNum2)
  {
    #calculate the shortest paths from starting point to the t step node
    tmp <- get.shortest.paths(g2,from=rw$walk.path[1,w],to=rw$walk.path[t,w])
    shortestDistance <-c(shortestDistance,length(tmp$vpath[[1]])-1)

  }
  avg <- c(avg,mean(shortestDistance))
  sdv <- c(sdv,sd(shortestDistance))
}
png(filename="C:/Users/rjbai/Desktop/232/Q2d_1_10000.png")
plot(1:step,avg,type="o",xlab="Steps t",ylab="Average Distance")
dev.off()
png(filename="C:/Users/rjbai/Desktop/232/Q2d_2_10000.png")
plot(1:step,sdv,type="o",xlab="Steps t",ylab="Standard Deviation")
dev.off()
diameter1000 = diameter(g2)