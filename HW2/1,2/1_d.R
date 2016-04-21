library ("igraph")
library ("netrw")
#1)a random networks with 100 nodes
#create a random graph
p <- 0.01
nodeNum <- 100
#g <- random.graph.game(nodeNum, p, directed=FALSE);
#measure the average distance of the walker
step <- 100
# walkNum <-nodeNum
# avg <- numeric()
# sdv<- numeric()
# for(t in 1:step){
# 
#   rw <- netrw(g,walker.num = walkNum,
#               damping=1,weights=NULL,T=t,
#               output.walk.path=TRUE)
#   shortestDistance<-numeric()
#   for(w in 1:walkNum)
#   {
#     #calculate the shortest paths from starting point to the t step node
#     tmp <- get.shortest.paths(g,from=rw$walk.path[1,w],to=rw$walk.path[t,w])
#     #dist = shortest.paths(g,v =rw$walk.path[1,w],to=rw$walk.path[t,w] )
#     shortestDistance <-c(shortestDistance,length(tmp$vpath[[1]])-1)
# 
#   }
#   avg <- c(avg,mean(shortestDistance))
#   sdv <- c(sdv,sd(shortestDistance))
# }
# png(filename="C:/Users/rjbai/Desktop/232/Q1d_1_100.png")
# plot(1:step,avg,type="o",xlab="Steps t",ylab="Average Distance")
# dev.off()
# png(filename="C:/Users/rjbai/Desktop/232/Q1d_2_100.png")
# plot(1:step,sdv,type="o",xlab="Steps t",ylab="Standard Deviation")
# dev.off()
# diameter_2_1 = diameter(g)
# print (diameter_2_1)
#

# #2)10000node
# # #measure the average distance of the walker
nodeNum2 <- 10000
g2 <- random.graph.game(nodeNum2, p, directed=FALSE);
walkNum2 <-1000
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
    dist = shortest.paths(g2,v =rw$walk.path[1,w],to=rw$walk.path[t,w] )
    shortestDistance <-c(shortestDistance,length(tmp$vpath[[1]])-1)

  }
  avg <- c(avg,mean(shortestDistance))
  sdv <- c(sdv,sd(shortestDistance))
}
png(filename="C:/Users/rjbai/Desktop/232/Q1d_1_10000.png")
plot(1:step,avg,type="o",xlab="Steps t",ylab="Average Distance")
dev.off()
png(filename="C:/Users/rjbai/Desktop/232/Q1d_2_10000.png")
plot(1:step,sdv,type="o",xlab="Steps t",ylab="Standard Deviation")
dev.off()
diameter2 <-diameter(g2)
print(diameter2)