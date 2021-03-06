library("igraph")
library("netrw")
#(3a)
RandomWalker2 =function(g,nNodes, DF)
{
  r=netrw(g, damping=DF,T=1000, output.walk.path=TRUE,output.visit.prob=TRUE)
  par(mfrow=c(1,1))
  #plot(r$ave.visit.prob)
  
  #plot relationship between degree distribution and visted nodes'degree distribution
  deg=numeric(0)
  vst=numeric(0)
  #degree for undirected graph
  #deg=degree(g)
  #degree for directed graph
  #deg=degree(g,mode="in")
  deg=degree(g,mode="out")
  png(filename="/Users/mysteryjoe/Desktop/ucla/ee232/EE-232E/HW2/problem3/p3_3.png")
  plot(rw_damp$ave.visit.prob[1:100],col="green",type="l",xlab="Node",ylab="Average Visit Probability")
  par(new="TRUE")
  plot(degree(g_un)[1:100],col="red",type="l",xlab="Node",ylab="Degree") 
  dev.off()
  vst=r$ave.visit.prob
  sumprob= numeric(max(deg)-min(deg)+1)
  count= numeric(max(deg)-min(deg)+1)
   #print(deg)
  for (i in 1:1000){
    sumprob[deg[i]]<-sumprob[deg[i]]+r$ave.visit.prob[i]
    count[deg[i]]<-count[deg[i]]+1
  }
  for (k in min(deg):max(deg))
  {
    sumprob[k]=sumprob[k]/count[k]
  }
 #  print(sumprob)
  plot(sumprob,main="Relationship Between Prob and In-Degree",xlab="degree",ylab="prob",type="o")
  cor=cor(deg,vst)
  print(cor)
}

#the answer is yes, linearly related for undirected, no for directed
nNodes=1000
p=0.01
g=random.graph.game(nNodes,p,directed=FALSE)
#RandomWalker2(g,nNodes,1)

#(3b)
gg=random.graph.game(nNodes,p,directed=TRUE)
RandomWalker2(ggg,nNodes,1)
#3c
#RandomWalker2(g,nNodes,0.85)
