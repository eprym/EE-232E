library("igraph")
dir <- dirname(sys.frame(1)$ofile)
setwd(dir)
g<-read.graph("../project_2_data/movie_edge_list.txt",format="ncol",directed=FALSE)
movie_interest <- c("Batman v Superman: Dawn of Justice (2016)", "Mission: Impossible - Rogue Nation (2015)",
                   "Minions (2015)")

neighbor<-list()
edgeW<-list()
comId<-rep(0,3)
for(i in 1:3){
  nodeID<-(1:vcount(g))[V(g)$movieName==movie_interest[i]]
  tmp<-ego(g,1,V(g)[nodeID])
  neighbor[[i]]<-tmp[[1]][2:length(tmp[[1]])]
  print(movie_interest[i])
  edge_weight <- rep(0,length(neighbor[[i]]))
  for(j in 1:length(neighbor[[i]])){
    edge_weight[j] <- g[from=nodeID,to=neighbor[[i]][j]]
  }
  names(edge_weight)<-neighbor[[i]]
  edgeW[[i]]<- edge_weight
  edge_weight <- sort(edge_weight,decreasing=TRUE)
  neighbor_name<-as.numeric(names(edge_weight[1:5]))
  neighbor_name<-V(g)[neighbor_name]$movieName
  #print(neighbor_name)
  comId[i]<-com$membership[nodeID]
  #print(comId[i])
  # print(gene_i[[comId[i]]])
}

for(i in 1:3) {
  #select rates from neighbors
  neigh_rate<-V(g)[neighbor[[i]]]$Rate
  neigh_rate<-as.numeric(neigh_rate)
  neigh_rate<-neigh_rate[which(neigh_rate!=0)]
  #select rates from the same community
  comNode<-(1:vcount(g))[com$membership==comId[i]]
  com_rate<-V(g)[comNode]$Rate
  com_rate<-as.numeric(com_rate)
  com_rate<-com_rate[which(com_rate!=0)]
  
  
  r1<-mean(neigh_rate)
  r2<-mean(com_rate) 
  edgeW[[i]]<-edgeW[[i]]/sum(edgeW[[i]])
  r1_p<-as.numeric(V(g)[neighbor[[i]]]$Rate)*edgeW[[i]]
  r1_p <- sum(r1_p[which(r1_p != 0)])
  
  r_simple1<-0.5*r1+0.5*r2
  r_simple2<-(sum(neigh_rate)+sum(com_rate))/(length(neigh_rate)+length(com_rate))
  r_weightAdd1<-0.5*r1_p+0.5*r2
  r_weightAdd2<-(r1_p*length(neigh_rate)+sum(com_rate))/(length(neigh_rate)+length(com_rate))
  
  print(movie_interest[i])
  print(r_simple1)
  print(r_simple2)
  print(r_weightAdd1)
  print(r_weightAdd2)
}

