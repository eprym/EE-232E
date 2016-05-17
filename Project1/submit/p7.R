library(igraph)
path <-"/Users/bairongjing/Documents/ucla\ /16_spring/232/gplus/"
circlesFiles <- list.files(path,pattern="*.circles",full.names=FALSE)
edgesFiles <- list.files(path,pattern="*.edges",full.names=FALSE)
nodeIndex <- strsplit(circlesFiles,".circles")
g_new<-list()#corresponding personal network for each fit node(with more than 2 circles)
circle<-list()#corresponding circle info for each fit node
walk_c<-list()#corresponding community info for walktrap algorithm for each fit node
info_c<-list()#corresponding community info for infomap algorithm each fit node
fitIndex<-1
for(i in 1:length(nodeIndex))
{
  circlesFile <- paste(path,nodeIndex[[i]],".circles",sep="")
  fileConnection <- file(circlesFile, open="r")
  lines<-readLines(fileConnection)
  close(fileConnection)
  
  circles <-strsplit(lines,"\t")
  if(length(circles)>2)
  {
    edgeFile <- paste(path,nodeIndex[[i]],".edges",sep="")
    g_noEgo <-read.graph(edgeFile,format="ncol",directed=TRUE)
    g_noEgo<-g_noEgo+vertex(nodeIndex[[i]],color="black")
    g_noEgo[vcount(g_noEgo),1:(vcount(g_noEgo)-1)]<-1
    g_new[[fitIndex]]<-g_noEgo
    circle[[fitIndex]]<-circles
    walk_c[[fitIndex]] <- walktrap.community(g_noEgo)
    info_c[[fitIndex]] <- infomap.community (g_noEgo)
    fitIndex=fitIndex+1
  }
}

png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/7_1.png")
plot(walk_c[[2]],g_new[[2]],vertex.label=NA,vertex.size=3,edge.arrow.size=0.2,layout=layout.lgl)
dev.off()
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/7_2.png")
plot(info_c[[2]],g_new[[2]],vertex.label=NA,vertex.size=3,edge.arrow.size=0.2)
dev.off()
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/7_3.png")
hist(walk_c[[2]]$membership,col="blue",main="",xlab="Community Number",ylab="Numbers of Nodes in a Community")
dev.off()
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/7_4.png")
hist(info_c[[2]]$membership,col="green",main="",xlab="Community Number",ylab="Numbers of Nodes in a Community")
dev.off()

# we use the BER to calculate the overlap between circle and community
#first: walktrap algorithm
BER_wc_all<-rep(0,fitIndex-1)
for(t in 1:(fitIndex-1)){
  nodeNum<-vcount(g_new[[t]])
  #calculate each circle and community BER
  map_wc<-rep(0,length(sizes(walk_c[[t]])))
  BER_wc<-rep(0,length(sizes(walk_c[[t]])))
  for(i in 1:length(sizes(walk_c[[t]])))
  {
    communityNode<- (walk_c[[t]])$names[which(walk_c[[t]]$membership==i)]
    match<-rep(0,length(circle[[t]]))
    for(j in 1:length(circle[[t]])){
      circleNode <- circle[[t]][[j]]
      #nodenum in both community and circle
      CinC <- length(intersect(communityNode,circleNode))
      Com <- length(communityNode)
      Cir <- length(circleNode)-1
      #calculate the BER of the map
      match[j]<- 1-(abs(Com-CinC)/Com)
    }
    map_wc[i]<-which.max(match)
    BER_wc[i]<-match[map_wc[i]]
  }
  BER_wc_all[t]<-sum(BER_wc)
}

###infomap algorithm

BER_im_all<-rep(0,fitIndex-1)

for(t in 1:(fitIndex-1)){
  nodeNum<-vcount(g_new[[t]])
  map_inc<-rep(0,length(sizes(info_c[[t]])))
  BER_inc<-rep(0,length(sizes(info_c[[t]])))
  for(i in 1:length(sizes(info_c[[t]])))
  {
    communityNode<- (info_c[[t]])$names[which(info_c[[t]]$membership==i)]
    match<-rep(0,length(circle[[t]]))
    for(j in 1:length(circle[[t]])){
      circleNode <- circle[[t]][[j]] 
      #nodenum in both community and circle
      CinC <- length(intersect(communityNode,circleNode))
      Com <- length(communityNode)
      Cir <- length(circleNode)-1
      #calculate the BER of the map
      match[j]<- 1-(abs(Com-CinC)/Com)
    }
    map_inc[i]<-which.max(match)
    BER_inc[i]<-match[map_inc[i]]
  }
  BER_im_all[t]<-sum(BER_inc)
}
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/7_5.png")
plot(BER_wc_all, type = "l", xlab = "index", ylab = "1-BER")
dev.off()
png(filename="/Users/bairongjing/Documents/ucla\ /16_spring/232/project/7_6.png")
plot(BER_im_all, type = "l", xlab = "index", ylab = "1-BER")
dev.off()