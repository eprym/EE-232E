library("igraph")
g_fb <-read.graph("~/Desktop/ucla/ee232/project1/facebook_combined.txt",format="ncol",directed=FALSE)

############Part1############
print(is.connected(g_fb))
print(diameter(g_fb))

dd<-degree.distribution(g_fb)
dd<-dd[2:length(dd)]

png(filename="~/Desktop/ucla/ee232/project1/Q1_1.png")
plot(dd,type="l",main="Degree Distribution",xlab="Degree",ylab="Relative Frequency")
dev.off()
ds<-data.frame(x=1:length(dd),y=dd)
m<-nls(y~I(1/(a*x^power+b)),data=ds,start=list(power=1,a=1,b=0),trace=T)

summary(m)
power<-round(summary(m)$coefficient[1],3)
a<-round(summary(m)$coefficient[2],3)
b<-round(summary(m)$coefficient[3],3)

png(filename="~/Desktop/ucla/ee232/project1/Q1_2.png")
plot(dd,type="l",main="Fitted Power Model",xlab="Degree",ylab="Relative Frequency",sub="red:model;black:data")
lines(predict(m),lty=1,col="red")
text(10,0.02,paste("y=1/(",a,"*x^",power,"+",b,")",sep=""),pos=4)
dev.off()

print(aveDegree <- mean(degree(g_fb)))
RSS.p<-sum(residuals(m)^2)
TSS <- sum((dd-mean(dd))^2)
pro<-1-(RSS.p/TSS)
MSE<-RSS.p/length(dd)
print(MSE)



############Part2############
personalNetwork <- graph.neighborhood(g_fb,1,nodes=V(g_fb))
subNet<-personalNetwork[[1]]
nodeSize <- rep(3,vcount(subNet))
nodeSize[V(subNet)$name==V(g_fb)[1]$name]=6
V(subNet)$color="skyblue"
V(subNet)[V(subNet)$name==V(g_fb)[1]$name]$color="black"
png(filename="~/Desktop/ucla/ee232/project1/Q2.png")
plot(subNet,main= "Personal Network of Node 1", vertex.size=nodeSize,vertex.label=NA,asp=9/16)
dev.off()
numOfNodes <- vcount(subNet)
numOfEdges <- ecount(subNet)