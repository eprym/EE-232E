library("igraph")
# g <- sample_forestfire(1000, fw.prob=0.37, bw.factor=0.32/0.37, directed=TRUE)
# dd1 <- degree_distribution(g, mode="in")
# dd2 <- degree_distribution(g, mode="out")
# png(filename="degree_in.png")
# plot(dd1, type="o")
# dev.off()
# png(filename="degree_out.png")
# plot(dd2, type="o")
# dev.off()

dd1<-rep(0,220)
dd2<-rep(0,220)
diam <- 0
mod <- 0
for(i in 1:100){
  g <- sample_forestfire(1000, fw.prob=0.37, bw.factor=0.32/0.37, directed=TRUE)
  temp1<- degree.distribution(g, mode="in")
  temp2<- degree.distribution(g, mode="out")
  dd1 <-dd1+c(temp1,rep(0,220-length(temp1)))
  dd2 <-dd2+c(temp2,rep(0,220-length(temp2)))
  diam <- diam + diameter(g)
  fg <- walktrap.community(g)
  mod <- mod + modularity(g,membership(fg))
}
png(filename="degree_in.png")
plot(dd1/100, type="o", xlab="degree", ylab="frequency")
dev.off()

png(filename="degree_out.png")
plot(dd2/100, type="o",xlab="degree", ylab="frequency")
dev.off()

png(filename="in&out.png")
if (interactive()) {
  plot(seq(along=dd1)-1, dd1/100, log="xy", xlab="degree",ylab="frequency")
  points(seq(along=dd2)-1, dd2/100, col=2, pch=2)
}
dev.off()
print(diam/100)

png(filename="community.png")
fg <- walktrap.community(g)
plot(fg,g)
dev.off()
print(mod/100)
#print(modularity(g,membership(fg)))