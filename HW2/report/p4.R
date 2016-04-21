library("igraph")
library("netrw")

#part a
g = random.graph.game(1000, 0.01, directed = TRUE)
rw <- netrw(g, walker.num = 1000, damping=0.85)

png(filename="p4_a.png")
plot(rw$ave.visit.prob, xlab="Node", ylab="The average visit probability")
dev.off()

#part b
pagerank = page.rank(g, damping = 0.85, directed = TRUE)
rw_own <-  netrw(g, walker.num = 1000, damping=0.85, local.pagerank = TRUE, teleport.prob = pagerank$vector)

png(filename="p4_b.png")
plot(rw_own$ave.visit.prob, xlab="Node", ylab="The average visit probability")
dev.off()

png(filename="p4_compare.png")
plot(rw$ave.visit.prob[seq(1,1000,10)], type="l", col="red", xlab="Node", ylab="The average visit probability")
par(new="TRUE")
plot(rw_own$ave.visit.prob[seq(1,1000,10)], type="l", col="green", xlab="Node", ylab="The average visit probability")
legend("topright", lty=c(1,1), c("visit prob", "visit prob with own importance"), col=c("red", "green"))
dev.off()