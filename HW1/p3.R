library("igraph")
g1 <- sample_pa_age(1000, directed=FALSE, pa.exp=1, aging.exp=0, aging.bin=1000)
d1 <- degree.distribution(g1)
png(filename="p3_1.png")
plot(d1, type="o", xlab="Degree", ylab="Frequency")
dev.off()

fc <- cluster_fast_greedy(g1)
png(filename="p3_2.png")
plot(fc, g1)
dev.off()

mod <- 0
for(i in 1:100){
  g <- sample_pa_age(1000, directed=FALSE, pa.exp=1, aging.exp=0, aging.bin=1000)
  fc <- cluster_fast_greedy(g)
  mod <- mod+modularity(g, membership(fc))
}

print(mod/100)
