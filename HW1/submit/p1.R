library ("igraph")
p1 <- 0.01
degreeVector1 = numeric()
dia1 <- 0
count1 <- 0
for (iteration in 1:50)
{
  g1<- random.graph.game(1000, p1, directed=F)
  degreeVector1 = c(degreeVector1,degree(g1))
  if (is.connected(g1))
  {
    count1 <-count1 + 1
  }
  dia1 <- dia1 + diameter(g1)

}
count1 <-count1 * 0.02
print(count1)
dia1 <- dia1 / 50
print(dia1)
h1 = hist(degreeVector1, breaks = seq(from = 0, to = max(degreeVector1) + 1, by = 1), freq = FALSE, main = "Histrogram1",
          xlab = "Degree")
for p = 0.05
p2 <- 0.05
degreeVector2 = numeric()
dia2 <- 0
count2 <- 0
for (iteration in 1:50)
{
  g2<- random.graph.game(1000, p2, directed=F)
  degreeVector2 = c(degreeVector2,degree(g2))
  if (is.connected(g2))
  {
    count2 <-count2 + 1
  }
  dia2 <- dia2 + diameter(g2)

}
count2 <-count2 * 0.02
print(count2)
dia2 <- dia2 / 50
print(dia2)
h2 = hist(degreeVector2, breaks = seq(from = 0, to = max(degreeVector2) + 1, by = 1), freq = FALSE, main = "Histrogram2",
          xlab = "Degree")
# for p = 0.1
p3 <- 0.1
degreeVector3 = numeric()
dia3 <- 0
count3 <- 0
for (iteration in 1:50)
{
  g3<- random.graph.game(1000, p3, directed=F)
  degreeVector3 = c(degreeVector3,degree(g3))
  if (is.connected(g3))
  {
    count3 <-count3 + 1
  }
  dia3 <- dia3 + diameter(g3)

}
count3 <-count3 * 0.02
print(count3)
dia3 <- dia3 / 50
print(dia3)
h3 = hist(degreeVector3, breaks = seq(from = 0, to = max(degreeVector3) + 1, by = 1), freq = FALSE, main = "Histrogram3",
          xlab = "Degree")

iteration = 100
res = numeric()
for (i in 1:iteration)
{
       for (p in seq(from = 0, to = 0.1, by = 1e-4)){
         g <- random.graph.game(1000,p, directed = FALSE)
         if (is.connected(g))
         {
           res <- c(res,p)
           break
         }
       }
}
print (mean(res))


