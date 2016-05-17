library(igraph)

rm(list=ls())
g <- read.graph("~/Desktop/ucla/ee232/project1/facebook_combined.txt", format="edgelist", directed=F)

coreNum <- 0
coreList <- numeric(vcount(g))
totalDegree <- 0

for (i in 1:vcount(g)) {
  neiNum <- length(neighborhood(g, 1, i)[[1]]) - 1
  if (neiNum > 200) {
    coreNum <- coreNum + 1
    coreList[coreNum] <- i
    totalDegree <- totalDegree + neiNum
  }
}

coreList <- coreList[1:coreNum]
avgDegree <- totalDegree / coreNum

# use Node No.484
VList <- neighborhood(g, 1, 484)[[1]]
ego <- induced.subgraph(g, VList)
# find core node
max <- -1
for (i in 1:vcount(ego)){
  if (max < length(neighborhood(ego, 1, i)[[1]])) {
    max <- length(neighborhood(ego, 1, i)[[1]])
    core <- i
  }
}
if (degree(ego, core) + 1 != vcount(ego))
  print("Not core node")

embed <- numeric(vcount(ego))
disp <- numeric(vcount(ego))
for (i in 1:vcount(ego)) {
  if (i == core)
    next
  tmpDeg <- degree(ego, i) - 1
  # check if only one in tmpNeibor or none
  if (tmpDeg == 0 || tmpDeg == 1) {
    embed[i] <- tmpDeg
    disp[i] <- 0
    next
  }
  embed[i] <- tmpDeg
  tmpNeibor <- neighborhood(ego, 1, i)[[1]]
  # remove core and itself
  tmpNeibor <- tmpNeibor[which(tmpNeibor != i)]
  tmpNeibor <- tmpNeibor[which(tmpNeibor != core)]
  if (length(tmpNeibor) != tmpDeg)
    print("screw up!")
  tmpDisp <- 0
  # check all pairs
  for (s in 1:(tmpDeg-1)) {
    for (t in (s+1):tmpDeg) {
      sNeibor <- neighborhood(ego, 1, tmpNeibor[s])[[1]]
      # if direct link
      if (tmpNeibor[t] %in% sNeibor)
        next
      # if have common neibor other than...
      tNeibor <- neighborhood(ego, 1, tmpNeibor[t])[[1]]
      commonNeibor <- intersect(sNeibor, tNeibor)
      if (!(i %in% commonNeibor) || !(core %in% commonNeibor))
        print("screw again")
      if (length(commonNeibor) > 2)
        next
      tmpDisp <- tmpDisp + 1
    }
  }
  disp[i] <- tmpDisp
}
plot(embed, ylab="Embeddedness")
plot(disp, ylab="Dispersion")

#################
# traverse all personal networks

rm(ego, embed, disp, core)
# allow size growing
EMBED_NUM <- 400 
DISP_NUM <- 10000
embed <- numeric(EMBED_NUM)
disp <- numeric(DISP_NUM)

for (iter in 1:coreNum) {
  print(iter)
  core <- coreList[iter]
  VList <- neighborhood(g, 1, core)[[1]]
  ego <- induced.subgraph(g, VList)
  # find core node
  max <- -1
  for (i in 1:vcount(ego)){
    if (max < length(neighborhood(ego, 1, i)[[1]])) {
      max <- length(neighborhood(ego, 1, i)[[1]])
      core <- i
    }
  }
  if (max != vcount(ego)) {
    print("Not core node")
    break
  }
  
  for (i in 1:vcount(ego)) {
    if (i == core)
      next
    tmpDeg <- degree(ego, i) - 1
    # check if no neighbor
    if (tmpDeg == 0) {
      embed[1] <- embed[1] + 1
      disp[1] <- disp[1] + 1
      next
    }
    
    # update embed (may contain 0)
    while (tmpDeg > EMBED_NUM - 1) {
      embed2 <- numeric(EMBED_NUM * 2)
      embed2[1:EMBED_NUM] <- embed
      EMBED_NUM <- EMBED_NUM * 2
      embed <- embed2
      rm(embed2)
    }
    embed[tmpDeg+1] <- embed[tmpDeg+1] + 1
    
    tmpNeibor <- neighborhood(ego, 1, i)[[1]]
    # remove core and itself
    tmpNeibor <- tmpNeibor[which(tmpNeibor != i)]
    tmpNeibor <- tmpNeibor[which(tmpNeibor != core)]
    if (length(tmpNeibor) != tmpDeg)
      print("screw up!")
    tmpDisp <- 0
    # check if only one in tmpNeibor
    if (tmpDeg == 1) {
      disp[1] <- disp[1] + 1
      next
    }
    # check all pairs
    for (s in 1:(tmpDeg-1)) {
      for (t in (s+1):tmpDeg) {
        sNeibor <- neighborhood(ego, 1, tmpNeibor[s])[[1]]
        # if direct link
        if (tmpNeibor[t] %in% sNeibor)
          next
        # if have common neibor other than...
        tNeibor <- neighborhood(ego, 1, tmpNeibor[t])[[1]]
        commonNeibor <- intersect(sNeibor, tNeibor)
        if (!(i %in% commonNeibor) || !(core %in% commonNeibor))
          print("screw again")
        if (length(commonNeibor) > 2)
          next
        tmpDisp <- tmpDisp + 1
      }
    }
    # update disp (may contain 0)
    while (tmpDisp > DISP_NUM - 1) {
      disp2 <- numeric(DISP_NUM * 2)
      disp2[1:DISP_NUM] <- disp
      DISP_NUM <- DISP_NUM * 2
      disp <- disp2
      rm(disp2)
    }
    disp[tmpDisp+1] <- disp[tmpDisp+1] + 1
  }
  rm(VList, ego)
}

# cutoff
i <- EMBED_NUM
while (embed[i] == 0) 
  i <- i-1
embed2 <- embed[1:i]

i <- DISP_NUM
while (disp[i] == 0) 
  i <- i-1
disp2 <- disp[1:i]

write.csv(embed2, "~/Desktop/ucla/ee232/project1/embed.csv", row.names = F)
write.csv(disp2, "~/Desktop/ucla/ee232/project1/disp.csv", row.names = F)
# embed3 <- read.csv("~/Downloads/EE232E/pro1/embed.csv")$x

embedSum <- sum(embed2)
dispSum <- sum(disp2)
embed2 <- embed2 / embedSum
disp2 <- disp2 / dispSum
plot(embed2, xlab="Embeddedness", ylab="Distribution")
plot(disp2, xlab="Dispersion", ylab="Distribution")

################################
# plot 3 examples
rm(embed, disp, fc, ego, VList)
# use 1, 349, 484
VList <- neighborhood(g, 1, 349)[[1]]
ego <- induced.subgraph(g, VList)
# find core node
max <- -1
for (i in 1:vcount(ego)){
  if (max < length(neighborhood(ego, 1, i)[[1]])) {
    max <- length(neighborhood(ego, 1, i)[[1]])
    core <- i
  }
}
if (degree(ego, core) + 1 != vcount(ego))
  print("Not core node")

embed <- numeric(vcount(ego))
disp <- numeric(vcount(ego))
for (i in 1:vcount(ego)) {
  if (i == core)
    next
  tmpDeg <- degree(ego, i) - 1
  # check if only one in tmpNeibor or none
  if (tmpDeg == 0 || tmpDeg == 1) {
    embed[i] <- tmpDeg
    disp[i] <- 0
    next
  }
  embed[i] <- tmpDeg
  tmpNeibor <- neighborhood(ego, 1, i)[[1]]
  # remove core and itself
  tmpNeibor <- tmpNeibor[which(tmpNeibor != i)]
  tmpNeibor <- tmpNeibor[which(tmpNeibor != core)]
  if (length(tmpNeibor) != tmpDeg)
    print("screw up!")
  tmpDisp <- 0
  # check all pairs
  for (s in 1:(tmpDeg-1)) {
    for (t in (s+1):tmpDeg) {
      sNeibor <- neighborhood(ego, 1, tmpNeibor[s])[[1]]
      # if direct link
      if (tmpNeibor[t] %in% sNeibor)
        next
      # if have common neibor other than...
      tNeibor <- neighborhood(ego, 1, tmpNeibor[t])[[1]]
      commonNeibor <- intersect(sNeibor, tNeibor)
      if (!(i %in% commonNeibor) || !(core %in% commonNeibor))
        print("screw again")
      if (length(commonNeibor) > 2)
        next
      tmpDisp <- tmpDisp + 1
    }
  }
  disp[i] <- tmpDisp
}

fc <- fastgreedy.community(ego)

maxDispIndex <- which(disp == max(disp))
maxEmbedIndex <- which(embed == max(embed))
ratio <- disp / (embed + 1e-5)
maxRatioIndex <- which(ratio == max(ratio))

V(ego) $ color <- membership(fc)
sizeArray <- rep.int(2, vcount(ego))
sizeArray[maxRatioIndex] <- 9
E(ego) $ color <- "grey"
E(ego)[incident(ego, maxRatioIndex)] $ color <- "red"
E(ego)[incident(ego, maxRatioIndex)] $ width <- 4
plot(ego, vertex.label = NA, vertex.size = sizeArray)
