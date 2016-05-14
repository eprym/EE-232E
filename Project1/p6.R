library("igraph")

fb_graph = read.graph("facebook_combined.txt", format = "ncol", directed = FALSE)
personalNetworks <- make_ego_graph(fb_graph, 1, nodes = V(fb_graph))
coreNodes =  which(ego_size(fb_graph, 1) > 200)
coreNum = length(coreNodes)
type1_cluster <- rep(0, coreNum)
type2_cluster <- rep(0, coreNum)
type1_density <- rep(0, coreNum)
type2_density<- rep(0, coreNum)
for(i in 1:coreNum){
  pn = personalNetworks[[coreNodes[i]]]
  fgc = fastgreedy.community(pn)
  cluster_coff = rep(0, length(sizes(fgc)))
  density = rep(0, length(sizes(fgc)))
  for(j in 1:length(sizes(fgc))){
    if(sizes(fgc)[j] <= 10) next
    node_to_delete = which(fgc$membership != j)
    pn_tmp = delete.vertices(pn, node_to_delete)
    cluster_coff[j] = transitivity(pn_tmp, type="globalundirected")
    density[j] = graph.density(pn_tmp)
  }
  type1_cluster[i] = which.max(cluster_coff)
  type2_cluster[i] = which.min(cluster_coff)
  type1_density[i] = which.max(cluster_coff)
  type2_density[i] = which.min(cluster_coff)
}
print(type1_cluster)
print(type1_density)
print(type2_cluster)
print(type2_density)