library(igraph)
g <- readRDS("C:\\Users\\ferr\\Documents\\ACSAI\\A2SEM2\\Data-Quattrociocchi\\Project\\movie_graph_full.rds")

print(g)
summary(g)

# Check if 'community' exists as a vertex attribute
if("community" %in% vertex_attr_names(g)) {  # CORRECTED LINE
  # Communities are stored in the graph
  communities <- V(g)$community
  print("Communities found in graph attributes!")
  print(paste("Number of communities:", length(unique(communities))))
} else {
  print("Communities not found in graph attributes!")
  # You'll need to recalculate communities
}
summary(components(g))