library(tnet)
net <- read.table("http://opsahl.co.uk/tnet/datasets/OClinks_w.txt")

# Create a binary version of the network
bnet <- dichotomise_w(net)
# Create igraph object
# e.x. g <- tnet_igraph(net,type="weighted one-mode tnet")
graph1 <- tnet_igraph(net)
# Get summary statistics
# Count number of edges
edge_number <- gsize(graph1)

# Count number of vertices
vertex_number <- gorder(graph1)

# calculate mean degree
mean_degree <- mean(degree(graph1))

# hist(degree(graph1),
#      xlab = "k",
#      ylab = "Frequency",
#      main = "Histogram of nodes degrees",
#      col = "skyblue")

# Calculate the average distance in the binary network
#mean(distance_w(bnet),na.rm=T)
actual_path_length <- mean_distance(graph1)

# Calculate the average distance in the weighted network
#mean(distance_w(net),na.rm=T)
actual_clustering_coef <- transitivity(graph1, type = "average")

## set p = 0 using same number of edges and vertices. This is the case of regular lattice.
g0 <- sample_smallworld(1, vertex_number, mean_degree, 0.01)
path_length0 <- mean_distance(g0)
cluster_coef0 <- transitivity(g0, type = "average")

## set p = 1 using same number of edges and vertices. This is the case of completely random graph.
g1 <- sample_smallworld(1, vertex_number, mean_degree, 1)
path_length1 <- mean_distance(g1)
cluster_coef1 <- transitivity(g1, type = "average")
actual_path_length
actual_clustering_coef
path_length0
cluster_coef0
path_length1
cluster_coef1


