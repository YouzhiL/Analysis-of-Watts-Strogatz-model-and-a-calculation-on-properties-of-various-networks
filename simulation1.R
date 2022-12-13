## Download the package
install.packages("igraph")
install.packages("ggplot2")
install.packages("reshape2")
## Load package
library(igraph)
library(ggplot2)
library(reshape2)
## Usage: sample_smallworld(dim, size, nei, p, loops = FALSE, multiple = FALSE)
## e.x. g <- sample_smallworld(1, 1000, 10, 0)

probability = c()
for (i in seq(-4, 0, 4/20)) {
  probability = c(probability, 10^i)
}

n <- 1000
m <- 10
total_path_length <- rep(0, 21)
total_cluster_coef <- rep(0, 21)

for (i in 1: 10){
  path_length = c()
  cluster_coef = c()
  for (p in probability) {
    g <- sample_smallworld(1, 1000, 10, p)
    
    path_length = c(path_length, mean_distance(g))
    
    cluster_coef = c(cluster_coef, transitivity(g, type = "average"))
  }
  total_path_length = total_path_length + path_length
  total_cluster_coef = total_cluster_coef + cluster_coef
}

## average and normalize
avg_path_length = total_path_length/10
avg_path_length = avg_path_length/avg_path_length[1]

avg_cluster_coef = total_cluster_coef/10
avg_cluster_coef = avg_cluster_coef/avg_cluster_coef[1]
x = data.frame(v1 = probability, v2 = avg_path_length, v3 = avg_cluster_coef)

## plot

plot(probability, avg_path_length, ylim = c(0,1), ylab = "path_length/cluster_coef", log = "x", pch = 16)
points(probability, avg_cluster_coef, ylim = c(0,1), ylab = "cluster_coef", pch = 15)
legend("bottomleft", legend=c("path_length","cluster_coef"), 
       pch=c(16, 15), ncol=1)

