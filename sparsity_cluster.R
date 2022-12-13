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

total_cluster_coef1 <- rep(0, 21)
total_cluster_coef2 <- rep(0, 21)
total_cluster_coef3 <- rep(0, 21)

for (i in 1: 10){
  cluster_coef1 = c()
  cluster_coef2 = c()
  cluster_coef3 = c()
  for (p in probability) {
    g1 <- sample_smallworld(1, 1000, 10, p)
    g2 <- sample_smallworld(1, 1000, 30, p)
    g3 <- sample_smallworld(1, 1000, 50, p)
    
    cluster_coef1 = c(cluster_coef1, transitivity(g1, type = "average"))
    cluster_coef2 = c(cluster_coef2, transitivity(g2, type = "average"))
    cluster_coef3 = c(cluster_coef3, transitivity(g3, type = "average"))
  }

  total_cluster_coef1 = total_cluster_coef1 + cluster_coef1
  total_cluster_coef2 = total_cluster_coef2 + cluster_coef2
  total_cluster_coef3 = total_cluster_coef3 + cluster_coef3
}
## average and normalize
avg_cluster_coef1  = total_cluster_coef1 /10
avg_cluster_coef1  = avg_cluster_coef1 /avg_cluster_coef1 [1]
avg_cluster_coef2 = total_cluster_coef2/10
avg_cluster_coef2 = avg_cluster_coef2/avg_cluster_coef2[1]
avg_cluster_coef3 = total_cluster_coef3/10
avg_cluster_coef3 = avg_cluster_coef3/avg_cluster_coef3[1]

plot(probability, avg_cluster_coef1, ylim = c(0,1), ylab = "cluster_coef", log = "x",pch = 1)
points(probability, avg_cluster_coef2, ylim = c(0,1), ylab = "cluster_coef", pch = 0)
points(probability, avg_cluster_coef3, ylim = c(0,1), ylab = "cluster_coef", pch = 2)
legend("bottomleft", legend=c("average degree = 10","average degree = 30","average degree = 50"), 
       pch=c(1,0,2), ncol=1)


