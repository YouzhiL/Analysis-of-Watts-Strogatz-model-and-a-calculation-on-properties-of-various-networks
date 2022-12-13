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
total_path_length1 <- rep(0, 21)
total_path_length2 <- rep(0, 21)
total_path_length3 <- rep(0, 21)

# avg_path_length = c()
# avg_cluster_coef = c()
for (i in 1: 10){
  path_length1 = c()
  path_length2 = c()
  path_length3 = c()

  for (p in probability) {
    g1 <- sample_smallworld(1, 1000, 10, p)
    g2 <- sample_smallworld(1, 1000, 30, p)
    g3 <- sample_smallworld(1, 1000, 50, p)
    
    path_length1 = c(path_length1, mean_distance(g1))
    path_length2 = c(path_length2, mean_distance(g2))
    path_length3 = c(path_length3, mean_distance(g3))
  }
  total_path_length1 = total_path_length1 + path_length1
  total_path_length2 = total_path_length2 + path_length2
  total_path_length3 = total_path_length3 + path_length3

}
## average and normalize
avg_path_length1 = total_path_length1/10
avg_path_length1 = avg_path_length1/avg_path_length1[1]
avg_path_length2 = total_path_length2/10
avg_path_length2 = avg_path_length2/avg_path_length2[1]
avg_path_length3 = total_path_length3/10
avg_path_length3 = avg_path_length3/avg_path_length3[1]

plot(probability, avg_path_length1, ylim = c(0,1), ylab = "path_length", log = "x", pch = 1)
points(probability, avg_path_length2, ylim = c(0,1), ylab = "path_length", pch = 0)
points(probability, avg_path_length3, ylim = c(0,1), ylab = "path_length", pch = 2)
legend("bottomleft", legend=c("average degree = 10","average degree = 30","average degree = 50"), 
       pch=c(1,0,2), ncol=1)


