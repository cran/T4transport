## ----init, eval=TRUE, echo=FALSE----------------------------------------------
if (!requireNamespace("mlbench", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
  message("Install 'mlbench' to run the code in this vignette.")
}

## ----setup, include=FALSE, echo=FALSE-----------------------------------------
knitr::opts_chunk$set(cache = TRUE)
library(mlbench)
library(T4transport)
set.seed(10)

## ----toy_code, echo=TRUE, eval=TRUE, cache=FALSE, fig.alt="Two dataets: Cassini and Smiley"----
# load the library
library(mlbench)

# generate two datasets
data1 = mlbench::mlbench.cassini(n=100)$x
data2 = mlbench::mlbench.smiley(n=100)$x

# normalize the datasets
data1 = as.matrix(scale(data1))
data2 = as.matrix(scale(data2))

# translate the second dataset
data2[,1] = data2[,1] + 5

# plot the datasets
plot(data1, col="blue", pch=19, cex=0.5, main="Two datasets", 
     xlim=c(-2, 7), xlab="x", ylab="y")
points(data2[,1], data2[,2], col="red", cex=0.5, pch=19)

## ----compute1, echo=TRUE, eval=TRUE, cache=TRUE, fig.alt="Wasserstein distance computation"----
# call the function
output = wasserstein(data1, data2, p=2)

# print the output
print(paste0("2-wasserstein distance: ",round(output$distance, 4)))

## ----compute2a, echo=FALSE, eval=TRUE, fig.alt="Optimal coupling matrix", fig.align="center"----
par(pty="s")
## --- Plot 1: Optimal coupling matrix
P = output$plan
image(
  x = 1:nrow(P), 
  y = 1:ncol(P), 
  z = t(P)[, nrow(P):1],         # transpose + flip rows
  col = gray.colors(100, start = 1, end = 0), 
  xlab = "Source Index", 
  ylab = "Target Index",
  main = "Optimal Coupling",
  axes = FALSE
)
axis(1, at = 1:nrow(P), labels = 1:nrow(P), las = 2, cex.axis = 0.6, tick=FALSE)
axis(2, at = 1:ncol(P), labels = rev(1:ncol(P)), las = 2, cex.axis = 0.6, tick=FALSE)

## ----compute2b, echo=FALSE, eval=TRUE, fig.alt="Optimal coupling scatterplot", fig.align="center"----
## --- Plot 2: Bipartite graph
plot(data1, col="blue", pch=19, cex=0.5, main="Bipartite Graph", 
     xlim=c(-2, 7), xlab="x", ylab="y")
points(data2[,1], data2[,2], col="red", cex=0.5, pch=19)
maxP = max(P)
multiplier = 0.5
for (i in 1:nrow(data1)){
  for (j in 1:nrow(data2)){
    if (P[i,j] > 0){
      lines(x = c(data1[i, 1], data2[j, 1]),
            y = c(data1[i, 2], data2[j, 2]),
            col = "gray40",
            lwd = multiplier * P[i, j] / maxP)
    }
  }
}

## ----computeD, echo=TRUE, eval=TRUE, cache=TRUE, fig.alt="Wasserstein distance computation with distances"----
# compute the cross distance with a helper function
cross_dist <- function(X, Y) {
  X2 <- rowSums(X^2)
  Y2 <- rowSums(Y^2)
  sqrt(outer(X2, Y2, "+") - 2 * tcrossprod(X, Y))
}
cdist = cross_dist(data1, data2)

# call the function
crossed = wassersteinD(cdist, p=2)

# print the output
print(paste0("2-wasserstein distance: ",round(crossed$distance, 4)))

