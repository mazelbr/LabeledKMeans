# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

LabeledKMeansEst <-function(X, y, labels, k){
  centroids<-c()
  centroids_label <- c()

  for(label in labels){
    print(label)

    #Fit K-Mean for each
    fit.kmean <- stats::kmeans(X[which(y==label),],k)


    centroids <- rbind(centroids, fit.kmean$centers)
    centroids_label <-c(centroids_label,rep(label,k))


  }

  list(centers = centroids, labels = centroids_label)
}

#Generic method
LabeledKMeans <- function(X, ...) UseMethod("LabeledKMeans")

# Default Method
LabeledKMeans.default <- function(X, y, labels, k, ...){
  X <- as.matrix(X)
  y <- as.vector(y)
  labels <- as.vector(labels)

  est <- LabeledKMeansEst(X, y, labels, k)
  class(est) <- "LabeledKMeans"
  est

}

#Predict Function
predict.LabeledKMeans <- function(object, newdata){
  centroids <- object$centers
  dim_centroids <- dim(centroids)

  cen_labels <- object$labels


  # (nx1) - vector to expand each row of  new data to (nxp) matrix
  # for faster calculations
  ones <- matrix(rep(1, dim_centroids[1]))

  #computation of closest cluster (centroids - n x xi)
  # eucledian distance ( sum(diag(X*t(X)))) )
  dist_i <- function(xi){
    rep_i <- ones %*% xi
    pointwise_diff <- centroids - rep_i
    which.min(
      diag(
        tcrossprod(pointwise_diff)
      )
    )
  }


  #calculate closest distance for each row of newdata
  closest_cluster <- apply(newdata, 1 ,FUN = dist_i)
  #distance <- as.matrix(dist(rbind(centroids,newdata)))

  return(cen_labels[closest_cluster])
}
