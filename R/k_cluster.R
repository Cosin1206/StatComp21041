#' @title A cluster algorithm in K_means
#' @description \code{PESQ_mos} and \code{MOS_lqo} are vital indexes in the period of Speech Separation with a funtcional relationship
#' @param x first dimension of the pointset
#' @param y second dimension of the pointset
#' @param n number of clusters
#' @return the belonged cluster of each point
#' @examples
#' \dontrun{
#' k_cluster(cars$dist, cars$speed, 3)
#' }
#' @export
k_cluster = function(x, y, n){
  
  # Define distance function
  distance = function(point1, point2){
    a <- point1[1] - point2[1]
    b <- point1[2] - point2[2]
    ds <- (a ^ 2 + b ^ 2) ^ 0.5
    return(ds)
  }
  
  # Setup pointset
  pointset = cbind(x,y)
  
  for(it in 1:100){
    
    # Starting position of n points
    start <- rep(0, 2 * n)
    starting_points = matrix(data = start, nrow = n, ncol = 2)
    
    #give initial value
    
    for(i in 1:n){
      d <- quantile(x, (i-0.5)/n)
      e <- quantile(y, (i-0.5)/n)
      starting_points[i,] = c(d, e)
    }
    
    value <- rep(0, dim(pointset)[1] * n)
    dist_mat = matrix(data <- value, nrow = dim(pointset)[1], ncol = n)
    
    for(i in 1:n){
      dist_pi = vector()
      for(j in 1:dim(pointset)[1]){
        dist_pi = c(dist_pi, distance(pointset[j,], starting_points[i,]))
      }
      dist_mat[,i] = dist_pi
    }
    
    which_group = vector()
    for(i in 1:dim(pointset)[1]){
      which_group[i] = which.min(dist_mat[i,])
    }
    
    for(i in 1:n){
      one_group = pointset[which_group == i,]
      if(length(one_group) > 0){
        starting_points[i,] = c(mean(one_group[,1]), mean(one_group[,2]))
      }
    }
    
  }
  
    
    plot(x, y, col = which_group + 1, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), pch = 16)
    
    # Plot out centers
    for(i in 1:n){
      points(starting_points[i,1], starting_points[i,2], cex = 2.5, col = i + 1, pch = 8)
    }
    
  return(which_group)
}