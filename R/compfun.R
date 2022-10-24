library("wavethresh")

#' @title Component Functions Estimates
#'@description Estimates component functions from aggregated curves by applying wavelet shrinkage under logistic prior.
#' @param A Aggregated dataset matrix.
#' @param y Weights of the component functions matrix.
#' @param a The weight of the point mass function at zero in the prior.
#' @param s The standard deviation of the random error.
#' @param t The scale parameter of the logistic distribution in the prior.
#'
#' @return The component functions estimates matrix.
#' @export
#'
#' @examples
#' A = matrix(rnorm(80),16,5)
#' y1 = runif(5)
#' y = t(matrix(c(y1,1-y1),5,2))
#' compfun(A,y,0.9,1,5)


compfun = function(A,y,a,s,t){

  J = dim(A)[1]
  n = dim(A)[2]
  L = dim(y)[1]

  W = t(GenW(J))
  D = W%*%A

  Dshrink = matrix(0,J,n)

  for(i in 1:n){

    Dshrink[,i] = logrule(D[,i],a,s,t)
  }

  thetahat = Dshrink%*%t(y)%*%solve(y%*%t(y))

  alphahat = t(W)%*%thetahat

  return(alphahat)

}
