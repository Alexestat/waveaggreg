
#' @title Bayesian risk of the logistic shrinkage rule
#' @description Performs the bayesian risk of the wavelet shrinkage rule under logistic prior.
#' @param a The weight of the point mass function at zero in the prior.
#' @param s The standard deviation of the random error.
#' @param t The scale parameter of the logistic distribution in the prior.
#'
#' @return The bayesian risk value.
#' @export
#'
#' @examples
#' logbrisk(0.9,1,5)
#'
logbrisk = function(a,s,t){

  u = rlogis(100,0,t)

  x = logrisk(u,a,s,t)

  logbrisk = a*logrisk(0,a,s,t) + (1-a)*mean(x)

  logbrisk

}
