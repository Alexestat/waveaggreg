
#' @title Classical risk of the logistic shrinkage rule
#' @description Performs the classical risks of the wavelet shrinkage rule under logistic prior.
#' @param theta The wavelet coefficients vector.
#' @param a The weight of the point mass function at zero in the prior.
#' @param s The standard deviation of the random error.
#' @param t The scale parameter of the logistic distribution in the prior.
#'
#' @return The classical risks vector.
#' @export
#'
#' @examples
#' logrisk(c(0,0.2,0.5),0.9,1,5)
#'
logrisk = function(theta,a,s,t){

  n = length(theta)

  logrisk = NA

  for(i in 1:n){

    integrand = function(z){

      (theta[i] - logrule(theta[i]+s*z,a,s,t))^2*dnorm(z)
    }


    logrisk[i] = integrate(integrand,lower = -Inf, upper =Inf)$value
  }
  logrisk
}
