
#' @title Variance of the logistic shrinkage rule.
#' @description Performs the variance of the wavelet shrinkage rule under logistic prior.
#' @param theta The wavelet coefficients vector.
#' @param a The weight of the point mass function at zero in the prior.
#' @param s The standard deviation of the random error.
#' @param t The scale parameter of the logistic distribution in the prior.
#'
#' @return The variances vector.
#' @export
#'
#' @examples
#' logvar(c(0,0.2,0.5),0.9,1,5)
#'
logvar = function(theta,a,s,t){

  n = length(theta)

  logvar = NA

  for(i in 1:n){

    integrand1 = function(z){

      logrule(theta[i]+s*z,a,s,t)*dnorm(z)

    }

    expec = integrate(integrand1,lower = -50, upper = 50)$value

    integrand2 = function(z){

      logrule(theta[i]+s*z,a,s,t)^2*dnorm(z)

    }

    mom2 = integrate(integrand2,lower = -50, upper = 50)$value

    logvar[i] = mom2 - expec^2

  }

  logvar

}
