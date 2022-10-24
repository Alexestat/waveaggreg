
#' @title Bias of the logistic shrinkage rule.
#'@description Performs the bias of the wavelet shrinkage rule under logistic prior.
#' @param theta The wavelet coefficients vector.
#' @param a The weight of the point mass function at zero in the prior.
#' @param s The standard deviation of the random error.
#' @param t The scale parameter of the logistic distribution in the prior.
#'
#' @return The bias vector.
#' @export
#'
#' @examples
#' logbias(c(0,0.2,0.5),0.9,1,5)

logbias = function(theta,a,s,t){

  n = length(theta)

  logbias = NA

  for(i in 1:n){

    integrand = function(z){

      logrule(theta[i]+s*z,a,s,t)*dnorm(z)

    }

    expec = integrate(integrand,lower = -Inf, upper = Inf, stop.on.error = FALSE)$value

    logbias[i] = theta[i] - expec

  }

  logbias

}
