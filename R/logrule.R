
#' @title Shrinkage Rule under Logistic Prior
#'@description Performs wavelet shrinkage under logistic prior.
#' @param d The empirical wavelet coefficients vector.
#' @param a The weight of the point mass function at zero in the prior.
#' @param s The standard deviation of the random error.
#' @param t The scale parameter of the logistic distribution in the prior.
#'
#' @return The shrunk wavelet coefficients vector.
#' @export
#'
#' @examples
#' logrule(c(2,1,2.3,0.5,0.1,-0.2,4.8,0.2),0.9,1,5)
#'
logrule = function(d,a,s,t){

  n = length(d)

  logrule = NA

  u = rnorm(10000)

  for(i in 1:n){

    x=(s*u+d[i])*(cosh((s*u+d[i])/(2*t)))^(-2)
    int1 = mean(x)

    y=(cosh((s*u+d[i])/(2*t)))^(-2)
    int2 = mean(y)

    num = (1-a)*int1

    den = 4*t*a*dnorm(d[i],0,s)/s + (1-a)*int2

    logrule[i] = num/den

  }

  logrule

}
