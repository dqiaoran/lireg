# Linear Regression
#
# The package is designed to do a simple linear regression
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#'lireg
#'
#'Linear Regression Model
#'
#'@param formula
#'
#'@return a list of class "Linear Regression"
#'
#'@examples
#'
#'
#'@export
#'
lireg <- function(formula){
  df = get_all_vars(formula)

  #assume that this is a correct formula
  Y = df[[1]]
  X = df[,2:ncol(df)]

  n = nrow(X)
  p = ncol(X)
  empty = rep(0, n)
  lg = list(response = Y, regressor = X, coefficients = empty,
            num_cases = n, residuals = empty,
            rank = 0, SSE = 0, SSR = 0, SSY = 0, df = 0,
            design_mat = matrix(NA))
  class(lg) = "lireg"

  lg = fit_mlr(lg)
  return(lg)
}

fit_mlr <- function(object){
  z = object
  X = z$regressor
  Y = z$response
  z$design_mat = cbind(rep(1, nrow(X)), X)
  eva_mat = t(z$design_mat) %*% z$design_mat
  z$rank = rankMatrix(eva_mat)

  if(z$rank < ncol(eva_mat)) stop("singular fit encountered")

  #Assume that the design matrix is not singular
  beta = solve(eva_mat)%*%t(z$design_mat)%*%Y
  z$coefficients = beta

  Y_fitted = beta %*% z$design_mat #fitted values

  p = length(beta)
  n = z$num_cases

  z$residuals = Y - beta %*% z$design_mat #residuals
  z$df = n - p

  y_bar = mean(Y)
  z$SSY = sum((Y - y_bar)^2)
  z$SSE = sum(z$residuals^2)
  z$SSR = z$SSY - z$SSE

  return(z)
}

summary.lireg <- function(object, correlation = FALSE){
  z = object
  sigma_est = z$SSE / z$df
  beta_var = sigma_est * solve(t(z$design_mat)%*%z$design_mat)

  #Assume that p is non-zero
  est <- z$coefficients
  tval <- est/se #where is se?
  ans <- z[c("call", "terms")]
  ans$residuals <- z$residuals
  ans$coefficients <-
    cbind(est, se, tval, 2*pt(abs(tval), rdf, lower.tail = FALSE))
  dimnames(ans$coefficients) <-
    list(names(z$coefficients),
         c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  ans$sigma <- sqrt(sigma_est)
  ans$df <- c(p, rdf, NCOL(Qr$qr))


  #Perform t-test on a vector of coefficients
  pt(beta/sqrt(beta_var))
  if(pt<0.05){

  }else{

  }

  #Perform global F-test
  pf(SSR/(p-1)/SSE/(n-p), p-1, n-p)
  if(pf<0.05){

  }else{

  }

  Rsq = SSR / SSY
  Rsq_adj = 1 - SSE/(n-p)/SSY/(n-1)

}
