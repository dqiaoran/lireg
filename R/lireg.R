# Linear Regression
#
# The package is used to fit linear regression model
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

library(Matrix)

#'lireg
#'
#'Linear Regression Model
#'
#'@param formula
#'
#'@return a list of class "lireg"
#'
#'@examples
#'
#'@import Matrix
#'
#'@export
#'
lireg <- function(formula){
  df = model.frame(formula)

  #assume that this is a correct formula
  Y = df[[1]]
  X = df[,2:ncol(df)]

  if(is.null(nrow(X))){
    n = length(X)
    p = 1
  }else{
    n = nrow(X)
    p = ncol(X)
  }

  lg = list(call = match.call(), response = Y, regressor = X,
            coefficients = NULL, num_cases = n, residuals = NULL,
            rank = 0, SSE = 0, SSR = 0, SSY = 0, df = 0, design_mat = NULL)
  class(lg) = "lireg"

  lg = fit_mlr(lg)
  print(lg)
}

fit_mlr <- function(object){
  z = object
  X = z$regressor
  Y = z$response
  z$design_mat = cbind(rep(1, z$num_cases), X)
  eva_mat = t(z$design_mat) %*% z$design_mat
  z$rank = rankMatrix(eva_mat)

  if(z$rank < ncol(eva_mat)) stop("singular fit encountered")

  #Assume that the design matrix is not singular
  beta = solve(eva_mat)%*%t(z$design_mat)%*%Y
  z$coefficients = beta

  Y_fitted = z$design_mat %*% beta #fitted values

  p = length(beta)
  n = z$num_cases

  z$residuals = Y - z$design_mat %*% beta #residuals
  z$df = n - p

  y_bar = mean(Y)
  z$SSY = sum((Y - y_bar)^2)
  z$SSE = sum(z$residuals^2)
  z$SSR = z$SSY - z$SSE

  return(z)
}


print.lireg <- function(x, digits = max(3L, getOption("digits") - 3L)){
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  if(length(x$coefficients)) {
    cat("Coefficients:\n")
    print.default(format(x$coefficients, digits = digits),
                  print.gap = 2L, quote = FALSE)
  } else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}


summary.lireg <- function(object){
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

  print(ans)
}


print.summary.lireg <- function(x){

}

