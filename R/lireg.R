# Linear Regression
#
# The package is used to fit linear regression model.
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
#'Fitting Linear Models
#'
#'@param formula
#'
#'@return a list of class "lireg"
#'
#'@examples
lireg <- function(formula){
  df = model.frame(formula)

  #assume that this is a correct formula
  Y = df[[1]]
  X = df[,2:ncol(df)]

  cn = colnames(df)

  if(is.null(nrow(X))){
    n = length(X)
    p = 2
  }else{
    n = nrow(X)
    p = ncol(X) + 1
  }

  lg = list(call = match.call(), terms = terms(formula),
            response = Y, regressor = X,
            coefficients = rep(0, p), num_cases = n, num_vars = p,
            residuals = NULL, rank = 0, SSE = 0, SSR = 0, SSY = 0,
            design_mat = NULL, Y_fitted = 0)
  class(lg) = "lireg"

  lg = fit_mlr(lg)
  names(lg$coefficients) = c("(Intercept)", cn[2:ncol(df)])

  print(lg)
}

fit_mlr <- function(object){
  z = object
  X = as.matrix(z$regressor)
  Y = z$response
  z$design_mat = cbind(rep(1, z$num_cases), X)
  eva_mat = t(z$design_mat) %*% z$design_mat
  z$rank = rankMatrix(eva_mat)

  #Assume that the design matrix is not singular
  beta = solve(eva_mat)%*%t(z$design_mat)%*%Y
  z$coefficients = as.vector(beta)

  z$Y_fitted = z$design_mat %*% beta #fitted values

  p = z$num_vars
  n = z$num_cases

  z$residuals = Y - z$design_mat %*% beta #residuals

  y_bar = mean(Y)
  z$SSY = c(value = sum((Y - y_bar)^2), df = n-1)
  z$SSE = c(value = sum(z$residuals^2), df = n-p)
  z$SSR = c(z$SSY["value"] - z$SSE["value"], df = p-1)

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
  lg = object
  sigma_est = lg$SSE[1L] / lg$SSE[2L]
  se = sqrt(diag(sigma_est * solve(t(lg$design_mat) %*% lg$design_mat)))
  #Assume that p is non-zero
  est = lg$coefficients
  tval = est/se

  ans = lg[c("call", "terms")]
  class(ans) = "summary.lireg"
  ans$residuals <- lg$residuals
  ans$aliased <- coef(lg)
  ans$coefficients =
    cbind(est, se, tval, 2*pt(abs(tval), lg$SSE["df"], lower.tail = FALSE))
  dimnames(ans$coefficients) =
    list(names(lg$coefficients),
         c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  ans$sigma = sqrt(sigma_est)
  ans$df = c(lg$num_vars, lg$num_cases - lg$num_vars)

  ans$Rsq = lg$SSR["value"] / lg$SSY["value"]
  ans$Rsq_adj = 1 - (lg$SSE[1L] / lg$SSE[2L]) / (lg$SSY[1L] / lg$SSY[2L])
  ans$fstat <- c(value = lg$SSR["value"] / lg$SSR["df"] / sigma_est,
                      numdf = lg$SSR["df"], dendf = lg$SSE["df"])
  print(ans)
}

print.summary.lireg <- function(x, digits = max(3L, getOption("digits") - 3L),
                                signif.stars = getOption("show.signif.stars")){
  ans = x
  cat("\nCall:\n",
      paste(deparse(ans$call), sep="\n", collapse = "\n"), "\n\n", sep = "")
  resid <- ans$residuals
  rdf <- ans$df[2L]
  cat("Residuals:\n", sep = "")

  #assume that number of data is greater than 5
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    zz <- zapsmall(quantile(resid), digits + 1L)
    rq <- structure(zz, names = nam)
    print(rq, digits = digits)

    cat("\nCoefficients:\n")
    coefs <- ans$coefficients
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars)

  cat("\nResidual standard error:",
      format(signif(ans$sigma, digits)), "on", rdf, "degrees of freedom")

  if (!is.null(ans$fstat)) {
    cat("\nMultiple R-squared: ", formatC(ans$Rsq, digits = digits))
    cat(",\tAdjusted R-squared: ",formatC(ans$Rsq_adj, digits = digits),
        "\nF-statistic:", formatC(ans$fstat[1L], digits = digits),
        "on", ans$fstat[2L], "and",
        ans$fstat[3L], "DF,  p-value:",
        format.pval(pf(ans$fstat[1L], ans$fstat[2L],
                       ans$fstat[3L], lower.tail = FALSE), digits = digits))
    cat("\n")
  }
  cat("\n")
  invisible(x)
}
