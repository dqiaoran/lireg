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

#'slr
#'
#'Simple linear regression
#'
#'@param res_var
#'
#'@return fitted beta-zero and beta-one
#'
#'@examples
#'slr(2,3)
#'
#'@export
#'
slr <- function(res_var, cov1) {
  print("Simple linear regression Y = X*beta_1 + beta_0")
}

#'mlr
#'
#'Multiple Linear Regression
#'
#'@param res_var
#'
#'@return fitted beta-zero and beta-one
#'
#'@examples
#'slr(2,3)
#'
#'@export
#'
mlr <- function(res_var, cov){
  print("Multiple linear regression")
}
