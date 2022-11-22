\name{lireg}
\alias{lireg}
\alias{print.lireg}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Linear Regression Model}
\description{
  \code{lireg} is used to fit linear models, including multivariate ones.
}
\usage{
lireg(formula)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. Currently the package does not support polynomial regression, interaction, or variable with type "factor".}
}
\details{
  Models for \code{lireg} are specified symbolically.  A typical model has the form \code{response ~ terms} where \code{response} is the (numeric) response vector and \code{terms} is a series of terms which specifies a linear predictor for \code{response}. A terms specification of the form \code{first + second} indicates all the terms in \code{first} together with all the terms in \code{second}. Currently the function does not support interaction (\code{first:second}) or cross (\code{first*second}).

  A formula has an implied intercept term, which cannot be removed.
}
\value{
  \code{lireg} returns an object of \code{\link{class}} \code{"lireg"}.

  The function \code{summary} are used to obtain and print a summary of the results.

  An object of class \code{"lireg"} is a list containing at least the
  following components:

  \item{call}{the matched call.}
  \item{terms}{the \code{\link{terms}} object used.}
  \item{response}{the vector of the response variable.}
  \item{regressor}{the vector of regressors.}
  \item{coefficients}{a named vector of coefficients.}
  \item{num_cases}{total number of observations.}
  \item{residuals}{the residuals, that is response minus fitted values.}
  \item{rank}{the numeric rank of the fitted linear model.}
  \item{SSE}{a 2-vector storing value and degree of freedom of Residual Sum of Squares.}
  \item{SSR}{a 2-vector storing value and degree of freedom of Regression Sum of Squares.}
  \item{SSY}{a 2-vector storing value and degree of freedom of Total Sum of Squares.}
  \item{Y_fitted}{the fitted mean values.}
}

\references{
https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.R
}
\author{
The structure of the function was inspired by R source library function lm(). The function is implemented by Qiaoran Dong.
}

\examples{
input = mtcars
lg = lireg(input$mpg ~ input$am)
summary(lg)
}
