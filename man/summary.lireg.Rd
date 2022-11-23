% Part of the lireg package

\name{summary.lireg}
\alias{summary.lireg}
\alias{print.summary.lireg}
\title{Summarizing Linear Regression Model}
\usage{
\method{summary}{lireg}(object, ...)

\method{print}{summary.lireg}(x, digits = max(3L, getOption("digits") - 3L),
                   signif.stars = getOption("show.signif.stars"), ...)
}
\arguments{
  \item{object}{an object of class \code{"lireg"}, usually, a result of a
    call to \code{\link{lireg}}.}
  \item{x}{an object of class \code{"summary.lm"}, usually, a result of a
    call to \code{summary.lm}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{signif.stars}{logical. If \code{TRUE}, \sQuote{significance stars}
    are printed for each coefficient.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
\code{summary} method for class \code{"lireg"}.
}
\details{
  \code{print.summary.lireg} tries to be smart about formatting the
  coefficients, standard errors, etc. and additionally gives
  \sQuote{significance stars}.
}
\value{
  The function \code{summary.lireg} computes and returns a list of summary
  statistics of the fitted linear model given in \code{object}, using
  the components (list elements) \code{"call"} and \code{"terms"}
  from its argument, plus
  \item{residuals}{the usual residuals in \code{lireg}.}
  \item{coefficients}{a \eqn{p \times 4}{p x 4} matrix with columns for
    the estimated coefficient, its standard error, t-statistic and
    corresponding (two-sided) p-value.}
  \item{sigma}{the square root of the estimated variance of the random error
    \deqn{\hat\sigma^2 = \frac{1}{n-p}\sum_i{R_i^2},}{\sigma^2 = 1/(n-p) Sum(R[i]^2),}
    where \eqn{R_i}{R[i]} is the \eqn{i}-th residual, \code{residuals[i]}.}
  \item{df}{degrees of freedom, a 2-vector \eqn{(p, n-p)}, first being the total number of coefficients, and the second is the residual degree of freedom.}
  \item{fstat}{(for models including non-intercept terms)
    a 3-vector with the value of the F-statistic with
    its numerator and denominator degrees of freedom.}
  \item{Rsq}{\eqn{R^2}, the \sQuote{fraction of variance explained by
    the model},
    \deqn{R^2 = \frac{SSR}{SSY}}
    where \eqn{SSR} is the Regression Sum of Squares and \eqn{SSY} is the Total Sum of Squares.}
  \item{Rsq_adj}{the above \eqn{R^2} statistic
    \sQuote{\emph{adjusted}}, penalizing for higher \eqn{p}.}
}
\seealso{
  The model fitting function \code{\link{lireg}}, \code{\link{summary}}.

  Function \code{\link{coef}} will extract the matrix of coefficients
  with standard errors, t-statistics and p-values.
}
\examples{
input = mtcars
lg = lireg(input$mpg ~ input$am)
summary(lg)
}
\keyword{regression}
\keyword{models}
