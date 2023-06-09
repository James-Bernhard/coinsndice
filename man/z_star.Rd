% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{z_star}
\alias{z_star}
\title{Compute a standard normal central quantile (\eqn{z^*} value)}
\usage{
z_star(p)
}
\arguments{
\item{p}{The level of the central quantile to be computed, expressed either as a proportion (between
0 and 1) or a percent (between 1 and 100).}
}
\value{
The standard normal central \code{p}-quantile, which is a number.
}
\description{
This function computes the standard normal \code{p}-quantile (also called a \eqn{z^*} value),
which is the nonnegative number \eqn{z^*} with the property that the area under a standard normal
distribution probability density function between \eqn{-z^*} and \eqn{z^*} equals \code{p}.
}
