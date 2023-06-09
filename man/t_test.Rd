% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_inferers.R
\name{t_test}
\alias{t_test}
\title{t inference about means}
\usage{
t_test(x, y = NULL, null_value = 0, confidence_level = 95, ...)
}
\arguments{
\item{x}{Numeric vector containing the first sample for t inference.}

\item{y}{Optional numeric vector containing the second sample for t inference.}

\item{null_value}{Number giving the hypothesized value of the mean.}

\item{confidence_level}{Either a percent (between 1 and 100) or a proportion
(between 0 and 1) giving the confidence level for the confidence interval
to be computed.}

\item{...}{All other arguments are passed to \code{\link[stats]{t.test}}.}
}
\value{
A list with class \code{"htest"} containing the following components:
\tabular{ll}{
\code{statistic} \tab the value of the t-statistic.\cr
\code{parameter} \tab the degrees of freedom for the t-statistic.\cr
\code{p.value} \tab the p-value for the test.\cr
\code{conf.int} \tab a confidence interval for the mean appropriate to the specified alternative hypothesis.\cr
\code{estimate} \tab the estimated mean or difference in means depending on whether it was a one-sample test or a two-sample test.\cr
\code{null.value} \tab the specified hypothesized value of the mean or mean difference depending on whether it was a one-sample test or a two-sample test.\cr
\code{stderr} \tab the standard error of the mean (difference), used as denominator in the t-statistic formula.\cr
\code{alternative} \tab a character string describing the alternative hypothesis.\cr
\code{method} \tab a character string indicating what type of t-test was performed.\cr
\code{data.name} \tab a character string giving the name(s) of the data.\cr
\code{sample.size} \tab for paired and 1-sample t tests, a number giving the sample size; for 2-sample t tests, a numeric vector of length 2 giving the sample sizes.
}
}
\description{
Conduct t-based statistical inference about means.
}
\details{
This is a convenience wrapper function for \code{\link[stats]{t.test}}, with
a few minor changes:

\itemize{
\item Although \code{\link[stats]{t.test}} accepts model formulas, \code{t_test}
does not.
\item While \code{\link[stats]{t.test}} has the argument \code{mu} for the
hypothesized value of the mean or difference in means, \code{t_test}
uses the argument \code{null_value} for this.
\item While \code{\link[stats]{t.test}} uses the argument \code{conf.level} for the
confidence level of the confidence interval to be computed, \code{t_test}
instead uses \code{confidence_level}. Also, \code{t_test} accepts either
a proportion (between 0 and 1) or a percentage (between 1 and 100).
\item While \code{\link[stats]{t.test}} does not record the sample size,
\code{t_test} records it in the \code{sample.size} slot of the
\code{htest} output.
}
}
