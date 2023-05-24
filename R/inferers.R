####################################
#
# STEP 2: MAKE INFERERS
#
# inferers take a single sample (possibly of many variables) and turn it into an htest
#
####################################
# alternative: two-sided, greater, less
# plus_four: use plus-four adjusted method for confints

#' @export
p_test <- function(x, y=NULL, n, null_value=NULL, success=NULL, alternative="two-sided", confidence_level=0.95, pooled=(null_value==0), plus_four=TRUE){
  if (is.null(success)){
    # if success isn't given, then x must be a success count (and n must be given)
    k <- x
  } else {
    if (is.null(y)){
      # if success is given, then x (and possibly y) should be factors
      # if y isn't given, this is a one-sample test; simply count the successes
      k <- sum(x==success)
      n <- length(x)
    } else {
      # y is given; count the successes in each
      k <- c(sum(x==success), sum(y==success))
      n <- c(length(x), length(y))
    }
  }

  # if success is non-null, then treat x as a factor and compute k from there
  if (alternative=="two-sided" | alternative=="two sided"){
    alternative <- "two.sided"
  }

  n_samples <- max(length(k), length(n))
  k <- rep(k,length=n_samples)
  n <- rep(n,length=n_samples)

  if (n_samples==1){
    method <- "One-sample normal proportions test"
    if (is.null(null_value)){
      null_value <- 0.5
    }
    names(null_value) <- "p"
    estimate <- k/n
    names(estimate) <- "proportion"
    p_ci <- estimate
    if (plus_four==TRUE){
      p_plus_four <- (k+2)/(n+4)
      ci_standard_error <- sqrt(p_plus_four * (1-p_plus_four)/(n+4))
    } else {
      ci_standard_error <- sqrt(p_ci * (1-p_ci) / n)
    }
    p_ht <- null_value
    ht_standard_error <- sqrt(p_ht * (1-p_ht) / n)
    standard_error <- c(ci_standard_error, ht_standard_error)
    names(standard_error) <- c("ci", "ht")
    test_statistic <- (estimate - null_value)/ht_standard_error
    names(test_statistic) <- "z"
    if (alternative=="left"){
      p_value <- stats::pnorm(test_statistic, lower=TRUE)
    } else if (alternative=="right"){
      p_value <- stats::pnorm(test_statistic, lower=FALSE)
    } else {
      p_value <- 2*stats::pnorm(-abs(test_statistic), lower=TRUE)
    }
    zst <- z_star(confidence_level)
    ci_lower <- estimate - zst*ci_standard_error
    ci_upper <- estimate + zst*ci_standard_error
    ci_lower <- max(0, ci_lower)
    ci_upper <- min(1, ci_upper)
    confidence_interval <- c(ci_lower, ci_upper)
    attributes(confidence_interval) <- list(conf.level=confidence_level)
  } else if (n_samples==2){
    method <- "Two-sample normal proportions test"
    if (is.null(null_value)){
      null_value <- 0
    }
    names(null_value) <- "difference in p"
    estimate <- k/n
    names(estimate) <- c("first proportion", "second proportion")
    delta <- k[1]/n[1] - k[2]/n[2]
    p1_hat <- estimate[1]
    p2_hat <- estimate[2]
    if (plus_four==TRUE){
      p1_plus_four <- (k[1]+1)/(n[1]+2)
      p2_plus_four <- (k[2]+1)/(n[2]+2)
      ci_standard_error <- sqrt( (p1_plus_four*(1-p1_plus_four)/(n[1]+2) + p2_plus_four*(1-p2_plus_four)/(n[2]+2)) )
    } else {
      ci_standard_error <- sqrt( (p1_hat*(1-p1_hat) / n[1]) + (p2_hat*(1-p2_hat) / n[2]) )
    }
    if (pooled){
      p_pooled <- sum(k)/sum(n)
      ht_standard_error <- sqrt(p_pooled * (1-p_pooled) * (1/n[1] + 1/n[2]) )
    } else {
      ht_standard_error <- sqrt( (p1_hat*(1-p1_hat) / n[1]) + (p2_hat*(1-p2_hat) / n[2]) )
    }
    standard_error <- c(ci_standard_error, ht_standard_error)
    names(standard_error) <- c("ci", "ht")
    test_statistic <- (delta - null_value) / ht_standard_error
    names(test_statistic) <- "z"

    if (alternative=="left"){
      p_value <- stats::pnorm(test_statistic, lower=TRUE)
    } else if (alternative=="right"){
      p_value <- stats::pnorm(test_statistic, lower=FALSE)
    } else {
      p_value <- 2*stats::pnorm(-abs(test_statistic), lower=TRUE)
    }
    zst <- z_star(confidence_level)
    ci_lower <- delta - zst*ci_standard_error
    ci_upper <- delta + zst*ci_standard_error
    confidence_interval <- c(ci_lower, ci_upper)
    attributes(confidence_interval) <- list(conf.level=confidence_level)
  }



  output <- list(null.value=null_value,
                 alternative=alternative,
                 method=method,
                 estimate=estimate,
                 data.name=paste0(paste(k, collapse=" and "), " successes in ", paste(n, collapse=" and "), " trials"),
                 statistic=test_statistic,
                 p.value=p_value,
                 conf.int=confidence_interval,
                 stderr=standard_error,
                 sample.size=n)
  class(output) <- "htest"
  output
}


#' t inference about means
#'
#' Conduct t-based statistical inference about means.
#'
#' This is a convenience wrapper function for \code{\link[stats]{t.test}}, with
#' a few minor changes:
#'
#' \itemize{
#'   \item Although \code{\link[stats]{t.test}} accepts model formulas, \code{t_test}
#'     does not.
#'   \item While \code{\link[stats]{t.test}} has the argument \code{mu} for the
#'     hypothesized value of the mean or difference in means, \code{t_test}
#'     uses the argument \code{null_value} for this.
#'   \item While \code{\link[stats]{t.test}} uses the argument \code{conf.level} for the
#'     confidence level of the confidence interval to be computed, \code{t_test}
#'     instead uses \code{confidence_level}. Also, \code{t_test} accepts either
#'     a proportion (between 0 and 1) or a percentage (between 1 and 100).
#'   \item While \code{\link[stats]{t.test}} does not record the sample size,
#'     \code{t_test} records it in the \code{sample.size} slot of the
#'     \code{htest} output.
#' }
#'
#' @param x Numeric vector containing the first sample for t inference.
#' @param y Optional numeric vector containing the second sample for t inference.
#' @param null_value Number giving the hypothesized value of the mean.
#' @param confidence_level Either a percent (between 1 and 100) or a proportion
#'   (between 0 and 1) giving the confidence level for the confidence interval
#'   to be computed.
#' @param ... All other arguments are passed to \code{\link[stats]{t.test}}.
#' @return A list with class \code{"htest"} containing the following components:
#' \tabular{ll}{
#'   \code{statistic} \tab the value of the t-statistic.\cr
#'   \code{parameter} \tab the degrees of freedom for the t-statistic.\cr
#'   \code{p.value} \tab the p-value for the test.\cr
#'   \code{conf.int} \tab a confidence interval for the mean appropriate to the specified alternative hypothesis.\cr
#'   \code{estimate} \tab the estimated mean or difference in means depending on whether it was a one-sample test or a two-sample test.\cr
#'   \code{null.value} \tab the specified hypothesized value of the mean or mean difference depending on whether it was a one-sample test or a two-sample test.\cr
#'   \code{stderr} \tab the standard error of the mean (difference), used as denominator in the t-statistic formula.\cr
#'   \code{alternative} \tab a character string describing the alternative hypothesis.\cr
#'   \code{method} \tab a character string indicating what type of t-test was performed.\cr
#'   \code{data.name} \tab a character string giving the name(s) of the data.\cr
#'   \code{sample.size} \tab for paired and 1-sample t tests, a number giving the sample size; for 2-sample t tests, a numeric vector of length 2 giving the sample sizes.
#' }
#' @export
t_test <- function(x, y=NULL, null_value=0, confidence_level=95, ...){
  if (inherits(x, "formula")){
    # don't allow formulas; only x and y
    stop("data must be given as x and y, not as a model formula")
  } else {
    # figure out the one or two samples from x and y
    if (is.null(y)){
      sample_size <- length(x)
      data_name <- deparse(substitute(x))
      estimate_names <- "sample mean"
    } else {
      sample_size <- c(length(x), length(y))
      data_name <- paste(deparse(substitute(x)),"and",deparse(substitute(y)))
      estimate_names <- c("first sample mean", "second sample mean")
    }
  }

  output <- stats::t.test(x=x, y=y, mu=null_value, conf.level=make_proportion(confidence_level), ...)
  output$data.name <- data_name
  names(output$estimate) <- estimate_names
  output$sample.size <- sample_size

  output
}
