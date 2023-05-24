########################################
#
# the main start-to-finish front ends
#
########################################

#' Simulate coin flips followed by normal proportions inference
#'
#' \code{flip} simulates \code{N} iterations of \code{n} flips of a coin or two
#'
#' This function simulates flipping one or two coins a specified number of
#'   times (which constitutes a single "iteration"), and then repeating that
#'   process again and again, for a total of \code{N} iterations. Then some
#'   sort of descriptive of inferential statistics is applied to each
#'   iteration, and the results of these are summarized in written and
#'   graphical forms. DETAILS FROM HERE.
#' @param n A vector of one or two positive integers. A single positive integer
#'   specifies the number of simulated flips of a coin per iteration. Two
#'   positive integers specify the numbers of simulated flips for each of two
#'   coins per iteration.
#' @param id The unique one or two id's of the coins whose flips are to be
#'   simulated. These can be given either as vectors or lists. See the details
#'   below.
#' @param N A positive integer specifying the number of iterations to simulate,
#'   each iteration having the number of simulated flips specified by \code{n}.
#' @param confidence_level A number giving the confidence level of the
#'   confidence intervals to be computed.
#' @param null_value A number giving the value of the test parameter under the
#'   null hypothesis in the hypothesis tests to be conducted.
#' @param true_value A number giving what is to be taken as the true value of
#'   the test parameter, for the purpose of determining which of the
#'   computed confidence intervals contain the true value.
#' @param significance_level A number giving the significance level for the
#'   hypothesis tests to be conducted, for the purpose of determining which
#'   of the hypothesis tests attained statistical significance.
#' @param alternative A character string specifying which type of alternative
#'   hypothesis should be used: either "two-sided" (or "two.sided"), "less",
#'   or "greater".
#' @param sync An integer or character string that is used to set the random
#'   seed immediately prior to the simulations. An integer or \code{NULL} will
#'   be passed to \code{set.seed} directly, while a character string will
#'   first be converted to an integer by \code{\link{make_seed}} and then
#'   passed to \code{set.seed}.
#' @param sides This can be given either as a character vector or factor of
#'   length 2, or as a list of two such character vectors or factors. A single
#'   character vector or factor specifies the labels for the sides of a
#'   coin that will be simulated; two such vectors or factors specify the
#'   labels for two coins to be simulated.
#' @param success A character string identifying which label from \code{sides}
#'   should be treated as "success" (in the sense of a Bernoulli trial) in
#'   the simulated coin flips.
#' @param show_summary Boolean indicating: should a text summary be printed?
#' @param show_plot Boolean indicating: should a plot be displayed?
#' @param xlim Numeric vector of length 2 giving the lower and upper limits
#'    of the x axis on the plot. NA means to use the default limits, if any.
#' @param ylim Numeric vector of length 2 giving the lower and upper limits
#'    of the y axis on the plot. NA means to use the default limits, if any.
#' @param digits Integer giving the number of digits to round values to. A
#'   value of NA indicates no rounding should be done.
#' @return A ggplot2 plot object (whether or not such an object was
#'   displayed).
#' @export
flip <- function(n=1, id="fair", N=1, confidence_level=NA, null_value=NA, true_value=NA, significance_level=0.05, alternative="two-sided", sync=NULL, sides=c("H","T"), success="H", show_summary=TRUE, show_plot=TRUE, xlim=NA, ylim=NA, digits=6){
  if (alternative=="two-sided"){
    alternative="two.sided"
  }

  if (is.na(confidence_level)){
    confidence_intervals <- FALSE
    confidence_level <- 0.95
  } else {
    confidence_intervals <- TRUE
  }
  if (is.na(null_value)){
    hypothesis_tests <- FALSE
    null_value <- 0.5
  } else {
    hypothesis_tests <- TRUE
  }

  inferences <- flip_and_infer(n=n, id=id, N=N, confidence_level=confidence_level, null_value=null_value, alternative=alternative, sync=sync, sides=sides, success=success)

  if (show_summary){
    summarize_htest_list(inferences, confidence_intervals=confidence_intervals, hypothesis_tests=hypothesis_tests, true_value=true_value, significance_level=significance_level, digits=digits)
  }


  output_plot <- plot_htest_list(inferences, confidence_intervals=confidence_intervals, p_values=hypothesis_tests, true_value=true_value, significance_level=significance_level)

  # set default xlim and ylim, if they aren't specified
  if (is.na(xlim[1]) & (hypothesis_tests | (!confidence_intervals) & max(length(n), length(id))==1) ){
    xlim <- c(-0.25, 1.25)
  }

  if (is.na(ylim[1]) & confidence_intervals & max(length(n), length(id))==1 ){
    ylim <- c(-0.25, 1.25)
  }

  # set x and y limits on the plot, if specified or defaulted
  if (!is.na(xlim[1])){
    output_plot <- output_plot + ggplot2::xlim(xlim)
  }
  if (!is.na(ylim[1])){
    output_plot <- output_plot + ggplot2::ylim(ylim)
  }

  if (show_plot){
    print(output_plot)
  }
  invisible(output_plot)
}

#' Simulate die rolls followed by t means inference
#'
#' \code{roll} simulates \code{N} iterations of \code{n} rolls of a die or two
#'
#' @param n A vector of one or two positive integers. A single positive integer
#'   specifies the number of simulated rolls of a die per iteration. Two
#'   positive integers specify the numbers of simulated rolls for each of two
#'   dice per iteration.
#' @param id The unique one or two id's of the dice whose rolls are to be
#'   simulated. These can be given either as vectors or lists. See the details
#'   below.
#' @param N A positive integer specifying the number of iterations to simulate,
#'   each iteration having the number of simulated rolls specified by \code{n}.
#' @param confidence_level A number giving the confidence level of the
#'   confidence intervals to be computed.
#' @param null_value A number giving the value of the test parameter under the
#'   null hypothesis in the hypothesis tests to be conducted.
#' @param true_value A number giving what is to be taken as the true value of
#'   the test parameter, for the purpose of determining which of the
#'   computed confidence intervals contain the true value.
#' @param significance_level A number giving the significance level for the
#'   hypothesis tests to be conducted, for the purpose of determining which
#'   of the hypothesis tests attained statistical significance.
#' @param alternative A character string specifying which type of alternative
#'   hypothesis should be used: either "two-sided" (or "two.sided"), "less",
#'   or "greater".
#' @param sync An integer or character string that is used to set the random
#'   seed immediately prior to the simulations. An integer or \code{NULL} will
#'   be passed to \code{set.seed} directly, while a character string will
#'   first be converted to an integer by \code{\link{make_seed}} and then
#'   passed to \code{set.seed}.
#' @param sides This can be given either as a numerical vector, or as a list
#'   of two such numerical vectors. A single numerical vector specifies the
#'   labels for the sides of a die that will be simulated; two such vectors
#'   specify the labels for two dice to be simulated.
#' @param show_summary Boolean indicating: should a text summary be printed?
#' @param show_plot Boolean indicating: should a plot be displayed?
#' @param xlim Numeric vector of length 2 giving the lower and upper limits
#'    of the x axis on the plot. NA means to use the default limits, if any.
#' @param ylim Numeric vector of length 2 giving the lower and upper limits
#'    of the y axis on the plot. NA means to use the default limits, if any.
#' @param digits Integer giving the number of digits to round values to. A
#'   value of NA indicates no rounding should be done.
#' @return A ggplot2 plot object (whether or not such an object was
#'   displayed).
#' @export
roll <- function(n=1, id="fair", N=1, confidence_level=NA, null_value=NA, true_value=NA, significance_level=0.05, alternative="two-sided", sync=NULL, sides=1:6,  show_summary=TRUE, show_plot=TRUE, xlim=NA, ylim=NA, digits=6){
  if (alternative=="two-sided"){
    alternative="two.sided"
  }

  if (is.na(confidence_level)){
    confidence_intervals <- FALSE
    confidence_level <- 0.95
  } else {
    confidence_intervals <- TRUE
  }
  if (is.na(null_value)){
    hypothesis_tests <- FALSE
    null_value <- 0.5
  } else {
    hypothesis_tests <- TRUE
  }

  inferences <- roll_and_infer(n=n, id=id, N=N, confidence_level=confidence_level, null_value=null_value, alternative=alternative, sync=sync, sides=sides)

  if (show_summary){
    summarize_htest_list(inferences, confidence_intervals=confidence_intervals, hypothesis_tests=hypothesis_tests, true_value=true_value, significance_level=significance_level, digits=digits)
  }

  output_plot <- plot_htest_list(inferences, confidence_intervals=confidence_intervals, p_values=hypothesis_tests, true_value=true_value, significance_level=significance_level)

  # set default xlim and ylim, if they aren't specified
  if (is.na(xlim[1])){
    if (!hypothesis_tests & !confidence_intervals & max(length(id), length(n)) == 1){
      # if no hyptests and no confints and only one sample (not a difference)
      xlim <- c(min(sides)-0.25, max(sides)+0.25)
    } else if (hypothesis_tests){
      xlim <- c(-0.25, 1.25)
    }
  }

  # if ylim is unspecified and if the confidence intervals are all within
  #    a fairly reasonable range, then specify default y axis limits
  if (is.na(ylim[1]) & confidence_intervals){
    possible_min <- min(sides)-0.25
    possible_max <- max(sides)+0.25
    if ( (sum(ci_lowers(inferences) < possible_min) == 0) & (sum(ci_uppers(inferences) > possible_max) == 0) ){
      ylim <- c(min(sides)-0.25, max(sides)+0.25)
    }
  }

  # set x and y limits on the plot, if specified or defaulted
  if (!is.na(xlim[1])){
    output_plot <- output_plot + ggplot2::xlim(xlim)
  }
  if (!is.na(ylim[1])){
    output_plot <- output_plot + ggplot2::ylim(ylim)
  }


  if (show_plot){
    print(output_plot)
  }
  invisible(output_plot)
}
