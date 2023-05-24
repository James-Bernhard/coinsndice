#######################################
#
# STEP 4: TURN RAW DATA LISTS INTO LISTS OF HTESTS
#
#######################################



# make output of simulations into a list of HTESTS
# each entry of x should be a list (or just a single sample, which is converted to a list with one entry)
# inferer should take as its first arguments the elements of each entry of x, or if the arguments of x are named, it can use those
# ... is the rest of the arguments for inferer, besides what is in each entry of x (e.g., hypothesized value, confidence level, etc.)
# degrees of freedom

#' Apply an inference function to each element of a list
#'
#' \code{list_infer} applies \code{inferer} to each element of \code{x}
#'
#' The argument \code{x} should be a list, and the argument \code{inferer}
#'   should be a function that returns an S3 object of class \code{htest}.
#'   The \code{list_infer} function applies
#'   \code{inferer} individually to each element of \code{x}. Because of
#'   this, each element of \code{x} should either be suitable as the
#'   first argument of \code{inferer}, or should itself be a list whose
#'   elements are the first several arguments of \code{inferer}.
#'
#' Any arguments given to \code{list_infer} in \code{...} will be provided
#'   to \code{inferer} as further arguments, beyond those given within
#'   each element of \code{x}.
#'
#' @param x A list of data, each element of which can be processed by the
#'   function given as the \code{inferer} argument.
#' @param inferer A function whose first arguments (in order) are given by the
#'   elements within each element of \code{x}, and which returns an S3 object
#'   of class \code{htest}.
#' @param ... Any remaining arguments to give to \code{inferer} beyond those
#'   provided by each element of \code{x}.
#' @return An object of class \code{htest_list}.
#' @export
list_infer <- function(x, inferer, ...){
  if (!is.list(x)){
    stop("x must be a list of data to be processed by inferer.")
  }

  processor <- function(y, ...){
    # if y isn't a list, make it one
    # (and its arguments must match the order of the first ones in inferer)
    if (!is.list(y)){
      y <- list(y)
    }
    do.call(inferer, args=append(y, list(...)))
  }
  lapply(X=x, FUN=processor, ...)
}


#############################
# coin-flipping (or die-rolling) and inference functions
#############################
# sampler: function of n that does the sampling
# inferer: function that does inference on a single sample, producing an htest
# ...: arguments passed to inferer

#' Generate samples and apply an inference function to them
#'
#' Generates samples and applies an inference function to them
#'
#' This function does the following:
#'
#' \enumerate{
#'   \item Calls \code{sampler(n)}.
#'   \item Does Item 1 again and again, a total of \code{N} times, resulting in
#'         a list of length \code{N}. This is accomplished by calling
#'         \code{\link{iterate}}.
#'   \item Calls \code{inferer} individually on each element of this list,
#'         with the first argument(s) of \code{inferer} taken from the
#'         element of the list and the remaining argument(s) taken
#'         from \code{...}. This is accomplished by calling
#'         \code{\link{list_infer}}.
#' }
#'
#' The result of the third is an S3 object of class \code{hlist}, which is
#'   returned.
#'
#' @param sampler Function that returns either an object that can be used as
#'   the first argument of the \code{inferer} function, or else a list whose
#'   elements can be used as the first several arguments of the \code{inferer}
#'   function.
#' @param inferer A function whose first arguments (in order) are given by
#'   what is returned by the \code{sampler} function, and which returns an S3
#'   object of class \code{htest}.
#' @param n A positive integer or vector of positive integers to be given as
#'   an argument to the \code{sampler} function.
#' @param N A positive integer specifying the number of times the
#'   \code{sampler} function is to be called, each call producing an element
#'   in the \code{htest_list} that is returned by \code{sample_and_infer}.
#' @param ... Any remaining arguments to give to \code{inferer} beyond those
#'   provided by the output of the \code{sampler} function.
#' @return An object of class \code{htest_list}.
#' @export
sample_and_infer <- function(sampler, inferer, n=1, N=1, ...){
  list_infer(x=iterate(sampler=sampler, n=n, N=N), inferer=inferer, ...)
}

##############################
# RESUME DOCUMENTING HERE
##############################
#' @export
flip_and_infer <- function(n=1, id="fair", N=1, confidence_level=0.95, null_value=0.5, sync=NULL, sides=c("H","T"), success="H", ...){
  # figure out whether this is one or two coins being flipped
  if (is.character(id)){
    id <- as.list(id)
  } else if (!is.list(id)){
    id <- list(id)
  }
  n_samples <- max(length(n), length(id))
  n <- rep(n, length=n_samples)
  id <- rep(id, length=n_samples)

  # make the sampler first, since that resets the random seed
  sampler <- make_finite_sampler(id=id, sides=sides)

  # if seed is a string, then convert it to a random seed
  if (is.character(sync)){
    sync <- make_seed(id=sync[1])
  }
  set.seed(sync)

  # simulate with this sampler and conduct p_tests
  sample_and_infer(sampler=sampler, inferer=p_test, n=n, N=N, confidence_level=make_proportion(confidence_level), null_value=null_value, success=success, ...)
}


#' @export
roll_and_infer <- function(n=1, id="fair", N=1, confidence_level=0.95, null_value=3.5, sync=NULL, sides=1:6, ...){
  # figure out whether this is one or two dice being rolled
  if (is.character(id)){
    id <- as.list(id)
  } else if (!is.list(id)){
    id <- list(id)
  }
  n_samples <- max(length(n), length(id))
  n <- rep(n, length=n_samples)
  id <- rep(id, length=n_samples)

  # make the sampler first, since that resets the random seed
  sampler <- make_finite_sampler(id=id, sides=sides)

  # if seed is a string, then convert it to a random seed
  if (is.character(sync)){
    sync <- make_seed(id=sync[1])
  }
  set.seed(sync)

  # simulate with this sampler and conduct t_tests
  sample_and_infer(sampler=sampler, inferer=t_test, n=n, N=N, confidence_level=make_proportion(confidence_level), null_value=null_value, ...)
}

