#########################################
#
# STEP 3: MAKE AND USE ITERATE TO "MULTIPLY" SAMPLING INTO A LIST
#
#########################################

#' Repeat a sampling process multiple times
#'
#' Produce a list of samples from the specified sampling function
#'
#' This function produces a list of length \code{N}, each of whose entries is a sample
#'   generated by calling \code{sampler(n)}.
#'
#' @param sampler A function of a single argument \code{n}. When called, this
#'   function should generate a sample of size \code{n}.
#' @param n A positive integer giving the sample size for each sample that is
#'   to be repeated.
#' @param N A positive integer giving the number of samples of size \code{n} to
#'   produce.
#' @return A list of length \code{N}.
#' @export
iterate <- function(sampler, n=1, N=1){
  replicate(n=N, expr=sampler(n), simplify=FALSE)
}


#' Generate a list of simulated coin flips
#'
#' \code{flip_coin} generates a list of simulated coin flip samples.
#'
#' Ordinarily, this function produces a list of length \code{N}, each of
#'   whose entries is a length \code{n} vector of simulated coin flips.
#'   However, it can also be used to simulate samples of several coins
#'   instead of just one. These instructions first describe the single-
#'   coin case, and then the case where multiple coins are simulated.
#'
#' @section Single-coin simulations:
#' For single-coin simulations, the argument \code{n} is a positive integer
#'   specifying the number of simulated flips there should be in each of
#'   the \code{N} elements of the list that the function returns.
#'
#' The \code{sides} argument is a character vector giving the labels for the
#'   sides of the coin. By default, these are \code{H} and \code{T}, but
#'   other choices are possible. In fact, the simulated "coin" need not even
#'   have two "sides": its number of "sides" is determined by the length of
#'   the \code{sides} argument.
#'
#' The \code{id} argument determines the probabilities with which the sides
#'   occur in the simulated flips. If \code{id} is \code{"fair"}, then all
#'   sides will have equal probabilities. If \code{id} is some other
#'   character vector, these probabilities are determined in a deliberately
#'   opaque way. If \code{id} is a numeric vector with nonnegative entries and
#'   a nonzero sum, then dividing the vector by its sum gives the
#'   probabilities of the sides.
#'
#' @section Multiple-coin simulations:
#' A multiple-coin simulation can be specified in any of the following ways:
#' \enumerate{
#'   \item \code{n} can be a number vector of length greater than 1.
#'   \item \code{id} can be a list of length greater than 1.
#'   \item \code{sides} can be a list of length greater than 1.
#' }
#' If one or more of these hold, then the number of coins to simulate is the
#'   maximum of these three lengths, and the other two arguments are repeated
#'   or shortened to match the maximum length.
#'
#' For example, if \code{n=c(20, 30)} and \code{id="fair"} and
#'   \code{sides=c("H","T")}, then each element of the returned list will
#'   itself be a list with two simulated samples: one of size 20 and the
#'   other of size 30. Both simulated samples with be for coins with
#'   \code{id="fair"} and \code{sides=c("H","T")}, since both of these
#'   arguments will be repeated to match the length of \code{n}.
#'
#' In order to have an id of "first" for the first coin and "second" for the
#'   second, you can specify \code{id=list("first", "second")}. It's important
#'   that \code{id} be a list in this case (not a vector) because if it were a
#'   vector, then it could be mistaken for a list of probabilities. For
#'   example, to specify probabilities \code{c(0.25, 0.75)} and
#'   \code{0.5, 0.5} for the sides of the two coins, set
#'   \code{id=list(c(0.25, 0.75), c(0.5, 0.5))}.
#'
#' Similarly, \code{sides} can be a list rather than a single vector, if you
#'   want the coins being simulated to have different labels for their sides.
#'
#' @param n A positive integer giving the length of each element of the
#'   list to be returned.
#' @param N A positive integer specifying the length of the list to be
#'   returned (that is, the number of elements in that list).
#' @param sync A number or character string that is used to set the random seed
#'   prior to the simulations. If \code{sync} is \code{NULL}, the random seed
#'   is cleared and not set to any specific value.
#' @param sides A vector from which the output function is to
#'   sample from, or a list of such vectors, one vector for each sample
#'   that the output function is to produce.
#' @param id A string or numeric vector, or a list of these, one for
#'   each sample that the output function is to produce. As described below,
#'   this is used to determine the probabilities with which the \code{sides}
#'   are to be sampled (with replacement).
#' @return A list of length \code{N}.
#' @export
flip_coin <- function(n=1, N=1, id="fair", sync=NULL, sides=c("H","T")){
  # figure out whether this is one or two coins being flipped
  if (is.character(id)){
    id <- as.list(id)
  } else if (!is.list(id)){
    id <- list(id)
  }

  if (!is.list(sides)){
    sides <- list(sides)
  }


  n_samples <- max(length(n), length(id), length(sides))
  n <- rep(n, length=n_samples)
  id <- rep(id, length=n_samples)
  sides <- rep(sides, length=n_samples)

  sampler <- make_finite_sampler(id=id, sides=sides)

  # if seed is a string, then convert it to a random seed
  if (is.character(sync)){
    sync <- make_seed(id=sync[1])
  }
  set.seed(sync)
  iterate(sampler=sampler, n=n, N=N)
}


#' Generate a list of simulated die rolls
#'
#' \code{roll_die} generates a list of simulated die roll samples.
#'
#' Ordinarily, this function produces a list of length \code{N}, each of
#'   whose entries is a length \code{n} vector of simulated die rolls.
#'   However, it can also be used to simulate samples of several dice
#'   instead of just one die. These instructions first describe the single-
#'   die case, and then the case where multiple dice are simulated.
#'
#' @section Single-die simulations:
#' For single-die simulations, the argument \code{n} is a positive integer
#'   specifying the number of simulated rolls there should be in each of
#'   the \code{N} elements of the list that the function returns.
#'
#' The \code{sides} argument is a character vector giving the labels for the
#'   sides of the die. These should be numeric (use \code{link{flip_coin}}
#'   when character string labels are desired). By default, they are
#'   \code{1:6},, but other choices are possible. In fact, the simulated
#'   "die" need not even have six "sides": its number of "sides" is
#'   determined by the length of the \code{sides} argument.
#'
#' The \code{id} argument determines the probabilities with which the sides
#'   occur in the simulated rolls. If \code{id} is \code{"fair"}, then all
#'   sides will have equal probabilities. If \code{id} is some other
#'   character vector, these probabilities are determined in a deliberately
#'   opaque way. If \code{id} is a numeric vector with nonnegative entries and
#'   a nonzero sum, then dividing the vector by its sum gives the
#'   probabilities of the sides.
#'
#' @section Multiple-die simulations:
#' A multiple-die simulation can be specified in any of the following ways:
#' \enumerate{
#'   \item \code{n} can be a number vector of length greater than 1.
#'   \item \code{id} can be a list of length greater than 1.
#'   \item \code{sides} can be a list of length greater than 1.
#' }
#' If one or more of these hold, then the number of dice to simulate is the
#'   maximum of these three lengths, and the other two arguments are repeated
#'   or shortened to match the maximum length.
#'
#' For example, if \code{n=c(20, 30)} and \code{id="fair"} and
#'   \code{sides=1:6}, then each element of the returned list will
#'   itself be a list with two simulated samples: one of size 20 and the
#'   other of size 30. Both simulated samples with be for dice with
#'   \code{id="fair"} and \code{sides=1:6}, since both of these
#'   arguments will be repeated to match the length of \code{n}.
#'
#' In order to have an id of "first" for the first die and "second" for the
#'   second, you can specify \code{id=list("first", "second")}. It's important
#'   that \code{id} be a list in this case (not a vector) because if it were a
#'   vector, then it could be mistaken for a list of probabilities. For
#'   example, to specify probabilities \code{c(0.25, 0.75)} and \code{0.5, 0.5}
#'   for the sides of the two coins, set
#'   \code{id=list(c(0.25, 0.75), c(0.5, 0.5))}.
#'
#' Similarly, \code{sides} can be a list rather than a single vector, if you
#'   want the dice being simulated to have different labels for their sides.
#'
#' @param n A positive integer giving the length of each element of the
#'   list to be returned.
#' @param N A positive integer specifying the length of the list to be
#'   returned (that is, the number of elements in that list).
#' @param sync A number or character string that is used to set the random seed
#'   prior to the simulations. If \code{sync} is \code{NULL}, the random seed
#'   is not cleared and not set to any specific value.
#' @param sides A vector from which the output function is to
#'   sample from, or a list of such vectors, one vector for each sample
#'   that the output function is to produce.
#' @param id A string or numeric vector, or a list of these, one for
#'   each sample that the output function is to produce. As described below,
#'   this is used to determine the probabilities with which the \code{sides}
#'   are to be sampled (with replacement).
#' @return A list of length \code{N}.
#' @export
roll_die <- function(n=1, N=1, id="fair", sync=NULL, sides=1:6){
  # figure out whether this is one or two dice being rolled
  if (is.character(id)){
    id <- as.list(id)
  } else if (!is.list(id)){
    id <- list(id)
  }
  n_samples <- max(length(n), length(id))
  n <- rep(n, length=n_samples)
  id <- rep(id, length=n_samples)

  sampler <- make_die(id=id, sides=sides)
  # if seed is a string, then convert it to a random seed
  if (is.character(sync)){
    sync <- make_seed(id=sync[1])
  }

  set.seed(sync)
  iterate(sampler=sampler, n=n, N=N)
}
