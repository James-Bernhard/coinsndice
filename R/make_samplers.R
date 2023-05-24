#######################################
#
# STEP 1: MAKE SAMPLERS
#
# samplers generate a single sample (possibly of multiple variables)
#
#######################################

#' Convert a character string to an integer.
#'
#' \code{make_seed} converts a character string to an integer.
#'
#' This function converts a character string (the \code{id}) to an integer.
#' If the inputted charcter string is already an integer, then that integer
#' is returned unchanged.
#'
#' The conversion from character string to integer is accomplished by the
#' \code{digest2int} function in them \code{digest} package.
#'
#' The main use of this function is to provide a way to construct
#' finite sampling functions whose probabilities that are unknown to the
#' user, and which therefore can sensibly be studied using statistical
#' inference techniques.
#'
#' @param id A character vector or a numeric vector.
#' @return This function returns an integer.
#'
#' @export
make_seed <- function(id){
  if (is.numeric(id)){
    as.integer(id)
  } else {
    digest::digest2int(id)
  }
}


#' Make a function that samples from a finite set.
#'
#' \code{make_finite_sampler} returns a function that samples from a finite set.
#'
#' In its simplest use, \code{make_finite_sampler} outputs a function of a
#'   single argument \code{n} that generates
#'   samples (with replacement) of size \code{n} from \code{sides}. This
#'   output function can be thought of way to generalize simulating
#'   repeated coin flips (where \code{sides = c("H","T")}) and die rolls
#'   (where \code{sides = 1:6}).
#'
#' The probabilities with which the output function samples the entries of
#'   \code{sides} are determined by \code{id}, in three different ways:
#'
#' \enumerate{
#'   \item If \code{id} is \code{"fair"},
#'   then the probabilities of all the sides will be equal.
#'   \item If \code{id} is any other character string, the probabilities
#'   are determined by a deliberately opaque method. This allows
#'   \code{make_finite_sampler} to create sampling functions whose
#'   probabilities are unknown and must be explored by statistical methods.
#'   \item If \code{id} is a numeric vector with positive entries, that
#'   vector will first be repeated or shortened until it has the same length
#'   as \code{sides}. Then it will be rescaled so that its sum is 1. The
#'   resulting numeric vector will give the probabilities with which the output
#'   function will sample the entries of \code{sides}. This allows
#'   \code{make_finite_sampler} to create sampling functions whose probabilities
#'   are known. Such functions can be used to verify and assess statistical
#'   methods.
#' }
#'
#' @section Multiple samples:
#' The \code{make_finite_sampler} function can also be used to make a
#' function that produces more than one simulataneous
#' sample. In this case, the output function is a function of not just a
#' number \code{n}, but of a numeric vector \code{n}, which will be repeated
#' or shortened until its length matches the number of output samples
#' to be produced.
#'
#' To create such a function, instead of giving the \code{id} argument
#' as a single string or single numeric vector, give it
#' as a list of single strings and/or single numeric vectors. (The list
#' can contain one or both types.) Each string or numeric vector
#' will correspond to a single sample produced by the output function.
#'
#' For another way to create such a function, instead of giving the
#' \code{sides} argument as a single vector, give it as a list of vectors.
#' Each vector in the list will correspond to a single sample produced by
#' the output function.
#'
#' If both \code{id} and \code{sides} are lists, the number of samples
#' produced by the output function will equal the length of the longer list,
#' and the shorter list will be repeated until it has that length. In other
#' words, a separate id and collection of labels for the sides can be
#' provided for each sample produced by the output function, and if separate
#' id's or sides are not provided, they will be filled in by repeating
#' whatever has been provided.
#'
#' @param sides A vector from which the output function is to
#'   sample from, or a list of such vectors, one vector for each sample
#'   that the output function is to produce.
#' @param id A string or numeric vector, or a list of these, one for
#'   each sample that the output function is to produce. As described below,
#'   this is used to determine the probabilities with which the \code{sides}
#'   are to be sampled (with replacement).
#' @return This function outputs a function of a single argument \code{n}.
#' @export
make_finite_sampler <- function(sides, id="fair"){
  # NOTE TO SELF:
  # put in checks to ensure that if probability weights are given, they are positive

  # id_to_probabilities:
  # get the probabilities for a finite sampler based on its id string
  # id: id of die, or a vector of probabilities for the die's sides
  # sides: vector of side names
  # both arguments can be provided as a list for sampling in order to get a list of probability vectors
  # i
  id_to_probabilities <- function(id="fair", n_sides){
    # if id isn't a list, make it a list of length 1
    if (!is.list(id)){
      id <- list(id)
    }
    # length of the output list (how many probability vectors to make)
    output_length <- length(id)

    # make sure that n_sides is formatted as a list (it might be a number or a vector), and recycle as need be
    n_sides <- as.list(n_sides)
    n_sides <- rep(n_sides, length=output_length)

    probabilities <- vector(mode="list", length=output_length)
    for (i in seq(along=id)){
      if (is.numeric(id[[i]])){
        id[[i]] <- rep(id[[i]], length=n_sides[[i]])
        probs <- id[[i]]/sum(id[[i]])
      } else if (id[[i]]=="fair"){
        probs <- 1/n_sides[[i]] * rep(1, n_sides[[i]])
      } else {
        # since we set the randomness here...
        set.seed(make_seed(id[[i]]))
        #set.seed(digest::digest2int(id[[i]]))
        probs <- stats::runif(n_sides[[i]])
        probs <- probs/sum(probs)
      }
      probabilities[[i]] <- probs
    }

    # clear the random seed
    set.seed(NULL)
    probabilities
  }

  # if id is a character vector, convert it to a list
  if (is.character(id) | is.factor(id)){
    id <- as.list(id)
  } else if (!is.list(id)){
    # otherwise if id isn't a list, make it a list of length 1
    id <- list(id)
  }

  # if sides isn't a list, make it a list of length 1
  if (!is.list(sides)){
    sides <- list(sides)
  }

  # length of the output list (how many probability vectors to make)
  output_length <- max(length(id), length(sides))

  # recycle to have a long enough sides list
  id <- rep(id, length=output_length)
  sides <- rep(sides, length=output_length)

  probabilities <- id_to_probabilities(id=id, n_sides=lapply(sides, length))

  function(n=1){
    # if n is a vector, make it into a list
    n <- as.list(n)
    # make n the same length as the probabilities list
    n <- rep(n, length=length(probabilities))

    output <- vector(mode="list", length=length(probabilities))
    for (i in seq(along=probabilities)){
      output[[i]] <- sample(sides[[i]], size=n[[i]], replace=TRUE, prob=probabilities[[i]])
    }
    if (length(output) > 1){
      output
    } else {
      output[[1]]
    }
  }
}


#' Make a function that simulates coin flips
#'
#' \code{make_coin} outputs a function that simulates coin flips.
#'
#' This function is a convenience wrapper function for
#' \code{\link{make_finite_sampler}} with \code{id} as its first argument
#' and with \code{c("H","T")} as the default
#' value for \code{sides}.
#'
#' @param sides A vector from which the output function is to
#'   sample from, or a list of such vectors, one vector for each sample
#'   that the output function is to produce. In essence, these vectors
#'   give labels for the sides of the coins whose flips are to be simulated.
#' @param id A string or numeric vector, or a list of these, one for
#'   each sample that the output function is to produce. As described in
#'   \code{\link{make_finite_sampler}},
#'   this is used to determine the probabilities for the \code{sides}.
#' @return This function outputs a function of a single argument \code{n}.
#' @export
make_coin <- function(id="fair", sides=c("H","T")){
  make_finite_sampler(sides=sides, id=id)
}


#' Make a function that simulates die rolls
#'
#' \code{make_die} outputs a function that simulates die rolls.
#'
#' This function is a convenience wrapper function for
#' \code{\link{make_finite_sampler}} with \code{id} as its first argument
#' and with \code{1:6} as the default
#' value for \code{sides}. In addition, while
#' \code{\link{make_finite_sampler}} allows all types of vectors for
#' its \code{sides} argument, \code{make_die} expects \code{sides} to
#' be a numeric vector (or a list of numeric vectors) and will coerce
#' it to such if need be.
#'
#' @param sides A vector from which the output function is to
#'   sample from, or a list of such vectors, one vector for each sample
#'   that the output function is to produce. In essence, these vectors
#'   give labels for the sides of the dice whose rolls are to be simulated.
#' @param id A string or numeric vector, or a list of these, one for
#'   each sample that the output function is to produce. As described in
#'   \code{\link{make_finite_sampler}},
#'   this is used to determine the probabilities for the \code{sides}.
#' @return This function outputs a function of a single argument \code{n}.
#' @export
make_die <- function(id="fair", sides=1:6){
  if (is.list(sides)){
    sides <- lapply(sides, as.numeric)
  } else {
    sides <- as.numeric(sides)
  }

  make_finite_sampler(sides=sides, id=id)
}
