% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterate_samplers.R
\name{flip_coin}
\alias{flip_coin}
\title{Generate a list of simulated coin flips}
\usage{
flip_coin(n = 1, N = 1, id = "fair", sync = NULL, sides = c("H", "T"))
}
\arguments{
\item{n}{A positive integer giving the length of each element of the
list to be returned.}

\item{N}{A positive integer specifying the length of the list to be
returned (that is, the number of elements in that list).}

\item{id}{A string or numeric vector, or a list of these, one for
each sample that the output function is to produce. As described below,
this is used to determine the probabilities with which the \code{sides}
are to be sampled (with replacement).}

\item{sync}{A number or character string that is used to set the random seed
prior to the simulations. If \code{sync} is \code{NULL}, the random seed
is cleared and not set to any specific value.}

\item{sides}{A vector from which the output function is to
sample from, or a list of such vectors, one vector for each sample
that the output function is to produce.}
}
\value{
A list of length \code{N}.
}
\description{
\code{flip_coin} generates a list of simulated coin flip samples.
}
\details{
Ordinarily, this function produces a list of length \code{N}, each of
whose entries is a length \code{n} vector of simulated coin flips.
However, it can also be used to simulate samples of several coins
instead of just one. These instructions first describe the single-
coin case, and then the case where multiple coins are simulated.
}
\section{Single-coin simulations}{

For single-coin simulations, the argument \code{n} is a positive integer
specifying the number of simulated flips there should be in each of
the \code{N} elements of the list that the function returns.

The \code{sides} argument is a character vector giving the labels for the
sides of the coin. By default, these are \code{H} and \code{T}, but
other choices are possible. In fact, the simulated "coin" need not even
have two "sides": its number of "sides" is determined by the length of
the \code{sides} argument.

The \code{id} argument determines the probabilities with which the sides
occur in the simulated flips. If \code{id} is \code{"fair"}, then all
sides will have equal probabilities. If \code{id} is some other
character vector, these probabilities are determined in a deliberately
opaque way. If \code{id} is a numeric vector with nonnegative entries and
a nonzero sum, then dividing the vector by its sum gives the
probabilities of the sides.
}

\section{Multiple-coin simulations}{

A multiple-coin simulation can be specified in any of the following ways:
\enumerate{
\item \code{n} can be a number vector of length greater than 1.
\item \code{id} can be a list of length greater than 1.
\item \code{sides} can be a list of length greater than 1.
}
If one or more of these hold, then the number of coins to simulate is the
maximum of these three lengths, and the other two arguments are repeated
or shortened to match the maximum length.

For example, if \code{n=c(20, 30)} and \code{id="fair"} and
\code{sides=c("H","T")}, then each element of the returned list will
itself be a list with two simulated samples: one of size 20 and the
other of size 30. Both simulated samples with be for coins with
\code{id="fair"} and \code{sides=c("H","T")}, since both of these
arguments will be repeated to match the length of \code{n}.

In order to have an id of "first" for the first coin and "second" for the
second, you can specify \code{id=list("first", "second")}. It's important
that \code{id} be a list in this case (not a vector) because if it were a
vector, then it could be mistaken for a list of probabilities. For
example, to specify probabilities \code{c(0.25, 0.75)} and
\code{0.5, 0.5} for the sides of the two coins, set
\code{id=list(c(0.25, 0.75), c(0.5, 0.5))}.

Similarly, \code{sides} can be a list rather than a single vector, if you
want the coins being simulated to have different labels for their sides.
}

