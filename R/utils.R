#############################
# general utility functions
# (not for export)
#############################

#' Round a number and convert the output to a string
#'
#' @description
#' This function rounds the number `x` to the number of digits specified in `digits`. If `exact` is
#' `TRUE`, then `digits` will be the exact number of decimal places (including any zeroes at the end).
#'Otherwise, trailing zeroes will be removed. The rounded number is then returned as a string (and not in
#' scientific notation).
#'
#' @param x The number to be rounded.
#' @param digits The number of decimal places to round to.
#' @param exact Boolean. `TRUE` specifies that the number of decimal places to round to is exact.
#' @returns String of the resulting rounded number.
fround <- function(x, digits=6, exact=TRUE){
  if (is.na(digits)){
    as.character(x)
  } else if (sum(is.na(x))==0){
    if (exact==TRUE){
      format(round(x, digits=digits), nsmall=digits, trim=TRUE, sci=FALSE)
    } else {
      as.character(round(x, digits=digits))
    }
  } else {
    NA
  }
}


#' Convert a percent to a proportion, leaving a proportion unchanged
#'
#' @description
#' If \eqn{1 < x \leq 100}, then this function divides `x` by 100. If  \eqn{0 \leq x \leq 1}, then
#' `x` is returned unchanged.
#'
#' @param x The percent or proportion to convert.
#' @returns The converted value of `x`.
make_proportion <- function(x){
  if (x >= 0 & x <= 1){
    x
  } else if (x > 1 & x <= 100){
    x/100
  } else {
    stop("Not a percent or proportion")
  }
}

#' Convert a proportion to a percent, leaving a percent unchanged
#'
#' @description
#' If \eqn{0 \leq x \leq 1}, then this function multiplies `x` by 100. If \eqn{1 < x \leq 100}, then
#' `x` is returned unchanged.
#'
#' @param x The proportion or percent to convert.
#' @returns The converted value of `x`.
make_percent <- function(x){
  if (x >= 0 & x <= 1){
    100*x
  } else if (x > 1 & x <= 100){
    x
  } else {
    stop("Not a percent or proportion")
  }
}


#' Test whether the arguments are in order from least to greatest
#'
#' @description
#' If `c` is specified, then this function returns `TRUE` if \eqn{a \leq b \leq c} and `FALSE` otherwise.
#' If `c` is not specified, then this function returns `TRUE` if \eqn{a \leq b} and `FALSE` otherwise.
#'
#' @param a First number in the sequence whose order is to be tested.
#' @param b Second number in the sequence whose order is to be tested.
#' @param c Optional third number in the sequence whose order is to be tested.
#'
#' @return Boolean that is `TRUE` if the arguments are in nondescending order, `FALSE` if not.
is_in_order <- function(a, b, c=NA){
  if (sum(is.na(c)) > 0){
    a <= b
  } else {
    (a <= b) & (b <= c)
  }
}

#' Convert the first letter of a string to a capital
#'
#' @description
#' If the first letter of `x` is a lowercase letter, this function converts it to a capital and returns
#' the resulting string. Otherwise the string `x` is returned.
#'
#' @param x The string to be converted.
#' @returns The capitalized string.
cap <- function(x){
  substring(x, 1, 1) <- toupper(substring(x, 1, 1))
  x
}


#' Convert a vector to a string
#'
#' @description
#' This function converts a vector to a string consisting of the elements of the vector separated by
#' the string specified in `comma`, and with the last element preceded by the string
#' specified in `and`. Vectors of length 1 and 2 are also handled according to the usual
#' English language conventions for listing elements.
#'
#' @param x The vector to be converted to a string.
#' @param and The string to precede the last element in the vector for vectors of length at least 2.
#' @param comma The string to separate elements of the vector in the output string
#' @returns String listing the elements of the vector `x`.
join <- function(x, and=" and ", comma=", "){
  if (sum(is.na(x)) > 0){
    return(as.character(NA))
  }

  if (length(x) ==1){
    output <- as.character(x)
  } else if (length(x)==2){
    output <- paste(x, collapse=and)
  } else {
    x[length(x)] <- paste0(and, x[length(x)])
    output <- paste(x, collapse=comma)
  }
  output
}



#' Compute a standard normal central quantile (\eqn{z^*} value)
#'
#' @description
#' This function computes the standard normal `p`-quantile (also called a \eqn{z^*} value),
#' which is the nonnegative number \eqn{z^*} with the property that the area under a standard normal
#' distribution probability density function between \eqn{-z^*} and \eqn{z^*} equals `p`.
#'
#' @param p The level of the central quantile to be computed, expressed either as a proportion (between
#'     0 and 1) or a percent (between 1 and 100).
#' @returns The standard normal central `p`-quantile, which is a number.
z_star <- function(p){
  stats::qnorm((1+make_proportion(p))/2)
}


#' Compute a \eqn{t} distribution central quantile (\eqn{t^*} value)
#'
#' @description
#' This function computes the central `p`-quantile for a \eqn{t} distribution with `df` degrees of freedom
#' (which is also called a \eqn{t^*} value).
#' This is the nonnegative number \eqn{t^*} with the property that the area under the
#' probability density function of a \eqn{t} distribution with `df` degrees of freedom
#' between \eqn{-t^*} and \eqn{t^*} equals `p`.
#'
#' @param p The level of the central quantile to be computed, expressed either as a proportion (between
#'     0 and 1) or a percent (between 1 and 100).
#' @param df The degrees of freedom of the \eqn{t} distribution.
#' @returns The central `p`-quantile for a \eqn{t} distribution with `df` degrees of freedom, which is a number.
t_star <- function(p, df){
  stats::qt((1+make_proportion(p))/2, df=df)
}

