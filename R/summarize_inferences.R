#######################################
#
# STEP 5: DO THINGS WITH HTEST_LISTS
#
#######################################

# to process the data, we'll use these new functions:
#' @export
list_map <- function(x, fct){
  sapply(X=x, FUN=fct)
}

# extract a specific slot from every element in a list, returning a list

#' @export
get_slot <- function(x, slot){
  lapply(X=x, FUN=function(y) y[[slot]])
}


# extract the length of a given slot from every element in a list

#' @export
map_length <- function(x, slot){
  get_length <- function(y){
    length(y[[slot]])
  }
  vapply(X=x, FUN=get_length, FUN.VALUE=numeric(1))
}


# get a slot (or index of a slot) from an entire list
# returns a vector, assuming that that slot is a single number
# (WHAT IF INDEX HAS LENGTH > 1?)
# attributes: get this attribute instead of the value

#' @export
list_get <- function(x, slot, index=1, attribute=NA){
  fct <- function(x){
    if (is.na(attribute)){
      temp <- x[[slot]][index]
      if (!is.null(temp)){
        as.vector(temp)
      } else {
        NA
      }
    } else {
      temp <- attributes(x[[slot]])[[attribute]][index]
      if (!is.null(temp)){
        temp
      } else {
        NA
      }
    }
  }
  sapply(X=x, FUN=fct)
}


##############################################
#
# extracting from lists of htests
# note: every extraction function outputs a vector
#
##############################################
###########################
# getters
###########################
# test statistics

#' @export
test_statistics <- function(x, names=FALSE){
  attribute <- NA
  if (names){
    attribute <- "names"
  }
  list_get(x, "statistic", attribute=attribute)
}

# degrees of freedom

#' @export
degrees_of_freedom <- function(x, number=NA, names=FALSE){
  attribute <- NA
  if (names){
    attribute <- "names"
  }

  if (is.na(number) & slot_length(x, "parameter") > 1){
    stop("There is more than one degrees of freedom paramater, so you must specify 'number=...'.")
  }

  # if number isn't specified but slot length is 1, set number to 1
  if (is.na(number)){
    number <- 1
  }


  if (names){
    list_get(x, "parameter", attribute=attribute)
  } else {
    list_get(x, "parameter", index=number)
  }
}

# p values

#' @export
p_values <- function(x){
  list_get(x, "p.value")
}

# confidence levels

#' @export
confidence_levels <- function(x){
  list_get(x, "conf.int", attribute="conf.level")
}

# lower endpoints of confidence intervals

#' @export
ci_lowers <- function(x){
  list_get(x, "conf.int", index=1)
}

# upper endpoints of confidence intervals

#' @export
ci_uppers <- function(x){
  list_get(x, "conf.int", index=2)
}

# estimates

#' @export
estimates <- function(x, number=NA, names=FALSE){
  attribute <- NA
  if (names){
    attribute <- "names"
  }

  if (is.na(number) & slot_length(x, "estimate") > 1){
    stop("There is more than one estimate, so you must specify 'number=...'.")
  }

  # if number isn't specified and there's only one entry, select that
  if (is.na(number)){
    number <- 1
  }

  list_get(x, "estimate", index=number, attribute=attribute)
}

# null values

#' @export
null_values <- function(x, names=FALSE){
  attribute <- NA
  if (names){
    attribute <- "names"
  }
  list_get(x, "null.value", attribute=attribute)
}

# standard errors (type="ci" or "ht" for p_test standard errors)
# an type can be given as a number also
# by setting names to TRUE, one can view the type name that goes with a number


#' @export
standard_errors <- function(x, type=NA, names=FALSE){
  attribute <- NA
  if (names){
    attribute <- "names"
  }

  if (is.na(type) & slot_length(x, "stderr") > 1){
    stop("There is more than one type of standard error ('ci' for the confidence interval and 'ht' for the hypothesis test), so you must specify one of these two in the 'type=...'.")
  }

  # if the type isn't specified by the slot length is 1, then set the index to 1
  if (is.na(type)){
    type <- 1
  }

  list_get(x, "stderr", index=type, attribute=attribute)
}


#' @export
alternatives <- function(x){
  list_get(x, "alternative")
}


#' @export
statistical_methods <- function(x){
  list_get(x, "method")
}


#' @export
data_names <- function(x){
  list_get(x, "data.name")
}

#' @export
sample_sizes <- function(x, number=NA){
  if (is.na(number) & slot_length(x, "sample.size") > 1){
    stop("There is more than one sample size, so you must specify 'number=...'.")
  }

  list_get(x, "sample.size", index=number)
}


#' @export
margins_of_error <- function(x){
  # margin of error should be the distance to the further endpoint
  # (rather than half the width of the confidence interval, since
  # confidence intervals for proportions can be truncated)
  if (slot_length(x, "sample.size") > 1){
    ests <- differences_in_estimates(x)
  } else {
    ests <- estimates(x)
  }
  max(ests-ci_lowers(x), ci_uppers(x)-ests)
}


#' @export
differences_in_estimates <- function(x){
  estimates(x, number=1) - estimates(x, number=2)
}


#' @export
successes <- function(x, number){
  if (is.na(number) & slot_length(x, "estimates") > 1){
    stop("There is more than one success count, so you must specify 'number=...'.")
  }

  round(estimates(x, number=number)*sample_sizes(x, number=number), digits=0)
}


#' @export
null_hypotheses <- function(x, max_digits=6){
  paste0(list_get(x, "null.value", attribute="names"), " equals ", round(null_values(x), digits=max_digits))
}


#' @export
alternative_hypotheses <- function(x, max_digits=6){
  N <- length(x)
  nulls <- round(null_values(x), digits=max_digits)
  parameter_name <- list_get(x, "null.value", attribute="names")
  output <- character(N)
  alts <- alternatives(x)
  output[alts=="two.sided"] <- paste0(parameter_name, " does not equal ", nulls)
  output[alts=="less"] <- paste0(parameter_name, " is less than ", nulls)
  output[alts=="greater"] <- paste0(parameter_name, " is greater than ", nulls)
  output
}


#' @export
contains_true_value <- function(x, true_value){
  is_in_order(ci_lowers(x), true_value, ci_uppers(x))
}


#' @export
is_significant <- function(x, significance_level=0.05){
  is_in_order(p_values(x), significance_level)
}



###########################
# more extensive methods for lists of hypothesis tests
###########################

# maximum length in a specific slot of a list
#' @export
slot_length <- function(x, slot){
  max(map_length(x, slot))
}


# if slots of list entries are vectors, then summarize each index separately
# this gives output of a vector of summarized indices

#' @export
summarize_componentwise <- function(x, slot, attribute=NA, summarizer=mean){
  n_samples <- slot_length(x, slot)
  if (n_samples > 0){
    avgs <- numeric(n_samples)
    for (i in 1:n_samples){
      avgs[i] <- summarizer(list_get(x, slot, index=i, attribute=attribute))
    }
  } else {
    avgs <- NA
  }
  avgs
}


#' @export
summarize_htest_list <- function(x, confidence_intervals=TRUE, hypothesis_tests=TRUE, true_value=NA, significance_level=0.05, digits=6, numbers=1:length(x)){
  # use only the requested subset
  x <- x[numbers]

  # check whether the requested subset is of length 1
  plural <- (length(x) != 1)

  ### computations
  # sample size (use fround to avoid scientific notation)
  sampsize <- fround(summarize_componentwise(x, "sample.size"), 0)
  if (slot_length(x, "sample.size") > 1){
    sampsize_label <- ifelse(plural, "Average sample sizes", "Sample sizes")
  } else {
    sampsize_label <- ifelse(plural, "Average sample size", "Sample size")
  }

  # estimate
  est <- summarize_componentwise(x, "estimate")
  est_label <- ifelse(plural,
                      paste0("Average ", join(summarize_componentwise(x, "estimate", attribute="names", summarizer=function(x) x[[1]]))),
                      cap(join(summarize_componentwise(x, "estimate", attribute="names", summarizer=function(x) x[[1]]))))

  # difference in estimate
  if (slot_length(x,"estimate") > 1){
    estdiff <- mean(differences_in_estimates(x))
    estdiff_label <- ifelse(plural,
                            paste0("Average ", join(summarize_componentwise(x, "estimate", attribute="names", summarizer=function(x) x[[1]]), and=" minus ")),
                            cap(join(summarize_componentwise(x, "estimate", attribute="names", summarizer=function(x) x[[1]]), and=" minus ")))
  } else {
    estdiff <- NA
    estdiff_label <- ifelse(plural, "Average difference in estimates", "Difference in estimates")
  }


  # standard error
  if (slot_length(x,"stderr")==1){
    stderr <- fround(mean(standard_errors(x)), digits=digits)
    stderr_label <- ifelse(plural, "Average standard error", "Standard error")
  } else if (slot_length(x,"stderr")==2) {
    if (confidence_intervals & !hypothesis_tests){
      # if only confints are requested, use those
      stderr <- fround(mean(standard_errors(x, type="ci")), digits=digits)
      stderr_label <- ifelse(plural, "Average standard error", "Standard error")
    } else if (!confidence_intervals & hypothesis_tests){
      # if only hyptests are requested, use those
      stderr <- fround(mean(standard_errors(x, type="ht")), digits=digits)
      stderr_label <- ifelse(plural, "Average standard error", "Standard error")
    } else {
      # if neither or both are requested, paste them together and use those
      stderr_values <- fround(summarize_componentwise(x, "stderr"), digits=digits)
      stderr_indices <- summarize_componentwise(x, "stderr", attribute="names", summarizer=function(x) x[[1]])
      stderr_names <- paste0(" (", summarize_componentwise(x, "stderr", attribute="names", summarizer=function(x) x[[1]]), ")")
      stderr <- paste0(stderr_values, stderr_names)
      stderr_label <- ifelse(plural, "Average standard error", "Standard error")
    }

  } else {
    stderr <- NA
    stderr_label <- ifelse(plural, "Average standard error", "Standard error")
  }

  # margins of error
  moe <- mean(margins_of_error(x))
  moe_label <- ifelse(plural, "Average margin of error", "Margin of error")

  # confidence intervals
  conflev <- mean(confidence_levels(x))
  ciendpts <- summarize_componentwise(x, "conf.int")
  ciendpts_label <- ifelse(plural,
                           paste0(100*conflev, "% confidence interval average endpoints"),
                           paste0(100*conflev, "% confidence interval endpoints"))

  # containing true value
  if (!is.na(true_value)){
    containing <- contains_true_value(x, true_value=true_value)
    ci_contains <- ifelse(plural,
                          paste0(fround(100*mean(containing), digits=digits, exact=FALSE), " (", sum(containing), " of ", length(containing), ")"),
                          ifelse(containing[1], "yes", "no"))
    ci_contains_label <- ifelse(plural,
                                paste0("Percent of confidence intervals containing ", true_value),
                                paste0("Confidence interval contains ", true_value))
  } else {
    ci_contains=NA
    ci_contains_label <- "Missing true value"
  }

  # null and alternative hypotheses
  nullhyp <- null_hypotheses(x[1], max_digits=digits)
  nullhyp_label <- "Null hypothesis"
  althyp <- alternative_hypotheses(x[1], max_digits=digits)
  althyp_label <- "Alternative hypothesis"

  # test statistic
  teststat <- mean(test_statistics(x))
  teststat_label <- ifelse(plural, "Average value of the test statistic", "Value of the test statistic")

  # degrees of freedom
  dof <- mean(degrees_of_freedom(x))
  dof_label <- ifelse(plural, "Average degrees of freedom", "Degrees of freedom")

  # p-values
  pval <- mean(p_values(x))
  pval_label <- ifelse(plural, "Average p-value", "p-value")

  # significant
  if (!is.na(significance_level)){
    significant <- is_significant(x, significance_level=significance_level)
    ht_significant <- ifelse(plural,
                             paste0(fround(mean(significant), digits=digits, exact=FALSE), " (", sum(significant), " of ", length(significant), ")"),
                             ifelse(significant[1], "yes", "no"))
    ht_significant_label <- ifelse(plural,
                                   paste0("Proportion less than or equal to ", fround(significance_level, digits=digits, exact=FALSE)),
                                   paste0("Less than or equal to ", fround(significance_level, digits=digits, exact=FALSE)))
  } else {
    ht_significant=NA
    ht_significant_label="Significance level missing"
  }


  # labels
  summary_labels <- c(sample_size=sampsize_label,
                      estimate=est_label,
                      difference_in_estimates=estdiff_label,
                      standard_error=stderr_label,
                      margin_of_error=moe_label,
                      ci_endpoints=ciendpts_label,
                      ci_contains=ci_contains_label,
                      null_hypothesis=nullhyp_label,
                      alternative_hypothesis=althyp_label,
                      test_statistic=teststat_label,
                      degrees_of_freedom=dof_label,
                      p_value=pval_label,
                      ht_significant=ht_significant_label
  )

  # values
  summary_values <- list(sample_size=sampsize,
                         estimate=fround(est, digits=digits),
                         difference_in_estimates=fround(estdiff, digits=digits),
                         standard_error=stderr, # rounded above
                         margin_of_error=fround(moe, digits=digits),

                         ci_endpoints=fround(ciendpts, digits=digits),
                         ci_contains=ci_contains, # rounded above,
                         null_hypothesis=nullhyp,
                         alternative_hypothesis=althyp,
                         test_statistic=fround(teststat, digits=digits),
                         degrees_of_freedom=fround(dof, digits=digits, exact=FALSE),
                         p_value=fround(pval, digits=digits),
                         ht_significant=ht_significant
  )


  # join the plural ones
  summary_values <- vapply(X=summary_values, FUN=join, FUN.VALUE=character(1))

  # remove the missing ones
  summary_labels <- summary_labels[!is.na(summary_values)]
  summary_values <- summary_values[!is.na(summary_values)]

  # names: sample_size, estimate, difference_in_estimates, standard_error, ci_endpoints, ci_contains, margin_of_error, null_hypothesis,alternative_hypothesis, test_statistic,degrees_of_freedom,p_value
  # subset if confints and hyptests are not both requested
  estimate_names <- c("sample_size", "estimate", "difference_in_estimates")
  confint_names <- c("standard_error", "margin_of_error", "ci_endpoints", "ci_contains")
  hyptest_names <- c("null_hypothesis","alternative_hypothesis", "standard_error", "test_statistic", "degrees_of_freedom", "p_value", "ht_significant")
  summary_names <- names(summary_labels)
  if (!confidence_intervals & !hypothesis_tests){
    summary_labels <- summary_labels[summary_names %in% estimate_names]
    summary_values <- summary_values[summary_names %in% estimate_names]
  } else if (confidence_intervals & !hypothesis_tests){
    summary_labels <- summary_labels[summary_names %in% c(estimate_names, confint_names)]
    summary_values <- summary_values[summary_names %in% c(estimate_names, confint_names)]
  } else if (!confidence_intervals & hypothesis_tests){
    summary_labels <- summary_labels[summary_names %in% c(estimate_names, hyptest_names)]
    summary_values <- summary_values[summary_names %in% c(estimate_names, hyptest_names)]
  }

  # function to make the header for the summary
  make_header <- function(number){
    if (length(number)==1){
      title <- paste0(" Statistics for simulation number ", number, " ")
    } else {
      title <- paste0(" Summary of ", length(number), " iterations ")
    }
    separator <- paste(rep("=", times=nchar(title)), collapse="")
    paste(separator, title, separator, sep="\n")
  }


  # output the summary
  cat("\n", make_header(numbers), "\n", sep="")
  cat(paste0(summary_labels, ": ", summary_values), sep="\n")
  cat("\n")
}


