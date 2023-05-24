###########################################
#
# plots of various kinds
#
###########################################
# function to graph conf ints given a data frame of them
#   with first column equal to conf int lower and second conf int upper
# lowers: lower endpoints of intervals
# uppers: upper endpoints of intervals
# x: vector to use for x coordinates (typically 1:N or the p-values)

#' @export
plot_intervals <- function(lowers, uppers, estimates=NA, x=seq(along=lowers), true_value=NA, vertical=NA, contain_color="red", significance_level=NA, significance_color="blue"){
  N <- length(lowers)
  if (length(uppers) != N){
    stop("lower and upper endpoints must be vectors of the same length.")
  }

  segment_colors <- rep("A", N)
  if (!is.na(true_value)){
    segment_colors[true_value < lowers | true_value > uppers] <- "B"
  }

  output_plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(ggplot2::aes(x=x, xend=x, y=lowers, yend=uppers, color=segment_colors)) +
    ggplot2::theme(legend.position="none") +
    #ggplot2::scale_color_manual(values=c("black",contain_color, significance_color)) +
    ggplot2::ylab("")

  # if estimates doesn't have any missing values, plot it
  if (sum(is.na(estimates))==0){
    estimate_colors <- rep("A", N)
    if (!is.na(significance_level)){
      estimate_colors[x <= significance_level] <- "C"
    }

    output_plot <- output_plot + ggplot2::geom_point(mapping = ggplot2::aes(x=x, y=estimates, color=estimate_colors))
  }

  # color the output plot
  output_plot <- output_plot + ggplot2::scale_color_manual(values=c("black", contain_color, significance_color))

  if (!is.na(true_value)){
    output_plot <- output_plot + ggplot2::geom_hline(yintercept=true_value, linetype="dashed", color=contain_color)
  }

  if (sum(!is.na(vertical)) > 0){
    for (v in vertical){
      output_plot <- output_plot + ggplot2::geom_vline(xintercept=v, linetype="dashed", color=significance_color)
    }
  }

  output_plot
}

# density plot with points jittered on the x axis

#' @export
gg_density_plot <- function(x, point_colors="a"){
  # make the main plot
  if (length(x) > 1){
    # if there is more than one point, then make a density plot
    output_plot <- ggplot2::ggplot() +
      #ggplot2::geom_histogram(ggplot2::aes(x=x, y= ..density..)) +
      ggplot2::geom_density(ggplot2::aes(x=x))

    # add jittered points to the plot
    y_range <- ggplot2::layer_scales(output_plot)$y$get_limits()
    jitter_height <- diff(y_range)/10
    output_plot <- output_plot + ggplot2::geom_jitter(mapping=ggplot2::aes(x=x, y=0, color=point_colors), width=0, height=jitter_height)  + ggplot2::theme(legend.position="none") + ggplot2::scale_color_manual(values=c("black","red"))
  } else {
    # if there is only one point, just plot it
    output_plot <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x=x, y=0))
  }

  output_plot
}


#' @export
plot_p_values <- function(x, null_value, significance_level=NA, ...){
  point_colors <- rep("A", length(x))
  if (!is.na(significance_level)){
    point_colors[x <= significance_level] <- "B"
  }

  # make the main plot
  output_plot <- gg_density_plot(x, point_colors=point_colors) + ggplot2::xlab("p-value") + ggplot2::ylab("Density")

  # add a dashed red line for the significance level
  if (!is.na(significance_level)){
    output_plot <- output_plot + ggplot2::geom_vline(xintercept=significance_level, linetype="dashed", color="red")
  }

  output_plot
}


# standard plots for htest_lists

#' @export
plot_htest_list <- function(x, confidence_intervals=TRUE, p_values=TRUE, true_value=NA, significance_level=0.05, ...){
  # get estimates if they'll be needed
  if (!(!confidence_intervals & p_values) & slot_length(x, "estimate")==1){
    ests <- estimates(x)
    ests_label <- "Estimate"
  } else {
    ests <- estimates(x, number=1)-estimates(x, number=2)
    ests_label <- cap(join(summarize_componentwise(x, "estimate", attribute="names", summarizer=function(x) x[[1]]), and=" minus "))
  }

  if (!confidence_intervals & !p_values){
    # plot of only estimates
    xlab <- cap(ests_label)

    output_plot <- gg_density_plot(x=ests) + ggplot2::xlab(xlab) + ggplot2::ylab("Density")
  } else if (!confidence_intervals & p_values){
    # plot of only p-values
    output_plot <- plot_p_values(x=p_values(x), significance_level=significance_level, ...)
  } else if (confidence_intervals & !p_values){
    # plot of only confidence intervals
    output_plot <- plot_intervals(lowers=ci_lowers(x), uppers=ci_uppers(x), estimates=ests, true_value=true_value, ...) + ggplot2::xlab("Number")
  } else if (confidence_intervals & p_values){
    # plot of confidence intervals vs p-values
    output_plot <- plot_intervals(lowers=ci_lowers(x), uppers=ci_uppers(x), estimates=ests, x=p_values(x), true_value=true_value, vertical=significance_level, significance_level=significance_level, ...)
  }
  output_plot
}

