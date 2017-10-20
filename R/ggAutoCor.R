#' ggAutoCor
#'
#' Makes an autocorrealtion plot using ggplot2 - which looks nicer than the
#' core R plot functions.
#' @param dat The data for the plot, in an object of class vector.
#' @param conf The confidence level you want to see the limits for.
#' @param max.lag Maximum lag
#' @param min.lag Minimum lag

ggAutoCor <- function(output, pars, conf, max.lag = NULL, min.lag = 0, title = "") {
  dat <- output[ ,pars]
  confline <- qnorm((1 - conf)/2)/sqrt(length(dat))
  x <- acf(dat, plot = FALSE, lag.max = max.lag)
  xacf <- with(x, data.frame(lag, acf))

  if (min.lag > 0) {
    xacf <- xacf[-seq(1, min.lag), ]
  }

  sig <- (abs(xacf[ ,2]) > abs(confline)) ^2
  p <- ggplot2::ggplot(xacf, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_bar(color = "darkgray", stat = "identity", position = "identity", fill = "dodgerblue") +
    ggplot2::geom_hline(yintercept = -confline, color = "blue", size = 0.2) +
    ggplot2::geom_hline(yintercept = confline, color = "blue", size = 0.2) +
    ggplot2::geom_hline(yintercept = 0, color = "red", size = 0.2) +
    ggplot2::ggtitle(paste(pars, title))

  return(p)
}

