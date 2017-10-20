#' ggDens
#'
#' Make a density plot for an MCMC parameter using ggplot2.
#' @param dat The vector of paramater you want to see an autocorrelation plot
#' for.

ggDens <- function(output, pars, title = ""){
  dat <- output[ ,pars]
  bw <- 1.06 * min(sd(dat), IQR(dat)/1.34) * length(dat)^-0.2
  p <- ggplot2::ggplot(data.frame(p = dat), ggplot2::aes(x = p)) +
    ggplot2::geom_density(alpha = 0.3, fill = "dodgerblue", binwidth = bw) +
    ggplot2::ggtitle(paste(pars, title))

  return(p)
}

