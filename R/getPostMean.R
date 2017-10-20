#' getPostStats
#'
#' Given a posterior of MCMC samples from a BayesTraits analysis (a logfile -
#' typically with the .log.txt extension) and the name of one or morea
#' parameters present in that logfile this will return the mean, median, mode
#' and standard deviation of the parameter estimates.
#' @param logfile The name of the logfile of the BayesTraits analysis.
#' @param params The name(s) of one or more parameters present in the logfile.
#' @param thinning Thinning parameter for the posterior - defaults to 1
#' (all samples). 2 uses every second sample, 3 every third and so on.
#' @param burnin The number of generations to remove from the start of the
#' chain as burnin. Use if the chain has not reached convergence before sampling
#' began. Useful if the burnin parameter for the analysis itself was not long
#' enough.
#' @return An object of class data.frame with a row for each parameter, and
#' columns for the mean, median, mode and standard deviation of those
#' parameters.
#' @export
#' @examples
#' params <- getParams("cool-data.log.txt")
#' getPostMean("cool-data.log.txt", params[c(2:5)]
#' getPostMean("cool-data.log.txt", "Lh")

getPostStats <- function(logfile, params, thinning = 1, burnin = 0) {

  modeStat <- function(x) {
    z <- unique(x)
    x[which.max(tabulate(match(x, z)))]
  }

  params.exc <- c("Model.string", "Tree.No", "No.Off.Parmeters", "No.Off.Zero")
  output <- btmcmc(logfile, thinning = thinning, burnin = burnin)
  # TODO include a test to check that param names are present in the posterior,
  # otherwise return an error.

  params.tmp <- params[which(!params %in% params.exc)]
  res <- matrix(ncol = 5, nrow = length(params.tmp))
  colnames(res) <- c("param", "mean", "median", "mode", "sd")

  for (i in 1:length(params.tmp)) {
    print(params.tmp[i])
    res[i, 1] <- params.tmp[i]
    res[i, 2] <- round(mean(output[params.tmp[i]][ ,1]), 4)
    res[i, 3] <- round(median(output[params.tmp[i]][ ,1]), 4)
    res[i, 4] <- round(modeStat(output[params.tmp[i]][ ,1]), 4)
    res[i, 5] <- round(sd(output[params.tmp[i]][ ,1]), 4)
  }

  return(data.frame(res))
}

