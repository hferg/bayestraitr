#' getParams
#'
#' This functions returns the names of the estimated parameters from a BayesTraits
#' analysis logfile for input into plotting and other analysis functions.
#' Takes a logfile from a BayesTraits MCMC analysis and returns a vector
#' containing the names of all parameters estimated during the analysis. Useful
#' for defining parameters for other bayestraitr functions.
#' @param logfile The name of the trait data file on which BayesTraits was run.
#' @return A vector of the parameter names of the model logfile results from.
#' @export
#' @examples
#' getParams("cool-data.txt.log.txt")

getParams <- function(logfile) {
  raw <- readLines(logfile)
  params <- do.call(rbind, strsplit(raw[grep("\\bIteration\\b", raw):length(raw)], "\t"))[1, ]
  params <- params[c(2:length(params))]
  params <- chartr(" ()^-", ".....", params)
  return(params)
}

