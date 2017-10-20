################################################################################
#' findShifts
#'
#' Returns the node numbers of nodes that have been scaled in a specified
#' fraction of iterations in the posterior of a BayesTraits MCMC analysis using
#' variable rates of a variable transformation.
#'
#' Works for rate scalars, and transformations. Also can return branch scalars,
#' if the specified scalar is "rate". Only looks at the position that scalars
#' are placed, not the cumulative effects of these scalars (i.e. if a node is
#' never given a scalar itself but is affected by a scalar on an ancestral node,
#' it will not be identified by this function).
#' @param PP The postprocessor (\link[bayestraitr]{getRates}) output.
#' @param scalar The scalar to find and plot from the post processo:r
#'
#' - delta
#'
#' - lambda
#'
#' - kappa
#'
#' - node
#'
#' - branch
#'
#' - rate (returns both branch and node scalars).
#' @param threshold The fraction of samples a scalar needs to be present in to
#' be returned. e.g. if 0.5, then all scalars present in the posterior more than
#' 50\% of the time will be returned.
#' @name findShifts
#' @return A vector of node/edge numbers (depending on scalar) that are scaled
#' over the threshold value.
#' @export

findShifts <- function(PP, scalar, threshold) {
  if (scalar == "delta") {
    cl <- "nOrgnDelta"
    mode <- "trans"
  } else if (scalar == "kappa") {
    cl <- "nOrgnKappa"
    mode <- "trans"
  } else if (scalar == "lambda") {
    cl <- "nOrgnLambda"
    mode <- "trans"
  } else if (scalar == "branch") {
    cl <- "nOrgnBRate"
    mode <- "trans"
  } else if (scalar == "node") {
    cl <- "nOrgnNRate"
    mode <- "trans"
  } else if (scalar == "rate") {
    cl <- "nOrgnScalar"
    mode <- "rate"
  }

  if (mode == "rate") {
    output <- findRateShifts(PP, threshold)
  } else if (mode == "trans") {
    if (type == "branch") {
      stop("Branch scalars are not valid for transformations.")
    }
    output <- findTransShifts(PP, threshold, cl)
  }

  return(output)
}

################################################################################
#' findRateShifts
#'
#' Return the node or branch numbers with rate scalars applied over the
#' threshold number.
#' @param PP an output from rjpp
#' @param threshold the threshold over which scalars are considered interesting.
#' @param type Node or branch
#' @keywords internal
#' @name findRateShifts

findRateShifts <- function(PP, threshold, type) {

  if (threshold == 0) {
    threshold <- 1 / PP$niter
  }

  nodes <- PP$data$descNode[which((PP$data[ , "nOrgnNRate"] / PP$niter) >= threshold)]
  branches <- PP$data$descNode[which((PP$data[ , "nOrgnBRate"] / PP$niter) >= threshold)]
  edges <- which(tree$edge[ , 2] %in% branches)

  return(list(nodes = nodes, edges = edges))
}

################################################################################
#' findTransShifts
#'
#' return the node numbers that have had transformations applied over a given
#' threshold.
#' @param PP an output from rjpp
#' @param threshold The threshold over which transformations are considered
#' "significant".
#' @keywords internal
#' @name findTransShifts

findTransShifts <- function(PP, threshold, cl) {
  if (threshold == 0) {
    threshold <- 1 / PP$niter
  }

  nodes <- PP$data$descNode[which((PP$data[ , cl] / PP$niter) >= threshold)]
  return(nodes)
}