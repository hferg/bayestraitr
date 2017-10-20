#' getStones
#'
#' Get the marginal likelihoods from from one or more stepping stones log
#' files.
#' @param logs A vector of one or more log files from stepping stones analysis.
#' @param order If TRUE then the resultant data.frame is ordered according to
#' marginal likelihood.
#' @name getStones
#' @return A data.frame with a row per stones file, and a column with the
#' marginal likelihoods of each model in.
#' @export

getStones <- function(logs, order = TRUE) {
  # TODO A function to count the parameters in the original model and return
  # them as well as the marginal likelihoods (to look at improvement in
  # marginal Lh as parameter numbers increase).
  res <- matrix(ncol = 2, nrow = length(logs))
  colnames(res) <- c("logfile", "marginalLh")
  for (i in 1:length(logs)) {
    raw <- readLines(logs[[i]])
    res[i, 1] <- logs[[i]]
    res[i, 2] <- as.numeric(strsplit(raw[length(raw)], "\t")[[1]][2])
  }
  res <- data.frame(res)
  res$marginalLh <- as.numeric(as.character(res$marginalLh))

  if (order) {
    res <- res[order(res$marginalLh, decreasing = TRUE), ]
  }

  return(res)
}
