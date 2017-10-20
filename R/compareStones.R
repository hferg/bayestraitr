#' compareStones
#'
#' A function to extract the marginal likelihood from the stepping
#' stone sampler output of two BayesTraits runs, and work out the
#' (NOT log) Bayes Factors.
#' @param comp The name of the stepping stones log file of the more complex of
#' the two BayesTraits models.
#' @param simp The name of the stepping stone log file of the more simple of the
#' two BayesTraits models.
#' @return The Bayes factor of the comparison between the more complex and the
#' more simple model. This is not logged for saftey (e.g. in the case of a
#' negative difference between complex and simple model).
#' @export
#' @examples
#' compareStones("complex.model.stones.log.txt", "simple.model.stones.log.txt")

compareStones <- function(comp, simp) {
  raw.comp <- readLines(comp)
  raw.simp <- readLines(simp)

  marg.lh.comp <-  as.numeric(strsplit(raw.comp[length(raw.comp)], "\t")[[1]][2])
  marg.lh.simp <- as.numeric(strsplit(raw.simp[length(raw.simp)], "\t")[[1]][2])

  bf <- 2 * (marg.lh.comp - marg.lh.simp)
  evidence <- vector()

  if (bf < 2) {
    evidence <- "Weak evidence for complex model"
  } else if (bf >= 2 && bf < 5) {
    evidence <- "Positive evidence for complex model"
  } else if (bf >= 5 && logBF <= 10) {
    evidence <- "Strong evidence for complex model"
  } else if (bf > 10) {
    evidence <- "Very strong evidence for complex model"
  }

  names(bf) <- "Bayes Factor"
  return(bf)
}

