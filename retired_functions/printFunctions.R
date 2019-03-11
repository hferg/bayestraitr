#' Print function for the S3 class trees_summary
#'
#' @name print.trees_summary

print.trees_summary <- function(x) {
  n <- length(x)
  nm <- names(x)
  cat(n, "phylogenetic trees\n")
  for (i in seq_along(x)) {
    cat("tree", i, ":", nm[i], length(x[[i]]$tip.label), "tips\n")
  }
}
