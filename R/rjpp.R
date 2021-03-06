##############################################################################
#' createCounts
#' Creates the counts table for the rjpp.
#' @param extree The time tree
#' @param meanbl The output of meanBranches
#' @name createCounts
#' @keywords internal
createCountsTable <- function(extree, meanbl) {
  counts <- matrix(ncol = 53, nrow = (nrow(extree$edge) + 1))
  colnames(counts) <- c("branch", "ancNode", "descNode", "nTips", "start", "end", "mid", "orgBL", 
      "meanBL", "medianBL", "modeBL", "quart25", "quart75", 
      "itersScaled", "itersRatescaled", "itersDelta", "itersKappa", "itersLambda", 
      "pScaled", "pRate", "pDelta", "pKappa", "pLambda",
      "nScalar", "nRate", "nDelta", "nKappa", "nLambda",
      "nOrgnScalar", "nOrgnNRate", "nOrgnBRate", "nOrgnDelta", "nOrgnKappa", "nOrgnLambda",
      "rangeRate", "lqRate", "uqRate", "meanRate", "medianRate", "modeRate",
      "rangeDelta", "meanDelta", "medianDelta", "modeDelta",
      "rangeKappa", "meanKappa", "medianKappa", "modeKappa",
      "rangeLambda", "meanLambda", "medianLambda", "modeLambda", "species")

  counts[ , "branch"] <- c(0:nrow(extree$edge))
  counts[ , "ancNode"] <- c(0, extree$edge[ , 1])
  counts[ , "descNode"] <- c((length(extree$tip.label) + 1), extree$edge[ , 2])
  counts[ , "orgBL"] <- c(0, extree$edge.length)
  
  if (is.list(meanbl)) {
    counts[ , "meanBL"] <- c(0, meanbl$meanbranches)
    counts[ , "medianBL"] <- c(0, meanbl$medianbranches)
    counts[ , "modeBL"] <- c(0, meanbl$modebranches)
    counts[ , "quart25"] <- c(0, meanbl$quart25)
    counts[ , "quart75"] <- c(0, meanbl$quart75)
  } else {
    counts[ , "meanBL"] <- rep(1, nrow(counts))
    counts[ , "medianBL"] <- rep(1, nrow(counts))
    counts[ , "modeBL"] <- rep(1, nrow(counts))
    counts[ , "quart25"] <- rep(1, nrow(counts))
    counts[ , "quart75"] <- rep(1, nrow(counts))
  }

  hts <- phytools::nodeHeights(extree)
  hts <- round(abs(hts - max(hts)), 4)
  counts[ , "start"] <- c(0, hts[ , 1])
  counts[ , "end"] <- c(0, hts[ , 2])
  counts <- as.data.frame(counts)

  # Deal with the root
  descs <- getDescs(extree, node = counts[1, "descNode"])
  counts[1, "nTips"] <- sum(descs <= length(extree$tip.label))
  counts[1, "mid"] <- 0
  counts[1, "species"] <- paste0(extree$tip.label[order(extree$tip.label)], collapse = ",")

  for (i in 2:nrow(counts)) {
    descs <- getDescs(extree, node = counts[i, "descNode"])
    counts[i, "nTips"] <- sum(descs <= length(extree$tip.label))
    if (counts[i, "nTips"] == 0) {
      counts[i, "nTips"] <- 1
    }
    if (counts[i, "descNode"] <= length(extree$tip.label)) {
      counts[i, "species"] <- extree$tip.label[counts[i, "descNode"]]
    } else {
      tips <- getDescs(extree, counts[i, "descNode"])
      tips <- tips[tips <= length(extree$tip.label)]
      tips <- extree$tip.label[tips]
      counts[i, "species"] <- paste0(sort(tips), collapse = ",")
    }
    counts[i, "mid"] <- mean(c(hts[(i - 1), 1], hts[(i - 1), 2]))
  }
  
  counts[ , c(14:52)] <- 0
  return(counts)
}

##############################################################################
#' scalarSearch
#' Searches through the posterior of an RJ continuous model for scalars and 
#' returns them.
#' @param rj_output partially processed RJ output.
#' @param counts The counts table.
#' @keywords internal
#' @name scalarSearch
scalarSearch <- function(rj_output, counts, fullmrcas) {
  alltypes <- vector(mode = "list", length = nrow(rj_output))
  allmrcas <- vector(mode = "list", length = nrow(rj_output))

  rates <- matrix(rep(1, nrow(counts) * nrow(rj_output)), ncol = nrow(rj_output))
  rownames(rates) <- counts[ , "descNode"]

  # make lists for the origins of deltas etc.
  .tmp <- rep(1, nrow(rj_output))
  Node <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Branch <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Delta <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Lambda <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Kappa <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  Node_effects <- replicate(nrow(counts), as.numeric(paste(.tmp)), simplify = FALSE)
  names(Node) <- counts[ , "descNode"]
  names(Branch) <- counts[ , "descNode"]
  names(Delta) <- counts[ , "descNode"]
  names(Lambda) <- counts[ , "descNode"]
  names(Kappa) <- counts[ , "descNode"]
  names(Node_effects) <- counts[ , "descNode"]

  print("Searching for scalars...")
    pb <- txtProgressBar(min = 0, max = nrow(rj_output), style = 3)
    for (i in 1:nrow(rj_output)) {
      lastrates <- rj_output[i, !is.na(rj_output[i, ])]
      
      # If the number of columns is seven, there are no scalars applied this generation.
      if (ncol(lastrates) == 7) {
        nodes <- NA
        scales <- NA
        types <- NA
      } else {
        
        int <- lastrates[8:length(lastrates)]
   
        nodes <- unlist(c(int[grep("NodeID*", names(int))]))
        scales <- unlist(c(int[grep("Scale*", names(int))]))
        types <- unlist(c(int[grep("NodeBranch*", names(int))]))
        mrcas <- sapply(nodes, function(x) fullmrcas[fullmrcas$node %in% x, "mrca"])
        alltypes[[i]] <- types
        allmrcas[[i]] <- mrcas

        # Is this for-loop filling the scalar objects? Do I need to make them 
        # within this function?
        for (j in 1:length(mrcas)) {
          nm <- paste0(types[j], "[[\"", as.character(mrcas[j]), "\"]]", "[", i, "]")
          eval(parse(text = paste0(nm, "<-", scales[j])))
        }
      }
      setTxtProgressBar(pb, i)    
    }

    close(pb)
    res <- list(alltypes = alltypes,
                allmrcas = allmrcas,
                rates = rates,
                Node = Node,
                Branch = Branch,
                Delta = Delta,
                Lambda = Lambda,
                Kappa = Kappa,
                Node_effects = Node_effects)
  return(res)
}

##############################################################################
#' rjpp
#'
#' A function that takes the output of a kappa, lambda, delta, VRates etc. RJ bayesTraits run and runs post-processing on it.
#' @param rjlog The RJ output of the run - typically suffixed with .VarRates.txt
#' @param tree The time tree the analysis was run on as an object of class "phylo", or the filename of the timetree.
#' @param burnin The burnin (if required) for the mcmc (generally worked out from the other logfile)
#' @param thinning Thinning parameter for the MCMC output - again, worked out from the raw MCMC output logfile.
#' @param meanbranches If true, calculates mean, median and mode branch lengths and returns mean tree.
#' @param ratestable 
#' @import phytools pbapply ape
#' @export
#' @name rjpp
rjpp <- function(rjlog, rjtrees, tree, burnin = 0, thinning = 1, 
  meanbranches = TRUE, ratestable = TRUE) {

  pbapply::pboptions(type = "txt", style = 3, char = "=")

  if (class(tree) == "phylo") {
    extree <- ladderize(tree)
  } else {
    extree <- ladderize(read.nexus(tree))
  }

  print("Loading log file.")
  rjout <- loadRJ(rjlog, burnin = burnin, thinning = thinning)

  print("Loading posterior trees.")
  posttrees <- read.nexus(rjtrees)
  posttrees <- posttrees[burnin:length(posttrees)]

  if (meanbranches) {
    print("Calculating mean branch lengths.")
    meanbl <- meanBranches(reftree = extree, trees = posttrees, burnin = burnin, 
      thinning = thinning, pbar = TRUE)
  } else {
    meanbl = FALSE
  }

  rj_output <- rjout$rj_output
  subtrees <- rjout$subtrees
  rjtaxa <- rjout$taxa
  niter <- nrow(rj_output)
  print("Finding taxa.")
  
  taxa <- pblapply(subtrees$node, function(x) getTaxa(x, subtrees = subtrees))
  
  print("Calculating MRCAs.")
  fullmrcas <- unlist(pblapply(taxa, function(x) getMRCAbtr(x , tree = extree, rjtaxa = rjtaxa)))
  fullmrcas <- data.frame(node = subtrees$node, mrca = fullmrcas)
 
  counts <- createCountsTable(extree, meanbl)

  # Find the scalars.
  all_scalars <- scalarSearch(rj_output, counts, fullmrcas)

  # Calculate cumulative node effects
  for (i in 1:length(all_scalars$Node)) {
    .tmp <- multiplyNodes(all_scalars$Node[[i]], 
      names(all_scalars$Node)[i], 
      extree, 
      all_scalars$Node_effects)
    all_scalars$Node_effects[names(.tmp)] <- .tmp
  }

  all_scalars$Node_effects <- lapply(1:length(all_scalars$Node_effects), 
    function(x) all_scalars$Node_effects[[x]] * all_scalars$Branch[[x]])
  names(all_scalars$Node_effects) <- counts[ , "descNode"]
  
  origins <- list(nodes = do.call(rbind, all_scalars$Node), 
                  branches = do.call(rbind, all_scalars$Branch), 
                  delta = do.call(rbind, all_scalars$Delta),
                  lambda = do.call(rbind, all_scalars$Lambda), 
                  kappa = do.call(rbind, all_scalars$Kappa), 
                  rates = do.call(rbind, all_scalars$Node_effects)
                  )

  alltypes <- unlist(all_scalars$alltypes)
  allmrcas <- unlist(all_scalars$allmrcas)

  bs <- table(unlist(allmrcas)[alltypes == "Branch"])
  ns <- table(unlist(allmrcas)[alltypes == "Node"])
  ds <- table(unlist(allmrcas)[alltypes == "Delta"])
  ks <- table(unlist(allmrcas)[alltypes == "Kappa"])
  ls <- table(unlist(allmrcas)[alltypes == "Lambda"])

  bstaxa <- counts$descNode %in% names(bs)
  nstaxa <- counts$descNode %in% names(ns)
  dstaxa <- counts$descNode %in% names(ds)
  kstaxa <- counts$descNode %in% names(ks)
  lstaxa <- counts$descNode %in% names(ls)

  counts$nOrgnBRate[bstaxa] <- bs[match(counts$descNode[bstaxa], names(bs))] 
  counts$nOrgnScalar[bstaxa] <- counts$nOrgnBRate[bstaxa] + bs[match(counts$descNode[bstaxa], names(bs))] 
  
  counts$nOrgnNRate[nstaxa] <- ns[match(counts$descNode[nstaxa], names(ns))]
  counts$nOrgnScalar[nstaxa] <- counts$nOrgnBRate[nstaxa] + ns[match(counts$descNode[nstaxa], names(ns))]
  
  counts$nOrgnDelta[dstaxa] <- ds[match(counts$descNode[dstaxa], names(ds))]
  counts$nOrgnScalar[dstaxa] <- counts$nOrgnBRate[dstaxa] + ds[match(counts$descNode[dstaxa], names(ds))]
  
  counts$nOrgnKappa[kstaxa] <- ks[match(counts$descNode[kstaxa], names(ks))]
  counts$nOrgnScalar[kstaxa] <- counts$nOrgnBRate[kstaxa] + ks[match(counts$descNode[kstaxa], names(ks))]
  
  counts$nOrgnLambda[lstaxa] <- ls[match(counts$descNode[lstaxa], names(ls))]
  counts$nOrgnScalar[lstaxa] <- counts$nOrgnBRate[lstaxa] + ls[match(counts$descNode[lstaxa], names(ls))]
  
  # Fill in transformation detail.

  counts[ , "meanDelta"] <- rowMeans(origins$delta)
  counts[ , "medianDelta"] <- apply(origins$delta, 1, median)
  counts[ , "modeDelta"] <-  apply(origins$delta, 1, modeStat)
  counts[ , "rangeDelta"] <- suppressWarnings(apply(origins$delta, 1, max) - apply(origins$delta, 1, min))

  counts[ , "meanKappa"] <- rowMeans(origins$kappa)
  counts[ , "medianKappa"] <- apply(origins$kappa, 1, median)
  counts[ , "modeKappa"] <- apply(origins$kappa, 1, modeStat)
  counts[ , "rangeKappa"] <- suppressWarnings(apply(origins$kappa, 1, max) - apply(origins$kappa, 1, min))

  counts[ , "meanLambda"] <- rowMeans(origins$lambda)
  counts[ , "medianLambda"] <- apply(origins$lambda, 1, median)
  counts[ , "modeLambda"] <- apply(origins$lambda, 1, modeStat)
  counts[ , "rangeLambda"] <- suppressWarnings(apply(origins$lambda, 1, max) - apply(origins$lambda, 1, min))

  counts[ , "meanRate"] <- rowMeans(origins$rates)
  counts[ , "medianRate"] <- apply(origins$rates, 1, median)
  counts[ , "modeRate"] <- apply(origins$rates, 1, modeStat)
  counts[ , "rangeRate"] <- suppressWarnings(apply(origins$rates, 1, max) - apply(origins$rates, 1, min))

  counts[ , "itersScaled"] <- 
  counts[ , "itersRatescaled"] <- apply(origins$rates, 1, function(x) sum(x != 1))
  counts[ , "itersDelta"] <- counts[ , "nOrgnDelta"]
  counts[ , "itersKappa"] <- counts[ , "nOrgnDelta"]
  counts[ , "itersLambda"] <- counts[ , "nOrgnDelta"]

  counts[ , "pScaled"] <- 
  counts[ , "pRate"] <- apply(origins$rates, 1, function(x) sum(x != 1)) / niter
  counts[ , "pDelta"] <- counts[ , "nOrgnDelta"] / niter
  counts[ , "pKappa"] <- counts[ , "nOrgnKappa"] / niter
  counts[ , "pLambda"] <- counts[ , "nOrgnLambda"] / niter

  counts[ , "nScalar"] <- 
  counts[ , "nRate"] <- counts[ , "nOrgnNRate"] + counts[ , "nOrgnBRate"]
  counts[ , "nDelta"] <- counts[ , "nOrgnDelta"]
  counts[ , "nKappa"] <- counts[ , "nOrgnDelta"]
  counts[ , "nLambda"] <- counts[ , "nOrgnDelta"]

  # Now just remove anything that is irrelevant (i.e. all values == 1) and then work out
  # what to return.

  counts <- counts[ , apply(counts, 2, function(x) all(x != 1))]
  counts <- counts[ , apply(counts, 2, function(x) all(x != 0))]

  if (meanbranches) {
    meantree <- extree
    meantree$edge.length <- counts[c(2:nrow(counts)) , "meanBL"]
  }

  if (all(sapply(origins$delta, function(x) all(x == 1)))) {
    origins$delta <- NULL
  }

  if (all(sapply(origins$kappa, function(x) all(x == 1)))) {
    origins$kappa <- NULL
  }

  if (all(sapply(origins$lambda, function(x) all(x == 1)))) {
    origins$lambda <- NULL
  }

  if (all(sapply(origins$nodes, function(x) all(x == 1)))) {
    origins$nodes <- NULL
  }  

  if (all(sapply(origins$branches, function(x) all(x == 1)))) {
    origins$branches <- NULL
  }

  if (all(sapply(origins$rates, function(x) all(x == 1)))) {
    origins$rates <- NULL
  }

  if (meanbranches) { 
    res <- list(data = counts, niter = niter, meantree = meantree)
  } else {
    res <- list(data = counts, niter = niter)
  }

  if (!is.null(origins$rates)) {
    scalars <- list(rates = origins$rates)
    origins$rates <- NULL
    res <- c(res, list(scalars = scalars))
  }

  res <- c(res, list(origins = origins))

  return(res)
}

