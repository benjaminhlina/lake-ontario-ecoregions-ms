siberMVN_bh <- function (siber, parms, priors)
  # Define a function with three parameters: siber, parms, and priors
{
  siber.posterior <- list()
  ct <- 1
  for (k in 1:siber$n.communities) {
    for (j in 1:siber$n.groups[2, k]) {
      grp.j <- siber$zscore.data[[k]][, "group"] == siber$group.names[[k]][j]
      x.zscore <- siber$zscore.data[[k]][grp.j, 1]
      y.zscore <- siber$zscore.data[[k]][grp.j, 2]
      # Extract the y (second column) z-scores for the j-th group
      id <- paste0("group", k, "_community", j)
      model <- fitEllipse(x.zscore, y.zscore, parms, priors,
                          id)
      corrected.posteriors <- SIBER:::ellipseBackTransform(model,
                                                   siber, k, j)
      siber.posterior[[ct]] <- corrected.posteriors
      ct <- ct + 1
    }
  }
  tmp.names <- paste(unique(siber$original.data[, "group"]),
                            unique(siber$original.data[, "community"]), sep = ".")
  names(siber.posterior) <- tmp.names
  return(siber.posterior)
}
