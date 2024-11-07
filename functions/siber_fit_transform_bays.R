fit_and_transform_group <- function(data, parms, priors) {
  x.zscore <- data$iso2
  y.zscore <- data$iso1
  id <- paste0("group", data$group[1], "_community", data$community[1])
  model <- fitEllipse(x.zscore, y.zscore, parms, priors, id)
  SIBER:::ellipseBackTransform(model, siber_example, data$community[1], data$group[1])
}
