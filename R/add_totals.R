
#' Add subscale scores
#' @export
#' @importFrom psych scoreItems
#'
add_totals <- function(data, keys) {
  my.scales.3 <- scoreItems(keys, data, totals = TRUE)
  my.scores.3 <- my.scales.3$scores
  # Add scale scores to data frame
  data$risk <- my.scores.3[, 1]
  data$cardioresp <- my.scores.3[, 2]
  data$airway <- my.scores.3[, 3]
  data$ncmscs_total <- my.scores.3[, 4]
  return(data)
}
