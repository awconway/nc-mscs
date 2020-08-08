#' @title Impute missing data
#' @importFrom TestDataImputation EMimpute
#' @export
#'
impute_missing <- function(data) {
  data <- as.matrix(data)
  EMimpute(data, Mvalue = "NA", max.score = 7)
}
