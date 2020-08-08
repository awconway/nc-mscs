#' @title Impute missing data
#' @importFrom TestDataImputation EMimpute
#' @export
#'
impute <- function(data) {
  EMimpute(data, Mvalue = "NA", max.score = 7)
}
