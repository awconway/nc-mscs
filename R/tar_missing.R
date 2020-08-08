#' @title Amount of missing data
#' @export
#'
#'
tar_missing <- function(ncmscs_full) {
  (sum(is.na(ncmscs_full)) / prod(dim(ncmscs_full))) * 100
}
