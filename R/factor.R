#' @title Factor analysis
#' @rdname factor
#' @export
#' @importFrom psych polychoric fa


factor <- function(ncmscs_factor_df) {
  polycor <- polychoric(ncmscs_factor_df, smooth = FALSE)
  fa(polycor$rho, nfactors = 3, rotate = "Promax", fm = "ols")
}
