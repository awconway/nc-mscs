#' @title Scree plot
#' @export
#' @importFrom nFactors nScree parallel plotnScree
#' @importFrom psych polychoric
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select
#' @importFrom  ggplot2 aes ggplot geom_line geom_point theme element_blank
#'
scree_plot <- function(ncmscs_items) {
  polycor <- polychoric(as.matrix(ncmscs_items), smooth = FALSE)
  ev <- eigen(cor(polycor$rho))
  ap <- parallel(
    subject = nrow(polycor$rho), var = ncol(polycor$rho),
    rep = 100, cent = .05, model = "factors"
  )
  nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
  pa <- nS$Analysis

  pa %>%
    rowid_to_column("Components") %>%
    select(Components,
      `Parallel Analysis` = `Par.Analysis`,
      Eigenvalues
    ) %>%
    pivot_longer(cols = -Components, names_to = "type", values_to = "Eigenvalues") %>%
    ggplot(aes(y = Eigenvalues, x = Components, color = type)) +
    geom_line() +
    geom_point() +
    theme(legend.title = element_blank())
}
