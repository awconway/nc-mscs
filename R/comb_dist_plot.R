#' Combine distribution plots
#' @export
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom ggplot2 theme_classic
make_comb_dist_plot <- function(risk_dist,
                                cardioresp_dist,
                                airway_dist,
                                ncmscs_total_dist) {
  comb <- risk_dist /
    cardioresp_dist /
    airway_dist /
    ncmscs_total_dist

  comb +
    plot_annotation(tag_levels = "A")&
    theme_classic()
}
