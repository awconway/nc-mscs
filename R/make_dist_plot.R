#' @title
#' @return
#' @export
#' @importFrom ggplot2 ggplot aes
#' labs geom_histogram
#' @importFrom glue glue
make_dist_plot <- function(data, scale, xaxis) {
  {{ data }} %>%
    ggplot(aes(x = {{ scale }})) +
    geom_histogram(
      binwidth = 0.5,
      fill = "#2a6ebb"
    ) +
    labs(
      y = "Count",
      x = glue::glue("{xaxis}")
    )
}
