#' Knowledge regression plot
#' @name ggplotRegression_know
#' @export
#' @importFrom ggplot2 ggplot aes stat_smooth labs theme_bw theme
#' element_blank element_line aes_string
ggplotRegression_know <- function(fit) {
  ggplot(fit$model, aes_string(
    x = names(fit$model)[2],
    y = names(fit$model)[1]
  )) +
    geom_point() +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    stat_smooth(method = "lm", col = "red") +
    labs(
      caption = paste(
        "Adj R squared = ", signif(summary(fit)$adj.r.squared, 3),
        "Intercept =", signif(fit$coef[[1]], 3),
        " Slope =", signif(fit$coef[[2]], 3),
        " P =", format(summary(fit)$coef[2, 4], scientific = F, digits = 3)
      ),
      y = "Post minus pre NC-MSCS",
      x = "Post minus pre perceived overall knowledge about sedation"
    )
}

#' Confidence regression plot
#' @name ggplotRegression_conf
#' @export
#' @importFrom ggplot2 ggplot aes stat_smooth labs theme_bw theme
#' element_blank element_line aes_string
ggplotRegression_conf <- function(fit) {
  ggplot(fit$model, aes_string(
    x = names(fit$model)[2],
    y = names(fit$model)[1]
  )) +
    geom_point() +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    stat_smooth(method = "lm", col = "red") +
    labs(
      caption = paste(
        "Adj R squared = ", signif(summary(fit)$adj.r.squared, 3),
        "Intercept =", signif(fit$coef[[1]], 3),
        " Slope =", signif(fit$coef[[2]], 3),
        " P =", format(summary(fit)$coef[2, 4], scientific = F, digits = 3)
      ),
      y = "Post minus pre NC-MSCS",
      x = "Post minus pre perceived overall confidence managing sedation"
    )
}

#' Combine plots
#' @name ggplotRegression_comb
#' @export
#' @importFrom patchwork plot_layout plot_annotation
ggplotRegression_comb <- function(conf_reg,
                                  knowledge_reg) {
  comb <- conf_reg /
    knowledge_reg

  comb +
    plot_annotation(tag_levels = "A")
}
