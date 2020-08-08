#' Plot of loadings
#' @export
#' @importFrom ggplot2 ggplot aes facet_wrap geom_bar
#' coord_flip scale_fill_gradient2 ylab theme_bw
#' @importFrom plyr mapvalues

factor_plot <- function(data) {
  t <- as.table(print(data$loadings))
  t1 <- as.data.frame(t)
  t1$Var2 <- mapvalues(t1$Var2, from = c("A", "B", "C"), to = c("Identify and respond", "Risk assessment", "Technical skills"))
  t1$Var1 <- mapvalues(t1$Var1, from = c(
    "RiskHistory", "RiskOSA", "DifficultIntubate", "DifficultBMV",
    "RiskASA", "RiskBMI", "RiskCardioresp",
    "IdentifyRespRate",
    "IdentifyBradycardia", "IdentifyHypotension", "RespondHypoventilation",
    "RespondHypoxia", "RespondBradycardia", "RespondHypotension",
    "OPA", "NPA", "JawSupport",
    "ChinLift"
  ), to = c(
    "Assess risk from anaesthetic history", "Assess risk from sleep apnoea", "Determine risk for difficult intubation", "Determine risk for difficult bag mask ventilation",
    "Assess risk from ASA classification status", "Assess risk from body mass index", "Assess risk from cardiorespiratory reserve",
    "Identify abnormal respiratory rate",
    "Identify bradycardia", "Identify hypotension", "Respond to hypoventilation",
    "Respond to hypoxia", "Respond to bradycardia", "Respond to hypotension",
    "Insert oropharyngeal airway", "Insert nasopharyngeal airway", "Apply jaw support",
    "Apply chin lift"
  ))
  names <- c("Item", "Factor", "Loading")
  colnames(t1) <- names

  # For each test, plot the loading as length and fill color of a bar
  # note that the length will be the absolute value of the loading but the
  # fill color will be the signed value, more on this below

  ggplot(t1, aes(Item, abs(Loading), fill = Loading)) +
    facet_wrap(~Factor, nrow = 1) + # place the factors in separate facets
    geom_bar(stat = "identity") + # make the bars
    coord_flip() + # flip the axes so the test names can be horizontal
    # define the fill color gradient: blue=positive, red=negative
    scale_fill_gradient2(
      name = "Loading",
      high = "blue", mid = "white", low = "red",
      midpoint = 0, guide = F
    ) +
    ylab("Loading Strength") + # improve y-axis label
    theme_bw(base_size = 10) # use a black and white theme with set font size
}
