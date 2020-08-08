#' Add variables for models
#' @export
#' @importFrom dplyr rowwise mutate c_across ungroup
#'
add_modelling_vars <- function(data) {
  data$critcare <- data$Specialty == 10 | data$Specialty == 9 | data$Specialty == 8 | data$Specialty == 7


  rf <- rowwise(data)

  rf <- rf %>%
    mutate(GSE = sum(c_across(contains("GSES"))))



  ungroup(rf)
}
