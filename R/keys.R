#' Keys
#' @export
#'

keys <- function() {
  list(
    risk = c(
      "RiskHistory",
      "RiskOSA", "DifficultIntubate", "DifficultBMV",
      "RiskASA", "RiskBMI", "RiskCardioresp"
    ),
    cardioresp = c(
      "IdentifyRespRate",
      "IdentifyBradycardia", "IdentifyHypotension", "RespondHypoventilation",
      "RespondHypoxia", "RespondBradycardia", "RespondHypotension"
    ),
    airway = c(
      "OPA", "NPA", "JawSupport",
      "ChinLift"
    ),
    ncmscs_total = c(
      "RiskHistory",
      "RiskOSA", "DifficultIntubate", "DifficultBMV",
      "RiskASA", "RiskBMI", "RiskCardioresp",
      "IdentifyRespRate",
      "IdentifyBradycardia", "IdentifyHypotension", "RespondHypoventilation",
      "RespondHypoxia", "RespondBradycardia", "RespondHypotension",
      "OPA", "NPA", "JawSupport",
      "ChinLift"
    )
  )
}
