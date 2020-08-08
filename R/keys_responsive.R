#' Add scale scores
#' @export
#' @importFrom psych scoreItems
#'
keys_responsive <- function(responsiveness) {
  pre.keys <- list(
    risk = c(
      "Risk_anaesthesia_pre", "Risk_OSA_pre", "Risk_intubate_pre",
      "Risk_BMV_pre", "Risk_ASA_pre", "Risk_BMI_pre",
      "Risk_cardiorespiratory_pre"
    ),
    airway = c(
      "Intervention_OPA_pre", "Intervention_NPA_pre", "Intervention_jawsupport_pre",
      "Intervention_chinlift_pre"
    ),
    cardioresp = c(
      "Identify_resprate_pre",
      "Identify_bradycardia_pre",
      "Identify_hypotension_pre",
      "Respond_hypoventilation_pre", "Respond_hypoxia_pre",
      "Respond_bradycardia_pre", "Respond_hypotension_pre"
    ),
    total = c(
      "Risk_anaesthesia_pre", "Risk_OSA_pre", "Risk_intubate_pre",
      "Risk_BMV_pre", "Risk_ASA_pre", "Risk_BMI_pre",
      "Risk_cardiorespiratory_pre",
      "Intervention_OPA_pre", "Intervention_NPA_pre", "Intervention_jawsupport_pre",
      "Intervention_chinlift_pre",
      "Identify_resprate_pre",
      "Identify_bradycardia_pre",
      "Identify_hypotension_pre",
      "Respond_hypoventilation_pre", "Respond_hypoxia_pre",
      "Respond_bradycardia_pre", "Respond_hypotension_pre"
    )
  )

  post.keys <- list(
    risk = c(
      "Risk_anaesthesia_post", "Risk_OSA_post", "Risk_intubate_post",
      "Risk_BMV_post", "Risk_ASA_post", "Risk_BMI_post",
      "Risk_cardiorespiratory_post"
    ),
    airway = c(
      "Intervention_OPA_post", "Intervention_NPA_post", "Intervention_jawsupport_post",
      "Intervention_chinlift_post"
    ),
    cardioresp = c(
      "Identify_resprate_post",
      "Identify_bradycardia_post",
      "Identify_hypotension_post",
      "Respond_hypoventilation_post", "Respond_hypoxia_post",
      "Respond_bradycardia_post", "Respond_hypotension_post"
    ),
    total = c(
      "Risk_anaesthesia_post", "Risk_OSA_post", "Risk_intubate_post",
      "Risk_BMV_post", "Risk_ASA_post", "Risk_BMI_post",
      "Risk_cardiorespiratory_post",
      "Intervention_OPA_post", "Intervention_NPA_post", "Intervention_jawsupport_post",
      "Intervention_chinlift_post",
      "Identify_resprate_post",
      "Identify_bradycardia_post",
      "Identify_hypotension_post",
      "Respond_hypoventilation_post", "Respond_hypoxia_post",
      "Respond_bradycardia_post", "Respond_hypotension_post"
    )
  )


  pre.scales <- scoreItems(pre.keys, responsiveness,
    totals = TRUE
  )
  pre.scores <- pre.scales$scores
  post.scales <- scoreItems(post.keys, responsiveness,
    totals = TRUE
  )
  post.scores <- post.scales$scores


  responsiveness$pre_risk <- pre.scores[, 1]
  responsiveness$pre_airway <- pre.scores[, 2]
  responsiveness$pre_cardioresp <- pre.scores[, 3]
  responsiveness$pre_ncmscs_total <- pre.scores[, 4]

  responsiveness$post_risk <- post.scores[, 1]
  responsiveness$post_airway <- post.scores[, 2]
  responsiveness$post_cardioresp <- post.scores[, 3]
  responsiveness$post_ncmscs_total <- post.scores[, 4]

  responsiveness
}
