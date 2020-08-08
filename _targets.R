library(targets)

devtools::load_all()
load("data/data.Rdata")
# Define targets
tar_pipeline(
  tar_target(ncmscs_full, data[, 24:57]),
  tar_target(missing, tar_missing(ncmscs_full)),
  tar_target(ncmscs_imp, impute_missing(ncmscs_full)),
  # Sedation reversal – worded with more than one part in the question
  # Laryngeal mask airway – not all trained for this
  # AssessLevel Sedation - wording was too similar to identify over-sedation
  tar_target(ncmscs_items, ncmscs_imp %>%
    dplyr::select(
      -Medication,
      -LMA,
      -AssessLevelSedation
    )),
  tar_target(
    factorability,
    psych::KMO(ncmscs_items)
  ),
  tar_target(screePlot, scree_plot(ncmscs_items)),
  # Remove RiskComorbidity, RespondObstruction,
  # IdentifyHypoventilation, BMV
  #   because difference <0.15
  # Remove RiskFrailty because it loaded on the wrong factor
  # Remove NontechAccessAssist because of Heywood case
  # This solution identified a factor structure consisting of:
  # 1. Advanced airway assessment skills
  # 2. Technical skills related to airway patency
  # 3. General skills related to identifying and responding to
  # sedation-related complications.
  # Removed the non-technical items and additional 'identify' items becuase they
  # didn't load consistently on one of the conceptually derived factors
  tar_target(ncmscs_factor_df, ncmscs_items %>%
    dplyr::select(
      -RiskComorbid,
      -IdentifyHypoventilation,
      -BMV,
      -RespondObstruction,
      -RiskFrailty,
      -NontechAccessAssist,
      -IdentifyObstruction,
      -IdentifyLaryngospasm,
      -IdentifyAllergy,
      -IdentifyOversedation,
      -NontechRespond,
      -NontechIdentifyAssist,
      -NontechAssess
    )),
  tar_target(ncmscs_factor, factor(ncmscs_factor_df)),
  tar_target(keys_list, keys()),

  tar_target(ncmscs_df, add_totals(data, keys_list)),
  tar_target(ncmscs_plot, factor_plot(ncmscs_factor)),
  tar_target(ncmscs_modelling, add_modelling_vars(ncmscs_df)),
  tar_target(
    overall_model,
    broom::tidy(lm(ncmscs_total ~ YearsNursing + YearsPSA +
      PSAFrequency + ALS + PSAEducation +
      GSE + critcare +
      OverallKnowledge, ncmscs_modelling), conf.int = TRUE)
  ),
  tar_target(
    reliability_total,
    psych::alpha(ncmscs_factor_df)
  ),
  tar_target(
    reliability_risk,
    ncmscs_factor_df %>% dplyr::select(
      targets::tar_read(keys_list)$risk
    ) %>%
      psych::alpha()
  ),
  tar_target(
    reliability_airway,
    ncmscs_factor_df %>% dplyr::select(
      targets::tar_read(keys_list)$airway
    ) %>%
      psych::alpha()
  ),
  tar_target(
    reliability_cardioresp,
    ncmscs_factor_df %>% dplyr::select(
      targets::tar_read(keys_list)$cardioresp
    ) %>%
      psych::alpha()
  ),
  tar_target(
    responsiveness,
    readr::read_csv("data/responsiveness.csv") %>%
      dplyr::mutate(
        Specialty = dplyr::case_when(
          Specialty == "1" ~ "CCL",
          Specialty == "3" ~ "Endoscopy",
          Specialty == "5" ~ "Surgery",
          Specialty == "7" ~ "Emergency",
          Specialty == "8" ~ "Anesthesia/Recovery",
          Specialty == "9" ~ "Medical ward",
          Specialty == "10" ~ "ICU"
        ),
        PSA_frequency = dplyr::case_when(
          PSA_frequency == "1" ~ "Never",
          PSA_frequency == "2" ~ "Rarely",
          PSA_frequency == "3" ~ "Sometimes",
          PSA_frequency == "4" ~ "Often",
          PSA_frequency == "5" ~ "Always"
        )
      )
  ),
  tar_target(resp_totals, keys_responsive(responsiveness)),
  tar_target(change_summaries, resp_totals %>%
    dplyr::mutate(
      totalchange = post_ncmscs_total - pre_ncmscs_total
    ) %>%
    dplyr::summarise(
      presd = sd(pre_ncmscs_total),
      changesd = sd(totalchange),
      changemean = mean(totalchange),
      SRM = changemean / changesd,
      SRMadj = round(SRM * cor(pre_ncmscs_total, post_ncmscs_total), 1)
    )),
  tar_target(paired, resp_totals %>%
    dplyr::select(ID,
      pre = pre_ncmscs_total,
      post = post_ncmscs_total
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::contains("p"),
      names_to = "time", values_to = "NC-MSCS"
    ) %>%
    dabestr::dabest(time, `NC-MSCS`,
      idx = c("pre", "post"),
      paired = TRUE, id.col = ID
    )),
  tar_target(paired_diff, dabestr::mean_diff(paired)),
  tar_target(paired_plot, paired %>%
    dabestr::mean_diff() %>%
    plot()),
  tar_target(resp_change, resp_totals %>%
    dplyr::mutate(
      totalchange = post_ncmscs_total - pre_ncmscs_total,
      confidence_change = PSAconfidence_post - PSAconfidence_pre,
      knowledge_change = PSAknowledge_post - PSAknowledge_pre
    )),
  tar_target(
    conf_reg,
    ggplotRegression_conf(lm(totalchange ~ confidence_change,
      data = resp_change
    ))
  ),
  tar_target(
    knowledge_reg,
    ggplotRegression_know(lm(totalchange ~ knowledge_change,
      data = resp_change
    ))
  ),
  tar_target(
    comb_reg,
    ggplotRegression_comb(conf_reg, knowledge_reg)
  ),
  tar_target(characteristics_table, ncmscs_df %>%
    dplyr::mutate(
      Specialty = haven::as_factor(Specialty),
      ALS = haven::as_factor(ALS),
      HospitalType = haven::as_factor(HospitalType),
      PSAFrequency = haven::as_factor(PSAFrequency),
      `Received education about sedation` = ifelse(PSAEducation == 1,
        TRUE, FALSE
      ),
      `Unit/hospital has a policy about sedation` = ifelse(PSAPolicy == 1,
        TRUE, FALSE
      ),
      `Required to undergo competency assessment in sedation` = ifelse(Competency == 1,
        TRUE, FALSE
      )
    ) %>%
    dplyr::select(
      Specialty,
      YearsNursing,
      ALS,
      HospitalType,
      YearsPSA,
      PSAFrequency,
      `Received education about sedation`,
      `Unit/hospital has a policy about sedation`,
      `Required to undergo competency assessment in sedation`,
      OverallKnowledge,
      OverallConfidence
    ) %>%
    gtsummary::tbl_summary(
      label =
        list(
          YearsPSA ~ "Years of experience with procedural sedation",
          YearsNursing ~ "Years of experience in nursing",
          OverallConfidence ~ "Rate your overall degree of confidence in identifying and treating sedation-related complications (0='No confidence' to 100='Very confident')",
          OverallKnowledge ~ "Rate your overall knowledge in identifying and treating sedation-related complications (0='No knowledge' to 100='Very knowledgable')"
        )
    ) %>%
    gtsummary::italicize_levels()),
  tar_target(
    resp_char_table,
    resp_change %>%
      dplyr::mutate(
        `Trained in advanced life support` =
          ifelse(ALS == 1, TRUE, FALSE)
      ) %>%
      dplyr::select(
        Specialty,
        yearsexp,
        yearspsa,
        PSA_frequency,
        `Trained in advanced life support`
      ) %>%
      gtsummary::tbl_summary(
        label =
          list(
            yearspsa ~ "Years of experience with procedural sedation",
            yearsexp ~ "Years of experience in nursing",
            PSA_frequency ~ "How frequently do you either administer, or monitor patients who have received, procedural sedation and analgesia (conscious sedation) in a typical working week?"
          )
      ) %>%
      gtsummary::italicize_levels()
  ),
  tarchetypes::tar_render(manuscript, "manuscript.Rmd"),
  tar_target(risk_dist, make_dist_plot(
    ncmscs_df, risk,
    xaxis = "Risk assessment subscale"
  )),
  tar_target(cardioresp_dist, make_dist_plot(
    ncmscs_df,
    cardioresp,
    xaxis = "Identifying and responding to cardiorespiratory complications subscale"
  )),
  tar_target(airway_dist, make_dist_plot(
    ncmscs_df, airway,
    xaxis = "Technical skills for airway interventions subscale"
  )),
  tar_target(ncmscs_total_dist, make_dist_plot(
    ncmscs_df, ncmscs_total,
    xaxis = "Total NC-MSCS scores"
  )),
  tar_target(
    comb_dist_plot,
    make_comb_dist_plot(
      risk_dist,
      cardioresp_dist,
      airway_dist,
      ncmscs_total_dist
    )
  ),
  tar_target(pre_risk_dist, make_dist_plot(
    resp_change, pre_risk,
    xaxis = "Risk assessment subscale"
  )),
  tar_target(pre_cardioresp_dist, make_dist_plot(
    resp_change,
    pre_cardioresp,
    xaxis = "Identifying and responding to cardiorespiratory complications subscale"
  )),
  tar_target(pre_airway_dist, make_dist_plot(
    resp_change, pre_airway,
    xaxis = "Technical skills for airway interventions subscale"
  )),
  tar_target(pre_ncmscs_total_dist, make_dist_plot(
    resp_change, pre_ncmscs_total,
    xaxis = "Total NC-MSCS scores"
  )),
  tar_target(
    pre_comb_dist_plot,
    make_comb_dist_plot(
      pre_risk_dist,
      pre_cardioresp_dist,
      pre_airway_dist,
      pre_ncmscs_total_dist
    )
  )
)