library(tidyverse)
library(gtsummary)

#' Create a regulatory-compliant TEAE Summary Table
#'
#' @description
#' Create a nested System Organ Class / Preferred Term summary table for
#' Treatment Emergent Adverse Effects. It uses {gtsummary} to produce a
#' hierarchical layout with counts and percentages of all adverse events
#' based on treatment arm population.
#'
#' @param adsl The ADaM Subject-Level Analysis data.frame. Data must follow
#' structure outlined in ADaM Implementation Guide v1.3
#' @param adae The ADaM Adverse Event Analysis data.frame. Data must follow
#' structure outlined in ADaM OCCDS Implementation Guide v1.1
#'
#' @returns TEAE Summary Table
#'
#' @examples
#' create_teae_table(pharmaverseadam::adsl, pharmaverseadam::adae)
create_teae_table <- function(adsl, adae) {
  # Verify ADaM compliance using ADSL and ADAE key variables
  adsl_req_vars <- c("STUDYID", "USUBJID", "SUBJID")
  adae_req_vars <- c("STUDYID", "USUBJID", "AETERM", "AESEQ")

  if (!all(adsl_req_vars %in% names(adsl))) {
    stop("ADSL data does not follow ADaM standards.")
  }

  if (!all(adae_req_vars %in% names(adae))) {
    stop("ADAE data does not follow ADaM standards.")
  }

  # Create dataframe of unique subjects in treatment arms (excluding Screen Failure)
  unique_subjects <- adsl |>
    filter(ACTARM != "Screen Failure") |>
    distinct(USUBJID, ACTARM)

  # Create dataframe of unique subject + treatment emergent adverse event combinations
  # Necessary to find unique combinations as some patients have multiple of
  # same TEAE but at different dates or with different outcomes
  teae_df <- adae |>
    filter(TRTEMFL == "Y") |>
    distinct(USUBJID, ACTARM, AESOC, AEDECOD)

  # Create hierarchical summary table to nest AESOC within AEDECOD
  teae_tbl <- teae_df |>
    tbl_hierarchical(
      variables = c(AESOC, AEDECOD),
      id = USUBJID,
      by = ACTARM,
      denominator = unique_subjects, # use unique subject from ADSL for percentage calculations
      overall_row = TRUE, # adds total "Treatment Emergent Adverse Events" row
      label = list(..ard_hierarchical_overall.. = "Treatment Emergent Adverse Events")
    ) |>
    modify_header(label = "**System Organ Class / Preferred Term**")

  return(teae_tbl)
}

tbl <- create_teae_table(pharmaverseadam::adsl, pharmaverseadam::adae)

# Save summary table as html
tbl |>
  as_gt() |>
  gt::gtsave(filename = "question1/output_q1.html")
