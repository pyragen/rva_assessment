library(tidyverse)

#' Create a bar chart visualizing distribution of adverse events
#'
#' @description
#' Create a stack bar chart showing the number of unique subjects per
#' System Organ Class and AE Severity. Uses {ggplot2} to create visualization.
#'
#' @param adae The ADaM Adverse Event Analysis data.frame. Data must follow
#' structure outlined in ADaM OCCDS Implementation Guide v1.1
#'
#' @returns ggplot2 visualization showing unique subjects per SOC and AE Severity
#'
#' @examples
#' aesev_barplot(pharmaverseadam::adae)
aesev_barplot <- function(adae) {
  # Verify that ADAE compliant with ADaM structure
  adae_req_vars <- c("STUDYID", "USUBJID", "AETERM", "AESEQ")

  if (!all(adae_req_vars %in% names(adae))) {
    stop("ADAE data does not follow ADaM standards.")
  }

  # Create dataframe of unique subject + SOC + severity combinations
  # Ensures each subject counted once per severity level in each SOC
  aesev_df <- adae |>
    # convert severity to a factor to ensure logical ordering
    mutate(AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))) |>
    distinct(USUBJID, AESOC, AESEV)

  # Create AE Severity bar plot
  p <- ggplot(
    aesev_df,
    aes(y = fct_rev(fct_infreq(AESOC)), fill = AESEV) # SOC ordered by total subjects
  ) +
    geom_bar(position = position_stack(reverse = TRUE)) + # reverse stack to make mild severity at bottom
    labs(
      title = "Unique Subjects per SOC and Severity Level",
      y = "System Organ Class",
      x = "Number of Unique Subjects",
      fill = "Severity"
    ) +
    scale_fill_brewer(palette = "Reds") + # better visualize severity with sequential color scale
    theme_minimal()

  return(p)
}

aesev_barplot(pharmaverseadam::adae)
ggsave("question2/output_q2.png", width = 16, height = 9) # save AE severity bar plot as png
