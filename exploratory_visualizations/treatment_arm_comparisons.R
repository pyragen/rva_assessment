library(tidyverse)
library(patchwork)

### Risk Statistic Calculations ####
#' Calculate Relative Risk Statistics for Adverse Events
#'
#' @description
#' This function takes ADSL and ADAE datasets to calculate event counts, 
#' denominators, risk differences, and relative risk ratios for each System
#' Organ Class (SOC), stratified by ACTARM. It also performs a Fisher's Exact 
#' Test for each SOC.
#'
#' @param adsl An ADaM Subject-Level Analysis data frame. Data must follow
#' structure outlined in ADaM Implementation Guide v1.3
#' @param adae An ADaM Adverse Event Analysis data frame. Data must follow
#' structure outlined in ADaM OCCDS Implementation Guide v1.1
#' @param arms A character vector of length 2 specifying the arms to compare. 
#'   The first element is treated as the control/reference arm.
#'
#' @return A data frame (tibble) with one row per \code{AESOC}, containing:
#' \itemize{
#'   \item \code{events_cntrl/treat}: Number of subjects with at least one event for each arm (n)
#'   \item \code{total_cntrl/treat}: Denominator (total N) for each arm.
#'   \item \code{risk_cntrl/treat}: Incidence rate (n/N).
#'   \item \code{rr, lower_ci, upper_ci}: Relative Risk and 95\% Confidence Intervals.
#'   \item \code{p_val}: P-value from Fisher's Exact Test.
#'   \item \code{neg_log10_p}: Negative log10 of the p-value (for plotting)
get_ae_risk_stats <- function(adsl, adae, arms){
  # Verify ADaM compliance using ADSL and ADAE key variables
  adsl_req_vars <- c("STUDYID", "USUBJID", "SUBJID")
  adae_req_vars <- c("STUDYID", "USUBJID", "AETERM", "AESEQ")
  
  if (!all(adsl_req_vars %in% names(adsl))) {
    stop("ADSL data does not follow ADaM standards.")
  }
  if (!all(adae_req_vars %in% names(adae))) {
    stop("ADAE data does not follow ADaM standards.")
  }
  
  # Count total unique subjects in selected treatment arms
  total_subjects <- adsl |>
    filter(ACTARM %in% arms) |>
    distinct(USUBJID, ACTARM) |>
    group_by(ACTARM) |>
    summarize(
      total = n()
    )
  
  # Verify that two valid arms were found in ACTARM column
  if (nrow(total_subjects) < 2){
    stop(paste("One or both values in 'arms' is missing from ACTARM"))
  }
  
  # Count unique AE in each treatment arm and each AESOC
  adverse_events <- adae |>
    filter(TRTEMFL == "Y", ACTARM %in% arms) |>
    distinct(USUBJID, ACTARM, AESOC) |>
    group_by(ACTARM, AESOC) |>
    summarize(
      events = n(),
      .groups = "drop"
    )
  
  # Create wider dataframe from AE counts that will be used to store AE risk data
  risk_data <- adverse_events |>
    pivot_wider(
      names_from = ACTARM,
      values_from = events,
      values_fill = 0
    ) |>
    rename( 
      # rename columns to be consistent with other calculated values
      events_cntrl = !!arms[1],
      events_treat = !!arms[2]
    ) |>
    mutate(
      # pull value from total_subject dataframe
      total_cntrl = total_subjects$total[total_subjects$ACTARM == arms[1]],
      total_treat = total_subjects$total[total_subjects$ACTARM == arms[2]],
      
      # calculate risk (incidence) percentage
      risk_treat = events_treat / total_treat,
      risk_cntrl = events_cntrl / total_cntrl,
      rd = risk_treat - risk_cntrl # risk difference
    )
  
  # Calculate remaining complex risk statistics
  risk_data <- risk_data |>
    filter(risk_treat >= 0.02 & risk_cntrl >= 0.02) |> # filter low incidence AE in any arm (likely outlier AE)
    mutate(
      # calculate relative risk and confidence interval
      stats = calc_rr_stats(events_treat, total_treat, events_cntrl, total_cntrl)
    ) |>
    unpack(stats) |>
    rowwise() |> # rowwise is necessary as fisher.test takes matrix input
    mutate(
      # calculate p-val for Fisher's exact test
      p_val = fisher.test(matrix(c(
        events_treat, (total_treat - events_treat),
        events_cntrl, (total_cntrl - events_cntrl)
      ), nrow = 2))$p.value,
      neg_log10_p = -log10(p_val)
    ) |>
    ungroup()
  
  return(risk_data)
}

#' Calculate Relative Risk Statistics (Internal)
#'
#' @description
#' An internal helper to compute RR and CIs. Called by \code{get_ae_risk_stats}.
#'
#' @keywords internal
calc_rr_stats <- function(a, n1, c, n2){
  pct1 <- a / n1
  pct2 <- c / n2
  rr <- pct1 / pct2
  
  # Standard error of ln (RR)
  se_log_rr <- sqrt((1/a) - 1/n1 + 1/c - 1/n2)
  
  return(data.frame(
    rr,
    lower_ci = exp(log(rr) - 1.96 * se_log_rr),
    upper_ci = exp(log(rr) + 1.96 * se_log_rr)
  ))
}

arms <- c("Placebo", "Xanomeline High Dose")
plot_data <- get_ae_risk_stats(pharmaverseadam::adsl, pharmaverseadam::adae, arms)

### Volcano plot ####
volcano_range <- 1.1 * max(abs(plot_data$rd))
high_rd <- plot_data |>
  filter(abs(rd) > 0.2)

# Volcano plot to compare risk differences between two arms
volcano_plot <- ggplot(plot_data, aes(x = rd, y = neg_log10_p)) +
  # size based on AE in both arms, color based on risk difference, transparency based on p-value
  geom_point(aes(size = events_treat + events_cntrl, color = rd >= 0, alpha = neg_log10_p)) +
  geom_text(
    # add labels for AESOC where risk difference is large in magnitude
    data = high_rd, 
    aes(label = AESOC),
    nudge_y = -0.15,
    hjust = 1
    ) +
  scale_alpha_continuous(range = c (0.25, 0.9)) +
  scale_color_manual(
    values = c("blue", "red"), # blue if risk difference < 0, red if risk difference > 0
    labels = c(paste("Increased risk in", arms[1]), paste("Increased risk in", arms[2])),
    name = NULL
  ) + 
  coord_cartesian(xlim = c(-volcano_range, volcano_range)) + # no risk difference centered on plot
  guides(alpha = "none", size = "none") +
  labs(
    x = "Risk Difference",
    y = bquote(-Log[10] ~ "(Fishers' Exact P-value)")
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

# Save volcano plot
ggsave("exploratory_visualizations/volcano_plot.png", width = 16, height = 9) 

### Dot plot ####
# Dot plot showing incidence rates in both arms, ordered by relative risk
plot1 <- ggplot(plot_data, aes(y = reorder(AESOC, rr))) +
  # shape and color of dots different for each arm
  geom_point(aes(x = risk_cntrl*100, color = arms[1]), shape = 16, size = 3) +
  geom_point(aes(x = risk_treat*100, color = arms[2]), shape = 18, size = 3) +
  scale_color_manual(
    name = NULL, 
    values = setNames(c("blue", "red"), arms)
  ) +
  labs(
    x = "Percentage",
    y = NULL
  ) +
  theme_classic() + 
  theme(
    panel.grid.major.y = element_line(linetype = "dotted", color = "grey50", linewidth = 0.8),
    legend.position = "bottom"
  )



# Relative risk plot with 95% CI, ordered by relative risk
plot2 <- ggplot(plot_data, aes(y = reorder(AESOC, rr))) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") + 
  geom_point(aes(x = rr), size = 3) + 
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.25) + 
  scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 6, 20)) + # use log scale for relative risk 
  labs(
    x = "Relative Risk",
    y = NULL
  ) + 
  theme_classic() + 
  theme(
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank())

# Table, which aligns with plot 1 and plot 2, showing number of AE in each arm
plot3 <- ggplot(plot_data, aes(y = reorder(AESOC, rr))) + 
  geom_text(aes(x = 1, label = events_cntrl), size = 3.5) +
  geom_text(aes(x = 2, label = events_treat), size = 3.5) + 
  # Label each arm and increase x limits to prevent table from being cut off
  scale_x_continuous(breaks = c(1, 2), labels = str_wrap(arms, width = 10), limits = c(0.5, 2.5), position = "top") +
  labs(x = NULL, y = NULL) + 
  theme_void() + 
  theme(
    axis.text.x.top = element_text(face = "bold")
  )

# Save combined dot plot, relative risk plot and table showing AE counts
dot_rr_plot <- plot1 + plot2 + plot3 + plot_layout(widths = c(1, 1, 0.5))
ggsave("exploratory_visualizations/dot_rr_plot.png", width = 16, height = 9)
    

