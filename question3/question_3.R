library(shiny)
library(tidyverse)

# Create dataframe of unique subject + SOC + severity combinations
# Ensures each subject counted once per severity level in each SOC
aesev_df <- pharmaverseadam::adae |>
  # convert severity to a factor to ensure logical ordering
  mutate(AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))) |>
  distinct(USUBJID, ACTARM, AESOC, AESEV)

# Derive treatment arm names from dataframe
tx_arms <- unique(aesev_df$ACTARM)

# User interface
ui <- fluidPage(
  titlePanel("AE Summary Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "actarm_filter",
        "Select Treatment Arm(s):",
        tx_arms,
        selected = tx_arms # all arms selected by default
      )
    ),
    mainPanel(
      # Increased height to ensure sufficient bar width per SOC
      plotOutput("plot", height = "600px")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive AE severity data; filters based on checkbox selection
  aesev_reactive <- reactive(
    aesev_df |>
      filter(ACTARM %in% input$actarm_filter)
  )

  # Create AE Severity bar plot to be rendered by plotOutput
  output$plot <- renderPlot({
    # Only renders plot if more than one treatment arm is selected
    req(input$actarm_filter)

    # Create AE severity bar plot
    ggplot(
      aesev_reactive(), # use reactive data
      aes(y = fct_rev(fct_infreq(AESOC)), fill = AESEV) # SOC ordered by total subjects
    ) +
      geom_bar(position = position_stack(reverse = TRUE)) + # reverse stack to make mild AE at bottom
      labs(
        title = "Unique Subjects per SOC and Severity Level",
        y = "System Organ Class",
        x = "Number of Unique Subjects",
        fill = "Severity"
      ) +
      scale_fill_brewer(palette = "Reds") + # better visualize severity with sequential color scale
      theme_minimal()
  })
}

# Run shiny app
shinyApp(ui, server)
