library(shiny)
library(shinyjs)


ui <- fluidPage(
  useShinyjs(),
  withMathJax(),

  tags$head(
    tags$style(HTML("
      body { background-color: #f4f1ea; font-family: 'Georgia', serif; color: #2c2c2c; }
      .article-container {
        max-width: 900px;
        margin: 50px auto;
        padding: 60px;
        background-color: #ffffff;
        box-shadow: 0 10px 25px rgba(0,0,0,0.1);
        line-height: 1.8;
      }
      .example-trigger {
        background-color: #e8f4f8;
        border-left: 3px solid #2c7fb8;
        padding: 2px 6px;
        cursor: pointer;
        transition: all 0.2s;
        border-radius: 2px;
      }
      .example-trigger:hover {
        background-color: #b3d9ed;
        border-left-color: #1565c0;
      }
      .example-container {
        margin: 25px 0;
        padding: 25px;
        background-color: #f8f9fa;
        border: 2px solid #dee2e6;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .group-box {
        padding: 15px;
        margin: 10px 0;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        background-color: #ffffff;
      }
      .result-box {
        padding: 20px;
        margin-top: 20px;
        background-color: #e7f3ff;
        border: 2px solid #2c7fb8;
        border-radius: 8px;
      }
    "))
  ),

  div(class = "article-container",
      h3("Example 16: Real Difference or Chance?"),

      p("The use of this may be illustrated by an example. By the census of 1870, it appears that the proportion of males among native white children under one year old was 0.5082, while among colored children of the same age the proportion was only 0.4977. The difference is 0.0105, or about one in a 100. ",
        strong("Can this be attributed to chance?"),
        " ",
        span(class = "example-trigger", id = "ex16-trigger",
             onclick = "Shiny.setInputValue('toggle_ex16', Math.random());",
             "[Click to see interactive example]")
      ),

      div(id = "example-16", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Testing if a Difference is Real or Due to Chance"),

          p("This example demonstrates Peirce's method for determining whether an observed difference is ",
            strong("real (systematic)"), " or merely ", strong("due to chance"),
            ". The key question: Can the observed difference be attributed to random variation?"),

          p("Peirce's approach: Calculate the probable error for each group, then check whether the observed difference falls ",
            strong("within"), " or ", strong("beyond"), " the error intervals. If the difference falls beyond even the largest error interval (4.77e), ",
            "it indicates a real, systematic difference rather than chance variation."),

          p("Using the formula ", withMathJax("$$e = c \\times \\sqrt{\\frac{2p(1-p)}{s}}$$"),
            " (where ", em("c"), " depends on the confidence level) we calculate the probable error, then compare the observed difference against multiples of this error."),

          fluidRow(
            column(12,
              sliderInput("ex16_p_assumed", "Assumed true proportion (p) for calculation:",
                         min = 0.001, max = 0.999, value = 0.5, step = 0.01),
              p(em("Note: Peirce assumes p = 1/2 for this calculation, as it represents the natural proportion before any difference."))
            )
          ),

          hr(),

          fluidRow(
            column(6,
              div(class = "group-box",
                h5(strong("Group 1")),
                numericInput("ex16_p1", "Observed proportion (p₁):",
                            value = 0.5082, min = 0.001, max = 0.999, step = 0.0001),
                numericInput("ex16_n1", "Sample size (n₁):",
                            value = 1000000, min = 1, max = 10000000, step = 1)
              )
            ),
            column(6,
              div(class = "group-box",
                h5(strong("Group 2")),
                numericInput("ex16_p2", "Observed proportion (p₂):",
                            value = 0.4977, min = 0.001, max = 0.999, step = 0.0001),
                numericInput("ex16_n2", "Sample size (n₂):",
                            value = 150000, min = 1, max = 10000000, step = 1)
              )
            )
          ),

          hr(),

          fluidRow(
            column(3,
              actionButton("ex16_reset", "Reset to 1870 Census", class = "btn-primary btn-block")
            ),
            column(3,
              actionButton("ex16_chance", "Example: Due to Chance", class = "btn-warning btn-block")
            ),
            column(3,
              actionButton("ex16_tea", "Lady Tasting Tea", class = "btn-info btn-block")
            ),
            column(3,
              checkboxInput("ex16_rescale", "Rescale chart (zoom)", TRUE)
            )
          ),

          hr(),

          h5("Combined Comparison"),
          checkboxInput("ex16_show_errors", "Show probable error bounds", TRUE),
          conditionalPanel(
            condition = "input.ex16_show_errors == true",
            sliderInput("ex16_confidence", "Confidence level (%):",
                       min = 50, max = 100, value = 50, step = 1)
          ),
          plotOutput("ex16_combined_plot", height = "350px"),

          uiOutput("ex16_odds_display"),

          hr(),

          uiOutput("ex16_results")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex16, { shinyjs::toggle("example-16") })

  # Reset to 1870 census values (exact values from Peirce's text)
  observeEvent(input$ex16_reset, {
    updateNumericInput(session, "ex16_p1", value = 0.5082)
    updateNumericInput(session, "ex16_n1", value = 1000000)
    updateNumericInput(session, "ex16_p2", value = 0.4977)
    updateNumericInput(session, "ex16_n2", value = 150000)
    updateSliderInput(session, "ex16_p_assumed", value = 0.5)
  })

  # Set to a scenario that's likely due to chance
  observeEvent(input$ex16_chance, {
    updateNumericInput(session, "ex16_p1", value = 0.505)
    updateNumericInput(session, "ex16_n1", value = 5000)
    updateNumericInput(session, "ex16_p2", value = 0.495)
    updateNumericInput(session, "ex16_n2", value = 5000)
    updateSliderInput(session, "ex16_p_assumed", value = 0.5)
  })

  # Set to Lady Tasting Tea example
  # In Fisher's experiment, the lady correctly identified 8 out of 8 cups
  # Group 1: Random guessing (p = 0.5)
  # Group 2: The lady's performance (7/8 correct = 0.875)
  # With only 8 trials each, we ask: is her performance real skill or luck?
  observeEvent(input$ex16_tea, {
    updateNumericInput(session, "ex16_p1", value = 0.5)      # Random guessing
    updateNumericInput(session, "ex16_n1", value = 8)        # 8 cups
    updateNumericInput(session, "ex16_p2", value = 0.875)    # Lady got 7/8 correct
    updateNumericInput(session, "ex16_n2", value = 8)        # Fisher's 8 cups
    updateSliderInput(session, "ex16_p_assumed", value = 0.5)  # Null: just guessing
  })

  # Calculate probable errors and significance
  calculations <- reactive({
    p1_obs <- input$ex16_p1
    n1 <- input$ex16_n1
    p2_obs <- input$ex16_p2
    n2 <- input$ex16_n2
    p_assumed <- input$ex16_p_assumed

    # Get confidence level from slider (default to 50% if not set)
    confidence <- if (!is.null(input$ex16_confidence)) input$ex16_confidence else 50

    # Calculate constant based on confidence level
    # For 50%, Peirce uses 0.477 (corresponds to ~0.674 standard errors)
    # We use the normal distribution to get the multiplier for any confidence level
    constant <- qnorm(0.5 + confidence/200)

    # Calculate probable error for each group
    # e = 0.477 × sqrt(2p(1-p)/s)
    se_factor1 <- sqrt(2 * p_assumed * (1 - p_assumed) / n1)
    se_factor2 <- sqrt(2 * p_assumed * (1 - p_assumed) / n2)

    prob_error1 <- constant * se_factor1
    prob_error2 <- constant * se_factor2

    # Observed difference
    diff_obs <- abs(p1_obs - p2_obs)

    # Sum of probable errors
    sum_errors <- prob_error1 + prob_error2

    # How many times the sum is the difference?
    ratio <- diff_obs / sum_errors

    # Standard errors for plotting
    se1 <- sqrt(p_assumed * (1 - p_assumed) / n1)
    se2 <- sqrt(p_assumed * (1 - p_assumed) / n2)

    # Combined standard error for difference
    se_diff <- sqrt(se1^2 + se2^2)

    # Z-score for the difference
    z_score <- diff_obs / se_diff

    # Probability of observing this difference by chance (two-tailed)
    p_value <- 2 * pnorm(-abs(z_score))

    # Odds (1 in X)
    if (p_value > 0) {
      odds <- 1 / p_value
    } else {
      odds <- Inf
    }

    list(
      p1_obs = p1_obs,
      p2_obs = p2_obs,
      n1 = n1,
      n2 = n2,
      prob_error1 = prob_error1,
      prob_error2 = prob_error2,
      diff_obs = diff_obs,
      sum_errors = sum_errors,
      ratio = ratio,
      se1 = se1,
      se2 = se2,
      se_diff = se_diff,
      z_score = z_score,
      p_value = p_value,
      odds = odds,
      p_assumed = p_assumed,
      constant = constant,
      confidence = confidence
    )
  })

  # Results display
  output$ex16_results <- renderUI({
    calc <- calculations()

    div(class = "result-box",
        h4("Analysis Results"),

        h5("Step 1: Calculate Probable Errors"),
        p("For Group 1 with n = ", format(calc$n1, big.mark = ","), ":"),
        p(withMathJax(sprintf("$$e_1 = %.3f \\times \\sqrt{\\frac{2 \\times %.3f \\times %.3f}{%d}} = %.4f$$",
                             calc$constant, calc$p_assumed, 1 - calc$p_assumed, calc$n1, calc$prob_error1))),

        p("For Group 2 with n = ", format(calc$n2, big.mark = ","), ":"),
        p(withMathJax(sprintf("$$e_2 = %.3f \\times \\sqrt{\\frac{2 \\times %.3f \\times %.3f}{%d}} = %.4f$$",
                             calc$constant, calc$p_assumed, 1 - calc$p_assumed, calc$n2, calc$prob_error2))),

        hr(),

        h5("Step 2: Test Against Error Intervals"),
        p(strong("Observed difference: "), sprintf("%.4f (about 1 in %.0f)", calc$diff_obs, 1/calc$diff_obs)),
        p(strong("Sum of probable errors (e₁ + e₂): "), sprintf("%.4f", calc$sum_errors)),
        p(strong("Multiple of error: "), sprintf("The observed difference is %.1f × (e₁ + e₂)", calc$ratio)),

        p("Does the difference fall within or beyond the error intervals?"),
        tags$ul(
          tags$li(sprintf("Within 0.477e: Would occur ~50%% of the time by chance")),
          tags$li(sprintf("Within 1.163e: Would occur ~90%% of the time by chance")),
          tags$li(sprintf("Within 1.821e: Would occur ~99%% of the time by chance")),
          tags$li(sprintf("Beyond 4.77e: Would occur < 0.00000001%% of the time by chance"))
        ),

        if (calc$ratio > 4.77) {
          div(
            p(style = "font-weight: bold; color: #d9534f;",
              sprintf("The difference (%.1f × error) falls BEYOND even the 4.77e interval!", calc$ratio)),
            p(style = "font-weight: bold; color: #d9534f;",
              sprintf("Such a result would be expected to occur %.6f%% of the time by chance.", calc$p_value * 100))
          )
        } else if (calc$ratio > 1.821) {
          p(style = "font-weight: bold; color: #f0ad4e;",
            sprintf("The difference (%.1f × error) falls beyond the 99%% interval.", calc$ratio))
        } else {
          p(style = "font-weight: bold; color: #5bc0de;",
            sprintf("The difference (%.1f × error) falls within common error intervals.", calc$ratio))
        },

        hr(),

        h5("Step 3: Statistical Assessment"),
        p(strong("Z-score: "), sprintf("%.2f", calc$z_score)),

        hr(),

        h5("Conclusion: Real or Chance?"),
        if (calc$ratio >= 4.77) {
          div(style = "padding: 15px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 5px;",
              p(strong("The difference is REAL (systematic), not due to chance."),
                " The observed difference falls beyond even the largest error interval (4.77e). ",
                "If it were due to chance, it would (with high probability) have been included in the interval. ",
                "This is a reliable probe: we can argue that the error of ruling out chance is absent."))
        } else if (calc$ratio >= 2.5) {
          div(style = "padding: 15px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px;",
              p(strong("The difference is likely REAL, though not as certain."),
                " The observed difference exceeds several error intervals, suggesting a systematic effect rather than pure chance."))
        } else {
          div(style = "padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px;",
              p(strong("The difference may be attributed to CHANCE."),
                " The observed discrepancy falls within expected random variation. ",
                "We cannot reliably rule out that this is merely chance fluctuation."))
        }
    )
  })


  # Odds display right under the plot
  output$ex16_odds_display <- renderUI({
    calc <- calculations()

    div(style = "padding: 15px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px; margin-top: 15px;",
        p(strong("Long-run frequency: "),
          if (calc$odds < Inf) {
            if (calc$odds > 1e9) {
              sprintf("Such a result would happen only once out of %s censuses, in the long run, something we would expect to occur by chance only %.6f%% of the time.",
                     format(calc$odds, scientific = TRUE, digits = 2), calc$p_value * 100)
            } else {
              sprintf("Such a result would happen only once out of %s censuses, in the long run, something we would expect to occur by chance only %.6f%% of the time.",
                     format(round(calc$odds), big.mark = ","), calc$p_value * 100)
            }
          } else {
            "Such a result would essentially never happen by chance alone."
          }
        )
    )
  })

  # Combined comparison plot
  output$ex16_combined_plot <- renderPlot({
    calc <- calculations()
    rescale <- input$ex16_rescale
    show_errors <- input$ex16_show_errors

    # Determine x-axis limits
    if (!is.null(rescale) && rescale) {
      # Zoom to the region around both distributions
      x_min <- min(calc$p1_obs - 3*calc$se1, calc$p2_obs - 3*calc$se2)
      x_max <- max(calc$p1_obs + 3*calc$se1, calc$p2_obs + 3*calc$se2)
      x_range <- seq(x_min, x_max, length.out = 500)
    } else {
      # Full 0-1 range
      x_range <- seq(0, 1, length.out = 500)
    }

    y1 <- dnorm(x_range, mean = calc$p1_obs, sd = calc$se1)
    y2 <- dnorm(x_range, mean = calc$p2_obs, sd = calc$se2)

    plot(x_range, y1, type = "l", lwd = 2, col = "#2c7fb8",
         xlab = "Proportion",
         ylab = "Density",
         main = "Comparison of Both Groups",
         ylim = c(0, max(y1, y2) * 1.1))

    lines(x_range, y2, lwd = 2, col = "#d95f02")

    # Show probable error bounds as shaded regions if toggled
    if (!is.null(show_errors) && show_errors) {
      # Group 1 probable error bounds - create shaded region
      error_region_1 <- x_range >= (calc$p1_obs - calc$prob_error1) & x_range <= (calc$p1_obs + calc$prob_error1)
      if (sum(error_region_1) > 0) {
        x_shade_1 <- c(x_range[error_region_1], rev(x_range[error_region_1]))
        y_shade_1 <- c(rep(0, sum(error_region_1)), rev(y1[error_region_1]))
        polygon(x_shade_1, y_shade_1, col = rgb(0.17, 0.50, 0.72, 0.2), border = NA)
      }

      # Group 2 probable error bounds - create shaded region
      error_region_2 <- x_range >= (calc$p2_obs - calc$prob_error2) & x_range <= (calc$p2_obs + calc$prob_error2)
      if (sum(error_region_2) > 0) {
        x_shade_2 <- c(x_range[error_region_2], rev(x_range[error_region_2]))
        y_shade_2 <- c(rep(0, sum(error_region_2)), rev(y2[error_region_2]))
        polygon(x_shade_2, y_shade_2, col = rgb(0.85, 0.37, 0.01, 0.2), border = NA)
      }
    }

    # Mark observed values
    abline(v = calc$p1_obs, col = "#2c7fb8", lwd = 2, lty = 2)
    abline(v = calc$p2_obs, col = "#d95f02", lwd = 2, lty = 2)

    # Show difference with arrow
    y_arrow <- max(y1, y2) * 0.5
    arrows(calc$p2_obs, y_arrow,
           calc$p1_obs, y_arrow,
           code = 3, angle = 20, length = 0.15, lwd = 2, col = "darkred")

    text((calc$p1_obs + calc$p2_obs) / 2, y_arrow * 1.1,
         sprintf("Difference: %.4f", calc$diff_obs),
         col = "darkred", cex = 0.9, font = 2)

    # Build legend
    if (!is.null(show_errors) && show_errors) {
      legend("topright",
             legend = c("Group 1", "Group 2", "Probable error bounds"),
             col = c("#2c7fb8", "#d95f02", "gray40"),
             lwd = c(2, 2, 1),
             lty = c(1, 1, 3),
             cex = 0.9)
    } else {
      legend("topright",
             legend = c("Group 1", "Group 2"),
             col = c("#2c7fb8", "#d95f02"),
             lwd = 2,
             cex = 0.9)
    }
  })
}

shinyApp(ui = ui, server = server)
