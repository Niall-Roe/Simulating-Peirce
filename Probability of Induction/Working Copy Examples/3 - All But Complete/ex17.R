library(shiny)
library(shinyjs)


#status: Nearly done. 

#To Do:
#1
#for the p slider:
# make the slider sticky for regular fractions, quarters, thirds and fifths. 

#end of instructions.

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
      .highlighted-row-50 { background-color: rgba(217, 30, 24, 0.15) !important; }
      .highlighted-row-90 { background-color: rgba(255, 127, 14, 0.15) !important; }
      .highlighted-row-99 { background-color: rgba(44, 160, 44, 0.15) !important; }
    "))
  ),

  div(class = "article-container",
      h3("Example 17: Peirce's Formula for Probable Error"),

      p(span(class = "example-trigger", id = "ex17-trigger",
             onclick = "Shiny.setInputValue('toggle_ex17', Math.random());",
             "It is found that, if the true proportion of white balls is ", em("p"), ", and ", em("s"), " balls are drawn, then the error of the proportion obtained by the induction will be—")
      ),

      div(id = "example-17", class = "example-container", style = "display: none;",
          div(
            h4("Interactive Demonstration: Peirce's Formula for Probable Error"),

            p("Peirce's formula tells us how accurate our induction will be. If the true proportion of white balls is ",
              strong("p"), " and we draw ", strong("s"), " balls, the error will be within certain bounds with known frequencies."),

            p("After drawing ", strong("s"), " balls, our estimate of ", strong("p"), " (called ",
              withMathJax("$$\\hat{p}$$"), ", equal to white balls sampled / number of samples) will be ",
              withMathJax("$$\\hat{p} \\pm e$$")),

            p("The value of ", em("e"), " is equal to: ", withMathJax("$$e = \\text{constant} \\times \\sqrt{\\frac{2p(1-p)}{s}}$$")),

            p(em("Try changing ", strong("s"), " to see how the distribution changes, or the confidence level to see how the interval width changes.")),

            fluidRow(
              column(4,
                sliderInput("ex17_p", "True proportion (p):",
                           min = 0.001, max = 0.999, value = 0.5, step = 0.001),
                sliderInput("ex17_s", "Number of balls drawn (s):",
                           min = 10, max = 1000, value = 100, step = 10),
                checkboxInput("ex17_rescale", "Rescale charts", FALSE),
                hr(),
                selectInput("ex17_confidence", "Select confidence level:",
                           choices = c("Custom" = "custom",
                                      "50% (half the time)" = "50",
                                      "90% (9 times out of 10)" = "90",
                                      "99% (99 times out of 100)" = "99",
                                      "99.9% (999 times out of 1,000)" = "99.9",
                                      "99.99% (9,999 times out of 10,000)" = "99.99",
                                      "99.99999999% (9,999,999,999 times out of 10,000,000,000)" = "99.99999999"),
                           selected = "90"),
                conditionalPanel(
                  condition = "input.ex17_confidence == 'custom'",
                  sliderInput("ex17_custom_conf", "Custom confidence level (%):",
                             min = 50, max = 100, value = 95, step = 1)
                )
              ),
              column(8,
                uiOutput("ex17_table")
              )
            ),

            hr(),

            p(strong("Test with Simulation:")),

            fluidRow(
              column(4,
                actionButton("ex17_simulate_single", "Draw Single Sample",
                            class = "btn-primary btn-block"),
                br(),
                actionButton("ex17_simulate_100", "Draw 100 Samples",
                            class = "btn-success btn-block"),
                br(),
                actionButton("ex17_reset", "Reset History",
                            class = "btn-danger btn-block")
              ),
              column(8,
                uiOutput("ex17_current_result")
              )
            ),

            br(),

            plotOutput("ex17_combined_plot", height = "400px"),

            hr(),

            p(strong("History of Last 100 Samples:")),

            p(em("The confidence interval either contains p or it doesn't — there is no probability about it once the interval is calculated. ",
                 "The confidence level (e.g., 90%) refers to the long-run frequency: if we repeated this sampling procedure many times, ",
                 "approximately 90% of the intervals would contain the true value of p.")),

            plotOutput("ex17_history_plot", height = "400px")
          )
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex17, { shinyjs::toggle("example-17") })

  # Store history of samples
  sample_history <- reactiveVal(data.frame())

  # Current sample data
  current_sample_data <- reactiveVal(NULL)

  # Example 17: Peirce's Formula for Probable Error
  output$ex17_table <- renderUI({
    p <- input$ex17_p
    s <- input$ex17_s
    selected_conf <- input$ex17_confidence

    # Calculate the standard error factor
    se_factor <- sqrt(2 * p * (1 - p) / s)

    # Peirce's constants and their corresponding confidence levels
    constants <- c(0.477, 1.163, 1.821, 2.328, 2.751, 4.77)
    confidence_levels <- c("50", "90", "99", "99.9", "99.99", "99.99999999")
    confidence_labels <- c("50% (half the time)",
                          "90% (9 times out of 10)",
                          "99% (99 times out of 100)",
                          "99.9% (999 times out of 1,000)",
                          "99.99% (9,999 times out of 10,000)",
                          "99.99999999% (9,999,999,999 times out of 10,000,000,000)")

    # Calculate error bounds
    error_bounds <- constants * se_factor

    # Create data frame
    df <- data.frame(
      "Confidence Level" = confidence_labels,
      "Peirce's Constant" = sprintf("%.3f", constants),
      "Error Bound" = sprintf("±%.4f", error_bounds),
      check.names = FALSE
    )

    # Add custom row if selected
    if(selected_conf == "custom") {
      custom_conf_val <- input$ex17_custom_conf
      if(!is.null(custom_conf_val)) {
        # Calculate custom constant using inverse normal CDF
        # z_alpha / sqrt(2) where z_alpha is the standard normal quantile
        custom_constant <- qnorm((1 + custom_conf_val/100) / 2) / sqrt(2)
        custom_bound <- custom_constant * se_factor
        custom_row <- data.frame(
          "Confidence Level" = paste0(custom_conf_val, "% (custom)"),
          "Peirce's Constant" = sprintf("%.3f", custom_constant),
          "Error Bound" = sprintf("±%.4f", custom_bound),
          check.names = FALSE
        )
        df <- rbind(df, custom_row)
        confidence_levels <- c(confidence_levels, "custom")
      }
    }

    # Determine which row to highlight
    row_class <- rep("", nrow(df))
    if(selected_conf == "50") row_class[1] <- "highlighted-row-50"
    if(selected_conf == "90") row_class[2] <- "highlighted-row-90"
    if(selected_conf == "99") row_class[3] <- "highlighted-row-99"
    if(selected_conf == "custom") row_class[nrow(df)] <- "highlighted-row-90"

    # Build HTML table with row highlighting
    table_html <- '<table class="table table-striped table-hover table-bordered">'
    table_html <- paste0(table_html, '<thead><tr>')
    table_html <- paste0(table_html, '<th>Confidence Level</th><th>Peirce\'s Constant</th><th>Error Bound</th>')
    table_html <- paste0(table_html, '</tr></thead><tbody>')

    for(i in 1:nrow(df)) {
      row_style <- if(row_class[i] != "") paste0(' class="', row_class[i], '"') else ''
      table_html <- paste0(table_html, '<tr', row_style, '>')
      table_html <- paste0(table_html, '<td>', df[i, 1], '</td>')
      table_html <- paste0(table_html, '<td>', df[i, 2], '</td>')
      table_html <- paste0(table_html, '<td>', df[i, 3], '</td>')
      table_html <- paste0(table_html, '</tr>')
    }

    table_html <- paste0(table_html, '</tbody></table>')
    HTML(table_html)
  })

  # Helper function to get constant based on confidence level
  get_peirce_constant <- function(conf_level, custom_val = NULL) {
    if(conf_level == "custom" && !is.null(custom_val)) {
      # z_alpha / sqrt(2) where z_alpha is the standard normal quantile
      return(qnorm((1 + custom_val/100) / 2) / sqrt(2))
    }
    constants_map <- list(
      "50" = 0.477,
      "90" = 1.163,
      "99" = 1.821,
      "99.9" = 2.328,
      "99.99" = 2.751,
      "99.99999999" = 4.77
    )
    return(constants_map[[conf_level]])
  }

  # Generate single sample
  observeEvent(input$ex17_simulate_single, {
    p <- input$ex17_p
    s <- input$ex17_s
    selected_conf <- input$ex17_confidence
    custom_val <- input$ex17_custom_conf

    # Draw sample
    white_balls <- rbinom(1, s, p)
    p_hat <- white_balls / s

    # Calculate SE using Peirce's formula
    se_factor <- sqrt(2 * p * (1 - p) / s)

    # Get Peirce's constant
    constant <- get_peirce_constant(selected_conf, custom_val)

    # Calculate margin of error
    margin <- constant * se_factor
    ci_lower <- p_hat - margin
    ci_upper <- p_hat + margin

    # Check if CI contains true p
    contains_p <- (p >= ci_lower & p <= ci_upper)

    # Store current sample
    current_sample_data(list(
      white_balls = white_balls,
      s = s,
      p_hat = p_hat,
      p = p,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      contains_p = contains_p,
      conf_level = if(selected_conf == "custom") custom_val else as.numeric(selected_conf)
    ))

    # Add to history
    new_row <- data.frame(
      id = ifelse(nrow(sample_history()) == 0, 1, max(sample_history()$id) + 1),
      p_hat = p_hat,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      contains_p = contains_p,
      p = p
    )
    sample_history(rbind(sample_history(), new_row))
  })

  # Generate 100 samples
  observeEvent(input$ex17_simulate_100, {
    p <- input$ex17_p
    s <- input$ex17_s
    selected_conf <- input$ex17_confidence
    custom_val <- input$ex17_custom_conf

    se_factor <- sqrt(2 * p * (1 - p) / s)
    constant <- get_peirce_constant(selected_conf, custom_val)

    # Generate 100 samples
    start_id <- ifelse(nrow(sample_history()) == 0, 1, max(sample_history()$id) + 1)

    new_samples <- lapply(1:100, function(i) {
      white_balls <- rbinom(1, s, p)
      p_hat <- white_balls / s
      margin <- constant * se_factor
      ci_lower <- p_hat - margin
      ci_upper <- p_hat + margin
      contains_p <- (p >= ci_lower & p <= ci_upper)

      data.frame(
        id = start_id + i - 1,
        p_hat = p_hat,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        contains_p = contains_p,
        p = p
      )
    })

    new_df <- do.call(rbind, new_samples)
    sample_history(rbind(sample_history(), new_df))

    # Update current sample to the last one
    last_sample <- new_df[nrow(new_df), ]
    white_balls <- round(last_sample$p_hat * s)
    current_sample_data(list(
      white_balls = white_balls,
      s = s,
      p_hat = last_sample$p_hat,
      p = p,
      ci_lower = last_sample$ci_lower,
      ci_upper = last_sample$ci_upper,
      contains_p = last_sample$contains_p,
      conf_level = if(selected_conf == "custom") custom_val else as.numeric(selected_conf)
    ))
  })

  # Reset history
  observeEvent(input$ex17_reset, {
    sample_history(data.frame())
    current_sample_data(NULL)
  })

  # Display current result
  output$ex17_current_result <- renderUI({
    req(current_sample_data())
    data <- current_sample_data()

    result_color <- if(data$contains_p) "#27ae60" else "#e74c3c"
    result_text <- if(data$contains_p) "✓ Contains p" else "✗ Does not contain p"

    # Calculate error margin
    margin <- (data$ci_upper - data$ci_lower) / 2

    div(
      style = "padding: 15px; background-color: #ecf0f1; border-radius: 5px;",
      h5("Most Recent Sample:"),
      p(tags$b("White balls drawn: "), data$white_balls, " out of ", data$s),
      p(tags$b("Observed ", withMathJax("$$\\hat{p}$$"), ": "), sprintf("%.4f", data$p_hat)),
      p(tags$b(paste0("Estimated true proportion (", data$conf_level, "% confidence): ")),
        withMathJax(sprintf("$$\\hat{p} = %.4f \\pm %.4f$$", data$p_hat, margin))),
      p(tags$b(paste0(data$conf_level, "% Confidence Interval: ")),
        sprintf("[%.4f, %.4f]", data$ci_lower, data$ci_upper)),
      p(tags$b("True p: "), sprintf("%.4f", data$p)),
      div(style = paste0("padding: 10px; margin-top: 10px; background-color:", result_color,
                        "; color: white; border-radius: 5px; text-align: center; font-weight: bold;"),
          result_text)
    )
  })

  # Combined plot (theoretical + current sample)
  output$ex17_combined_plot <- renderPlot({
    p <- input$ex17_p
    s <- input$ex17_s
    rescale <- input$ex17_rescale

    # Use exact binomial distribution (task 1)
    # Discrete support
    k <- 0:s
    phat <- k / s

    # Exact binomial distribution under true p
    probs <- dbinom(k, size = s, prob = p)

    # Standard error for proportion
    se <- sqrt(p * (1 - p) / s)
    delta <- 1 / s

    # Determine x-axis limits based on rescale option
    if (rescale && !is.null(input$ex17_rescale)) {
      # Zoom to ±5σ around true p
      xlim <- c(max(0, p - 5*se), min(1, p + 5*se))
    } else {
      xlim <- c(0, 1)
    }

    # If we have a current sample, show distribution and CI
    if(!is.null(current_sample_data())) {
      data <- current_sample_data()

      # Plot exact binomial distribution as bars
      plot(phat, probs,
           type = "h", lwd = 3,
           xlim = xlim,
           ylim = c(0, max(probs) * 1.3),
           xlab = expression(hat(p)),
           ylab = "Probability mass",
           main = "Sampling Distribution of p-hat",
           col = "gray60")

      points(phat, probs, pch = 16, cex = 1.2, col = "gray60")

      # Overlay normal approximation
      x_range <- seq(0, 1, length.out = 500)
      y_norm <- dnorm(x_range, mean = p, sd = se) * delta
      lines(x_range, y_norm, col = "#2c7fb8", lwd = 2, lty = 2)

      # Add vertical line at true p
      abline(v = p, col = "orange", lwd = 3, lty = 1)

      # Highlight the observed bar
      observed_prob <- dbinom(data$white_balls, size = s, prob = p)
      segments(data$p_hat, 0, data$p_hat, observed_prob, lwd = 5, col = "purple")
      points(data$p_hat, observed_prob, pch = 16, cex = 1.5, col = "purple")

      # Draw CI as a horizontal line segment
      ci_color <- if(data$contains_p) "#27ae60" else "#e74c3c"
      segments(data$ci_lower, 0, data$ci_upper, 0, col = ci_color, lwd = 5)
      points(data$p_hat, 0, pch = 19, col = "blue", cex = 2)

      legend("topright",
             legend = c("Exact binomial", "Normal approximation", "True p",
                       "Observed p-hat", paste0(data$conf_level, "% CI"),
                       if(data$contains_p) "CI contains p" else "CI misses p"),
             col = c("gray60", "#2c7fb8", "orange", "purple", ci_color, ci_color),
             lwd = c(3, 2, 3, 5, 5, NA),
             pch = c(NA, NA, NA, NA, NA, NA),
             lty = c(1, 2, 1, 1, 1, NA),
             cex = 0.75)
    } else {
      # Just show the binomial distribution without highlighting
      plot(phat, probs,
           type = "h", lwd = 3,
           xlim = xlim,
           ylim = c(0, max(probs) * 1.3),
           xlab = expression(hat(p)),
           ylab = "Probability mass",
           main = "Sampling Distribution of p-hat",
           col = "gray60")

      points(phat, probs, pch = 16, cex = 1.2, col = "gray60")

      # Overlay normal approximation
      x_range <- seq(0, 1, length.out = 500)
      y_norm <- dnorm(x_range, mean = p, sd = se) * delta
      lines(x_range, y_norm, col = "#2c7fb8", lwd = 2, lty = 2)

      # Add vertical line at true p
      abline(v = p, col = "orange", lwd = 3, lty = 1)

      text(0.5, max(probs) * 0.9, "Draw a sample to see\nthe observed outcome", cex = 1.2, col = "gray50")

      legend("topright",
             legend = c("Exact binomial", "Normal approximation", "True p"),
             col = c("gray60", "#2c7fb8", "orange"),
             lwd = c(3, 2, 3),
             lty = c(1, 2, 1),
             cex = 0.9)
    }
  })

  # History plot (horizontal version inspired by Better Confidence Intervals.R)
  output$ex17_history_plot <- renderPlot({
    if(nrow(sample_history()) == 0) {
      # Show empty plot with instructions
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
      text(1, 1, "Draw samples to see\nhistory accumulate here", cex = 1.5, col = "gray50")
    } else {
      df_plot <- tail(sample_history(), 100)
      rescale <- input$ex17_rescale

      # Determine y-axis limits based on rescale option
      if (!is.null(rescale) && rescale) {
        # Rescale to fit the data with some padding
        y_min <- min(df_plot$ci_lower, df_plot$p)
        y_max <- max(df_plot$ci_upper, df_plot$p)
        y_range <- y_max - y_min
        ylim <- c(y_min - 0.1*y_range, y_max + 0.1*y_range)
      } else {
        # Fixed y-axis from 0 to 1
        ylim <- c(0, 1)
      }

      # Create plot
      plot(NULL, xlim = c(0.5, nrow(df_plot) + 0.5),
           ylim = ylim,
           xlab = "Sample Number (most recent 100)",
           ylab = "Proportion",
           main = paste0("History of Confidence Intervals (",
                        sum(df_plot$contains_p), " out of ", nrow(df_plot),
                        " contain true p)"))

      # Add horizontal line at true p
      abline(h = df_plot$p[1], col = "orange", lwd = 2)

      # Draw each CI as a vertical line segment
      for(i in 1:nrow(df_plot)) {
        ci_color <- if(df_plot$contains_p[i]) "#27ae60" else "#e74c3c"
        segments(i, df_plot$ci_lower[i], i, df_plot$ci_upper[i],
                col = ci_color, lwd = 1.5)
        points(i, df_plot$p_hat[i], pch = 19, cex = 0.3, col = "blue")
      }

      legend("topright",
             legend = c("True p", "Contains p", "Misses p"),
             col = c("orange", "#27ae60", "#e74c3c"),
             lwd = c(2, 1.5, 1.5),
             cex = 0.9)
    }
  })
}

shinyApp(ui = ui, server = server)
