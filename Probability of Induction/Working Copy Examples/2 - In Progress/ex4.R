library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  withMathJax(),

  tags$head(
    tags$style(HTML("
      body { background-color: #f4f1ea; font-family: 'Georgia', serif; color: #2c2c2c; }
      .article-container {
        max-width: 1100px;
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
      .mode-tabs {
        display: flex;
        gap: 10px;
        margin-bottom: 20px;
      }
      .mode-tab {
        padding: 10px 20px;
        background-color: #e9ecef;
        border: 2px solid #dee2e6;
        border-radius: 6px;
        cursor: pointer;
        font-weight: 600;
        transition: all 0.2s;
      }
      .mode-tab.active {
        background-color: #2c7fb8;
        color: white;
        border-color: #2c7fb8;
      }
      .hl-antecedent { background-color: rgba(255, 243, 205, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-b { background-color: rgba(255, 182, 193, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-c { background-color: rgba(173, 216, 230, 0.6); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-both { background-color: rgba(147, 112, 219, 0.6); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .key-insight {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .independence-note {
        background-color: #d1ecf1;
        border-left: 4px solid #17a2b8;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .formula-box {
        text-align: center;
        font-size: 1.2em;
        margin: 20px 0;
        padding: 15px;
        background-color: #f0f0f0;
        border-radius: 6px;
        font-weight: bold;
      }
    "))
  ),

  div(class = "article-container",
      h3("Example 4: Special Rule for Independent Probabilities"),

      p(span(class = "example-trigger", id = "ex4-trigger",
             onclick = "Shiny.setInputValue('toggle_ex4', Math.random());",
             strong("Special Rule for the Multiplication of Independent Probabilities."), " — Given the separate probabilities of two consequences having the same antecedents, \"If A, then B,\" and \"If A, then C.\" Suppose that these consequences are such that the probability of the second is equal to the probability of the consequence, \"If both A and B, then C.\" Then the product of the two given numbers is equal to the probability of the consequence, \"If A, then both B and C.\"")
      ),

      div(id = "example-4", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Independent Probabilities"),

          p(strong("Independence"), " means that knowing B occurred doesn't change the probability of C:"),
          tags$ul(
            tags$li("P(A → C) = P(A∧B → C)  ", em("(C's probability is the same whether or not B occurred)")),
            tags$li("When this holds, events B and C are ", strong("independent")),
            tags$li("Then: P(A → B∧C) = P(A → B) × P(A → C)")
          ),

          div(class = "formula-box",
              "If P(A → C) = P(A∧B → C), then:",
              br(),
              "P(A → B∧C) = P(A → B) × P(A → C)"
          ),

          div(class = "independence-note",
              strong("Why does independence matter? "),
              "When B and C are independent, we can use the simpler formula above. When they're ",
              em("not"), " independent, we need the general multiplication rule from Example 3."
          ),

          hr(),

          # Mode toggle
          div(class = "mode-tabs",
              div(class = "mode-tab active", id = "theoretical-tab",
                  onclick = "Shiny.setInputValue('ex4_mode', 'theoretical', {priority: 'event'});",
                  "Theoretical Space"),
              div(class = "mode-tab", id = "empirical-tab",
                  onclick = "Shiny.setInputValue('ex4_mode', 'empirical', {priority: 'event'});",
                  "Long-Run Frequency")
          ),

          uiOutput("ex4_content")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex4, { shinyjs::toggle("example-4") })

  # Mode state
  current_mode <- reactiveVal("theoretical")

  observeEvent(input$ex4_mode, {
    current_mode(input$ex4_mode)

    # Update tab styling
    runjs("$('.mode-tab').removeClass('active');")
    if (input$ex4_mode == "theoretical") {
      runjs("$('#theoretical-tab').addClass('active');")
    } else {
      runjs("$('#empirical-tab').addClass('active');")
    }
  })

  output$ex4_content <- renderUI({
    mode <- current_mode()

    if (mode == "theoretical") {
      # Theoretical mode
      div(
        p(class = "key-insight",
          strong("Theoretical View: "),
          "With independent events, ",
          span(class = "hl-consequent-b", "B"),
          " and ",
          span(class = "hl-consequent-c", "C"),
          " occur independently. The ",
          span(class = "hl-both", "purple region"),
          " (both) is proportional to the product of their individual probabilities."
        ),

        fluidRow(
          column(4,
                 h5("Setup:"),
                 p("Roll two dice (independent events)."),
                 selectInput("ex4_die1_outcome",
                            span(class = "hl-consequent-b", "B: First die shows..."),
                            choices = c("6" = "6",
                                      "Even number" = "even",
                                      "Greater than 4" = "gt4")),
                 selectInput("ex4_die2_outcome",
                            span(class = "hl-consequent-c", "C: Second die shows..."),
                            choices = c("6" = "6",
                                      "Even number" = "even",
                                      "Greater than 4" = "gt4")),

                 hr(),
                 uiOutput("ex4_theoretical_calc")
          ),

          column(8,
                 plotOutput("ex4_theoretical_plot", height = "450px")
          )
        )
      )
    } else {
      # Empirical mode
      div(
        p(class = "key-insight",
          strong("Empirical View: "),
          "Run trials to verify that for independent events, P(Both) ≈ P(B) × P(C)."
        ),

        fluidRow(
          column(4,
                 h5("Setup:"),
                 p("Roll two dice (independent events)."),
                 selectInput("ex4_die1_outcome_emp",
                            span(class = "hl-consequent-b", "B: First die shows..."),
                            choices = c("6" = "6",
                                      "Even number" = "even",
                                      "Greater than 4" = "gt4")),
                 selectInput("ex4_die2_outcome_emp",
                            span(class = "hl-consequent-c", "C: Second die shows..."),
                            choices = c("6" = "6",
                                      "Even number" = "even",
                                      "Greater than 4" = "gt4")),

                 hr(),
                 sliderInput("ex4_n_trials", "Number of trials:",
                            min = 10, max = 10000, value = 100, step = 10),
                 actionButton("ex4_run_sim", "Run Simulation",
                             class = "btn-primary btn-lg",
                             style = "width: 100%;"),
                 hr(),
                 uiOutput("ex4_empirical_summary")
          ),

          column(8,
                 plotOutput("ex4_empirical_plot", height = "450px")
          )
        )
      )
    }
  })

  # Helper function to evaluate die outcome
  eval_die_outcome <- function(value, outcome_type) {
    if (outcome_type == "6") {
      return(value == 6)
    } else if (outcome_type == "even") {
      return(value %% 2 == 0)
    } else if (outcome_type == "gt4") {
      return(value > 4)
    }
    return(FALSE)
  }

  # Theoretical plot
  output$ex4_theoretical_plot <- renderPlot({
    req(input$ex4_die1_outcome, input$ex4_die2_outcome)

    outcome_b <- input$ex4_die1_outcome
    outcome_c <- input$ex4_die2_outcome

    par(mar = c(5, 5, 4, 2))
    plot(NULL, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5),
         xlab = "First Die", ylab = "Second Die",
         axes = FALSE, asp = 1,
         main = "Antecedent Space: All Possible Outcomes (Two Dice)")

    # Draw antecedent background
    rect(0.5, 0.5, 6.5, 6.5, col = rgb(1, 0.95, 0.8, alpha = 0.4), border = NA)

    # Draw grid
    for (die1 in 1:6) {
      for (die2 in 1:6) {
        b_true <- eval_die_outcome(die1, outcome_b)
        c_true <- eval_die_outcome(die2, outcome_c)

        col <- if (b_true && c_true) {
          rgb(0.58, 0.44, 0.86, alpha = 0.8)  # Both (purple)
        } else if (b_true) {
          rgb(1, 0.71, 0.76, alpha = 0.6)  # Only B (pink)
        } else if (c_true) {
          rgb(0.68, 0.85, 0.90, alpha = 0.6)  # Only C (blue)
        } else {
          "white"
        }

        rect(die1 - 0.45, die2 - 0.45, die1 + 0.45, die2 + 0.45,
             col = col, border = "black", lwd = 0.5)

        text(die1, die2, paste0("(", die1, ",", die2, ")"), cex = 0.6)
      }
    }

    # Axes
    axis(1, at = 1:6, labels = 1:6, tick = FALSE, line = -1)
    axis(2, at = 1:6, labels = 1:6, tick = FALSE, las = 1, line = -1)

    # Count outcomes
    count_b <- sum(sapply(1:6, function(d) eval_die_outcome(d, outcome_b)))
    count_c <- sum(sapply(1:6, function(d) eval_die_outcome(d, outcome_c)))
    count_both <- count_b * count_c  # Independent!

    text(3.5, -0.3,
         sprintf("Pink (B only): %d rows | Blue (C only): %d columns | Purple (Both): %d cells",
                count_b * 6 - count_both, count_c * 6 - count_both, count_both),
         cex = 0.85)
  })

  # Theoretical calculation
  output$ex4_theoretical_calc <- renderUI({
    req(input$ex4_die1_outcome, input$ex4_die2_outcome)

    outcome_b <- input$ex4_die1_outcome
    outcome_c <- input$ex4_die2_outcome

    # Count favorable outcomes
    count_b <- sum(sapply(1:6, function(d) eval_die_outcome(d, outcome_b)))
    count_c <- sum(sapply(1:6, function(d) eval_die_outcome(d, outcome_c)))

    p_b <- count_b / 6
    p_c <- count_c / 6
    p_both <- p_b * p_c

    div(
      h5("Independence Check:"),
      p("P(A → C) = ", strong(sprintf("%.4f", p_c))),
      p("P(A∧B → C) = ", strong(sprintf("%.4f", p_c)),
        br(), em("(same! dice are independent)")),
      hr(),
      h5("Calculation:"),
      p("P(A → B) = ", strong(sprintf("%d/6 = %.4f", count_b, p_b))),
      p("P(A → C) = ", strong(sprintf("%d/6 = %.4f", count_c, p_c))),
      hr(),
      p("P(A → B∧C) = ", strong(sprintf("%.4f", p_b)), " × ",
        strong(sprintf("%.4f", p_c)), " = ",
        strong(sprintf("%.4f", p_both))),
      p(em("(Equivalently: ", sprintf("%d/36", count_b * count_c), ")"))
    )
  })

  # Empirical simulation
  sim_results <- reactiveVal(NULL)

  observeEvent(input$ex4_run_sim, {
    req(input$ex4_die1_outcome_emp, input$ex4_die2_outcome_emp)

    n <- input$ex4_n_trials
    outcome_b <- input$ex4_die1_outcome_emp
    outcome_c <- input$ex4_die2_outcome_emp

    # Simulate dice rolls
    die1_rolls <- sample(1:6, n, replace = TRUE)
    die2_rolls <- sample(1:6, n, replace = TRUE)

    # Evaluate conditions
    b_occurs <- sapply(die1_rolls, function(d) eval_die_outcome(d, outcome_b))
    c_occurs <- sapply(die2_rolls, function(d) eval_die_outcome(d, outcome_c))
    both_occur <- b_occurs & c_occurs

    # Calculate frequencies
    freq_b <- cumsum(b_occurs) / seq_along(b_occurs)
    freq_c <- cumsum(c_occurs) / seq_along(c_occurs)
    freq_both <- cumsum(both_occur) / seq_along(both_occur)
    freq_product <- freq_b * freq_c  # What we expect for independence

    # Theoretical
    count_b <- sum(sapply(1:6, function(d) eval_die_outcome(d, outcome_b)))
    count_c <- sum(sapply(1:6, function(d) eval_die_outcome(d, outcome_c)))
    p_b_theory <- count_b / 6
    p_c_theory <- count_c / 6
    p_both_theory <- p_b_theory * p_c_theory

    sim_results(list(
      freq_b = freq_b,
      freq_c = freq_c,
      freq_both = freq_both,
      freq_product = freq_product,
      p_b_theory = p_b_theory,
      p_c_theory = p_c_theory,
      p_both_theory = p_both_theory,
      n_trials = n
    ))
  })

  output$ex4_empirical_plot <- renderPlot({
    results <- sim_results()

    if (is.null(results)) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Click 'Run Simulation' to begin")
      text(1, 1, "Awaiting simulation...", cex = 1.5, col = "gray")
    } else {
      par(mar = c(5, 5, 4, 2))
      plot(1:results$n_trials, results$freq_both, type = "l",
           col = "purple", lwd = 3,
           xlab = "Number of Trials", ylab = "Observed Frequency",
           main = "Independent Events: Long-Run Frequencies",
           ylim = c(0, max(1, max(c(results$freq_both, results$freq_b, results$freq_c)))))

      # Add P(B) × P(C) (expected for independence)
      lines(1:results$n_trials, results$freq_product, col = "orange", lwd = 2, lty = 2)

      # Add individual frequencies
      lines(1:results$n_trials, results$freq_b, col = rgb(1, 0.71, 0.76), lwd = 2)
      lines(1:results$n_trials, results$freq_c, col = rgb(0.68, 0.85, 0.90), lwd = 2)

      # Add theoretical lines
      abline(h = results$p_both_theory, col = "darkviolet", lwd = 2, lty = 3)

      grid(col = "gray", lty = "dotted")

      legend("topright",
             legend = c("Observed: Both B∧C",
                       "P(B) × P(C) (running product)",
                       "Observed: B",
                       "Observed: C",
                       sprintf("Theory: Both = %.4f", results$p_both_theory)),
             col = c("purple", "orange",
                    rgb(1, 0.71, 0.76), rgb(0.68, 0.85, 0.90),
                    "darkviolet"),
             lwd = c(3, 2, 2, 2, 2),
             lty = c(1, 2, 1, 1, 3),
             bg = "white",
             cex = 0.75)
    }
  })

  output$ex4_empirical_summary <- renderUI({
    results <- sim_results()

    if (is.null(results)) {
      return(p("Run a simulation to see results."))
    }

    final_freq_both <- results$freq_both[results$n_trials]
    final_freq_b <- results$freq_b[results$n_trials]
    final_freq_c <- results$freq_c[results$n_trials]
    product_observed <- final_freq_b * final_freq_c

    div(
      h5("Results:"),
      p("Trials: ", strong(results$n_trials)),
      p("Observed P(B): ", strong(sprintf("%.4f", final_freq_b))),
      p("Observed P(C): ", strong(sprintf("%.4f", final_freq_c))),
      p("Observed P(Both): ", strong(sprintf("%.4f", final_freq_both))),
      hr(),
      p("P(B) × P(C) = ", strong(sprintf("%.4f", product_observed))),
      p("Theory P(Both) = ", strong(sprintf("%.4f", results$p_both_theory))),
      p("Difference: ", strong(sprintf("%.4f", abs(final_freq_both - results$p_both_theory))))
    )
  })
}

shinyApp(ui = ui, server = server)
