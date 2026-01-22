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
      .preset-button {
        margin: 5px;
        padding: 8px 15px;
        background-color: #e9ecef;
        border: 2px solid #dee2e6;
        border-radius: 5px;
        cursor: pointer;
        transition: all 0.2s;
        display: inline-block;
      }
      .preset-button:hover {
        background-color: #2c7fb8;
        color: white;
        border-color: #2c7fb8;
      }
      .hl-antecedent { background-color: rgba(255, 243, 205, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent { background-color: rgba(144, 238, 144, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-a { background-color: rgba(255, 182, 193, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-b { background-color: rgba(173, 216, 230, 0.6); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-both { background-color: rgba(147, 112, 219, 0.6); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .key-insight {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .formula-box {
        text-align: center;
        font-size: 1.1em;
        margin: 15px 0;
        padding: 12px;
        background-color: #f0f0f0;
        border-radius: 6px;
        font-weight: bold;
      }
    "))
  ),

  div(class = "article-container",
      h3("Example 5: Dice Problems"),

      p("In this way all problems about dice, etc., may be solved. ",
        span(class = "example-trigger", id = "ex5-trigger",
             onclick = "Shiny.setInputValue('toggle_ex5', Math.random());",
             "[Click to see interactive example]")
      ),

      div(id = "example-5", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Working with Dice Probabilities"),

          p("Explore classic dice probability problems using the rules we've learned. Select a preset or customize your own scenario."),

          div(class = "key-insight",
              strong("Important Note: "),
              "The problems we are solving here are theoretical ones. The answers to those problems give us expectations about empirical long-run frequencies, assuming our theoretical assumptions hold. But we solve the problems of dice with the mathematics, not by rolling them 10,000 times—just like we solve geometry problems with trigonometry, not a protractor."
          ),

          # Preset buttons
          div(style = "margin: 20px 0; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
              h5("Peirce's Examples:"),
              div(
                actionButton("preset_single_six", "Single die shows 6", class = "btn-sm"),
                actionButton("preset_double_sixes", "Both dice show 6", class = "btn-sm"),
                actionButton("preset_deuce_ace", "Deuce-Ace (2 & 1)", class = "btn-sm")
              ),
              h5("Additional Examples:", style = "margin-top: 15px;"),
              div(
                actionButton("preset_sum_seven", "Dice sum to 7", class = "btn-sm"),
                actionButton("preset_at_least_one_six", "At least one 6", class = "btn-sm"),
                actionButton("preset_both_even", "Both dice even", class = "btn-sm"),
                actionButton("preset_doubles", "Any doubles", class = "btn-sm")
              )
          ),

          hr(),

          # Mode toggle
          div(class = "mode-tabs",
              div(class = "mode-tab active", id = "theoretical-tab",
                  onclick = "Shiny.setInputValue('ex5_mode', 'theoretical', {priority: 'event'});",
                  "Theoretical Space"),
              div(class = "mode-tab", id = "empirical-tab",
                  onclick = "Shiny.setInputValue('ex5_mode', 'empirical', {priority: 'event'});",
                  "Long-Run Frequency")
          ),

          uiOutput("ex5_content")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex5, { shinyjs::toggle("example-5") })

  # Mode state
  current_mode <- reactiveVal("theoretical")
  current_preset <- reactiveVal(list(
    name = "Single die shows 6",
    rule = "Basic Probability",
    rule_formula = "P(A → B) = (# times A and B occur) / (# times A occurs)",
    n_dice = 1,
    type = "single",
    target = 6,
    formula = "P(die = 6) = 1/6",
    explanation = "One outcome (6) out of six equally likely outcomes."
  ))

  observeEvent(input$ex5_mode, {
    current_mode(input$ex5_mode)
    runjs("$('.mode-tab').removeClass('active');")
    if (input$ex5_mode == "theoretical") {
      runjs("$('#theoretical-tab').addClass('active');")
    } else {
      runjs("$('#empirical-tab').addClass('active');")
    }
  })

  # Preset configurations
  observeEvent(input$preset_single_six, {
    current_preset(list(
      name = "Single die shows 6",
      rule = "Basic Probability",
      rule_formula = "P(A → B) = (# times A and B occur) / (# times A occurs)",
      n_dice = 1,
      type = "single",
      target = 6,
      formula = "P(die = 6) = 1/6",
      explanation = "One outcome (6) out of six equally likely outcomes."
    ))
  })

  observeEvent(input$preset_double_sixes, {
    current_preset(list(
      name = "Both dice show 6",
      rule = "Multiplication Rule (Independent Events)",
      rule_formula = "P(A → B∧C) = P(A → B) × P(A → C)  [when B and C are independent]",
      n_dice = 2,
      type = "both_match",
      target = 6,
      formula = "P(both = 6) = P(first = 6) × P(second = 6) = 1/6 × 1/6 = 1/36",
      explanation = "The events are independent. The probability that both occur is the product of their individual probabilities."
    ))
  })

  observeEvent(input$preset_deuce_ace, {
    current_preset(list(
      name = "Deuce-Ace (one shows 2, other shows 1)",
      rule = "Addition Rule (Incompatible Events)",
      rule_formula = "P(A → B or C) = P(A → B) + P(A → C)  [when B and C are incompatible]",
      n_dice = 2,
      type = "deuce_ace",
      formula = "P(2,1 or 1,2) = P(2,1) + P(1,2) = 1/36 + 1/36 = 1/18",
      explanation = "Two incompatible ways: (first=1, second=2) OR (first=2, second=1). We add their probabilities."
    ))
  })

  observeEvent(input$preset_sum_seven, {
    current_preset(list(
      name = "Dice sum to 7",
      rule = "Addition Rule (Multiple Incompatible Ways)",
      rule_formula = "P(A → B₁ or B₂ or ... or Bₙ) = P(A → B₁) + P(A → B₂) + ... + P(A → Bₙ)",
      n_dice = 2,
      type = "sum",
      target = 7,
      formula = "P(sum = 7) = 6/36 = 1/6",
      explanation = "Six ways to sum to 7: (1,6), (2,5), (3,4), (4,3), (5,2), (6,1). Each has probability 1/36."
    ))
  })

  observeEvent(input$preset_at_least_one_six, {
    current_preset(list(
      name = "At least one die shows 6",
      rule = "Complement Rule",
      rule_formula = "P(A → B) = 1 - P(A → not B)",
      n_dice = 2,
      type = "at_least_one",
      target = 6,
      formula = "P(at least one 6) = 1 - P(no 6s) = 1 - (5/6)² = 11/36",
      explanation = "Easier to calculate the complement: probability neither shows 6 is (5/6)²."
    ))
  })

  observeEvent(input$preset_both_even, {
    current_preset(list(
      name = "Both dice show even numbers",
      rule = "Multiplication Rule (Independent Events)",
      rule_formula = "P(A → B∧C) = P(A → B) × P(A → C)  [when B and C are independent]",
      n_dice = 2,
      type = "both_even",
      formula = "P(both even) = P(first even) × P(second even) = 1/2 × 1/2 = 1/4",
      explanation = "Each die has probability 1/2 of being even (2, 4, or 6). Independent events."
    ))
  })

  observeEvent(input$preset_doubles, {
    current_preset(list(
      name = "Any doubles (both dice match)",
      rule = "Addition Rule (Multiple Incompatible Ways)",
      rule_formula = "P(A → B₁ or B₂ or ... or Bₙ) = P(A → B₁) + P(A → B₂) + ... + P(A → Bₙ)",
      n_dice = 2,
      type = "any_doubles",
      formula = "P(doubles) = 6/36 = 1/6",
      explanation = "Six ways to get doubles: (1,1), (2,2), (3,3), (4,4), (5,5), (6,6). Each has probability 1/36."
    ))
  })

  output$ex5_content <- renderUI({
    mode <- current_mode()
    preset <- current_preset()

    if (mode == "theoretical") {
      fluidRow(
        column(5,
               div(class = "key-insight",
                   h5(strong(preset$name)),
                   p(strong("Rule: "), preset$rule),
                   p(preset$explanation),
                   hr(),
                   div(style = "background-color: #e9ecef; padding: 10px; border-radius: 4px; margin-top: 10px;",
                       p(strong("General Formula:"), style = "margin-bottom: 8px;"),
                       p(preset$rule_formula, style = "font-family: monospace; font-size: 0.95em;")
                   ),
                   hr(),
                   uiOutput("ex5_theoretical_calc")
               ),

               div(class = "formula-box", style = "margin-top: 15px;",
                   preset$formula
               )
        ),
        column(7,
               plotOutput("ex5_theoretical_plot", height = "550px")
        )
      )
    } else {
      fluidRow(
        column(4,
               p(class = "key-insight",
                 strong("Empirical View: "),
                 "Run trials to see how the long-run frequency converges to the theoretical probability. While we solve the problem with mathematics in theoretical mode, the empirical simulation demonstrates that our calculations predict real-world outcomes."
               ),
               hr(),
               sliderInput("ex5_n_trials", "Number of trials:",
                          min = 10, max = 10000, value = 100, step = 10),
               actionButton("ex5_run_sim", "Run Simulation",
                           class = "btn-primary btn-lg",
                           style = "width: 100%;"),
               hr(),
               uiOutput("ex5_empirical_summary")
        ),
        column(8,
               plotOutput("ex5_empirical_plot", height = "550px")
        )
      )
    }
  })

  # Theoretical plot
  output$ex5_theoretical_plot <- renderPlot({
    preset <- current_preset()

    if (preset$n_dice == 1) {
      # Single die
      par(mar = c(4, 4, 3, 2))
      plot(NULL, xlim = c(0.5, 6.5), ylim = c(0.5, 1.5),
           xlab = "", ylab = "", axes = FALSE, asp = 1,
           main = "Antecedent Space: Single Die")

      rect(0.5, 0.5, 6.5, 1.5, col = rgb(1, 0.95, 0.8, alpha = 0.5), border = NA)

      for (i in 1:6) {
        col <- if (i == preset$target) rgb(0.56, 0.93, 0.56, alpha = 0.7) else "white"
        rect(i - 0.4, 1 - 0.4, i + 0.4, 1 + 0.4,
             col = col, border = "black", lwd = 2)
        text(i, 1, as.character(i), cex = 1.5, font = 2)
      }

      text(3.5, 0.2, "Green cell is the favorable outcome: 1/6", cex = 1.1, font = 2)

    } else {
      # Two dice
      par(mar = c(5, 5, 4, 2))
      plot(NULL, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5),
           xlab = "First Die", ylab = "Second Die",
           axes = FALSE, asp = 1,
           main = "Antecedent Space: Two Dice (36 outcomes)")

      rect(0.5, 0.5, 6.5, 6.5, col = rgb(1, 0.95, 0.8, alpha = 0.4), border = NA)

      for (die1 in 1:6) {
        for (die2 in 1:6) {
          match <- FALSE

          if (preset$type == "both_match") {
            match <- (die1 == preset$target && die2 == preset$target)
          } else if (preset$type == "deuce_ace") {
            match <- ((die1 == 1 && die2 == 2) || (die1 == 2 && die2 == 1))
          } else if (preset$type == "sum") {
            match <- (die1 + die2 == preset$target)
          } else if (preset$type == "at_least_one") {
            match <- (die1 == preset$target || die2 == preset$target)
          } else if (preset$type == "both_even") {
            match <- (die1 %% 2 == 0 && die2 %% 2 == 0)
          } else if (preset$type == "any_doubles") {
            match <- (die1 == die2)
          }

          col <- if (match) rgb(0.56, 0.93, 0.56, alpha = 0.7) else "white"

          rect(die1 - 0.45, die2 - 0.45, die1 + 0.45, die2 + 0.45,
               col = col, border = "black", lwd = 0.5)
          text(die1, die2, paste0("(", die1, ",", die2, ")"), cex = 0.6)
        }
      }

      axis(1, at = 1:6, labels = 1:6, tick = FALSE, line = -1)
      axis(2, at = 1:6, labels = 1:6, tick = FALSE, las = 1, line = -1)
    }
  })

  # Theoretical calculation
  output$ex5_theoretical_calc <- renderUI({
    preset <- current_preset()

    # Calculate probability
    if (preset$type == "single") {
      p <- 1/6
      favorable <- 1
      total <- 6
    } else if (preset$type == "both_match") {
      p <- 1/36
      favorable <- 1
      total <- 36
    } else if (preset$type == "deuce_ace") {
      p <- 2/36
      favorable <- 2
      total <- 36
    } else if (preset$type == "sum") {
      if (preset$target == 7) {
        favorable <- 6
      } else if (preset$target == 2 || preset$target == 12) {
        favorable <- 1
      } else {
        # Calculate for other sums
        favorable <- sum(sapply(1:6, function(d1) sum(sapply(1:6, function(d2) (d1 + d2 == preset$target)))))
      }
      p <- favorable / 36
      total <- 36
    } else if (preset$type == "at_least_one") {
      favorable <- 11
      p <- 11/36
      total <- 36
    } else if (preset$type == "both_even") {
      favorable <- 9
      p <- 9/36
      total <- 36
    } else if (preset$type == "any_doubles") {
      favorable <- 6
      p <- 6/36
      total <- 36
    }

    div(
      h5("Calculation:"),
      p("Favorable outcomes: ", strong(favorable)),
      p("Total outcomes: ", strong(total)),
      p("Probability: ", strong(sprintf("%d/%d = %.4f", favorable, total, p)))
    )
  })

  # Empirical simulation
  sim_results <- reactiveVal(NULL)

  observeEvent(input$ex5_run_sim, {
    preset <- current_preset()
    n <- input$ex5_n_trials

    if (preset$n_dice == 1) {
      rolls <- sample(1:6, n, replace = TRUE)
      successes <- rolls == preset$target
      p_theory <- 1/6
    } else {
      die1 <- sample(1:6, n, replace = TRUE)
      die2 <- sample(1:6, n, replace = TRUE)

      if (preset$type == "both_match") {
        successes <- (die1 == preset$target & die2 == preset$target)
        p_theory <- 1/36
      } else if (preset$type == "deuce_ace") {
        successes <- ((die1 == 1 & die2 == 2) | (die1 == 2 & die2 == 1))
        p_theory <- 2/36
      } else if (preset$type == "sum") {
        successes <- (die1 + die2 == preset$target)
        if (preset$target == 7) {
          p_theory <- 6/36
        } else {
          favorable <- sum(sapply(1:6, function(d1) sum(sapply(1:6, function(d2) (d1 + d2 == preset$target)))))
          p_theory <- favorable / 36
        }
      } else if (preset$type == "at_least_one") {
        successes <- (die1 == preset$target | die2 == preset$target)
        p_theory <- 11/36
      } else if (preset$type == "both_even") {
        successes <- (die1 %% 2 == 0 & die2 %% 2 == 0)
        p_theory <- 9/36
      } else if (preset$type == "any_doubles") {
        successes <- (die1 == die2)
        p_theory <- 6/36
      }
    }

    freq <- cumsum(successes) / seq_along(successes)

    sim_results(list(
      freq = freq,
      p_theory = p_theory,
      n_trials = n,
      n_successes = sum(successes)
    ))
  })

  output$ex5_empirical_plot <- renderPlot({
    results <- sim_results()

    if (is.null(results)) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Click 'Run Simulation' to begin")
      text(1, 1, "Awaiting simulation...", cex = 1.5, col = "gray")
    } else {
      par(mar = c(5, 5, 4, 2))
      plot(1:results$n_trials, results$freq, type = "l",
           col = "#2c7fb8", lwd = 2,
           xlab = "Number of Trials", ylab = "Observed Frequency",
           main = "Long-Run Frequency Converging to Theoretical Probability",
           ylim = c(0, max(1, max(results$freq))))

      abline(h = results$p_theory, col = "red", lwd = 2, lty = 2)
      grid(col = "gray", lty = "dotted")

      legend("topright",
             legend = c("Observed Frequency",
                       sprintf("Theoretical P = %.4f", results$p_theory)),
             col = c("#2c7fb8", "red"),
             lwd = 2, lty = c(1, 2),
             bg = "white")
    }
  })

  output$ex5_empirical_summary <- renderUI({
    results <- sim_results()

    if (is.null(results)) {
      return(p("Run a simulation to see results."))
    }

    final_freq <- results$freq[results$n_trials]

    div(
      h5("Results:"),
      p("Trials run: ", strong(results$n_trials)),
      p("Successes: ", strong(results$n_successes)),
      p("Observed frequency: ", strong(sprintf("%.4f", final_freq))),
      p("Theoretical probability: ", strong(sprintf("%.4f", results$p_theory))),
      p("Difference: ", strong(sprintf("%.4f", abs(final_freq - results$p_theory))))
    )
  })
}

shinyApp(ui = ui, server = server)
