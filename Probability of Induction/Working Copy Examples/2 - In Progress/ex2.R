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
      .hl-consequent-a { background-color: rgba(255, 182, 193, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-b { background-color: rgba(173, 216, 230, 0.6); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-combined { background-color: rgba(221, 160, 221, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .key-insight {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
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
      h3("Example 2: Rule for the Addition of Probabilities"),

      p(span(class = "example-trigger", id = "ex2-trigger",
             onclick = "Shiny.setInputValue('toggle_ex2', Math.random());",
             strong("Rule for the Addition of Probabilities."), " — Given the separate probabilities of two consequences having the same antecedent and incompatible consequents. Then the sum of these two numbers is the probability of the consequence, that from the same antecedent one or other of those consequents follows.")
      ),

      div(id = "example-2", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Addition Rule"),

          p(strong("The Addition Rule"), " applies when:"),
          tags$ul(
            tags$li("You have the ", span(class = "hl-antecedent", "same antecedent"), " (same experiment)"),
            tags$li("Two ", strong("incompatible"), " consequents (they can't both happen)"),
            tags$li("You want to find: \"What's the probability that ", strong("either"), " happens?\"")
          ),

          div(class = "formula-box",
              "P(A → C₁ or C₂) = P(A → C₁) + P(A → C₂)",
              br(),
              em(style = "font-size: 0.9em;", "(when C₁ and C₂ are incompatible)")
          ),

          hr(),

          # Mode toggle
          div(class = "mode-tabs",
              div(class = "mode-tab active", id = "theoretical-tab",
                  onclick = "Shiny.setInputValue('ex2_mode', 'theoretical', {priority: 'event'});",
                  "Theoretical Space"),
              div(class = "mode-tab", id = "empirical-tab",
                  onclick = "Shiny.setInputValue('ex2_mode', 'empirical', {priority: 'event'});",
                  "Long-Run Frequency")
          ),

          uiOutput("ex2_content")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex2, { shinyjs::toggle("example-2") })

  # Mode state
  current_mode <- reactiveVal("theoretical")

  observeEvent(input$ex2_mode, {
    current_mode(input$ex2_mode)

    # Update tab styling
    runjs("$('.mode-tab').removeClass('active');")
    if (input$ex2_mode == "theoretical") {
      runjs("$('#theoretical-tab').addClass('active');")
    } else {
      runjs("$('#empirical-tab').addClass('active');")
    }
  })

  output$ex2_content <- renderUI({
    mode <- current_mode()

    if (mode == "theoretical") {
      # Theoretical mode
      div(
        p(class = "key-insight",
          strong("Theoretical View: "),
          "The ",
          span(class = "hl-consequent-a", "pink cells"),
          " and ",
          span(class = "hl-consequent-b", "blue cells"),
          " are mutually exclusive. Adding their probabilities gives the probability of getting ",
          strong("either"), " outcome."
        ),

        fluidRow(
          column(4,
                 h5("Setup:"),
                 selectInput("ex2_scenario", "Choose a scenario:",
                            choices = c(
                              "Roll a standard die" = "die",
                              "Draw from a standard deck" = "deck"
                            )),

                 conditionalPanel(
                   condition = "input.ex2_scenario == 'die'",
                   selectInput("ex2_die_cons1",
                              span(class = "hl-consequent-a", "First consequent:"),
                              choices = c("Shows a 6" = "6",
                                        "Shows a 1" = "1",
                                        "Shows a 5" = "5")),
                   selectInput("ex2_die_cons2",
                              span(class = "hl-consequent-b", "Second consequent:"),
                              choices = c("Shows a 2" = "2",
                                        "Shows a 3" = "3",
                                        "Shows a 4" = "4"))
                 ),

                 conditionalPanel(
                   condition = "input.ex2_scenario == 'deck'",
                   selectInput("ex2_deck_cons1",
                              span(class = "hl-consequent-a", "First consequent:"),
                              choices = c("Card is an Ace" = "ace",
                                        "Card is a King" = "king",
                                        "Card is a Heart" = "heart")),
                   selectInput("ex2_deck_cons2",
                              span(class = "hl-consequent-b", "Second consequent:"),
                              choices = c("Card is a Queen" = "queen",
                                        "Card is a Jack" = "jack",
                                        "Card is a Spade" = "spade"))
                 ),

                 hr(),
                 uiOutput("ex2_theoretical_calc")
          ),

          column(8,
                 plotOutput("ex2_theoretical_plot", height = "450px")
          )
        )
      )
    } else {
      # Empirical mode
      div(
        p(class = "key-insight",
          strong("Empirical View: "),
          "Run trials to see how the long-run frequencies of each outcome (and their sum) converge to the theoretical probabilities."
        ),

        fluidRow(
          column(4,
                 h5("Setup:"),
                 selectInput("ex2_scenario_emp", "Choose a scenario:",
                            choices = c(
                              "Roll a standard die" = "die",
                              "Draw from a standard deck" = "deck"
                            )),

                 conditionalPanel(
                   condition = "input.ex2_scenario_emp == 'die'",
                   selectInput("ex2_die_cons1_emp",
                              span(class = "hl-consequent-a", "First consequent:"),
                              choices = c("Shows a 6" = "6",
                                        "Shows a 1" = "1",
                                        "Shows a 5" = "5")),
                   selectInput("ex2_die_cons2_emp",
                              span(class = "hl-consequent-b", "Second consequent:"),
                              choices = c("Shows a 2" = "2",
                                        "Shows a 3" = "3",
                                        "Shows a 4" = "4"))
                 ),

                 conditionalPanel(
                   condition = "input.ex2_scenario_emp == 'deck'",
                   selectInput("ex2_deck_cons1_emp",
                              span(class = "hl-consequent-a", "First consequent:"),
                              choices = c("Card is an Ace" = "ace",
                                        "Card is a King" = "king",
                                        "Card is a Heart" = "heart")),
                   selectInput("ex2_deck_cons2_emp",
                              span(class = "hl-consequent-b", "Second consequent:"),
                              choices = c("Card is a Queen" = "queen",
                                        "Card is a Jack" = "jack",
                                        "Card is a Spade" = "spade"))
                 ),

                 hr(),
                 sliderInput("ex2_n_trials", "Number of trials:",
                            min = 10, max = 10000, value = 100, step = 10),
                 actionButton("ex2_run_sim", "Run Simulation",
                             class = "btn-primary btn-lg",
                             style = "width: 100%;"),
                 hr(),
                 uiOutput("ex2_empirical_summary")
          ),

          column(8,
                 plotOutput("ex2_empirical_plot", height = "450px")
          )
        )
      )
    }
  })

  # Theoretical plot
  output$ex2_theoretical_plot <- renderPlot({
    req(input$ex2_scenario)

    if (input$ex2_scenario == "die") {
      req(input$ex2_die_cons1, input$ex2_die_cons2)

      cons1 <- as.numeric(input$ex2_die_cons1)
      cons2 <- as.numeric(input$ex2_die_cons2)

      par(mar = c(4, 4, 3, 2))
      plot(NULL, xlim = c(0.5, 6.5), ylim = c(0.5, 1.5),
           xlab = "", ylab = "", axes = FALSE, asp = 1,
           main = "Antecedent Space: All Possible Die Outcomes")

      # Draw antecedent background
      rect(0.5, 0.5, 6.5, 1.5, col = rgb(1, 0.95, 0.8, alpha = 0.5), border = NA)

      # Draw cells
      for (i in 1:6) {
        col <- if (i == cons1) {
          rgb(1, 0.71, 0.76, alpha = 0.7)  # Pink
        } else if (i == cons2) {
          rgb(0.68, 0.85, 0.90, alpha = 0.7)  # Blue
        } else {
          "white"
        }
        rect(i - 0.4, 1 - 0.4, i + 0.4, 1 + 0.4,
             col = col, border = "black", lwd = 2)
        text(i, 1, as.character(i), cex = 1.5, font = 2)
      }

      # Legend
      text(3.5, 0.2,
           paste0("Pink + Blue = Either outcome: 2/6 total"),
           cex = 1.1, font = 2)

    } else if (input$ex2_scenario == "deck") {
      req(input$ex2_deck_cons1, input$ex2_deck_cons2)

      suits <- c("♠", "♥", "♦", "♣")
      ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")

      par(mar = c(4, 5, 3, 2))
      plot(NULL, xlim = c(0.5, 13.5), ylim = c(0.5, 4.5),
           xlab = "", ylab = "", axes = FALSE, asp = 1,
           main = "Antecedent Space: All Possible Cards")

      # Draw antecedent background
      rect(0.5, 0.5, 13.5, 4.5, col = rgb(1, 0.95, 0.8, alpha = 0.4), border = NA)

      cons1 <- input$ex2_deck_cons1
      cons2 <- input$ex2_deck_cons2

      # Draw cards
      for (s in 1:4) {
        for (r in 1:13) {
          is_red <- s %in% c(2, 3)
          rank <- ranks[r]
          suit <- suits[s]

          match1 <- FALSE
          match2 <- FALSE

          # First consequent
          if (cons1 == "ace" && r == 1) match1 <- TRUE
          if (cons1 == "king" && r == 13) match1 <- TRUE
          if (cons1 == "heart" && s == 2) match1 <- TRUE

          # Second consequent
          if (cons2 == "queen" && r == 12) match2 <- TRUE
          if (cons2 == "jack" && r == 11) match2 <- TRUE
          if (cons2 == "spade" && s == 1) match2 <- TRUE

          col <- if (match1) {
            rgb(1, 0.71, 0.76, alpha = 0.7)  # Pink
          } else if (match2) {
            rgb(0.68, 0.85, 0.90, alpha = 0.7)  # Blue
          } else {
            "white"
          }

          rect(r - 0.45, s - 0.45, r + 0.45, s + 0.45,
               col = col, border = "black", lwd = 0.5)

          suit_col <- if (is_red) "red" else "black"
          text(r, s, paste0(ranks[r], suits[s]), cex = 0.7, col = suit_col, font = 2)
        }
      }

      # Axes
      axis(1, at = 1:13, labels = ranks, tick = FALSE, line = -1)
      axis(2, at = 1:4, labels = c("Spades", "Hearts", "Diamonds", "Clubs"),
           tick = FALSE, las = 1, line = -1)
    }
  })

  # Theoretical calculation
  output$ex2_theoretical_calc <- renderUI({
    req(input$ex2_scenario)

    if (input$ex2_scenario == "die") {
      req(input$ex2_die_cons1, input$ex2_die_cons2)

      p1 <- 1/6
      p2 <- 1/6
      p_total <- p1 + p2

      div(
        h5("Calculation:"),
        p("P(First) = ", strong("1/6 = 0.1667")),
        p("P(Second) = ", strong("1/6 = 0.1667")),
        hr(),
        p("P(Either) = 1/6 + 1/6 = ", strong("2/6 = 0.3333"))
      )
    } else {
      req(input$ex2_deck_cons1, input$ex2_deck_cons2)

      cons1 <- input$ex2_deck_cons1
      cons2 <- input$ex2_deck_cons2

      n1 <- if (cons1 == "ace") 4 else if (cons1 == "king") 4 else 13
      n2 <- if (cons2 == "queen") 4 else if (cons2 == "jack") 4 else 13

      p1 <- n1/52
      p2 <- n2/52
      p_total <- p1 + p2

      div(
        h5("Calculation:"),
        p("P(First) = ", strong(sprintf("%d/52 = %.4f", n1, p1))),
        p("P(Second) = ", strong(sprintf("%d/52 = %.4f", n2, p2))),
        hr(),
        p("P(Either) = ", strong(sprintf("%d/52 = %.4f", n1 + n2, p_total)))
      )
    }
  })

  # Empirical simulation
  sim_results <- reactiveVal(NULL)

  observeEvent(input$ex2_run_sim, {
    req(input$ex2_scenario_emp)

    n <- input$ex2_n_trials
    scenario <- input$ex2_scenario_emp

    if (scenario == "die") {
      req(input$ex2_die_cons1_emp, input$ex2_die_cons2_emp)

      cons1 <- as.numeric(input$ex2_die_cons1_emp)
      cons2 <- as.numeric(input$ex2_die_cons2_emp)

      rolls <- sample(1:6, n, replace = TRUE)

      match1 <- rolls == cons1
      match2 <- rolls == cons2
      either <- match1 | match2

      freq1 <- cumsum(match1) / seq_along(match1)
      freq2 <- cumsum(match2) / seq_along(match2)
      freq_either <- cumsum(either) / seq_along(either)

      p1_theory <- 1/6
      p2_theory <- 1/6
      p_either_theory <- p1_theory + p2_theory

    } else {
      req(input$ex2_deck_cons1_emp, input$ex2_deck_cons2_emp)

      cons1 <- input$ex2_deck_cons1_emp
      cons2 <- input$ex2_deck_cons2_emp

      suits <- rep(c("S", "H", "D", "C"), each = 13)
      ranks <- rep(c("A", 2:10, "J", "Q", "K"), 4)

      draws <- sample(1:52, n, replace = TRUE)

      match1 <- if (cons1 == "ace") ranks[draws] == "A"
                else if (cons1 == "king") ranks[draws] == "K"
                else suits[draws] == "H"

      match2 <- if (cons2 == "queen") ranks[draws] == "Q"
                else if (cons2 == "jack") ranks[draws] == "J"
                else suits[draws] == "S"

      either <- match1 | match2

      freq1 <- cumsum(match1) / seq_along(match1)
      freq2 <- cumsum(match2) / seq_along(match2)
      freq_either <- cumsum(either) / seq_along(either)

      n1 <- if (cons1 == "ace") 4 else if (cons1 == "king") 4 else 13
      n2 <- if (cons2 == "queen") 4 else if (cons2 == "jack") 4 else 13

      p1_theory <- n1/52
      p2_theory <- n2/52
      p_either_theory <- p1_theory + p2_theory
    }

    sim_results(list(
      freq1 = freq1,
      freq2 = freq2,
      freq_either = freq_either,
      p1_theory = p1_theory,
      p2_theory = p2_theory,
      p_either_theory = p_either_theory,
      n_trials = n
    ))
  })

  output$ex2_empirical_plot <- renderPlot({
    results <- sim_results()

    if (is.null(results)) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Click 'Run Simulation' to begin")
      text(1, 1, "Awaiting simulation...", cex = 1.5, col = "gray")
    } else {
      par(mar = c(5, 5, 4, 2))
      plot(1:results$n_trials, results$freq_either, type = "l",
           col = "purple", lwd = 3,
           xlab = "Number of Trials", ylab = "Observed Frequency",
           main = "Addition Rule: Long-Run Frequencies",
           ylim = c(0, max(1, max(results$freq_either))))

      # Add individual frequencies
      lines(1:results$n_trials, results$freq1, col = rgb(1, 0.71, 0.76), lwd = 2)
      lines(1:results$n_trials, results$freq2, col = rgb(0.68, 0.85, 0.90), lwd = 2)

      # Add theoretical lines
      abline(h = results$p1_theory, col = rgb(1, 0.4, 0.5), lwd = 2, lty = 2)
      abline(h = results$p2_theory, col = rgb(0.2, 0.6, 0.8), lwd = 2, lty = 2)
      abline(h = results$p_either_theory, col = "purple", lwd = 2, lty = 2)

      grid(col = "gray", lty = "dotted")

      legend("topright",
             legend = c("Observed: Either",
                       "Observed: First",
                       "Observed: Second",
                       sprintf("Theory: Either = %.4f", results$p_either_theory),
                       sprintf("Theory: First = %.4f", results$p1_theory),
                       sprintf("Theory: Second = %.4f", results$p2_theory)),
             col = c("purple", rgb(1, 0.71, 0.76), rgb(0.68, 0.85, 0.90),
                    "purple", rgb(1, 0.4, 0.5), rgb(0.2, 0.6, 0.8)),
             lwd = c(3, 2, 2, 2, 2, 2),
             lty = c(1, 1, 1, 2, 2, 2),
             bg = "white",
             cex = 0.8)
    }
  })

  output$ex2_empirical_summary <- renderUI({
    results <- sim_results()

    if (is.null(results)) {
      return(p("Run a simulation to see results."))
    }

    final_freq_either <- results$freq_either[results$n_trials]

    div(
      h5("Results:"),
      p("Trials: ", strong(results$n_trials)),
      p("Observed P(Either): ", strong(sprintf("%.4f", final_freq_either))),
      p("Expected (sum): ", strong(sprintf("%.4f", results$p_either_theory))),
      p("Difference: ", strong(sprintf("%.4f", abs(final_freq_either - results$p_either_theory))))
    )
  })
}

shinyApp(ui = ui, server = server)
