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
      .hl-consequent { background-color: rgba(144, 238, 144, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
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
      h3("Example 1: Definition of Probability"),

      p("Using this language, we may say that probability belongs exclusively to consequences, and the probability of any consequence is the number of times in which antecedent and consequent both occur divided by the number of all the times in which the antecedent occurs. ",
        span(class = "example-trigger", id = "ex1-trigger",
             onclick = "Shiny.setInputValue('toggle_ex1', Math.random());",
             "[Click to see interactive example]")
      ),

      div(id = "example-1", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Definition of Probability"),

          p(strong("Peirce's Key Terms:")),
          tags$ul(
            tags$li(span(class = "hl-antecedent", "ANTECEDENT"), " = The experimental setup (e.g., \"roll a die\")"),
            tags$li(span(class = "hl-consequent", "CONSEQUENT"), " = The outcome we're tracking (e.g., \"it shows a 6\")"),
            tags$li(strong("CONSEQUENCE"), " = The rule connecting them: \"If ANTECEDENT, then CONSEQUENT\"")
          ),

          p("The ", strong("probability of the consequence"), " is the ratio:"),
          div(class = "formula-box",
              "P = (times both occur) / (times antecedent occurs)"
          ),

          hr(),

          # Mode toggle
          div(class = "mode-tabs",
              div(class = "mode-tab active", id = "theoretical-tab",
                  onclick = "Shiny.setInputValue('ex1_mode', 'theoretical', {priority: 'event'});",
                  "Theoretical Space"),
              div(class = "mode-tab", id = "empirical-tab",
                  onclick = "Shiny.setInputValue('ex1_mode', 'empirical', {priority: 'event'});",
                  "Long-Run Frequency")
          ),

          uiOutput("ex1_content")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex1, { shinyjs::toggle("example-1") })

  # Mode state
  current_mode <- reactiveVal("theoretical")

  observeEvent(input$ex1_mode, {
    current_mode(input$ex1_mode)

    # Update tab styling
    runjs("$('.mode-tab').removeClass('active');")
    if (input$ex1_mode == "theoretical") {
      runjs("$('#theoretical-tab').addClass('active');")
    } else {
      runjs("$('#empirical-tab').addClass('active');")
    }
  })

  output$ex1_content <- renderUI({
    mode <- current_mode()

    if (mode == "theoretical") {
      # Theoretical mode: just show the grid
      div(
        p(class = "key-insight",
          strong("Theoretical View: "),
          "This shows all possible outcomes in the ",
          span(class = "hl-antecedent", "antecedent space"),
          ". The ",
          span(class = "hl-consequent", "green cells"),
          " represent outcomes where the consequent is TRUE."
        ),

        fluidRow(
          column(4,
                 h5("Setup:"),
                 selectInput("ex1_scenario", "Choose a scenario:",
                            choices = c(
                              "Roll a standard die" = "die",
                              "Draw from a standard deck" = "deck"
                            )),

                 conditionalPanel(
                   condition = "input.ex1_scenario == 'die'",
                   selectInput("ex1_die_consequent",
                              span(class = "hl-consequent", "Consequent (outcome):"),
                              choices = c("Shows a 6" = "6",
                                        "Shows an even number" = "even",
                                        "Shows greater than 4" = "gt4"))
                 ),

                 conditionalPanel(
                   condition = "input.ex1_scenario == 'deck'",
                   selectInput("ex1_deck_consequent",
                              span(class = "hl-consequent", "Consequent (outcome):"),
                              choices = c("Card is red" = "red",
                                        "Card is an Ace" = "ace",
                                        "Card is a face card" = "face"))
                 ),

                 hr(),
                 uiOutput("ex1_theoretical_calc")
          ),

          column(8,
                 plotOutput("ex1_theoretical_plot", height = "450px")
          )
        )
      )
    } else {
      # Empirical mode: show simulation
      div(
        p(class = "key-insight",
          strong("Empirical View: "),
          "Run trials to see how the long-run frequency converges to the theoretical probability."
        ),

        fluidRow(
          column(4,
                 h5("Setup:"),
                 selectInput("ex1_scenario_emp", "Choose a scenario:",
                            choices = c(
                              "Roll a standard die" = "die",
                              "Draw from a standard deck" = "deck"
                            )),

                 conditionalPanel(
                   condition = "input.ex1_scenario_emp == 'die'",
                   selectInput("ex1_die_consequent_emp",
                              span(class = "hl-consequent", "Consequent (outcome):"),
                              choices = c("Shows a 6" = "6",
                                        "Shows an even number" = "even",
                                        "Shows greater than 4" = "gt4"))
                 ),

                 conditionalPanel(
                   condition = "input.ex1_scenario_emp == 'deck'",
                   selectInput("ex1_deck_consequent_emp",
                              span(class = "hl-consequent", "Consequent (outcome):"),
                              choices = c("Card is red" = "red",
                                        "Card is an Ace" = "ace",
                                        "Card is a face card" = "face"))
                 ),

                 hr(),
                 sliderInput("ex1_n_trials", "Number of trials:",
                            min = 10, max = 10000, value = 100, step = 10),
                 actionButton("ex1_run_sim", "Run Simulation",
                             class = "btn-primary btn-lg",
                             style = "width: 100%;"),
                 hr(),
                 uiOutput("ex1_empirical_summary")
          ),

          column(8,
                 plotOutput("ex1_empirical_plot", height = "450px")
          )
        )
      )
    }
  })

  # Theoretical plot
  output$ex1_theoretical_plot <- renderPlot({
    req(input$ex1_scenario)

    if (input$ex1_scenario == "die") {
      req(input$ex1_die_consequent)

      # Create grid for die (1x6)
      par(mar = c(4, 4, 3, 2))
      plot(NULL, xlim = c(0.5, 6.5), ylim = c(0.5, 1.5),
           xlab = "", ylab = "", axes = FALSE, asp = 1,
           main = "Antecedent Space: All Possible Die Outcomes")

      # Determine which outcomes match consequent
      consequent <- input$ex1_die_consequent
      matches <- rep(FALSE, 6)
      if (consequent == "6") {
        matches[6] <- TRUE
      } else if (consequent == "even") {
        matches[c(2, 4, 6)] <- TRUE
      } else if (consequent == "gt4") {
        matches[c(5, 6)] <- TRUE
      }

      # Draw antecedent background (light yellow)
      rect(0.5, 0.5, 6.5, 1.5, col = rgb(1, 0.95, 0.8, alpha = 0.5), border = NA)

      # Draw cells
      for (i in 1:6) {
        col <- if (matches[i]) rgb(0.56, 0.93, 0.56, alpha = 0.7) else "white"
        rect(i - 0.4, 1 - 0.4, i + 0.4, 1 + 0.4,
             col = col, border = "black", lwd = 2)
        text(i, 1, as.character(i), cex = 1.5, font = 2)
      }

      # Add legend
      n_match <- sum(matches)
      text(3.5, 0.2,
           paste0("Consequent TRUE: ", n_match, "/6 outcomes"),
           cex = 1.1, font = 2)

    } else if (input$ex1_scenario == "deck") {
      req(input$ex1_deck_consequent)

      # Create simplified grid for deck (4 suits x 13 ranks)
      suits <- c("♠", "♥", "♦", "♣")
      ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")

      par(mar = c(4, 5, 3, 2))
      plot(NULL, xlim = c(0.5, 13.5), ylim = c(0.5, 4.5),
           xlab = "", ylab = "", axes = FALSE, asp = 1,
           main = "Antecedent Space: All Possible Cards (52 total)")

      # Determine matches
      consequent <- input$ex1_deck_consequent

      # Draw antecedent background
      rect(0.5, 0.5, 13.5, 4.5, col = rgb(1, 0.95, 0.8, alpha = 0.4), border = NA)

      # Draw cards
      for (s in 1:4) {
        for (r in 1:13) {
          is_red <- s %in% c(2, 3)
          is_ace <- r == 1
          is_face <- r %in% c(11, 12, 13)

          match <- FALSE
          if (consequent == "red" && is_red) match <- TRUE
          if (consequent == "ace" && is_ace) match <- TRUE
          if (consequent == "face" && is_face) match <- TRUE

          col <- if (match) rgb(0.56, 0.93, 0.56, alpha = 0.7) else "white"
          rect(r - 0.45, s - 0.45, r + 0.45, s + 0.45,
               col = col, border = "black", lwd = 0.5)

          # Color suit symbols
          suit_col <- if (is_red) "red" else "black"
          text(r, s, paste0(ranks[r], suits[s]), cex = 0.7, col = suit_col, font = 2)
        }
      }

      # Axes
      axis(1, at = 1:13, labels = ranks, tick = FALSE, line = -1)
      axis(2, at = 1:4, labels = c("Spades", "Hearts", "Diamonds", "Clubs"),
           tick = FALSE, las = 1, line = -1)

      # Legend
      n_match <- if (consequent == "red") 26 else if (consequent == "ace") 4 else 12
      text(7, -0.3,
           paste0("Consequent TRUE: ", n_match, "/52 cards"),
           cex = 1.1, font = 2)
    }
  })

  # Theoretical calculation
  output$ex1_theoretical_calc <- renderUI({
    req(input$ex1_scenario)

    if (input$ex1_scenario == "die") {
      req(input$ex1_die_consequent)

      consequent <- input$ex1_die_consequent
      n_favorable <- if (consequent == "6") 1
                     else if (consequent == "even") 3
                     else 2
      prob <- n_favorable / 6

      div(
        h5("Calculation:"),
        p("Total antecedent outcomes: ", strong("6")),
        p("Consequent TRUE outcomes: ", strong(n_favorable)),
        p("Probability = ", strong(n_favorable), " / 6 = ",
          strong(sprintf("%.4f", prob)))
      )
    } else {
      req(input$ex1_deck_consequent)

      consequent <- input$ex1_deck_consequent
      n_favorable <- if (consequent == "red") 26
                     else if (consequent == "ace") 4
                     else 12
      prob <- n_favorable / 52

      div(
        h5("Calculation:"),
        p("Total antecedent outcomes: ", strong("52")),
        p("Consequent TRUE outcomes: ", strong(n_favorable)),
        p("Probability = ", strong(n_favorable), " / 52 = ",
          strong(sprintf("%.4f", prob)))
      )
    }
  })

  # Empirical simulation results
  sim_results <- reactiveVal(NULL)

  observeEvent(input$ex1_run_sim, {
    req(input$ex1_scenario_emp)

    n <- input$ex1_n_trials
    scenario <- input$ex1_scenario_emp

    if (scenario == "die") {
      req(input$ex1_die_consequent_emp)
      consequent <- input$ex1_die_consequent_emp

      # Simulate die rolls
      rolls <- sample(1:6, n, replace = TRUE)

      if (consequent == "6") {
        successes <- rolls == 6
      } else if (consequent == "even") {
        successes <- rolls %% 2 == 0
      } else {
        successes <- rolls > 4
      }

      theoretical_p <- if (consequent == "6") 1/6
                       else if (consequent == "even") 3/6
                       else 2/6

    } else {
      req(input$ex1_deck_consequent_emp)
      consequent <- input$ex1_deck_consequent_emp

      # Simulate card draws
      suits <- rep(c("S", "H", "D", "C"), each = 13)
      ranks <- rep(c("A", 2:10, "J", "Q", "K"), 4)

      draws <- sample(1:52, n, replace = TRUE)

      if (consequent == "red") {
        successes <- suits[draws] %in% c("H", "D")
        theoretical_p <- 26/52
      } else if (consequent == "ace") {
        successes <- ranks[draws] == "A"
        theoretical_p <- 4/52
      } else {
        successes <- ranks[draws] %in% c("J", "Q", "K")
        theoretical_p <- 12/52
      }
    }

    # Calculate running frequency
    cumulative_freq <- cumsum(successes) / seq_along(successes)

    sim_results(list(
      successes = successes,
      cumulative_freq = cumulative_freq,
      theoretical_p = theoretical_p,
      n_trials = n
    ))
  })

  output$ex1_empirical_plot <- renderPlot({
    results <- sim_results()

    if (is.null(results)) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Click 'Run Simulation' to begin")
      text(1, 1, "Awaiting simulation...", cex = 1.5, col = "gray")
    } else {
      par(mar = c(5, 5, 4, 2))
      plot(1:results$n_trials, results$cumulative_freq, type = "l",
           col = "#2c7fb8", lwd = 2,
           xlab = "Number of Trials", ylab = "Observed Frequency",
           main = "Long-Run Frequency Converging to Theoretical Probability",
           ylim = c(0, max(1, max(results$cumulative_freq))))

      # Add theoretical probability line
      abline(h = results$theoretical_p, col = "red", lwd = 2, lty = 2)

      # Add grid
      grid(col = "gray", lty = "dotted")

      # Legend
      legend("topright",
             legend = c("Observed Frequency",
                       sprintf("Theoretical P = %.4f", results$theoretical_p)),
             col = c("#2c7fb8", "red"),
             lwd = 2, lty = c(1, 2),
             bg = "white")
    }
  })

  output$ex1_empirical_summary <- renderUI({
    results <- sim_results()

    if (is.null(results)) {
      return(p("Run a simulation to see results."))
    }

    final_freq <- results$cumulative_freq[results$n_trials]
    n_successes <- sum(results$successes)

    div(
      h5("Results:"),
      p("Trials run: ", strong(results$n_trials)),
      p("Successes: ", strong(n_successes)),
      p("Observed frequency: ", strong(sprintf("%.4f", final_freq))),
      p("Theoretical probability: ", strong(sprintf("%.4f", results$theoretical_p))),
      p("Difference: ", strong(sprintf("%.4f", abs(final_freq - results$theoretical_p))))
    )
  })
}

shinyApp(ui = ui, server = server)
