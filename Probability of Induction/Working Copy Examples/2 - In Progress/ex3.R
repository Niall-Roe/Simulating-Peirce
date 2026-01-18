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
      h3("Example 3: Rule for the Multiplication of Probabilities"),

      p(span(class = "example-trigger", id = "ex3-trigger",
             onclick = "Shiny.setInputValue('toggle_ex3', Math.random());",
             strong("Rule for the Multiplication of Probabilities."), " — Given the separate probabilities of the two consequences, \"If A then B,\" and \"If both A and B, then C.\" Then the product of these two numbers is the probability of the consequence, \"If A, then both B and C.\"")
      ),

      div(id = "example-3", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Multiplication Rule"),

          p(strong("The Multiplication Rule"), " connects two related consequences:"),
          tags$ul(
            tags$li("First: ", span(class = "hl-antecedent", "\"If A, then B\""), " has probability P(A → B)"),
            tags$li("Second: ", span(class = "hl-consequent-c", "\"If A and B both occur, then C\""), " has probability P(A∧B → C)"),
            tags$li("Result: ", span(class = "hl-both", "\"If A, then both B and C\""), " has probability P(A → B) × P(A∧B → C)")
          ),

          div(class = "formula-box",
              "P(A → B∧C) = P(A → B) × P(A∧B → C)"
          ),

          hr(),

          # Mode toggle
          div(class = "mode-tabs",
              div(class = "mode-tab active", id = "theoretical-tab",
                  onclick = "Shiny.setInputValue('ex3_mode', 'theoretical', {priority: 'event'});",
                  "Theoretical Space"),
              div(class = "mode-tab", id = "empirical-tab",
                  onclick = "Shiny.setInputValue('ex3_mode', 'empirical', {priority: 'event'});",
                  "Long-Run Frequency")
          ),

          uiOutput("ex3_content")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex3, { shinyjs::toggle("example-3") })

  # Mode state
  current_mode <- reactiveVal("theoretical")

  observeEvent(input$ex3_mode, {
    current_mode(input$ex3_mode)

    # Update tab styling
    runjs("$('.mode-tab').removeClass('active');")
    if (input$ex3_mode == "theoretical") {
      runjs("$('#theoretical-tab').addClass('active');")
    } else {
      runjs("$('#empirical-tab').addClass('active');")
    }
  })

  output$ex3_content <- renderUI({
    mode <- current_mode()

    if (mode == "theoretical") {
      # Theoretical mode
      div(
        p(class = "key-insight",
          strong("Theoretical View: "),
          "First find where B happens (",
          span(class = "hl-consequent-b", "pink region"),
          "). Then, within that region, find where C also happens (",
          span(class = "hl-both", "purple cells"),
          ")."
        ),

        fluidRow(
          column(4,
                 h5("Setup:"),
                 p("We'll draw two cards with replacement."),
                 selectInput("ex3_cons_b",
                            span(class = "hl-consequent-b", "B: First card is..."),
                            choices = c("Red" = "red",
                                      "An Ace" = "ace",
                                      "A face card" = "face")),
                 selectInput("ex3_cons_c",
                            span(class = "hl-consequent-c", "C: Second card is..."),
                            choices = c("Red" = "red",
                                      "An Ace" = "ace",
                                      "A face card" = "face")),

                 hr(),
                 uiOutput("ex3_theoretical_calc")
          ),

          column(8,
                 plotOutput("ex3_theoretical_plot", height = "450px")
          )
        )
      )
    } else {
      # Empirical mode
      div(
        p(class = "key-insight",
          strong("Empirical View: "),
          "Run trials to see how the multiplication rule holds in the long run."
        ),

        fluidRow(
          column(4,
                 h5("Setup:"),
                 p("We'll draw two cards with replacement."),
                 selectInput("ex3_cons_b_emp",
                            span(class = "hl-consequent-b", "B: First card is..."),
                            choices = c("Red" = "red",
                                      "An Ace" = "ace",
                                      "A face card" = "face")),
                 selectInput("ex3_cons_c_emp",
                            span(class = "hl-consequent-c", "C: Second card is..."),
                            choices = c("Red" = "red",
                                      "An Ace" = "ace",
                                      "A face card" = "face")),

                 hr(),
                 sliderInput("ex3_n_trials", "Number of trials:",
                            min = 10, max = 10000, value = 100, step = 10),
                 actionButton("ex3_run_sim", "Run Simulation",
                             class = "btn-primary btn-lg",
                             style = "width: 100%;"),
                 hr(),
                 uiOutput("ex3_empirical_summary")
          ),

          column(8,
                 plotOutput("ex3_empirical_plot", height = "450px")
          )
        )
      )
    }
  })

  # Helper to evaluate card property
  eval_card_prop <- function(rank, suit, property) {
    if (property == "red") {
      return(suit %in% c("H", "D"))
    } else if (property == "ace") {
      return(rank == "A")
    } else if (property == "face") {
      return(rank %in% c("J", "Q", "K"))
    }
    return(FALSE)
  }

  # Theoretical plot
  output$ex3_theoretical_plot <- renderPlot({
    req(input$ex3_cons_b, input$ex3_cons_c)

    suits <- c("S", "H", "D", "C")
    ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
    suit_symbols <- c("♠", "♥", "♦", "♣")

    # Create grid: 52 x 52 (too large, so we'll show a simplified version)
    # Instead, show a 4x4 grid representing suit combinations
    par(mar = c(5, 5, 4, 2))
    plot(NULL, xlim = c(0.5, 4.5), ylim = c(0.5, 4.5),
         xlab = "First Card Suit", ylab = "Second Card Suit",
         axes = FALSE, asp = 1,
         main = "Simplified Space: Suit Combinations\n(Each cell represents 169 rank combinations)")

    prop_b <- input$ex3_cons_b
    prop_c <- input$ex3_cons_c

    # Draw antecedent background
    rect(0.5, 0.5, 4.5, 4.5, col = rgb(1, 0.95, 0.8, alpha = 0.4), border = NA)

    # Check each suit combination
    for (s1 in 1:4) {
      for (s2 in 1:4) {
        # Count how many rank combinations satisfy each property
        count_b <- 0
        count_both <- 0
        total <- 0

        for (r1 in ranks) {
          for (r2 in ranks) {
            total <- total + 1
            b_true <- eval_card_prop(r1, suits[s1], prop_b)
            c_true <- eval_card_prop(r2, suits[s2], prop_c)

            if (b_true) count_b <- count_b + 1
            if (b_true && c_true) count_both <- count_both + 1
          }
        }

        # Determine color
        if (count_both > 0 && count_both == count_b) {
          col <- rgb(0.58, 0.44, 0.86, alpha = 0.8)  # All satisfy both (purple)
        } else if (count_both > 0) {
          col <- rgb(0.7, 0.5, 0.8, alpha = 0.6)  # Some satisfy both (light purple)
        } else if (count_b > 0) {
          col <- rgb(1, 0.71, 0.76, alpha = 0.6)  # Only B satisfied (pink)
        } else {
          col <- "white"
        }

        rect(s1 - 0.45, s2 - 0.45, s1 + 0.45, s2 + 0.45,
             col = col, border = "black", lwd = 0.8)

        # Label
        is_red_s1 <- s1 %in% c(2, 3)
        is_red_s2 <- s2 %in% c(2, 3)
        text_col1 <- if (is_red_s1) "red" else "black"
        text_col2 <- if (is_red_s2) "red" else "black"

        text(s1, s2 + 0.15, suit_symbols[s1], cex = 1, col = text_col1, font = 2)
        text(s1, s2 - 0.15, suit_symbols[s2], cex = 1, col = text_col2, font = 2)
      }
    }

    # Axes
    axis(1, at = 1:4, labels = c("♠", "♥", "♦", "♣"), tick = FALSE, line = -1, cex.axis = 1.2)
    axis(2, at = 1:4, labels = c("♠", "♥", "♦", "♣"), tick = FALSE, las = 1, line = -1, cex.axis = 1.2)
  })

  # Theoretical calculation
  output$ex3_theoretical_calc <- renderUI({
    req(input$ex3_cons_b, input$ex3_cons_c)

    prop_b <- input$ex3_cons_b
    prop_c <- input$ex3_cons_c

    # Calculate P(A → B)
    n_b <- if (prop_b == "red") 26 else if (prop_b == "ace") 4 else 12
    p_b <- n_b / 52

    # Calculate P(A∧B → C)
    # This is: given first card satisfies B, what's prob second card satisfies C?
    n_c <- if (prop_c == "red") 26 else if (prop_c == "ace") 4 else 12
    p_c_given_ab <- n_c / 52  # Independent draws with replacement

    # Calculate P(A → B∧C)
    p_both <- p_b * p_c_given_ab

    div(
      h5("Step-by-step:"),
      p("1. P(First card is ", prop_b, ") = ",
        strong(sprintf("%d/52 = %.4f", n_b, p_b))),
      p("2. P(Second card is ", prop_c, " | First was ", prop_b, ") = ",
        strong(sprintf("%d/52 = %.4f", n_c, p_c_given_ab)),
        br(), em("(with replacement, this is independent)")),
      hr(),
      p("3. P(Both happen) = ", strong(sprintf("%.4f", p_b)), " × ",
        strong(sprintf("%.4f", p_c_given_ab)), " = ",
        strong(sprintf("%.4f", p_both)))
    )
  })

  # Empirical simulation
  sim_results <- reactiveVal(NULL)

  observeEvent(input$ex3_run_sim, {
    req(input$ex3_cons_b_emp, input$ex3_cons_c_emp)

    n <- input$ex3_n_trials
    prop_b <- input$ex3_cons_b_emp
    prop_c <- input$ex3_cons_c_emp

    # Simulate card draws
    suits <- rep(c("S", "H", "D", "C"), each = 13)
    ranks <- rep(c("A", 2:10, "J", "Q", "K"), 4)

    # Draw two cards with replacement
    draws1 <- sample(1:52, n, replace = TRUE)
    draws2 <- sample(1:52, n, replace = TRUE)

    # Evaluate conditions
    b_occurs <- sapply(draws1, function(i) eval_card_prop(ranks[i], suits[i], prop_b))
    c_occurs <- sapply(draws2, function(i) eval_card_prop(ranks[i], suits[i], prop_c))
    both_occur <- b_occurs & c_occurs

    # Calculate frequencies
    freq_b <- cumsum(b_occurs) / seq_along(b_occurs)
    freq_c_given_b <- cumsum(c_occurs[b_occurs]) / seq_along(which(b_occurs))
    freq_both <- cumsum(both_occur) / seq_along(both_occur)

    # Theoretical
    n_b <- if (prop_b == "red") 26 else if (prop_b == "ace") 4 else 12
    n_c <- if (prop_c == "red") 26 else if (prop_c == "ace") 4 else 12
    p_b_theory <- n_b / 52
    p_c_theory <- n_c / 52
    p_both_theory <- p_b_theory * p_c_theory

    sim_results(list(
      freq_b = freq_b,
      freq_both = freq_both,
      p_b_theory = p_b_theory,
      p_c_theory = p_c_theory,
      p_both_theory = p_both_theory,
      n_trials = n
    ))
  })

  output$ex3_empirical_plot <- renderPlot({
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
           main = "Multiplication Rule: Long-Run Frequencies",
           ylim = c(0, max(1, max(results$freq_both))))

      # Add P(B) frequency
      lines(1:results$n_trials, results$freq_b, col = rgb(1, 0.71, 0.76), lwd = 2)

      # Add theoretical lines
      abline(h = results$p_b_theory, col = rgb(1, 0.4, 0.5), lwd = 2, lty = 2)
      abline(h = results$p_both_theory, col = "purple", lwd = 2, lty = 2)

      grid(col = "gray", lty = "dotted")

      legend("topright",
             legend = c("Observed: Both B and C",
                       "Observed: Just B",
                       sprintf("Theory: Both = %.4f", results$p_both_theory),
                       sprintf("Theory: Just B = %.4f", results$p_b_theory)),
             col = c("purple", rgb(1, 0.71, 0.76),
                    "purple", rgb(1, 0.4, 0.5)),
             lwd = c(3, 2, 2, 2),
             lty = c(1, 1, 2, 2),
             bg = "white",
             cex = 0.85)
    }
  })

  output$ex3_empirical_summary <- renderUI({
    results <- sim_results()

    if (is.null(results)) {
      return(p("Run a simulation to see results."))
    }

    final_freq_both <- results$freq_both[results$n_trials]
    product <- results$p_b_theory * results$p_c_theory

    div(
      h5("Results:"),
      p("Trials: ", strong(results$n_trials)),
      p("Observed P(Both): ", strong(sprintf("%.4f", final_freq_both))),
      p("Expected (product): ",
        strong(sprintf("%.4f × %.4f = %.4f",
                      results$p_b_theory, results$p_c_theory, product))),
      p("Difference: ", strong(sprintf("%.4f", abs(final_freq_both - product))))
    )
  })
}

shinyApp(ui = ui, server = server)
