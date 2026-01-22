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
      .hl-antecedent { background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-a { background-color: rgba(255, 182, 193, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-b { background-color: rgba(173, 216, 230, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-both { background-color: rgba(221, 160, 221, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
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

  # Helper function to check if die roll matches consequent
  die_matches <- function(roll, cons) {
    if (cons == "even") {
      return(roll %% 2 == 0)
    } else if (cons == "odd") {
      return(roll %% 2 == 1)
    } else if (cons == "gt4") {
      return(roll > 4)
    } else {
      return(roll == as.numeric(cons))
    }
  }

  # Helper function to check if card matches consequent
  card_matches <- function(rank, suit, cons, antecedent) {
    if (cons == "ace") return(rank == 1)
    if (cons == "king") return(rank == 13)
    if (cons == "queen") return(rank == 12)
    if (cons == "jack") return(rank == 11)
    if (cons == "heart") return(suit == 2)
    if (cons == "spade") return(suit == 1)
    if (cons == "red") return(suit %in% c(2, 3))
    if (cons == "even") return(rank %in% c(2, 4, 6, 8, 10))
    if (cons == "better_than_9") return(rank %in% c(1, 10, 11, 12, 13))
    if (cons == "face") return(rank %in% c(11, 12, 13))
    return(FALSE)
  }

  # Cumulative trial tracking
  cumulative_trials <- reactiveVal(0)
  cumulative_successes1 <- reactiveVal(0)
  cumulative_successes2 <- reactiveVal(0)
  cumulative_successes_both <- reactiveVal(0)
  all_frequencies1 <- reactiveVal(numeric(0))
  all_frequencies2 <- reactiveVal(numeric(0))
  all_frequencies_both <- reactiveVal(numeric(0))

  # Track if 10000 trials button has been used
  used_10000 <- reactiveVal(FALSE)

  observeEvent(input$ex4_mode, {
    current_mode(input$ex4_mode)

    # Update tab styling
    runjs("$('.mode-tab').removeClass('active');")
    if (input$ex4_mode == "theoretical") {
      runjs("$('#theoretical-tab').addClass('active');")
      # Sync FROM empirical TO theoretical
      if (!is.null(input$ex4_antecedent_emp)) {
        updateSelectInput(session, "ex4_antecedent", selected = input$ex4_antecedent_emp)
      }
      if (!is.null(input$ex4_cons1_emp)) {
        updateSelectInput(session, "ex4_cons1", selected = input$ex4_cons1_emp)
      }
      if (!is.null(input$ex4_cons2_emp)) {
        updateSelectInput(session, "ex4_cons2", selected = input$ex4_cons2_emp)
      }
    } else {
      runjs("$('#empirical-tab').addClass('active');")
      # Sync FROM theoretical TO empirical
      if (!is.null(input$ex4_antecedent)) {
        updateSelectInput(session, "ex4_antecedent_emp", selected = input$ex4_antecedent)
      }
      if (!is.null(input$ex4_cons1)) {
        updateSelectInput(session, "ex4_cons1_emp", selected = input$ex4_cons1)
      }
      if (!is.null(input$ex4_cons2)) {
        updateSelectInput(session, "ex4_cons2_emp", selected = input$ex4_cons2)
      }
    }
  })

  # Reset button handler
  observeEvent(input$ex4_reset, {
    cumulative_trials(0)
    cumulative_successes1(0)
    cumulative_successes2(0)
    cumulative_successes_both(0)
    all_frequencies1(numeric(0))
    all_frequencies2(numeric(0))
    all_frequencies_both(numeric(0))
    used_10000(FALSE)
  })

  output$ex4_content <- renderUI({
    mode <- current_mode()

    if (mode == "theoretical") {
      # Theoretical mode
      div(
        uiOutput("ex4_independence_message"),

        fluidRow(
          column(4,
                 h5("Setup:"),

                 selectInput("ex4_antecedent",
                            span(class = "hl-antecedent", "Antecedent:"),
                            choices = c(
                              "Roll a standard die" = "die",
                              "Draw from a well shuffled standard deck" = "standard",
                              "Draw from a well shuffled Piquet Pack (7-A)" = "piquet"
                            ),
                            selected = "standard"),

                 uiOutput("ex4_cons1_ui"),
                 uiOutput("ex4_cons2_ui"),

                 hr(),
                 uiOutput("ex4_theoretical_calc")
          ),

          column(8,
                 plotOutput("ex4_theoretical_plot", height = "500px")
          )
        )
      )
    } else {
      # Empirical mode
      div(
        p(class = "key-insight",
          strong("Empirical View: "),
          "Run trials to see how the long-run frequencies converge. For independent events, P(Both) ≈ P(B) × P(C)."
        ),

        uiOutput("ex4_independence_message_emp"),

        # Show empirical formula here
        uiOutput("ex4_empirical_formula_display"),

        fluidRow(
          column(4,
                 h5("Setup:"),

                 selectInput("ex4_antecedent_emp",
                            span(class = "hl-antecedent", "Antecedent:"),
                            choices = c(
                              "Roll a standard die" = "die",
                              "Draw from a well shuffled standard deck" = "standard",
                              "Draw from a well shuffled Piquet Pack (7-A)" = "piquet"
                            ),
                            selected = "standard"),

                 uiOutput("ex4_cons1_ui_emp"),
                 uiOutput("ex4_cons2_ui_emp"),

                 hr(),
                 actionButton("ex4_run_sim", "Run 10 More Trials",
                             class = "btn-primary btn-lg",
                             style = "width: 100%; margin-bottom: 10px;"),
                 actionButton("ex4_run_10000", "Run 10,000 Trials",
                             class = "btn-info btn-lg",
                             style = "width: 100%; margin-bottom: 10px;"),
                 actionButton("ex4_reset", "Reset",
                             class = "btn-warning btn-lg",
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

  # Independence message
  output$ex4_independence_message <- renderUI({
    req(input$ex4_antecedent, input$ex4_cons1, input$ex4_cons2)

    p(class = "key-insight",
      strong("Theoretical View: "),
      "The ",
      span(class = "hl-consequent-a", "pink cells"),
      " and ",
      span(class = "hl-consequent-b", "blue cells"),
      " show outcomes where each consequent occurs. The ",
      span(class = "hl-both", "purple cells"),
      " show where both occur. For independent events, the purple region is proportional to the product of the individual probabilities."
    )
  })

  output$ex4_independence_message_emp <- renderUI({
    req(input$ex4_antecedent_emp, input$ex4_cons1_emp, input$ex4_cons2_emp)

    if (cumulative_trials() > 0) {
      p(class = "independence-note",
        strong("Independence Check: "),
        "The purple line (both) should converge to the product of the pink and blue lines' theoretical values, demonstrating independence."
      )
    } else {
      NULL
    }
  })

  # Dynamic consequent UIs for theoretical mode
  output$ex4_cons1_ui <- renderUI({
    req(input$ex4_antecedent)

    if (input$ex4_antecedent == "die") {
      selectInput("ex4_cons1",
                 span(class = "hl-consequent-a", "First consequent:"),
                 choices = c("Shows a 6" = "6",
                           "Shows a 1" = "1",
                           "Shows a 5" = "5",
                           "Shows an even number" = "even",
                           "Shows an odd number" = "odd",
                           "Greater than 4" = "gt4"),
                 selected = "even")
    } else {
      # Cards
      selectInput("ex4_cons1",
                 span(class = "hl-consequent-a", "First consequent:"),
                 choices = c("Card is an Ace" = "ace",
                           "Card is a King" = "king",
                           "Card is a Heart" = "heart",
                           "Card is red" = "red",
                           "Card is even (2,4,6,8,10)" = "even",
                           "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                           "Card is a face card (J,Q,K)" = "face"),
                 selected = "red")
    }
  })

  output$ex4_cons2_ui <- renderUI({
    req(input$ex4_antecedent)

    if (input$ex4_antecedent == "die") {
      selectInput("ex4_cons2",
                 span(class = "hl-consequent-b", "Second consequent:"),
                 choices = c("Shows a 2" = "2",
                           "Shows a 3" = "3",
                           "Shows a 4" = "4",
                           "Shows an even number" = "even",
                           "Shows an odd number" = "odd",
                           "Greater than 4" = "gt4"),
                 selected = "gt4")
    } else {
      # Cards
      selectInput("ex4_cons2",
                 span(class = "hl-consequent-b", "Second consequent:"),
                 choices = c("Card is a Queen" = "queen",
                           "Card is a Jack" = "jack",
                           "Card is a Spade" = "spade",
                           "Card is red" = "red",
                           "Card is even (2,4,6,8,10)" = "even",
                           "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                           "Card is a face card (J,Q,K)" = "face"),
                 selected = "face")
    }
  })

  # Dynamic consequent UIs for empirical mode
  output$ex4_cons1_ui_emp <- renderUI({
    req(input$ex4_antecedent_emp)

    if (input$ex4_antecedent_emp == "die") {
      selectInput("ex4_cons1_emp",
                 span(class = "hl-consequent-a", "First consequent:"),
                 choices = c("Shows a 6" = "6",
                           "Shows a 1" = "1",
                           "Shows a 5" = "5",
                           "Shows an even number" = "even",
                           "Shows an odd number" = "odd",
                           "Greater than 4" = "gt4"),
                 selected = "even")
    } else {
      # Cards
      selectInput("ex4_cons1_emp",
                 span(class = "hl-consequent-a", "First consequent:"),
                 choices = c("Card is an Ace" = "ace",
                           "Card is a King" = "king",
                           "Card is a Heart" = "heart",
                           "Card is red" = "red",
                           "Card is even (2,4,6,8,10)" = "even",
                           "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                           "Card is a face card (J,Q,K)" = "face"),
                 selected = "red")
    }
  })

  output$ex4_cons2_ui_emp <- renderUI({
    req(input$ex4_antecedent_emp)

    if (input$ex4_antecedent_emp == "die") {
      selectInput("ex4_cons2_emp",
                 span(class = "hl-consequent-b", "Second consequent:"),
                 choices = c("Shows a 2" = "2",
                           "Shows a 3" = "3",
                           "Shows a 4" = "4",
                           "Shows an even number" = "even",
                           "Shows an odd number" = "odd",
                           "Greater than 4" = "gt4"),
                 selected = "gt4")
    } else {
      # Cards
      selectInput("ex4_cons2_emp",
                 span(class = "hl-consequent-b", "Second consequent:"),
                 choices = c("Card is a Queen" = "queen",
                           "Card is a Jack" = "jack",
                           "Card is a Spade" = "spade",
                           "Card is red" = "red",
                           "Card is even (2,4,6,8,10)" = "even",
                           "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                           "Card is a face card (J,Q,K)" = "face"),
                 selected = "face")
    }
  })

  # Theoretical plot
  output$ex4_theoretical_plot <- renderPlot({
    req(input$ex4_antecedent)

    if (input$ex4_antecedent == "die") {
      req(input$ex4_cons1, input$ex4_cons2)

      cons1 <- input$ex4_cons1
      cons2 <- input$ex4_cons2

      par(mar = c(4, 4, 3, 2))
      plot(NULL, xlim = c(0.5, 6.5), ylim = c(0.5, 1.5),
           xlab = "", ylab = "", axes = FALSE, asp = 1,
           main = "Antecedent Space: All Possible Die Outcomes")

      # Draw antecedent background
      rect(0.5, 0.5, 6.5, 1.5, col = rgb(1, 0.95, 0.8, 0.6), border = "black", lwd = 2)

      # Draw cells
      for (i in 1:6) {
        match1 <- die_matches(i, cons1)
        match2 <- die_matches(i, cons2)

        col <- if (match1 && match2) {
          rgb(0.87, 0.63, 0.87, 0.7)  # Purple for both (good!)
        } else if (match1) {
          rgb(1, 0.71, 0.76, 0.7)  # Pink
        } else if (match2) {
          rgb(0.68, 0.85, 0.90, 0.7)  # Blue
        } else {
          "white"
        }
        rect(i - 0.4, 1 - 0.4, i + 0.4, 1 + 0.4,
             col = col, border = "black", lwd = 2)
        text(i, 1, as.character(i), cex = 1.5, font = 2)
      }

    } else {
      # Cards
      req(input$ex4_cons1, input$ex4_cons2)

      antecedent <- input$ex4_antecedent

      if (antecedent == "piquet") {
        ranks <- c("7", "8", "9", "10", "J", "Q", "K", "A")
        n_ranks <- 8
      } else {
        ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
        n_ranks <- 13
      }

      suits <- c("♠", "♥", "♦", "♣")

      par(mar = c(4, 5, 3, 2))
      plot(NULL, xlim = c(0.5, n_ranks + 0.5), ylim = c(0.5, 4.5),
           xlab = "", ylab = "", axes = FALSE, asp = 1,
           main = "Antecedent Space: All Possible Cards")

      # Draw antecedent background
      rect(0.5, 0.5, n_ranks + 0.5, 4.5, col = rgb(1, 0.95, 0.8, 0.6), border = "black", lwd = 2)

      cons1 <- input$ex4_cons1
      cons2 <- input$ex4_cons2

      # Draw cards
      for (s in 1:4) {
        for (r in 1:n_ranks) {
          is_red <- s %in% c(2, 3)
          rank <- ranks[r]
          suit <- suits[s]

          # Convert rank string to numeric for checking
          rank_num <- if (rank == "A") 1
                      else if (rank == "J") 11
                      else if (rank == "Q") 12
                      else if (rank == "K") 13
                      else as.numeric(rank)

          match1 <- card_matches(rank_num, s, cons1, antecedent)
          match2 <- card_matches(rank_num, s, cons2, antecedent)

          col <- if (match1 && match2) {
            rgb(0.87, 0.63, 0.87, 0.7)  # Purple for both (good!)
          } else if (match1) {
            rgb(1, 0.71, 0.76, 0.7)  # Pink
          } else if (match2) {
            rgb(0.68, 0.85, 0.90, 0.7)  # Blue
          } else {
            "white"
          }

          rect(r - 0.45, s - 0.45, r + 0.45, s + 0.45,
               col = col, border = "black", lwd = 0.5)

          suit_col <- if (is_red) "red" else "black"
          text(r, s, paste0(rank, suit), cex = 0.7, col = suit_col, font = 2)
        }
      }

      # Axes
      axis(1, at = 1:n_ranks, labels = ranks, tick = FALSE, line = -1)
      axis(2, at = 1:4, labels = c("Spades", "Hearts", "Diamonds", "Clubs"),
           tick = FALSE, las = 1, line = -1)
    }
  })

  # Helper to count matches for consequents
  count_matches <- function(antecedent, cons) {
    if (antecedent == "die") {
      return(sum(sapply(1:6, function(r) die_matches(r, cons))))
    } else {
      # Cards
      if (antecedent == "piquet") {
        ranks <- rep(c(7:10, 11, 12, 13, 1), 4)
      } else {
        ranks <- rep(c(1:10, 11, 12, 13), 4)
      }
      suits <- rep(1:4, each = ifelse(antecedent == "piquet", 8, 13))

      return(sum(mapply(function(r, s) card_matches(r, s, cons, antecedent), ranks, suits)))
    }
  }

  # Theoretical calculation
  output$ex4_theoretical_calc <- renderUI({
    req(input$ex4_antecedent, input$ex4_cons1, input$ex4_cons2)

    antecedent <- input$ex4_antecedent
    cons1 <- input$ex4_cons1
    cons2 <- input$ex4_cons2

    total <- if (antecedent == "die") 6 else if (antecedent == "piquet") 32 else 52

    n1 <- count_matches(antecedent, cons1)
    n2 <- count_matches(antecedent, cons2)

    p1 <- n1/total
    p2 <- n2/total

    # For independence check: count both
    if (antecedent == "die") {
      n_both <- sum(sapply(1:6, function(r) die_matches(r, cons1) && die_matches(r, cons2)))
    } else {
      if (antecedent == "piquet") {
        ranks <- rep(c(7:10, 11, 12, 13, 1), 4)
      } else {
        ranks <- rep(c(1:10, 11, 12, 13), 4)
      }
      suits <- rep(1:4, each = ifelse(antecedent == "piquet", 8, 13))
      n_both <- sum(mapply(function(r, s) {
        card_matches(r, s, cons1, antecedent) && card_matches(r, s, cons2, antecedent)
      }, ranks, suits))
    }

    p_both <- n_both / total
    p_product <- p1 * p2

    div(
      h5("Independence Check:"),
      p("P(A → C) = ", strong(sprintf("%.4f", p2))),
      p("P(A∧B → C) = ", strong(sprintf("%.4f", if (n1 > 0) n_both/n1 else 0))),
      if (abs(p2 - (if (n1 > 0) n_both/n1 else 0)) < 0.01) {
        p(em("(approximately equal - events are independent!)"), style = "color: green;")
      } else {
        p(em("(not equal - events are NOT independent)"), style = "color: orange;")
      },
      hr(),
      h5("Calculation:"),
      p("P(First) = ",
        span(style = "background-color: rgba(255, 182, 193, 0.7); padding: 2px 4px;",
             strong(n1)),
        " / ",
        span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;",
             strong(total)),
        " = ",
        strong(sprintf("%.4f", p1))),
      p("P(Second) = ",
        span(style = "background-color: rgba(173, 216, 230, 0.7); padding: 2px 4px;",
             strong(n2)),
        " / ",
        span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;",
             strong(total)),
        " = ",
        strong(sprintf("%.4f", p2))),
      hr(),
      p("P(Both) = ",
        sprintf("%d/%d", n_both, total),
        " = ",
        span(style = "background-color: rgba(221, 160, 221, 0.5); padding: 2px 4px;",
             strong(sprintf("%.4f", p_both)))),
      p("P(First) × P(Second) = ",
        sprintf("%.4f × %.4f", p1, p2),
        " = ",
        strong(sprintf("%.4f", p_product)))
    )
  })

  # Empirical simulation - cumulative
  observeEvent(input$ex4_run_sim, {
    run_simulation(10)
  })

  observeEvent(input$ex4_run_10000, {
    run_simulation(10000)
    used_10000(TRUE)
  })

  run_simulation <- function(n_new) {
    req(input$ex4_antecedent_emp, input$ex4_cons1_emp, input$ex4_cons2_emp)

    antecedent <- input$ex4_antecedent_emp
    cons1 <- input$ex4_cons1_emp
    cons2 <- input$ex4_cons2_emp

    if (antecedent == "die") {
      rolls <- sample(1:6, n_new, replace = TRUE)

      match1 <- sapply(rolls, function(r) die_matches(r, cons1))
      match2 <- sapply(rolls, function(r) die_matches(r, cons2))
      both <- match1 & match2

    } else {
      # Cards
      if (antecedent == "piquet") {
        ranks <- rep(c(7:10, 11, 12, 13, 1), 4)  # J=11, Q=12, K=13, A=1
      } else {
        ranks <- rep(c(1:10, 11, 12, 13), 4)
      }
      suits <- rep(1:4, each = ifelse(antecedent == "piquet", 8, 13))

      draws <- sample(1:length(ranks), n_new, replace = TRUE)

      match1 <- sapply(draws, function(d) card_matches(ranks[d], suits[d], cons1, antecedent))
      match2 <- sapply(draws, function(d) card_matches(ranks[d], suits[d], cons2, antecedent))
      both <- match1 & match2
    }

    # Update cumulative totals
    cumulative_trials(cumulative_trials() + n_new)
    cumulative_successes1(cumulative_successes1() + sum(match1))
    cumulative_successes2(cumulative_successes2() + sum(match2))
    cumulative_successes_both(cumulative_successes_both() + sum(both))

    # Update running frequencies
    new_freqs1 <- numeric(n_new)
    new_freqs2 <- numeric(n_new)
    new_freqs_both <- numeric(n_new)

    for (i in 1:n_new) {
      trial_num <- cumulative_trials() - n_new + i
      success_count1 <- cumulative_successes1() - sum(match1) + sum(match1[1:i])
      success_count2 <- cumulative_successes2() - sum(match2) + sum(match2[1:i])
      success_count_both <- cumulative_successes_both() - sum(both) + sum(both[1:i])

      new_freqs1[i] <- success_count1 / trial_num
      new_freqs2[i] <- success_count2 / trial_num
      new_freqs_both[i] <- success_count_both / trial_num
    }

    all_frequencies1(c(all_frequencies1(), new_freqs1))
    all_frequencies2(c(all_frequencies2(), new_freqs2))
    all_frequencies_both(c(all_frequencies_both(), new_freqs_both))
  }

  # Disable 10000 button after use
  observe({
    if (used_10000()) {
      shinyjs::disable("ex4_run_10000")
    } else {
      shinyjs::enable("ex4_run_10000")
    }
  })

  output$ex4_empirical_plot <- renderPlot({
    if (cumulative_trials() == 0) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Click 'Run 10 More Trials' to begin")
      text(1, 1, "Awaiting simulation...", cex = 1.5, col = "gray")
    } else {
      antecedent <- input$ex4_antecedent_emp
      cons1 <- input$ex4_cons1_emp
      cons2 <- input$ex4_cons2_emp

      # Calculate theoretical probabilities
      total <- if (antecedent == "die") 6 else if (antecedent == "piquet") 32 else 52

      n1 <- count_matches(antecedent, cons1)
      n2 <- count_matches(antecedent, cons2)

      p1_theory <- n1/total
      p2_theory <- n2/total
      p_product_theory <- p1_theory * p2_theory

      freqs1 <- all_frequencies1()
      freqs2 <- all_frequencies2()
      freqs_both <- all_frequencies_both()
      n <- length(freqs_both)

      par(mar = c(5, 5, 4, 2))
      plot(1:n, freqs_both, type = "l",
           col = "purple", lwd = 3,
           xlab = "Number of Trials", ylab = "Observed Frequency",
           main = "Independent Events: Long-Run Frequencies",
           ylim = c(0, max(1, max(freqs_both, freqs1, freqs2))))

      # Add individual frequencies
      lines(1:n, freqs1, col = rgb(1, 0.71, 0.76), lwd = 2)
      lines(1:n, freqs2, col = rgb(0.68, 0.85, 0.90), lwd = 2)

      # Add theoretical lines
      abline(h = p1_theory, col = rgb(1, 0.4, 0.5), lwd = 2, lty = 2)
      abline(h = p2_theory, col = rgb(0.2, 0.6, 0.8), lwd = 2, lty = 2)
      abline(h = p_product_theory, col = "purple", lwd = 2, lty = 2)

      grid(col = "gray", lty = "dotted")

      legend("topright",
             legend = c("Observed: Both",
                       "Observed: First",
                       "Observed: Second",
                       sprintf("Theory: Both (product) = %.4f", p_product_theory),
                       sprintf("Theory: First = %.4f", p1_theory),
                       sprintf("Theory: Second = %.4f", p2_theory)),
             col = c("purple", rgb(1, 0.71, 0.76), rgb(0.68, 0.85, 0.90),
                    "purple", rgb(1, 0.4, 0.5), rgb(0.2, 0.6, 0.8)),
             lwd = c(3, 2, 2, 2, 2, 2),
             lty = c(1, 1, 1, 2, 2, 2),
             bg = "white",
             cex = 0.8)
    }
  })

  # Display empirical formula at top
  output$ex4_empirical_formula_display <- renderUI({
    if (cumulative_trials() == 0) {
      return(NULL)
    }

    observed_freq_both <- cumulative_successes_both() / cumulative_trials()

    div(class = "formula-box",
        p(
          "P(Both) = ",
          span(style = "background-color: rgba(221, 160, 221, 0.5); padding: 2px 4px;",
               strong(cumulative_successes_both())),
          " / ",
          span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;",
               strong(cumulative_trials())),
          " = ",
          span(style = "background-color: rgba(221, 160, 221, 0.5); padding: 2px 4px;",
               strong(sprintf("%.1f%%", observed_freq_both * 100))),
          style = "margin: 0;"
        )
    )
  })

  output$ex4_empirical_summary <- renderUI({
    if (cumulative_trials() == 0) {
      return(p("Run trials to see results."))
    }

    antecedent <- input$ex4_antecedent_emp
    cons1 <- input$ex4_cons1_emp
    cons2 <- input$ex4_cons2_emp

    # Calculate theoretical probabilities
    total <- if (antecedent == "die") 6 else if (antecedent == "piquet") 32 else 52

    n1 <- count_matches(antecedent, cons1)
    n2 <- count_matches(antecedent, cons2)

    p1_theory <- n1/total
    p2_theory <- n2/total
    p_product_theory <- p1_theory * p2_theory

    observed_freq1 <- cumulative_successes1() / cumulative_trials()
    observed_freq2 <- cumulative_successes2() / cumulative_trials()
    observed_freq_both <- cumulative_successes_both() / cumulative_trials()
    observed_product <- observed_freq1 * observed_freq2

    div(
      h5("Current Results:"),
      p("Total trials: ", strong(cumulative_trials())),
      p("Observed P(Both): ", strong(sprintf("%.4f", observed_freq_both))),
      p("Theoretical (product): ", strong(sprintf("%.4f", p_product_theory))),
      p("Difference: ", strong(sprintf("%.4f", abs(observed_freq_both - p_product_theory)))),
      hr(),
      p("Observed P(First) × P(Second): ", strong(sprintf("%.4f", observed_product)))
    )
  })
}

shinyApp(ui = ui, server = server)
