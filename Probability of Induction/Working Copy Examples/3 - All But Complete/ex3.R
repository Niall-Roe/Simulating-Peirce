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
      .hl-consequent-b { background-color: rgba(255, 182, 193, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-c { background-color: rgba(173, 216, 230, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-both { background-color: rgba(221, 160, 221, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
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
      .toggle-switch {
        margin: 15px 0;
        padding: 10px;
        background-color: #e9ecef;
        border-radius: 4px;
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

  # Cumulative trial tracking
  cumulative_trials <- reactiveVal(0)
  cumulative_successes_b <- reactiveVal(0)
  cumulative_successes_c_given_b <- reactiveVal(0)
  cumulative_successes_both <- reactiveVal(0)
  all_frequencies_b <- reactiveVal(numeric(0))
  all_frequencies_c_given_b <- reactiveVal(numeric(0))
  all_frequencies_both <- reactiveVal(numeric(0))
  used_10000 <- reactiveVal(FALSE)

  observeEvent(input$ex3_mode, {
    current_mode(input$ex3_mode)

    # Update tab styling
    runjs("$('.mode-tab').removeClass('active');")
    if (input$ex3_mode == "theoretical") {
      runjs("$('#theoretical-tab').addClass('active');")
      # Sync FROM empirical TO theoretical
      if (!is.null(input$ex3_antecedent_emp)) {
        updateSelectInput(session, "ex3_antecedent", selected = input$ex3_antecedent_emp)
      }
      if (!is.null(input$ex3_cons_b_emp)) {
        updateSelectInput(session, "ex3_cons_b", selected = input$ex3_cons_b_emp)
      }
      if (!is.null(input$ex3_cons_c_emp)) {
        updateSelectInput(session, "ex3_cons_c", selected = input$ex3_cons_c_emp)
      }
      if (!is.null(input$ex3_use_ab_as_antecedent_emp)) {
        updateCheckboxInput(session, "ex3_use_ab_as_antecedent", value = input$ex3_use_ab_as_antecedent_emp)
      }
      if (!is.null(input$ex3_show_combined_emp)) {
        updateCheckboxInput(session, "ex3_show_combined", value = input$ex3_show_combined_emp)
      }
    } else {
      runjs("$('#empirical-tab').addClass('active');")
      # Sync FROM theoretical TO empirical
      if (!is.null(input$ex3_antecedent)) {
        updateSelectInput(session, "ex3_antecedent_emp", selected = input$ex3_antecedent)
      }
      if (!is.null(input$ex3_cons_b)) {
        updateSelectInput(session, "ex3_cons_b_emp", selected = input$ex3_cons_b)
      }
      if (!is.null(input$ex3_cons_c)) {
        updateSelectInput(session, "ex3_cons_c_emp", selected = input$ex3_cons_c)
      }
      if (!is.null(input$ex3_use_ab_as_antecedent)) {
        updateCheckboxInput(session, "ex3_use_ab_as_antecedent_emp", value = input$ex3_use_ab_as_antecedent)
      }
      if (!is.null(input$ex3_show_combined)) {
        updateCheckboxInput(session, "ex3_show_combined_emp", value = input$ex3_show_combined)
      }
    }
  })

  # Reset button handler
  observeEvent(input$ex3_reset, {
    cumulative_trials(0)
    cumulative_successes_b(0)
    cumulative_successes_c_given_b(0)
    cumulative_successes_both(0)
    all_frequencies_b(numeric(0))
    all_frequencies_c_given_b(numeric(0))
    all_frequencies_both(numeric(0))
    used_10000(FALSE)
  })

  # Helper to evaluate card property
  card_matches_prop <- function(rank, suit, property, antecedent) {
    if (property == "red") return(suit %in% c(2, 3))
    if (property == "ace") return(rank == 1)
    if (property == "king") return(rank == 13)
    if (property == "heart") return(suit == 2)
    if (property == "spade") return(suit == 1)
    if (property == "even") return(rank %in% c(2, 4, 6, 8, 10))
    if (property == "better_than_9") return(rank %in% c(1, 10, 11, 12, 13))
    if (property == "face") return(rank %in% c(11, 12, 13))
    return(FALSE)
  }

  output$ex3_content <- renderUI({
    mode <- current_mode()

    if (mode == "theoretical") {
      # Theoretical mode
      div(
        uiOutput("ex3_theoretical_message"),

        fluidRow(
          column(4,
                 h5("Setup:"),

                 selectInput("ex3_antecedent",
                            span(class = "hl-antecedent", "Antecedent:"),
                            choices = c(
                              "Draw from a well shuffled standard deck" = "standard",
                              "Draw from a well shuffled Piquet Pack (7-A)" = "piquet"
                            )),

                 selectInput("ex3_cons_b",
                            span(class = "hl-consequent-b", "Consequent B:"),
                            choices = c(
                              "Card is red" = "red",
                              "Card is an Ace" = "ace",
                              "Card is a King" = "king",
                              "Card is a Heart" = "heart",
                              "Card is even (2,4,6,8,10)" = "even",
                              "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                              "Card is a face card (J,Q,K)" = "face"
                            ),
                            selected = "red"),

                 div(class = "toggle-switch",
                     checkboxInput("ex3_use_ab_as_antecedent",
                                  strong("Treat A ∧ B as new Antecedent"),
                                  value = FALSE)
                 ),

                 conditionalPanel(
                   condition = "input.ex3_use_ab_as_antecedent == true",
                   selectInput("ex3_cons_c",
                              span(class = "hl-consequent-c", "Consequent C:"),
                              choices = c(
                                "Card is red" = "red",
                                "Card is an Ace" = "ace",
                                "Card is a King" = "king",
                                "Card is a Heart" = "heart",
                                "Card is even (2,4,6,8,10)" = "even",
                                "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                                "Card is a face card (J,Q,K)" = "face"
                              ),
                              selected = "face"),

                   div(class = "toggle-switch",
                       checkboxInput("ex3_show_combined",
                                    strong("Show P(A → B∧C) view"),
                                    value = FALSE)
                   )
                 ),

                 hr(),
                 uiOutput("ex3_theoretical_calc")
          ),

          column(8,
                 plotOutput("ex3_theoretical_plot", height = "500px")
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

        uiOutput("ex3_empirical_formula_display"),

        fluidRow(
          column(4,
                 h5("Setup:"),

                 selectInput("ex3_antecedent_emp",
                            span(class = "hl-antecedent", "Antecedent:"),
                            choices = c(
                              "Draw from a well shuffled standard deck" = "standard",
                              "Draw from a well shuffled Piquet Pack (7-A)" = "piquet"
                            )),

                 selectInput("ex3_cons_b_emp",
                            span(class = "hl-consequent-b", "Consequent B:"),
                            choices = c(
                              "Card is red" = "red",
                              "Card is an Ace" = "ace",
                              "Card is a King" = "king",
                              "Card is a Heart" = "heart",
                              "Card is even (2,4,6,8,10)" = "even",
                              "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                              "Card is a face card (J,Q,K)" = "face"
                            ),
                            selected = "red"),

                 div(class = "toggle-switch",
                     checkboxInput("ex3_use_ab_as_antecedent_emp",
                                  strong("Treat A ∧ B as new Antecedent"),
                                  value = FALSE)
                 ),

                 conditionalPanel(
                   condition = "input.ex3_use_ab_as_antecedent_emp == true",
                   selectInput("ex3_cons_c_emp",
                              span(class = "hl-consequent-c", "Consequent C:"),
                              choices = c(
                                "Card is red" = "red",
                                "Card is an Ace" = "ace",
                                "Card is a King" = "king",
                                "Card is a Heart" = "heart",
                                "Card is even (2,4,6,8,10)" = "even",
                                "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                                "Card is a face card (J,Q,K)" = "face"
                              ),
                              selected = "face"),

                   div(class = "toggle-switch",
                       checkboxInput("ex3_show_combined_emp",
                                    strong("Show P(A → B∧C) view"),
                                    value = FALSE)
                   )
                 ),

                 hr(),
                 actionButton("ex3_run_sim", "Run 10 More Trials",
                             class = "btn-primary btn-lg",
                             style = "width: 100%; margin-bottom: 10px;"),
                 actionButton("ex3_run_10000", "Run 10,000 Trials",
                             class = "btn-info btn-lg",
                             style = "width: 100%; margin-bottom: 10px;"),
                 actionButton("ex3_reset", "Reset",
                             class = "btn-warning btn-lg",
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

  # Theoretical message
  output$ex3_theoretical_message <- renderUI({
    req(input$ex3_antecedent, input$ex3_cons_b)

    if (isTRUE(input$ex3_use_ab_as_antecedent)) {
      req(input$ex3_cons_c)

      if (isTRUE(input$ex3_show_combined)) {
        p(class = "key-insight",
          strong("P(A → B∧C) View: "),
          "Starting from the full antecedent space (", span(class = "hl-antecedent", "pale yellow"), "), ",
          span(class = "hl-consequent-b", "pink cells"), " show where B is true, ",
          span(class = "hl-consequent-c", " blue cells"), " show where C is true, and ",
          span(class = "hl-both", "purple cells"), " show where both B and C are true."
        )
      } else {
        p(class = "key-insight",
          strong("A∧B as Antecedent: "),
          "The antecedent space is now only the ", span(class = "hl-antecedent", "pale yellow"),
          " cards where both A and B are true. Within this space, ",
          span(class = "hl-consequent-c", "blue cells"), " show where C is also true."
        )
      }
    } else {
      p(class = "key-insight",
        strong("A → B View: "),
        "The ", span(class = "hl-antecedent", "pale yellow background"), " shows all possible cards (antecedent A). ",
        "The ", span(class = "hl-consequent-b", "pink cells"), " show where consequent B is true."
      )
    }
  })

  # Theoretical plot
  output$ex3_theoretical_plot <- renderPlot({
    req(input$ex3_antecedent, input$ex3_cons_b)

    antecedent <- input$ex3_antecedent
    cons_b <- input$ex3_cons_b

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
         main = "Card Space")

    # Determine what to show
    use_ab <- isTRUE(input$ex3_use_ab_as_antecedent)
    show_combined <- isTRUE(input$ex3_show_combined)

    if (use_ab && !is.null(input$ex3_cons_c)) {
      cons_c <- input$ex3_cons_c

      if (show_combined) {
        # Show P(A → B∧C) view: full antecedent with B (pink), C (blue), both (purple)
        rect(0.5, 0.5, n_ranks + 0.5, 4.5, col = rgb(1, 0.95, 0.8, 0.6), border = "black", lwd = 2)

        for (s in 1:4) {
          for (r in 1:n_ranks) {
            is_red <- s %in% c(2, 3)
            rank <- ranks[r]
            suit <- suits[s]

            rank_num <- if (rank == "A") 1
                        else if (rank == "J") 11
                        else if (rank == "Q") 12
                        else if (rank == "K") 13
                        else as.numeric(rank)

            match_b <- card_matches_prop(rank_num, s, cons_b, antecedent)
            match_c <- card_matches_prop(rank_num, s, cons_c, antecedent)

            col <- if (match_b && match_c) {
              rgb(0.87, 0.63, 0.87, 0.7)  # Purple for both
            } else if (match_b) {
              rgb(1, 0.71, 0.76, 0.7)  # Pink for B
            } else if (match_c) {
              rgb(0.68, 0.85, 0.90, 0.7)  # Blue for C
            } else {
              "white"
            }

            rect(r - 0.45, s - 0.45, r + 0.45, s + 0.45,
                 col = col, border = "black", lwd = 0.5)

            suit_col <- if (is_red) "red" else "black"
            text(r, s, paste0(rank, suit), cex = 0.7, col = suit_col, font = 2)
          }
        }
      } else {
        # Show A∧B as antecedent view: only highlight B cards, then show C within them
        for (s in 1:4) {
          for (r in 1:n_ranks) {
            is_red <- s %in% c(2, 3)
            rank <- ranks[r]
            suit <- suits[s]

            rank_num <- if (rank == "A") 1
                        else if (rank == "J") 11
                        else if (rank == "Q") 12
                        else if (rank == "K") 13
                        else as.numeric(rank)

            match_b <- card_matches_prop(rank_num, s, cons_b, antecedent)
            match_c <- card_matches_prop(rank_num, s, cons_c, antecedent)

            # Background: pale yellow only for B cards
            bg_col <- if (match_b) rgb(1, 0.95, 0.8, 0.6) else "white"

            # Cell color: blue if C is true within B, else just background
            col <- if (match_b && match_c) {
              rgb(0.68, 0.85, 0.90, 0.7)  # Blue for C within B
            } else {
              bg_col
            }

            rect(r - 0.45, s - 0.45, r + 0.45, s + 0.45,
                 col = col, border = "black", lwd = 0.5)

            suit_col <- if (is_red) "red" else "black"
            text(r, s, paste0(rank, suit), cex = 0.7, col = suit_col, font = 2)
          }
        }
      }
    } else {
      # Show A → B view: full antecedent background, highlight B
      rect(0.5, 0.5, n_ranks + 0.5, 4.5, col = rgb(1, 0.95, 0.8, 0.6), border = "black", lwd = 2)

      for (s in 1:4) {
        for (r in 1:n_ranks) {
          is_red <- s %in% c(2, 3)
          rank <- ranks[r]
          suit <- suits[s]

          rank_num <- if (rank == "A") 1
                      else if (rank == "J") 11
                      else if (rank == "Q") 12
                      else if (rank == "K") 13
                      else as.numeric(rank)

          match_b <- card_matches_prop(rank_num, s, cons_b, antecedent)

          col <- if (match_b) {
            rgb(1, 0.71, 0.76, 0.7)  # Pink for B
          } else {
            "white"
          }

          rect(r - 0.45, s - 0.45, r + 0.45, s + 0.45,
               col = col, border = "black", lwd = 0.5)

          suit_col <- if (is_red) "red" else "black"
          text(r, s, paste0(rank, suit), cex = 0.7, col = suit_col, font = 2)
        }
      }
    }

    axis(1, at = 1:n_ranks, labels = ranks, tick = FALSE, line = -1)
    axis(2, at = 1:4, labels = c("Spades", "Hearts", "Diamonds", "Clubs"),
         tick = FALSE, las = 1, line = -1)
  })

  # Theoretical calculation
  output$ex3_theoretical_calc <- renderUI({
    req(input$ex3_antecedent, input$ex3_cons_b)

    antecedent <- input$ex3_antecedent
    cons_b <- input$ex3_cons_b
    total <- if (antecedent == "piquet") 32 else 52

    # Count matches for B
    if (antecedent == "piquet") {
      ranks <- rep(c(7:10, 11, 12, 13, 1), 4)
    } else {
      ranks <- rep(c(1:10, 11, 12, 13), 4)
    }
    suits <- rep(1:4, each = ifelse(antecedent == "piquet", 8, 13))

    n_b <- sum(mapply(function(r, s) card_matches_prop(r, s, cons_b, antecedent), ranks, suits))
    p_b <- n_b / total

    use_ab <- isTRUE(input$ex3_use_ab_as_antecedent)

    if (use_ab && !is.null(input$ex3_cons_c)) {
      cons_c <- input$ex3_cons_c

      # Count matches for C within B
      n_both <- sum(mapply(function(r, s) {
        card_matches_prop(r, s, cons_b, antecedent) && card_matches_prop(r, s, cons_c, antecedent)
      }, ranks, suits))

      p_c_given_ab <- if (n_b > 0) n_both / n_b else 0
      p_both <- n_both / total

      div(
        h5("Calculation:"),
        p("P(A → B) = ",
          span(style = "background-color: rgba(255, 182, 193, 0.7); padding: 2px 4px;",
               strong(n_b)),
          " / ",
          span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;",
               strong(total)),
          " = ",
          strong(sprintf("%.4f", p_b))),
        p("P(A∧B → C) = ",
          span(style = "background-color: rgba(221, 160, 221, 0.5); padding: 2px 4px;",
               strong(n_both)),
          " / ",
          span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;",
               strong(n_b)),
          " = ",
          strong(sprintf("%.4f", p_c_given_ab))),
        hr(),
        p("P(A → B∧C) = P(A → B) × P(A∧B → C)"),
        p(" = ",
          sprintf("%.4f", p_b),
          " × ",
          sprintf("%.4f", p_c_given_ab),
          " = ",
          span(style = "background-color: rgba(221, 160, 221, 0.5); padding: 2px 4px;",
               strong(sprintf("%.4f", p_both))))
      )
    } else {
      div(
        h5("Calculation:"),
        p("P(A → B) = ",
          span(style = "background-color: rgba(255, 182, 193, 0.7); padding: 2px 4px;",
               strong(n_b)),
          " / ",
          span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;",
               strong(total)),
          " = ",
          strong(sprintf("%.4f", p_b)))
      )
    }
  })

  # Empirical simulation - cumulative
  observeEvent(input$ex3_run_sim, {
    run_simulation(10)
  })

  observeEvent(input$ex3_run_10000, {
    run_simulation(10000)
    used_10000(TRUE)
  })

  run_simulation <- function(n_new) {
    req(input$ex3_antecedent_emp, input$ex3_cons_b_emp)

    antecedent <- input$ex3_antecedent_emp
    cons_b <- input$ex3_cons_b_emp

    # Generate cards
    if (antecedent == "piquet") {
      ranks <- rep(c(7:10, 11, 12, 13, 1), 4)
    } else {
      ranks <- rep(c(1:10, 11, 12, 13), 4)
    }
    suits <- rep(1:4, each = ifelse(antecedent == "piquet", 8, 13))

    draws <- sample(1:length(ranks), n_new, replace = TRUE)

    match_b <- sapply(draws, function(d) card_matches_prop(ranks[d], suits[d], cons_b, antecedent))

    use_ab <- isTRUE(input$ex3_use_ab_as_antecedent_emp)

    if (use_ab && !is.null(input$ex3_cons_c_emp)) {
      cons_c <- input$ex3_cons_c_emp
      match_c <- sapply(draws, function(d) card_matches_prop(ranks[d], suits[d], cons_c, antecedent))
      match_both <- match_b & match_c

      # Update cumulative totals
      cumulative_trials(cumulative_trials() + n_new)
      cumulative_successes_b(cumulative_successes_b() + sum(match_b))
      cumulative_successes_both(cumulative_successes_both() + sum(match_both))

      # For P(A∧B → C), we need count among B trials
      if (sum(match_b) > 0) {
        cumulative_successes_c_given_b(cumulative_successes_c_given_b() + sum(match_c[match_b]))
      }

      # Update running frequencies
      new_freqs_b <- numeric(n_new)
      new_freqs_both <- numeric(n_new)

      for (i in 1:n_new) {
        trial_num <- cumulative_trials() - n_new + i
        success_count_b <- cumulative_successes_b() - sum(match_b) + sum(match_b[1:i])
        success_count_both <- cumulative_successes_both() - sum(match_both) + sum(match_both[1:i])

        new_freqs_b[i] <- success_count_b / trial_num
        new_freqs_both[i] <- success_count_both / trial_num
      }

      all_frequencies_b(c(all_frequencies_b(), new_freqs_b))
      all_frequencies_both(c(all_frequencies_both(), new_freqs_both))

    } else {
      # Just tracking B
      cumulative_trials(cumulative_trials() + n_new)
      cumulative_successes_b(cumulative_successes_b() + sum(match_b))

      new_freqs_b <- numeric(n_new)
      for (i in 1:n_new) {
        trial_num <- cumulative_trials() - n_new + i
        success_count_b <- cumulative_successes_b() - sum(match_b) + sum(match_b[1:i])
        new_freqs_b[i] <- success_count_b / trial_num
      }

      all_frequencies_b(c(all_frequencies_b(), new_freqs_b))
    }
  }

  # Disable 10000 button after use
  observe({
    if (used_10000()) {
      shinyjs::disable("ex3_run_10000")
    } else {
      shinyjs::enable("ex3_run_10000")
    }
  })

  output$ex3_empirical_plot <- renderPlot({
    if (cumulative_trials() == 0) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Click 'Run 10 More Trials' to begin")
      text(1, 1, "Awaiting simulation...", cex = 1.5, col = "gray")
    } else {
      antecedent <- input$ex3_antecedent_emp
      cons_b <- input$ex3_cons_b_emp

      # Calculate theoretical probabilities
      total <- if (antecedent == "piquet") 32 else 52

      if (antecedent == "piquet") {
        ranks <- rep(c(7:10, 11, 12, 13, 1), 4)
      } else {
        ranks <- rep(c(1:10, 11, 12, 13), 4)
      }
      suits <- rep(1:4, each = ifelse(antecedent == "piquet", 8, 13))

      n_b <- sum(mapply(function(r, s) card_matches_prop(r, s, cons_b, antecedent), ranks, suits))
      p_b_theory <- n_b / total

      use_ab <- isTRUE(input$ex3_use_ab_as_antecedent_emp)

      freqs_b <- all_frequencies_b()
      n <- length(freqs_b)

      par(mar = c(5, 5, 4, 2))

      if (use_ab && !is.null(input$ex3_cons_c_emp)) {
        cons_c <- input$ex3_cons_c_emp

        n_both <- sum(mapply(function(r, s) {
          card_matches_prop(r, s, cons_b, antecedent) && card_matches_prop(r, s, cons_c, antecedent)
        }, ranks, suits))

        p_c_given_ab_theory <- if (n_b > 0) n_both / n_b else 0
        p_both_theory <- n_both / total

        freqs_both <- all_frequencies_both()
        n_both_len <- length(freqs_both)

        # Use the minimum length to avoid mismatch errors
        n_plot <- min(n, n_both_len)

        if (n_plot > 0) {
          plot(1:n_plot, freqs_both[1:n_plot], type = "l",
               col = "purple", lwd = 3,
               xlab = "Number of Trials", ylab = "Observed Frequency",
               main = "Multiplication Rule: Long-Run Frequencies",
               ylim = c(0, max(1, max(c(freqs_b[1:n_plot], freqs_both[1:n_plot])))))

          lines(1:n_plot, freqs_b[1:n_plot], col = rgb(1, 0.71, 0.76), lwd = 2)
        } else {
          plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "",
               main = "Run trials with current settings")
          text(1, 1, "No data for current configuration...", cex = 1.5, col = "gray")
        }

        abline(h = p_b_theory, col = rgb(1, 0.4, 0.5), lwd = 2, lty = 2)
        abline(h = p_both_theory, col = "purple", lwd = 2, lty = 2)

        grid(col = "gray", lty = "dotted")

        legend("topright",
               legend = c("Observed: Both B and C",
                         "Observed: Just B",
                         sprintf("Theory: Both = %.4f", p_both_theory),
                         sprintf("Theory: Just B = %.4f", p_b_theory)),
               col = c("purple", rgb(1, 0.71, 0.76),
                      "purple", rgb(1, 0.4, 0.5)),
               lwd = c(3, 2, 2, 2),
               lty = c(1, 1, 2, 2),
               bg = "white",
               cex = 0.8)
      } else {
        plot(1:n, freqs_b, type = "l",
             col = rgb(1, 0.71, 0.76), lwd = 3,
             xlab = "Number of Trials", ylab = "Observed Frequency",
             main = "Long-Run Frequency of B",
             ylim = c(0, max(1, max(freqs_b))))

        abline(h = p_b_theory, col = rgb(1, 0.4, 0.5), lwd = 2, lty = 2)

        grid(col = "gray", lty = "dotted")

        legend("topright",
               legend = c("Observed: B",
                         sprintf("Theory: B = %.4f", p_b_theory)),
               col = c(rgb(1, 0.71, 0.76), rgb(1, 0.4, 0.5)),
               lwd = c(3, 2),
               lty = c(1, 2),
               bg = "white",
               cex = 0.8)
      }
    }
  })

  output$ex3_empirical_formula_display <- renderUI({
    if (cumulative_trials() == 0) {
      return(NULL)
    }

    use_ab <- isTRUE(input$ex3_use_ab_as_antecedent_emp)

    if (use_ab && !is.null(input$ex3_cons_c_emp)) {
      observed_freq_both <- cumulative_successes_both() / cumulative_trials()

      div(class = "formula-box",
          p(
            "P(A → B∧C) = ",
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
    } else {
      NULL
    }
  })

  output$ex3_empirical_summary <- renderUI({
    if (cumulative_trials() == 0) {
      return(p("Run trials to see results."))
    }

    antecedent <- input$ex3_antecedent_emp
    cons_b <- input$ex3_cons_b_emp

    total <- if (antecedent == "piquet") 32 else 52

    if (antecedent == "piquet") {
      ranks <- rep(c(7:10, 11, 12, 13, 1), 4)
    } else {
      ranks <- rep(c(1:10, 11, 12, 13), 4)
    }
    suits <- rep(1:4, each = ifelse(antecedent == "piquet", 8, 13))

    n_b <- sum(mapply(function(r, s) card_matches_prop(r, s, cons_b, antecedent), ranks, suits))
    p_b_theory <- n_b / total

    use_ab <- isTRUE(input$ex3_use_ab_as_antecedent_emp)

    if (use_ab && !is.null(input$ex3_cons_c_emp)) {
      cons_c <- input$ex3_cons_c_emp

      n_both <- sum(mapply(function(r, s) {
        card_matches_prop(r, s, cons_b, antecedent) && card_matches_prop(r, s, cons_c, antecedent)
      }, ranks, suits))

      p_c_given_ab_theory <- if (n_b > 0) n_both / n_b else 0
      p_both_theory <- n_both / total

      observed_freq_both <- cumulative_successes_both() / cumulative_trials()

      div(
        h5("Current Results:"),
        p("Total trials: ", strong(cumulative_trials())),
        p("Successes (Both): ", strong(cumulative_successes_both())),
        p("Observed frequency: ", strong(sprintf("%.4f", observed_freq_both))),
        p("Theoretical (product): ",
          strong(sprintf("%.4f × %.4f = %.4f",
                        p_b_theory, p_c_given_ab_theory, p_both_theory))),
        p("Difference: ", strong(sprintf("%.4f", abs(observed_freq_both - p_both_theory))))
      )
    } else {
      observed_freq_b <- cumulative_successes_b() / cumulative_trials()

      div(
        h5("Current Results:"),
        p("Total trials: ", strong(cumulative_trials())),
        p("Successes (B): ", strong(cumulative_successes_b())),
        p("Observed frequency: ", strong(sprintf("%.4f", observed_freq_b))),
        p("Theoretical: ", strong(sprintf("%.4f", p_b_theory))),
        p("Difference: ", strong(sprintf("%.4f", abs(observed_freq_b - p_b_theory))))
      )
    }
  })
}

shinyApp(ui = ui, server = server)
