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
      .hl-consequent { background-color: rgba(173, 216, 230, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequence { background-color: rgba(144, 238, 144, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
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
        display: flex;
        gap: 10px;
        margin: 15px 0;
      }
      .toggle-btn {
        padding: 8px 16px;
        background-color: #e9ecef;
        border: 2px solid #dee2e6;
        border-radius: 5px;
        cursor: pointer;
        transition: all 0.2s;
        font-weight: 600;
      }
      .toggle-btn.active {
        background-color: #2c7fb8;
        color: white;
        border-color: #2c7fb8;
      }
      .deck-question {
        margin: 15px 0;
        padding: 12px;
        background-color: #fff3cd;
        border-radius: 4px;
        border-left: 4px solid #ffc107;
      }
      .deck-question-buttons {
        display: flex;
        gap: 10px;
        margin-top: 10px;
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
            tags$li(span(class = "hl-consequence", "CONSEQUENCE"), " = The rule connecting them: \"If ANTECEDENT, then CONSEQUENT\"")
          ),

          p("The ", strong("probability of the consequence"), " is the ratio:"),
          div(class = "formula-box",
              "P = (times both occur) / (times antecedent occurs)"
          ),
          div(class = "formula-box",
              "P(A → C) = # (A ∩ C) / # (A)"
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
  observeEvent(input$toggle_ex1, {
    shinyjs::toggle("example-1")
  })

  # Mode state
  current_mode <- reactiveVal("theoretical")

  # Card deck type
  current_deck <- reactiveVal("standard")

  # New deck knowledge state
  knows_top_card <- reactiveVal(NULL)

  # Cumulative trial tracking
  cumulative_trials <- reactiveVal(0)
  cumulative_successes <- reactiveVal(0)
  all_frequencies <- reactiveVal(numeric(0))

  # Track if 10000 trials button has been used
  used_10000 <- reactiveVal(FALSE)

  observeEvent(input$ex1_mode, {
    current_mode(input$ex1_mode)

    # Update tab styling
    runjs("$('.mode-tab').removeClass('active');")
    if (input$ex1_mode == "theoretical") {
      runjs("$('#theoretical-tab').addClass('active');")
      # Sync FROM empirical TO theoretical
      if (!is.null(input$ex1_antecedent_emp)) {
        updateSelectInput(session, "ex1_antecedent", selected = input$ex1_antecedent_emp)
      }
      if (!is.null(input$ex1_consequent_emp)) {
        updateSelectInput(session, "ex1_consequent", selected = input$ex1_consequent_emp)
      }
    } else {
      runjs("$('#empirical-tab').addClass('active');")
      # Sync FROM theoretical TO empirical
      if (!is.null(input$ex1_antecedent)) {
        updateSelectInput(session, "ex1_antecedent_emp", selected = input$ex1_antecedent)
      }
      if (!is.null(input$ex1_consequent)) {
        updateSelectInput(session, "ex1_consequent_emp", selected = input$ex1_consequent)
      }
    }
  })

  # Reset knowledge when changing antecedent
  observeEvent(input$ex1_antecedent, {
    if (!is.null(input$ex1_antecedent) && input$ex1_antecedent != "new") {
      knows_top_card(NULL)
    }
  })

  observeEvent(input$ex1_antecedent_emp, {
    if (!is.null(input$ex1_antecedent_emp) && input$ex1_antecedent_emp != "new") {
      knows_top_card(NULL)
    }
  })

  # New deck question handlers
  observeEvent(input$knows_card_yes, {
    knows_top_card(TRUE)
  })

  observeEvent(input$knows_card_no, {
    knows_top_card(FALSE)
  })

  # Reset button handler
  observeEvent(input$ex1_reset, {
    cumulative_trials(0)
    cumulative_successes(0)
    all_frequencies(numeric(0))
    used_10000(FALSE)
  })

  output$ex1_content <- renderUI({
    mode <- current_mode()

    if (mode == "theoretical") {
      # Theoretical mode
      div(
        p(class = "key-insight",
          strong("Theoretical View: "),
          "This shows all possible outcomes in the ",
          span(class = "hl-antecedent", "antecedent space (pale yellow)"),
          ". The ",
          span(class = "hl-consequence", "green cells"),
          " represent outcomes where both ",
          span(class = "hl-antecedent", "antecedent"),
          " and ",
          span(class = "hl-consequent", "consequent"),
          " occur."
        ),

        fluidRow(
          column(4,
                 h5("Setup:"),

                 uiOutput("ex1_antecedent_ui"),
                 uiOutput("ex1_consequent_ui"),

                 hr(),
                 uiOutput("ex1_theoretical_calc")
          ),

          column(8,
                 plotOutput("ex1_theoretical_plot", height = "500px")
          )
        )
      )
    } else {
      # Empirical mode
      div(
        p(class = "key-insight",
          strong("Empirical View: "),
          "Run trials to see how the long-run frequency converges to the theoretical probability. Each click adds 10 more trials."
        ),

        # Show empirical formula here
        uiOutput("ex1_empirical_formula_display"),

        fluidRow(
          column(4,
                 h5("Setup:"),

                 uiOutput("ex1_antecedent_ui_emp"),
                 uiOutput("ex1_consequent_ui_emp"),

                 hr(),
                 actionButton("ex1_run_sim", "Run 10 More Trials",
                             class = "btn-primary btn-lg",
                             style = "width: 100%; margin-bottom: 10px;"),
                 actionButton("ex1_run_10000", "Run 10,000 Trials",
                             class = "btn-info btn-lg",
                             style = "width: 100%; margin-bottom: 10px;"),
                 actionButton("ex1_reset", "Reset",
                             class = "btn-warning btn-lg",
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

  # Dynamic antecedent UI for theoretical mode
  output$ex1_antecedent_ui <- renderUI({
    selectInput("ex1_antecedent",
                span(class = "hl-antecedent", "Antecedent: Draw the top card from a"),
                choices = c(
                  "Well shuffled standard deck" = "standard",
                  "Well shuffled Piquet Pack (7-A)" = "piquet",
                  "Unshuffled new deck" = "new",
                  "None" = "none"
                ),
                selected = "standard")
  })

  # Dynamic consequent UI for theoretical mode
  output$ex1_consequent_ui <- renderUI({
    req(input$ex1_antecedent)

    if (input$ex1_antecedent == "none") {
      return(NULL)
    }

    if (input$ex1_antecedent == "new") {
      # Show question about knowing the top card
      div(
        selectInput("ex1_consequent",
                    span(class = "hl-consequent", "Consequent:"),
                    choices = c(
                      "Card is red" = "red",
                      "Ace of Spades" = "ace_spades",
                      "Less than 5 (2-5)" = "less_5",
                      "Card is a face card" = "face"
                    ),
                    selected = "face"),
        uiOutput("ex1_new_deck_question")
      )
    } else {
      # Standard or Piquet deck
      selectInput("ex1_consequent",
                  span(class = "hl-consequent", "Consequent:"),
                  choices = c(
                    "Card is red" = "red",
                    "Ace of Spades" = "ace_spades",
                    "Less than 5 (2-5)" = "less_5",
                    "Card is a face card" = "face"
                  ),
                  selected = "face")
    }
  })

  # Empirical mode antecedent UI
  output$ex1_antecedent_ui_emp <- renderUI({
    selectInput("ex1_antecedent_emp",
                span(class = "hl-antecedent", "Antecedent: Draw the top card from a"),
                choices = c(
                  "Well shuffled standard deck" = "standard",
                  "Well shuffled Piquet Pack (7-A)" = "piquet",
                  "Unshuffled new deck" = "new",
                  "None" = "none"
                ),
                selected = "standard")
  })

  # Empirical mode consequent UI
  output$ex1_consequent_ui_emp <- renderUI({
    req(input$ex1_antecedent_emp)

    if (input$ex1_antecedent_emp == "none") {
      return(NULL)
    }

    selectInput("ex1_consequent_emp",
                span(class = "hl-consequent", "Consequent:"),
                choices = c(
                  "Card is red" = "red",
                  "Ace of Spades" = "ace_spades",
                  "Less than 5 (2-5)" = "less_5",
                  "Card is a face card" = "face"
                ),
                selected = "face")
  })

  # New deck question
  output$ex1_new_deck_question <- renderUI({
    req(input$ex1_antecedent)

    if (input$ex1_antecedent == "new" && is.null(knows_top_card())) {
      div(
        class = "deck-question",
        p(strong("Do you know which card is on top in a new deck order?"), style = "margin: 0 0 10px 0;"),
        div(class = "deck-question-buttons",
            actionButton("knows_card_yes", "Yes", class = "btn-success"),
            actionButton("knows_card_no", "No", class = "btn-secondary")
        )
      )
    } else if (input$ex1_antecedent == "new" && isTRUE(knows_top_card())) {
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #d4edda; border-radius: 4px; border-left: 4px solid #28a745;",
        p(strong("Note:"), " In a new unshuffled deck, the Ace of Spades is on top.", style = "margin: 0;")
      )
    }
  })

  # Theoretical plot
  output$ex1_theoretical_plot <- renderPlot({
    req(input$ex1_antecedent)

    # Check for none
    if (input$ex1_antecedent == "none") {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Undefined Probability")
      text(1, 1, "No antecedent space:\nProbability is undefined", cex = 1.5, col = "red", font = 2)
      return()
    }

    req(input$ex1_consequent)

    antecedent <- input$ex1_antecedent
    consequent <- input$ex1_consequent

    # Check for new deck without knowledge
    if (antecedent == "new" && is.null(knows_top_card())) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
      text(1, 1, "Please answer the question\nabout the new deck", cex = 1.5, col = "gray", font = 2)
      return()
    }

    if (antecedent == "new" && isFALSE(knows_top_card())) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Undefined Probability")
      text(1, 1, "No antecedent space:\nProbability is undefined", cex = 1.5, col = "red", font = 2)
      return()
    }

    # Card visualization
    if (antecedent == "new" && consequent != "ace_spades") {
        # New deck but not asking about ace of spades
        par(mar = c(4, 2, 3, 2))
        plot(NULL, xlim = c(0, 2), ylim = c(0, 2), axes = FALSE, xlab = "", ylab = "",
             main = "New Unshuffled Deck")

        # Draw single card (pale yellow for antecedent)
        rect(0.5, 0.7, 1.5, 1.7, col = rgb(1, 0.95, 0.8, 0.8), border = "black", lwd = 3)
        text(1, 1.2, "A♠", cex = 3, font = 2)
        text(1, 0.3, "Consequent does not occur", cex = 1.2, font = 2)

      return()
    }

    if (antecedent == "new" && consequent == "ace_spades") {
        # New deck asking about ace of spades
        par(mar = c(4, 2, 3, 2))
        plot(NULL, xlim = c(0, 2), ylim = c(0, 2), axes = FALSE, xlab = "", ylab = "",
             main = "New Unshuffled Deck")

        # Draw single card in green (both occur)
        rect(0.5, 0.7, 1.5, 1.7, col = rgb(0.56, 0.93, 0.56, 0.7), border = "black", lwd = 3)
      text(1, 1.2, "A♠", cex = 3, font = 2)
      text(1, 0.3, "Certain outcome: P = 1", cex = 1.2, font = 2, col = "darkgreen")

      return()
    }

    # Determine deck composition
    if (antecedent == "piquet") {
      ranks <- c("7", "8", "9", "10", "J", "Q", "K", "A")
      n_ranks <- 8
      total_cards <- 32
    } else {
      ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
      n_ranks <- 13
      total_cards <- 52
    }

    suits <- c("♠", "♥", "♦", "♣")

    # Horizontal orientation: suits as rows, ranks as columns
    par(mar = c(4, 5, 3, 2))
    plot(NULL, xlim = c(0.5, n_ranks + 0.5), ylim = c(0.5, 4.5),
         xlab = "", ylab = "", axes = FALSE, asp = 1,
         main = paste0("Antecedent Space: All ", total_cards, " Cards"))

    # Draw antecedent background (pale yellow)
    rect(0.5, 0.5, n_ranks + 0.5, 4.5, col = rgb(1, 0.95, 0.8, 0.6), border = "black", lwd = 2)

    # Draw cards - suits are rows, ranks are columns
    for (s in 1:4) {
      for (r in 1:n_ranks) {
        is_red <- s %in% c(2, 3)
        rank_val <- ranks[r]

        match <- FALSE
        if (consequent == "red" && is_red) match <- TRUE
        if (consequent == "ace_spades" && s == 1 && rank_val == "A") match <- TRUE
        if (consequent == "less_5") {
          if (antecedent == "piquet") {
            match <- FALSE  # No cards less than 5 in piquet
          } else {
            match <- rank_val %in% c("2", "3", "4", "5")
          }
        }
        if (consequent == "face" && rank_val %in% c("J", "Q", "K")) match <- TRUE

        # Color: green if match (both occur), pale blue if consequent space, white otherwise
        if (match) {
          col <- rgb(0.56, 0.93, 0.56, 0.7)  # Green for both
        } else {
          # Check if in consequent space
          in_consequent <- FALSE
          if (consequent == "red" && is_red) in_consequent <- TRUE
          if (consequent == "ace_spades" && s == 1 && rank_val == "A") in_consequent <- TRUE
          if (consequent == "less_5" && antecedent != "piquet" && rank_val %in% c("2", "3", "4", "5")) in_consequent <- TRUE
          if (consequent == "face" && rank_val %in% c("J", "Q", "K")) in_consequent <- TRUE

          col <- if (in_consequent) rgb(0.68, 0.85, 0.9, 0.6) else "white"  # Pale blue for consequent
        }

        rect(r - 0.45, s - 0.45, r + 0.45, s + 0.45,
             col = col, border = "black", lwd = 0.5)

        suit_col <- if (is_red) "red" else "black"
        text(r, s, paste0(rank_val, suits[s]), cex = 0.7, col = suit_col, font = 2)
      }
    }

    axis(1, at = 1:n_ranks, labels = ranks, tick = FALSE, line = -1)
    axis(2, at = 1:4, labels = c("Spades", "Hearts", "Diamonds", "Clubs"),
         tick = FALSE, las = 1, line = -1)
  })

  # Theoretical calculation
  output$ex1_theoretical_calc <- renderUI({
    req(input$ex1_antecedent)

    antecedent <- input$ex1_antecedent

    # Check for none
    if (antecedent == "none") {
      return(div(
        h5("Calculation:"),
        p(style = "color: red;", strong("Probability is UNDEFINED")),
        p("Probability is a property of an antecedent and a consequent. It is undefined without an antecedent.")
      ))
    }

    req(input$ex1_consequent)
    consequent <- input$ex1_consequent

    # Check new deck knowledge
    if (antecedent == "new" && is.null(knows_top_card())) {
      return(NULL)  # Don't show calculation until they answer
    }

    if (antecedent == "new" && isFALSE(knows_top_card())) {
      return(div(
        h5("Calculation:"),
        p(style = "color: red;", strong("Probability is UNDEFINED")),
        p("Probability is a property of an antecedent and a consequent. It is undefined without an antecedent.")
      ))
    }

    # Card calculations
    if (antecedent == "new" && consequent != "ace_spades") {
      return(div(
        h5("Calculation:"),
        p("Known card: ", span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;", strong("1"))),
        p("Both occur: ", span(style = "background-color: rgba(144, 238, 144, 0.7); padding: 2px 4px;", strong("0"))),
        p("Probability = ",
          span(style = "background-color: rgba(173, 216, 230, 0.7); padding: 2px 4px;", strong("0")),
          " / ",
          span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;", strong("1")),
          " = ",
          span(style = "background-color: rgba(144, 238, 144, 0.7); padding: 2px 4px;", strong("0.0000")))
      ))
    }

    if (antecedent == "new" && consequent == "ace_spades") {
      return(div(
        h5("Calculation:"),
        p("Known card: ", span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;", strong("1"))),
        p("Both occur: ", span(style = "background-color: rgba(144, 238, 144, 0.7); padding: 2px 4px;", strong("1"))),
        p("Probability = ",
          span(style = "background-color: rgba(173, 216, 230, 0.7); padding: 2px 4px;", strong("1")),
          " / ",
          span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;", strong("1")),
          " = ",
          span(style = "background-color: rgba(144, 238, 144, 0.7); padding: 2px 4px;", strong("1.0000")))
      ))
    }

    # Calculate for regular decks
    total <- if (antecedent == "piquet") 32 else 52

    if (consequent == "red") {
      favorable <- total / 2
    } else if (consequent == "ace_spades") {
      favorable <- 1
    } else if (consequent == "less_5") {
      favorable <- if (antecedent == "piquet") 0 else 16
    } else {
      favorable <- if (antecedent == "piquet") 12 else 12
    }

    prob <- favorable / total

    div(
      h5("Calculation:"),
      p("Total antecedent outcomes: ", span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;", strong(total))),
      p("Both occur: ", span(style = "background-color: rgba(144, 238, 144, 0.7); padding: 2px 4px;", strong(favorable))),
      p("Probability = ",
        span(style = "background-color: rgba(173, 216, 230, 0.7); padding: 2px 4px;", strong(favorable)),
        " / ",
        span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;", strong(total)),
        " = ",
        span(style = "background-color: rgba(144, 238, 144, 0.7); padding: 2px 4px;", strong(sprintf("%.4f", prob))))
    )
  })

  # Empirical simulation - cumulative
  observeEvent(input$ex1_run_sim, {
    run_simulation(10)
  })

  observeEvent(input$ex1_run_10000, {
    run_simulation(10000)
    used_10000(TRUE)
  })

  run_simulation <- function(n_new) {
    req(input$ex1_antecedent_emp, input$ex1_consequent_emp)

    antecedent <- input$ex1_antecedent_emp
    consequent <- input$ex1_consequent_emp

    if (antecedent == "none") return()

    # Simulate card draws
    if (antecedent == "piquet") {
      ranks <- rep(c(7:10, 11, 12, 13, 1), 4)  # J=11, Q=12, K=13, A=1
    } else {
      ranks <- rep(c(1:10, 11, 12, 13), 4)
    }
    suits <- rep(1:4, each = ifelse(antecedent == "piquet", 8, 13))

    draws <- sample(1:length(ranks), n_new, replace = TRUE)

    if (consequent == "red") {
      new_successes <- suits[draws] %in% c(2, 3)
    } else if (consequent == "ace_spades") {
      new_successes <- (ranks[draws] == 1 & suits[draws] == 1)
    } else if (consequent == "less_5") {
      new_successes <- ranks[draws] %in% 2:5
    } else {
      new_successes <- ranks[draws] %in% c(11, 12, 13)
    }

    # Update cumulative totals
    cumulative_trials(cumulative_trials() + n_new)
    cumulative_successes(cumulative_successes() + sum(new_successes))

    # Update running frequencies
    new_freqs <- numeric(n_new)
    for (i in 1:n_new) {
      trial_num <- cumulative_trials() - n_new + i
      success_count <- cumulative_successes() - sum(new_successes) + sum(new_successes[1:i])
      new_freqs[i] <- success_count / trial_num
    }

    all_frequencies(c(all_frequencies(), new_freqs))
  }

  # Disable 10000 button after use
  observe({
    if (used_10000()) {
      shinyjs::disable("ex1_run_10000")
    } else {
      shinyjs::enable("ex1_run_10000")
    }
  })

  output$ex1_empirical_plot <- renderPlot({
    if (cumulative_trials() == 0) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Click 'Run 10 More Trials' to begin")
      text(1, 1, "Awaiting simulation...", cex = 1.5, col = "gray")
    } else {
      antecedent <- input$ex1_antecedent_emp
      consequent <- input$ex1_consequent_emp

      # Calculate theoretical probability
      total <- if (antecedent == "piquet") 32 else 52
      if (consequent == "red") theoretical_p <- 0.5
      else if (consequent == "ace_spades") theoretical_p <- 1/total
      else if (consequent == "less_5") theoretical_p <- if (antecedent == "piquet") 0 else 16/52
      else theoretical_p <- 12/total

      freqs <- all_frequencies()
      n <- length(freqs)

      par(mar = c(5, 5, 4, 2))
      plot(1:n, freqs, type = "l",
           col = "#2c7fb8", lwd = 2,
           xlab = "Number of Trials", ylab = "Observed Frequency",
           main = "Long-Run Frequency Converging to Theoretical Probability",
           ylim = c(0, max(1, max(freqs))))

      abline(h = theoretical_p, col = "red", lwd = 2, lty = 2)
      grid(col = "gray", lty = "dotted")

      legend("topright",
             legend = c("Observed Frequency",
                       sprintf("Theoretical P = %.4f", theoretical_p)),
             col = c("#2c7fb8", "red"),
             lwd = 2, lty = c(1, 2),
             bg = "white")
    }
  })

  # Display empirical formula at top
  output$ex1_empirical_formula_display <- renderUI({
    if (cumulative_trials() == 0) {
      return(NULL)
    }

    antecedent <- input$ex1_antecedent_emp
    consequent <- input$ex1_consequent_emp

    # Get antecedent description
    ant_desc <- switch(antecedent,
                       "standard" = "Draw the top card from a well shuffled standard deck",
                       "piquet" = "Draw the top card from a well shuffled Piquet Pack",
                       "new" = "Draw the top card from an unshuffled new deck")

    # Get consequent description
    cons_desc <- switch(consequent,
                        "red" = "That card is red",
                        "ace_spades" = "That card is the Ace of Spades",
                        "less_5" = "That card is less than 5 (2-5)",
                        "face" = "That card is a face card")

    observed_freq <- cumulative_successes() / cumulative_trials()

    div(class = "formula-box",
        p(
          "P(", ant_desc, " → ", cons_desc, ") = ",
          span(style = "background-color: rgba(173, 216, 230, 0.7); padding: 2px 4px;",
               strong(cumulative_successes())),
          " / ",
          span(style = "background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px;",
               strong(cumulative_trials())),
          " = ",
          span(style = "background-color: rgba(144, 238, 144, 0.7); padding: 2px 4px;",
               strong(sprintf("%.1f%%", observed_freq * 100))),
          style = "margin: 0;"
        )
    )
  })

  output$ex1_empirical_summary <- renderUI({
    if (cumulative_trials() == 0) {
      return(p("Run trials to see results."))
    }

    antecedent <- input$ex1_antecedent_emp
    consequent <- input$ex1_consequent_emp

    # Calculate theoretical probability
    total <- if (antecedent == "piquet") 32 else 52
    if (consequent == "red") theoretical_p <- 0.5
    else if (consequent == "ace_spades") theoretical_p <- 1/total
    else if (consequent == "less_5") theoretical_p <- if (antecedent == "piquet") 0 else 16/52
    else theoretical_p <- 12/total

    observed_freq <- cumulative_successes() / cumulative_trials()

    div(
      h5("Current Results:"),
      p("Total trials: ", strong(cumulative_trials())),
      p("Successes: ", strong(cumulative_successes())),
      p("Observed frequency: ", strong(sprintf("%.4f", observed_freq))),
      p("Theoretical: ", strong(sprintf("%.4f", theoretical_p))),
      p("Difference: ", strong(sprintf("%.4f", abs(observed_freq - theoretical_p))))
    )
  })
}

shinyApp(ui = ui, server = server)
