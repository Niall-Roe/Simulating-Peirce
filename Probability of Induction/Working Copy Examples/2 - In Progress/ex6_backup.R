library(shiny)
library(shinyjs)


# Reference text for this example (Peirce's description):
# Suppose that we have two rules of inference, such that, of all the questions to the solution of which both can be applied,
# the first yields correct answers to 81/100, and incorrect answers to the remaining 19/100; while the second yields correct
# answers to 93/100, and incorrect answers to the remaining 7/100. Suppose, further, that the two rules are entirely independent
# as to their truth, so that the second answers correctly 93/100 of the questions which the first answers correctly, and also
# 93/100 of the questions which the first answers incorrectly, and answers incorrectly the remaining 7/100 of the questions which
# the first answers correctly, and also the remaining 7/100 of the questions which the first answers incorrectly.
#
# This is, therefore, the probability that, if both modes of inference yield the same result, that result is correct.


ui <- fluidPage(
  useShinyjs(),
  withMathJax(),

  tags$head(
    tags$style(HTML("
      body { background-color: #f4f1ea; font-family: 'Georgia', serif; color: #2c2c2c; }
      .article-container {
        max-width: 1200px;
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
        margin: 20px 0;
        padding: 15px;
        background-color: #f0f0f0;
        border-radius: 6px;
      }
      .metal-box {
        width: 120px;
        height: 120px;
        border: 4px solid #333;
        border-radius: 8px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 2em;
        font-weight: bold;
        margin: 10px auto;
      }
      .metal-box.gold { background-color: #d4af37; color: #000; }
      .metal-box.lead { background-color: #808080; color: #fff; }
      .metal-box.opaque { background-color: #555; color: #fff; }
    "))
  ),

  div(class = "article-container",
      h3("Example 6: Independent Inference Rules"),

      p("Suppose that we have two rules of inference, such that, of all the questions to the solution of which both can be applied, ",
        span(class = "example-trigger", id = "ex6-trigger",
             onclick = "Shiny.setInputValue('toggle_ex6', Math.random());",
             "[Click to see interactive example]"),
        " the first yields correct answers to 81/100, and incorrect answers to the remaining 19/100; while the second yields correct answers to 93/100, and incorrect answers to the remaining 7/100. Suppose, further, that the two rules are entirely independent as to their truth, so that the second answers correctly 93/100 of the questions which the first answers correctly, and also 93/100 of the questions which the first answers incorrectly, and answers incorrectly the remaining 7/100 of the questions which the first answers correctly, and also the remaining 7/100 of the questions which the first answers incorrectly."
      ),

      div(id = "example-6", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Combining Independent Inference Rules"),

          p(class = "key-insight",
            strong("Scenario: "),
            "There is a piece of metal in a box. It is either ", strong("gold"), " (yellow & heavy) or ", strong("lead"), " (grey & light). You have two friends who can make observations. Friend 1 judges by ", strong("color"), " (says 'gold' if yellow). Friend 2 judges by ", strong("weight"), " (says 'gold' if heavy). Each friend is an independent inference rule with known accuracy."
          ),

          hr(),

          fluidRow(
            column(6,
                   h5("Friend 1: Judges by Color"),
                   sliderInput("ex6_r", "Accuracy of Friend 1 (correct answers):",
                              min = 0.5, max = 1, value = 0.81, step = 0.01),
                   p("Friend 1 is correct ", strong(textOutput("ex6_r_display", inline = TRUE)), " of the time.")
            ),
            column(6,
                   h5("Friend 2: Judges by Weight"),
                   sliderInput("ex6_s", "Accuracy of Friend 2 (correct answers):",
                              min = 0.5, max = 1, value = 0.93, step = 0.01),
                   p("Friend 2 is correct ", strong(textOutput("ex6_s_display", inline = TRUE)), " of the time.")
            )
          ),

          hr(),

          div(class = "mode-tabs",
              tags$button(class = "mode-tab active", id = "ex6-mode-tab-known",
                         onclick = "Shiny.setInputValue('ex6_mode_switch', 'known');",
                         "Known Metal"),
              tags$button(class = "mode-tab", id = "ex6-mode-tab-unknown",
                         onclick = "Shiny.setInputValue('ex6_mode_switch', 'unknown');",
                         "Unknown Metal")
          ),

          # Known Metal Mode
          div(id = "ex6-mode-known",
              h4("Mode 1: You Know What's In The Box"),

              p("Select what metal is in the box, then see how often each friend gives the correct answer:"),

              fluidRow(
                column(6,
                       radioButtons("ex6_true_metal", "What is actually in the box?",
                                   choices = c("Gold" = "gold", "Lead" = "lead"),
                                   selected = "gold", inline = TRUE)
                ),
                column(6,
                       uiOutput("ex6_metal_box")
                )
              ),

              hr(),

              div(class = "mode-tabs",
                  tags$button(class = "mode-tab active", id = "ex6-submode-tab-expectation",
                             onclick = "Shiny.setInputValue('ex6_submode_switch', 'expectation');",
                             "Expectation"),
                  tags$button(class = "mode-tab", id = "ex6-submode-tab-trial",
                             onclick = "Shiny.setInputValue('ex6_submode_switch', 'trial');",
                             "Trial")
              ),

              # Expectation sub-mode
              div(id = "ex6-submode-expectation",
                  p(class = "key-insight", "This shows the ", strong("expected"), " distribution: exactly the proportion you'd expect in the long run."),

                  actionButton("ex6_simplify_exp", "Simplify (Order Cells)", class = "btn-secondary", style = "margin-bottom: 15px;"),

                  fluidRow(
                    column(6,
                           h5("Friend 1's Judgments", style = "text-align: center;"),
                           plotOutput("ex6_known_friend1_exp", height = "250px"),
                           uiOutput("ex6_known_friend1_desc_exp")
                    ),
                    column(6,
                           h5("Friend 2's Judgments", style = "text-align: center;"),
                           plotOutput("ex6_known_friend2_exp", height = "250px"),
                           uiOutput("ex6_known_friend2_desc_exp")
                    )
                  ),

                  hr(),

                  h5("Combined Performance: All Possible Outcomes"),
                  p("When both friends judge the same piece of metal, there are four possible combinations of their answers:"),

                  actionButton("ex6_toggle_decompose_exp", "Decompose Grid", class = "btn-info", style = "margin-bottom: 15px;"),

                  uiOutput("ex6_combined_grid_exp")
              ),

              # Trial sub-mode
              div(id = "ex6-submode-trial", style = "display: none;",
                  p(class = "key-insight", "This shows ", strong("actual samples"), ": asking each friend 100 times. Results may vary from expected due to randomness."),

                  actionButton("ex6_run_100", "Ask Friends 100 Times", class = "btn-primary", style = "margin-right: 10px;"),
                  actionButton("ex6_run_10000", "Ask Friends 10,000 Times", class = "btn-success"),

                  uiOutput("ex6_trial_note"),

                  fluidRow(
                    column(6,
                           h5("Friend 1's Judgments", style = "text-align: center;"),
                           plotOutput("ex6_known_friend1_trial", height = "250px"),
                           uiOutput("ex6_known_friend1_desc_trial")
                    ),
                    column(6,
                           h5("Friend 2's Judgments", style = "text-align: center;"),
                           plotOutput("ex6_known_friend2_trial", height = "250px"),
                           uiOutput("ex6_known_friend2_desc_trial")
                    )
                  ),

                  hr(),

                  h5("Combined Performance: All Possible Outcomes"),
                  actionButton("ex6_toggle_decompose_trial", "Decompose Grid", class = "btn-info", style = "margin-bottom: 15px;"),
                  uiOutput("ex6_combined_grid_trial")
              )
          ),

          # Unknown Metal Mode
          div(id = "ex6-mode-unknown", style = "display: none;",
              h4("Mode 2: You Don't Know What's In The Box"),

              p("You can't see the metal, but you can ask your friends. What do they say?"),

              fluidRow(
                column(3,
                       uiOutput("ex6_metal_box_opaque")
                ),
                column(5,
                       radioButtons("ex6_answer_mode", "What do the friends say?",
                                   choices = c("Random (generate independently)" = "random",
                                             "Both say 'Gold'" = "both_gold",
                                             "Both say 'Lead'" = "both_lead",
                                             "They disagree" = "disagree"),
                                   selected = "random", inline = FALSE),

                       conditionalPanel(
                         condition = "input.ex6_answer_mode == 'random'",
                         actionButton("ex6_generate", "Generate New Answers", class = "btn-primary")
                       )
                ),
                column(4,
                       uiOutput("ex6_agreement_formula")
                )
              ),

              hr(),

              h5("What Your Friends Say:"),
              uiOutput("ex6_friend_answers"),

              hr(),

              actionButton("ex6_reveal", "Reveal What's In The Box", class = "btn-warning"),

              uiOutput("ex6_revelation"),

              hr(),

              h5("How Does This Answer Relate To The Possible Outcomes?"),
              p("Based on what the friends said, we can reason about how likely different scenarios are:"),
              plotOutput("ex6_unknown_analysis", height = "400px"),
              uiOutput("ex6_unknown_explanation")
          )
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex6, { shinyjs::toggle("example-6") })

  # Random answers for unknown mode
  random_state <- reactiveVal(list(metal = "gold", f1 = "gold", f2 = "gold"))
  revelation_shown <- reactiveVal(FALSE)

  # Trial mode results
  trial_results <- reactiveVal(NULL)
  decomposed_exp <- reactiveVal(FALSE)
  decomposed_trial <- reactiveVal(FALSE)
  simplified_exp <- reactiveVal(FALSE)

  observeEvent(input$ex6_generate, {
    r <- input$ex6_r
    s <- input$ex6_s

    # Randomly choose the true metal
    true_metal <- sample(c("gold", "lead"), 1)

    # Friends respond based on their accuracy
    f1_correct <- runif(1) < r
    f2_correct <- runif(1) < s

    f1_answer <- if (f1_correct) true_metal else ifelse(true_metal == "gold", "lead", "gold")
    f2_answer <- if (f2_correct) true_metal else ifelse(true_metal == "gold", "lead", "gold")

    random_state(list(metal = true_metal, f1 = f1_answer, f2 = f2_answer))
    revelation_shown(FALSE)
  })

  observeEvent(input$ex6_reveal, {
    revelation_shown(TRUE)
  })

  # Run 100 trials
  observeEvent(input$ex6_run_100, {
    r <- input$ex6_r
    s <- input$ex6_s

    f1_correct <- sum(runif(100) < r)
    f2_correct <- sum(runif(100) < s)

    trial_results(list(f1 = f1_correct, f2 = f2_correct, n = 100))
  })

  # Run 10,000 trials
  observeEvent(input$ex6_run_10000, {
    r <- input$ex6_r
    s <- input$ex6_s

    f1_correct <- sum(runif(10000) < r)
    f2_correct <- sum(runif(10000) < s)

    trial_results(list(f1 = round(f1_correct/100), f2 = round(f2_correct/100), n = 10000))
  })

  observeEvent(input$ex6_toggle_decompose_exp, {
    decomposed_exp(!decomposed_exp())
  })

  observeEvent(input$ex6_toggle_decompose_trial, {
    decomposed_trial(!decomposed_trial())
  })

  observeEvent(input$ex6_simplify_exp, {
    simplified_exp(!simplified_exp())
  })

  # Mode switching
  observeEvent(input$ex6_mode_switch, {
    if (input$ex6_mode_switch == "known") {
      shinyjs::show("ex6-mode-known")
      shinyjs::hide("ex6-mode-unknown")
      shinyjs::removeClass("ex6-mode-tab-unknown", "active")
      shinyjs::addClass("ex6-mode-tab-known", "active")
    } else {
      shinyjs::hide("ex6-mode-known")
      shinyjs::show("ex6-mode-unknown")
      shinyjs::removeClass("ex6-mode-tab-known", "active")
      shinyjs::addClass("ex6-mode-tab-unknown", "active")
      revelation_shown(FALSE)
    }
  })

  # Sub-mode switching
  observeEvent(input$ex6_submode_switch, {
    if (input$ex6_submode_switch == "expectation") {
      shinyjs::show("ex6-submode-expectation")
      shinyjs::hide("ex6-submode-trial")
      shinyjs::removeClass("ex6-submode-tab-trial", "active")
      shinyjs::addClass("ex6-submode-tab-expectation", "active")
    } else {
      shinyjs::hide("ex6-submode-expectation")
      shinyjs::show("ex6-submode-trial")
      shinyjs::removeClass("ex6-submode-tab-expectation", "active")
      shinyjs::addClass("ex6-submode-tab-trial", "active")
      trial_results(NULL)
    }
  })

  # Display accuracy percentages
  output$ex6_r_display <- renderText({
    sprintf("%.0f%%", input$ex6_r * 100)
  })

  output$ex6_s_display <- renderText({
    sprintf("%.0f%%", input$ex6_s * 100)
  })

  # Metal box display
  output$ex6_metal_box <- renderUI({
    true_metal <- input$ex6_true_metal
    box_class <- if(true_metal == "gold") "metal-box gold" else "metal-box lead"
    box_text <- if(true_metal == "gold") "GOLD" else "LEAD"

    div(
      div(class = box_class, box_text)
    )
  })

  output$ex6_metal_box_opaque <- renderUI({
    # If revealed and in random mode, show the actual metal
    if (revelation_shown() && input$ex6_answer_mode == "random") {
      state <- random_state()
      true_metal <- state$metal
      box_class <- if(true_metal == "gold") "metal-box gold" else "metal-box lead"
      box_text <- if(true_metal == "gold") "GOLD" else "LEAD"
      div(
        div(class = box_class, box_text)
      )
    } else {
      div(
        div(class = "metal-box opaque", "?")
      )
    }
  })

  # Helper function to draw 10x10 grid (scattered or ordered)
  draw_grid <- function(prob, correct_color, incorrect_color, title = "", ordered = FALSE) {
    n_correct <- round(prob * 100)
    n_incorrect <- 100 - n_correct

    par(mar = c(1, 1, 2, 1))
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = title)

    # Create a vector of colors and optionally shuffle it
    colors <- c(rep(correct_color, n_correct), rep(incorrect_color, n_incorrect))
    if (!ordered) {
      colors <- sample(colors)
    }

    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        rect(col, row, col + 1, row + 1, col = colors[count], border = "grey30", lwd = 0.5)
        count <- count + 1
      }
    }

    text(5, -0.8, sprintf("%d correct, %d incorrect", n_correct, n_incorrect),
         cex = 1.1, font = 2, xpd = TRUE)
  }

  # Expectation Mode: Friend 1
  output$ex6_known_friend1_exp <- renderPlot({
    r <- input$ex6_r
    true_metal <- input$ex6_true_metal
    ordered <- simplified_exp()

    if (true_metal == "gold") {
      draw_grid(r, "#d4af37", "#808080", "Friend 1 says 'Gold' vs 'Lead'", ordered)
    } else {
      draw_grid(r, "#808080", "#d4af37", "Friend 1 says 'Lead' vs 'Gold'", ordered)
    }
  })

  output$ex6_known_friend1_desc_exp <- renderUI({
    r <- input$ex6_r
    true_metal <- input$ex6_true_metal
    n_correct <- round(r * 100)
    n_incorrect <- 100 - n_correct

    if (true_metal == "gold") {
      p(sprintf("Expected: Friend 1 correctly says 'gold' %d times (yellow) and incorrectly says 'lead' %d times (grey).",
                n_correct, n_incorrect), style = "font-size: 0.95em;")
    } else {
      p(sprintf("Expected: Friend 1 correctly says 'lead' %d times (grey) and incorrectly says 'gold' %d times (yellow).",
                n_correct, n_incorrect), style = "font-size: 0.95em;")
    }
  })

  # Expectation Mode: Friend 2
  output$ex6_known_friend2_exp <- renderPlot({
    s <- input$ex6_s
    true_metal <- input$ex6_true_metal
    ordered <- simplified_exp()

    if (true_metal == "gold") {
      draw_grid(s, "#d4af37", "#808080", "Friend 2 says 'Gold' vs 'Lead'", ordered)
    } else {
      draw_grid(s, "#808080", "#d4af37", "Friend 2 says 'Lead' vs 'Gold'", ordered)
    }
  })

  output$ex6_known_friend2_desc_exp <- renderUI({
    s <- input$ex6_s
    true_metal <- input$ex6_true_metal
    n_correct <- round(s * 100)
    n_incorrect <- 100 - n_correct

    if (true_metal == "gold") {
      p(sprintf("Expected: Friend 2 correctly says 'gold' %d times (yellow) and incorrectly says 'lead' %d times (grey).",
                n_correct, n_incorrect), style = "font-size: 0.95em;")
    } else {
      p(sprintf("Expected: Friend 2 correctly says 'lead' %d times (grey) and incorrectly says 'gold' %d times (yellow).",
                n_correct, n_incorrect), style = "font-size: 0.95em;")
    }
  })

  # Trial Mode: Friend 1
  output$ex6_known_friend1_trial <- renderPlot({
    res <- trial_results()
    if (is.null(res)) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Click 'Ask Friends' to run trials", cex = 1.2, col = "gray")
      return()
    }

    r <- input$ex6_r
    true_metal <- input$ex6_true_metal
    prob <- res$f1 / 100

    if (true_metal == "gold") {
      draw_grid(prob, "#d4af37", "#808080", "Friend 1 says 'Gold' vs 'Lead'")
    } else {
      draw_grid(prob, "#808080", "#d4af37", "Friend 1 says 'Lead' vs 'Gold'")
    }
  })

  output$ex6_known_friend1_desc_trial <- renderUI({
    res <- trial_results()
    if (is.null(res)) return(NULL)

    true_metal <- input$ex6_true_metal
    n_correct <- res$f1
    n_incorrect <- 100 - n_correct

    if (true_metal == "gold") {
      p(sprintf("Trial result: Friend 1 correctly said 'gold' %d times and incorrectly said 'lead' %d times.",
                n_correct, n_incorrect), style = "font-size: 0.95em;")
    } else {
      p(sprintf("Trial result: Friend 1 correctly said 'lead' %d times and incorrectly said 'gold' %d times.",
                n_correct, n_incorrect), style = "font-size: 0.95em;")
    }
  })

  # Trial Mode: Friend 2
  output$ex6_known_friend2_trial <- renderPlot({
    res <- trial_results()
    if (is.null(res)) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Click 'Ask Friends' to run trials", cex = 1.2, col = "gray")
      return()
    }

    s <- input$ex6_s
    true_metal <- input$ex6_true_metal
    prob <- res$f2 / 100

    if (true_metal == "gold") {
      draw_grid(prob, "#d4af37", "#808080", "Friend 2 says 'Gold' vs 'Lead'")
    } else {
      draw_grid(prob, "#808080", "#d4af37", "Friend 2 says 'Lead' vs 'Gold'")
    }
  })

  output$ex6_known_friend2_desc_trial <- renderUI({
    res <- trial_results()
    if (is.null(res)) return(NULL)

    true_metal <- input$ex6_true_metal
    n_correct <- res$f2
    n_incorrect <- 100 - n_correct

    if (true_metal == "gold") {
      p(sprintf("Trial result: Friend 2 correctly said 'gold' %d times and incorrectly said 'lead' %d times.",
                n_correct, n_incorrect), style = "font-size: 0.95em;")
    } else {
      p(sprintf("Trial result: Friend 2 correctly said 'lead' %d times and incorrectly said 'gold' %d times.",
                n_correct, n_incorrect), style = "font-size: 0.95em;")
    }
  })

  output$ex6_trial_note <- renderUI({
    res <- trial_results()
    if (is.null(res)) return(NULL)

    r <- input$ex6_r
    s <- input$ex6_s
    expected_f1 <- round(r * 100)
    expected_f2 <- round(s * 100)

    msg <- if (res$n == 100) {
      sprintf("We expected Friend 1 to get %d correct and Friend 2 to get %d correct, but this is only 100 trials. The probability relates to the long run—that's why 10,000 trials gives a better approximation.",
              expected_f1, expected_f2)
    } else {
      sprintf("After 10,000 trials (shown as average per 100), Friend 1 got %d correct and Friend 2 got %d correct. This is much closer to the expected %d and %d.",
              res$f1, res$f2, expected_f1, expected_f2)
    }

    p(class = "key-insight", msg)
  })

  # Combined grid for expectation mode
  output$ex6_combined_grid_exp <- renderUI({
    if (decomposed_exp()) {
      list(
        plotOutput("ex6_known_combined_decomposed_exp", height = "350px"),
        uiOutput("ex6_known_combined_desc_exp")
      )
    } else {
      list(
        plotOutput("ex6_known_combined_composed_exp", height = "350px"),
        uiOutput("ex6_known_combined_desc_exp")
      )
    }
  })

  # Combined grid for trial mode
  output$ex6_combined_grid_trial <- renderUI({
    if (decomposed_trial()) {
      list(
        plotOutput("ex6_known_combined_decomposed_trial", height = "350px"),
        uiOutput("ex6_known_combined_desc_trial")
      )
    } else {
      list(
        plotOutput("ex6_known_combined_composed_trial", height = "350px"),
        uiOutput("ex6_known_combined_desc_trial")
      )
    }
  })

  # Composed grid (expectation) - shows 2D intersection
  output$ex6_known_combined_composed_exp <- renderPlot({
    r <- input$ex6_r
    s <- input$ex6_s

    # Calculate number of cells for each dimension
    n_f1_correct <- round(r * 10)
    n_f2_correct <- round(s * 10)

    # Create a 10x10 grid where rows = Friend 1, cols = Friend 2
    par(mar = c(5, 5, 3, 1))
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1,
         main = "All Possible Outcomes Combined (2D View)")

    # Add axis labels
    text(-1.5, 5, "Friend 1", srt = 90, cex = 1.2, font = 2, xpd = TRUE)
    text(5, -1.5, "Friend 2", cex = 1.2, font = 2, xpd = TRUE)

    # Draw grid with colors based on both friends' correctness
    for (row in 0:9) {
      for (col in 0:9) {
        f1_correct <- row < n_f1_correct
        f2_correct <- col < n_f2_correct

        # Determine color based on both
        color <- if (f1_correct && f2_correct) "#90EE90"      # Both correct
                 else if (f1_correct && !f2_correct) "#FFE4B5" # F1 correct, F2 wrong
                 else if (!f1_correct && f2_correct) "#FFD700" # F1 wrong, F2 correct
                 else "#FFB6C1"                                # Both wrong

        rect(col, row, col + 1, row + 1, col = color, border = "grey30", lwd = 0.5)
      }
    }

    # Add labels on axes
    text(-0.5, n_f1_correct - 0.5, paste0(n_f1_correct*10, "% ✓"), cex = 0.9, xpd = TRUE, font = 2)
    text(-0.5, n_f1_correct + (10 - n_f1_correct)/2 - 0.5, paste0((10-n_f1_correct)*10, "% ✗"), cex = 0.9, xpd = TRUE, font = 2)
    text(n_f2_correct - 0.5, -0.5, paste0(n_f2_correct*10, "% ✓"), cex = 0.9, xpd = TRUE, font = 2)
    text(n_f2_correct + (10 - n_f2_correct)/2 - 0.5, -0.5, paste0((10-n_f2_correct)*10, "% ✗"), cex = 0.9, xpd = TRUE, font = 2)

    # Legend
    legend("bottom", horiz = TRUE, legend = c("Both Correct", "F1✓ F2✗", "F1✗ F2✓", "Both Wrong"),
           fill = c("#90EE90", "#FFE4B5", "#FFD700", "#FFB6C1"), cex = 0.9, xpd = TRUE, inset = c(0, -0.25))
  })

  # Decomposed grid (expectation)
  output$ex6_known_combined_decomposed_exp <- renderPlot({
    r <- input$ex6_r
    s <- input$ex6_s

    both_correct <- r * s
    f1_correct_f2_wrong <- r * (1 - s)
    f1_wrong_f2_correct <- (1 - r) * s
    both_wrong <- (1 - r) * (1 - s)

    n_both_correct <- round(both_correct * 100)
    n_f1c_f2w <- round(f1_correct_f2_wrong * 100)
    n_f1w_f2c <- round(f1_wrong_f2_correct * 100)
    n_both_wrong <- round(both_wrong * 100)

    par(mfrow = c(1, 4), mar = c(3, 1, 3, 1))

    # Both correct
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = "Both Correct")
    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        color <- if (count <= n_both_correct) "#90EE90" else "white"
        border_col <- if (count <= n_both_correct) "darkgreen" else "grey80"
        lwd_val <- if (count <= n_both_correct) 1 else 0.5
        rect(col, row, col + 1, row + 1, col = color, border = border_col, lwd = lwd_val)
        count <- count + 1
      }
    }
    text(5, -1, sprintf("%d/100", n_both_correct), cex = 1.2, font = 2, xpd = TRUE)

    # F1 correct, F2 wrong
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = "F1✓ F2✗")
    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        color <- if (count <= n_f1c_f2w) "#FFE4B5" else "white"
        border_col <- if (count <= n_f1c_f2w) "orange" else "grey80"
        lwd_val <- if (count <= n_f1c_f2w) 1 else 0.5
        rect(col, row, col + 1, row + 1, col = color, border = border_col, lwd = lwd_val)
        count <- count + 1
      }
    }
    text(5, -1, sprintf("%d/100", n_f1c_f2w), cex = 1.2, font = 2, xpd = TRUE)

    # F1 wrong, F2 correct
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = "F1✗ F2✓")
    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        color <- if (count <= n_f1w_f2c) "#FFD700" else "white"
        border_col <- if (count <= n_f1w_f2c) "goldenrod" else "grey80"
        lwd_val <- if (count <= n_f1w_f2c) 1 else 0.5
        rect(col, row, col + 1, row + 1, col = color, border = border_col, lwd = lwd_val)
        count <- count + 1
      }
    }
    text(5, -1, sprintf("%d/100", n_f1w_f2c), cex = 1.2, font = 2, xpd = TRUE)

    # Both wrong
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = "Both Wrong")
    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        color <- if (count <= n_both_wrong) "#FFB6C1" else "white"
        border_col <- if (count <= n_both_wrong) "darkred" else "grey80"
        lwd_val <- if (count <= n_both_wrong) 1 else 0.5
        rect(col, row, col + 1, row + 1, col = color, border = border_col, lwd = lwd_val)
        count <- count + 1
      }
    }
    text(5, -1, sprintf("%d/100", n_both_wrong), cex = 1.2, font = 2, xpd = TRUE)
  })

  # Composed grid (trial)
  output$ex6_known_combined_composed_trial <- renderPlot({
    res <- trial_results()
    if (is.null(res)) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Click 'Ask Friends' to run trials", cex = 1.2, col = "gray")
      return()
    }

    r_actual <- res$f1 / 100
    s_actual <- res$f2 / 100

    both_correct <- r_actual * s_actual
    f1_correct_f2_wrong <- r_actual * (1 - s_actual)
    f1_wrong_f2_correct <- (1 - r_actual) * s_actual
    both_wrong <- (1 - r_actual) * (1 - s_actual)

    n_both_correct <- round(both_correct * 100)
    n_f1c_f2w <- round(f1_correct_f2_wrong * 100)
    n_f1w_f2c <- round(f1_wrong_f2_correct * 100)
    n_both_wrong <- round(both_wrong * 100)

    par(mar = c(3, 1, 3, 1))
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = "All Possible Outcomes Combined")

    # Assign colors to each of 100 cells and shuffle
    colors <- c(
      rep("#90EE90", n_both_correct),
      rep("#FFE4B5", n_f1c_f2w),
      rep("#FFD700", n_f1w_f2c),
      rep("#FFB6C1", n_both_wrong)
    )
    colors <- sample(colors)

    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        rect(col, row, col + 1, row + 1, col = colors[count], border = "grey30", lwd = 0.5)
        count <- count + 1
      }
    }

    legend("bottom", horiz = TRUE, legend = c("Both Correct", "F1✓ F2✗", "F1✗ F2✓", "Both Wrong"),
           fill = c("#90EE90", "#FFE4B5", "#FFD700", "#FFB6C1"), cex = 0.9, xpd = TRUE, inset = c(0, -0.15))
  })

  # Decomposed grid (trial)
  output$ex6_known_combined_decomposed_trial <- renderPlot({
    res <- trial_results()
    if (is.null(res)) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Click 'Ask Friends' to run trials", cex = 1.2, col = "gray")
      return()
    }

    r_actual <- res$f1 / 100
    s_actual <- res$f2 / 100

    both_correct <- r_actual * s_actual
    f1_correct_f2_wrong <- r_actual * (1 - s_actual)
    f1_wrong_f2_correct <- (1 - r_actual) * s_actual
    both_wrong <- (1 - r_actual) * (1 - s_actual)

    n_both_correct <- round(both_correct * 100)
    n_f1c_f2w <- round(f1_correct_f2_wrong * 100)
    n_f1w_f2c <- round(f1_wrong_f2_correct * 100)
    n_both_wrong <- round(both_wrong * 100)

    par(mfrow = c(1, 4), mar = c(3, 1, 3, 1))

    # Both correct
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = "Both Correct")
    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        color <- if (count <= n_both_correct) "#90EE90" else "white"
        border_col <- if (count <= n_both_correct) "darkgreen" else "grey80"
        lwd_val <- if (count <= n_both_correct) 1 else 0.5
        rect(col, row, col + 1, row + 1, col = color, border = border_col, lwd = lwd_val)
        count <- count + 1
      }
    }
    text(5, -1, sprintf("%d/100", n_both_correct), cex = 1.2, font = 2, xpd = TRUE)

    # F1 correct, F2 wrong
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = "F1✓ F2✗")
    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        color <- if (count <= n_f1c_f2w) "#FFE4B5" else "white"
        border_col <- if (count <= n_f1c_f2w) "orange" else "grey80"
        lwd_val <- if (count <= n_f1c_f2w) 1 else 0.5
        rect(col, row, col + 1, row + 1, col = color, border = border_col, lwd = lwd_val)
        count <- count + 1
      }
    }
    text(5, -1, sprintf("%d/100", n_f1c_f2w), cex = 1.2, font = 2, xpd = TRUE)

    # F1 wrong, F2 correct
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = "F1✗ F2✓")
    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        color <- if (count <= n_f1w_f2c) "#FFD700" else "white"
        border_col <- if (count <= n_f1w_f2c) "goldenrod" else "grey80"
        lwd_val <- if (count <= n_f1w_f2c) 1 else 0.5
        rect(col, row, col + 1, row + 1, col = color, border = border_col, lwd = lwd_val)
        count <- count + 1
      }
    }
    text(5, -1, sprintf("%d/100", n_f1w_f2c), cex = 1.2, font = 2, xpd = TRUE)

    # Both wrong
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1, main = "Both Wrong")
    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        color <- if (count <= n_both_wrong) "#FFB6C1" else "white"
        border_col <- if (count <= n_both_wrong) "darkred" else "grey80"
        lwd_val <- if (count <= n_both_wrong) 1 else 0.5
        rect(col, row, col + 1, row + 1, col = color, border = border_col, lwd = lwd_val)
        count <- count + 1
      }
    }
    text(5, -1, sprintf("%d/100", n_both_wrong), cex = 1.2, font = 2, xpd = TRUE)
  })

  output$ex6_known_combined_desc_exp <- renderUI({
    r <- input$ex6_r
    s <- input$ex6_s

    both_correct <- r * s
    both_wrong <- (1 - r) * (1 - s)
    total_agree <- both_correct + both_wrong
    prob_correct_given_agree <- both_correct / total_agree

    n_both_correct <- round(both_correct * 100)
    n_both_wrong <- round(both_wrong * 100)
    n_total_agree <- round(total_agree * 100)

    div(
      p(class = "key-insight",
        strong("Key Insight: "),
        sprintf("When both friends agree (which happens %d times out of 100), they are both correct %d times and both wrong %d times.",
                n_total_agree, n_both_correct, n_both_wrong)
      ),
      div(class = "formula-box",
          p(strong("Probability both correct when they agree:"),
            " (Peirce's formula)"),
          p(withMathJax(sprintf("$$\\frac{%d}{%d} = %.4f \\text{ (or %.2f\\%%)}$$",
                                n_both_correct, n_total_agree,
                                prob_correct_given_agree, prob_correct_given_agree * 100)))
      ),
      p(style = "font-size: 0.9em; font-style: italic;",
        "Note: In this case, the ",
        span(class = "hl-antecedent", "antecedents"),
        " are the facts that each inference rule gave a particular result, and the ",
        span(class = "hl-consequent", "consequent"),
        " is that that result is correct."
      )
    )
  })

  output$ex6_known_combined_desc_trial <- renderUI({
    res <- trial_results()
    if (is.null(res)) return(NULL)

    r_actual <- res$f1 / 100
    s_actual <- res$f2 / 100

    both_correct <- r_actual * s_actual
    both_wrong <- (1 - r_actual) * (1 - s_actual)
    total_agree <- both_correct + both_wrong
    prob_correct_given_agree <- both_correct / total_agree

    n_both_correct <- round(both_correct * 100)
    n_both_wrong <- round(both_wrong * 100)
    n_total_agree <- round(total_agree * 100)

    div(
      p(class = "key-insight",
        strong("Trial Result: "),
        sprintf("When both friends agree (which happened %d times out of 100), they were both correct %d times and both wrong %d times.",
                n_total_agree, n_both_correct, n_both_wrong)
      ),
      div(class = "formula-box",
          p(strong("Probability both correct when they agree:"),
            " (Peirce's formula)"),
          p(withMathJax(sprintf("$$\\frac{%d}{%d} = %.4f \\text{ (or %.2f\\%%)}$$",
                                n_both_correct, n_total_agree,
                                prob_correct_given_agree, prob_correct_given_agree * 100)))
      )
    )
  })

  # Agreement formula for unknown mode
  output$ex6_agreement_formula <- renderUI({
    r <- input$ex6_r
    s <- input$ex6_s

    both_correct <- r * s
    both_wrong <- (1 - r) * (1 - s)
    total_agree <- both_correct + both_wrong
    prob_correct_given_agree <- both_correct / total_agree

    div(class = "formula-box",
        p(strong("Probability both correct when they agree:")),
        p(withMathJax(sprintf("$$\\frac{%.2f \\times %.2f}{%.2f \\times %.2f + %.2f \\times %.2f}$$",
                              r, s, r, s, 1-r, 1-s))),
        p(withMathJax(sprintf("$$= %.4f \\text{ (or %.2f\\%%)}$$",
                              prob_correct_given_agree, prob_correct_given_agree * 100)))
    )
  })

  # Unknown Mode: Friend answers display
  output$ex6_friend_answers <- renderUI({
    mode <- input$ex6_answer_mode

    if (mode == "random") {
      state <- random_state()
      f1_says <- state$f1
      f2_says <- state$f2
    } else if (mode == "both_gold") {
      f1_says <- "gold"
      f2_says <- "gold"
    } else if (mode == "both_lead") {
      f1_says <- "lead"
      f2_says <- "lead"
    } else {
      f1_says <- "gold"
      f2_says <- "lead"
    }

    f1_text <- if (f1_says == "gold") "Gold (yellow)" else "Lead (grey)"
    f2_text <- if (f2_says == "gold") "Gold (heavy)" else "Lead (light)"
    f1_col <- if (f1_says == "gold") "#d4af37" else "#808080"
    f2_col <- if (f2_says == "gold") "#d4af37" else "#808080"

    div(
      fluidRow(
        column(6,
               div(style = sprintf("background-color: %s; padding: 20px; border-radius: 8px; text-align: center; border: 3px solid #333;", f1_col),
                   h4("Friend 1 says:"),
                   h3(f1_text, style = "margin: 0;")
               )
        ),
        column(6,
               div(style = sprintf("background-color: %s; padding: 20px; border-radius: 8px; text-align: center; border: 3px solid #333;", f2_col),
                   h4("Friend 2 says:"),
                   h3(f2_text, style = "margin: 0;")
               )
        )
      )
    )
  })

  # Revelation display
  output$ex6_revelation <- renderUI({
    if (!revelation_shown()) return(NULL)

    mode <- input$ex6_answer_mode
    if (mode != "random") {
      return(p(class = "key-insight",
               "Revelation only works in random mode where a true metal was generated."))
    }

    state <- random_state()
    true_metal <- state$metal
    f1_says <- state$f1
    f2_says <- state$f2

    # Determine which scenario we're in
    both_correct <- (f1_says == true_metal && f2_says == true_metal)
    f1_correct <- (f1_says == true_metal && f2_says != true_metal)
    f2_correct <- (f1_says != true_metal && f2_says == true_metal)
    both_wrong <- (f1_says != true_metal && f2_says != true_metal)

    scenario_name <- if (both_correct) "Both Correct"
      else if (f1_correct) "F1✓ F2✗"
      else if (f2_correct) "F1✗ F2✓"
      else "Both Wrong"

    scenario_color <- if (both_correct) "#90EE90"
      else if (f1_correct) "#FFE4B5"
      else if (f2_correct) "#FFD700"
      else "#FFB6C1"

    div(
      hr(),
      h5("Revelation: What's Actually In The Box"),
      p(class = "key-insight",
        sprintf("The metal was %s! You landed in the '%s' scenario.",
                toupper(true_metal), scenario_name)
      ),
      plotOutput("ex6_revelation_grid", height = "350px"),
      p(sprintf("The highlighted cell shows which of the 100 possible scenarios you landed in (the %s scenario, shown in a brighter shade).",
                scenario_name))
    )
  })

  output$ex6_revelation_grid <- renderPlot({
    if (!revelation_shown()) return()

    mode <- input$ex6_answer_mode
    if (mode != "random") return()

    state <- random_state()
    true_metal <- state$metal
    f1_says <- state$f1
    f2_says <- state$f2

    r <- input$ex6_r
    s <- input$ex6_s

    both_correct <- r * s
    f1_correct_f2_wrong <- r * (1 - s)
    f1_wrong_f2_correct <- (1 - r) * s
    both_wrong <- (1 - r) * (1 - s)

    n_both_correct <- round(both_correct * 100)
    n_f1c_f2w <- round(f1_correct_f2_wrong * 100)
    n_f1w_f2c <- round(f1_wrong_f2_correct * 100)
    n_both_wrong <- round(both_wrong * 100)

    # Determine which scenario we're in
    is_both_correct <- (f1_says == true_metal && f2_says == true_metal)
    is_f1_correct <- (f1_says == true_metal && f2_says != true_metal)
    is_f2_correct <- (f1_says != true_metal && f2_says == true_metal)
    is_both_wrong <- (f1_says != true_metal && f2_says != true_metal)

    # Create composed grid with one cell highlighted
    par(mar = c(3, 1, 3, 1))
    plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
         xlab = "", ylab = "", axes = FALSE, asp = 1,
         main = "All Possible Outcomes (Your Scenario Highlighted)")

    # Create color vector and shuffle it, tracking which cells are which scenario
    cell_types <- c(
      rep("both_correct", n_both_correct),
      rep("f1_correct", n_f1c_f2w),
      rep("f2_correct", n_f1w_f2c),
      rep("both_wrong", n_both_wrong)
    )

    # Shuffle
    shuffle_order <- sample(1:100)
    cell_types <- cell_types[shuffle_order]

    # Assign colors based on shuffled types
    colors <- character(100)
    colors[cell_types == "both_correct"] <- "#90EE90"
    colors[cell_types == "f1_correct"] <- "#FFE4B5"
    colors[cell_types == "f2_correct"] <- "#FFD700"
    colors[cell_types == "both_wrong"] <- "#FFB6C1"

    # Find which scenario type to highlight
    scenario_type <- if (is_both_correct) "both_correct"
      else if (is_f1_correct) "f1_correct"
      else if (is_f2_correct) "f2_correct"
      else "both_wrong"

    # Pick a random cell of that type to highlight
    matching_cells <- which(cell_types == scenario_type)
    highlight_cell <- sample(matching_cells, 1)

    count <- 1
    for (row in 0:9) {
      for (col in 0:9) {
        is_highlight <- (count == highlight_cell)

        # Brighten the color for highlight
        base_col <- colors[count]
        col_to_use <- if (is_highlight) {
          if (base_col == "#90EE90") "#00FF00"
          else if (base_col == "#FFE4B5") "#FFA500"
          else if (base_col == "#FFD700") "#DAA520"
          else "#FF1493"
        } else {
          base_col
        }

        lwd_val <- if (is_highlight) 3 else 0.5
        border_col <- if (is_highlight) "black" else "grey30"

        rect(col, row, col + 1, row + 1, col = col_to_use, border = border_col, lwd = lwd_val)
        count <- count + 1
      }
    }

    legend("bottom", horiz = TRUE, legend = c("Both Correct", "F1✓ F2✗", "F1✗ F2✓", "Both Wrong"),
           fill = c("#90EE90", "#FFE4B5", "#FFD700", "#FFB6C1"), cex = 0.9, xpd = TRUE, inset = c(0, -0.15))
  })

  # Unknown Mode: Analysis plot
  output$ex6_unknown_analysis <- renderPlot({
    r <- input$ex6_r
    s <- input$ex6_s
    mode <- input$ex6_answer_mode

    # Determine what friends said
    if (mode == "random") {
      state <- random_state()
      f1_says <- state$f1
      f2_says <- state$f2
    } else if (mode == "both_gold") {
      f1_says <- "gold"
      f2_says <- "gold"
    } else if (mode == "both_lead") {
      f1_says <- "lead"
      f2_says <- "lead"
    } else {
      f1_says <- "gold"
      f2_says <- "lead"
    }

    # Calculate probabilities using Peirce's approach
    both_correct <- r * s
    f1_correct_f2_wrong <- r * (1 - s)
    f1_wrong_f2_correct <- (1 - r) * s
    both_wrong <- (1 - r) * (1 - s)

    # Determine which scenarios are consistent with what they said
    if (f1_says == "gold" && f2_says == "gold") {
      scenario_probs <- c(both_correct, both_wrong)
      scenario_names <- c("Both Correct\n(metal is gold)", "Both Wrong\n(metal is lead)")
      scenario_colors <- c("#90EE90", "#FFB6C1")
    } else if (f1_says == "lead" && f2_says == "lead") {
      scenario_probs <- c(both_correct, both_wrong)
      scenario_names <- c("Both Correct\n(metal is lead)", "Both Wrong\n(metal is gold)")
      scenario_colors <- c("#90EE90", "#FFB6C1")
    } else {
      if (f1_says == "gold" && f2_says == "lead") {
        scenario_probs <- c(f1_correct_f2_wrong, f1_wrong_f2_correct)
        scenario_names <- c("F1 Correct, F2 Wrong\n(metal is gold)", "F1 Wrong, F2 Correct\n(metal is lead)")
      } else {
        scenario_probs <- c(f1_wrong_f2_correct, f1_correct_f2_wrong)
        scenario_names <- c("F1 Wrong, F2 Correct\n(metal is gold)", "F1 Correct, F2 Wrong\n(metal is lead)")
      }
      scenario_colors <- c("#FFE4B5", "#FFD700")
    }

    n_scenarios <- length(scenario_probs)
    n_cells <- round(scenario_probs * 100)

    par(mfrow = c(1, n_scenarios), mar = c(4, 1, 3, 1))

    for (i in 1:n_scenarios) {
      plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
           xlab = "", ylab = "", axes = FALSE, asp = 1,
           main = scenario_names[i])

      count <- 1
      for (row in 0:9) {
        for (col in 0:9) {
          color <- if (count <= n_cells[i]) scenario_colors[i] else "white"
          border_col <- if (count <= n_cells[i]) "grey30" else "grey80"
          lwd_val <- if (count <= n_cells[i]) 1 else 0.5
          rect(col, row, col + 1, row + 1, col = color, border = border_col, lwd = lwd_val)
          count <- count + 1
        }
      }

      text(5, -1.5, sprintf("%d out of 100 cases", n_cells[i]),
           cex = 1.1, font = 2, xpd = TRUE)
    }
  })

  output$ex6_unknown_explanation <- renderUI({
    r <- input$ex6_r
    s <- input$ex6_s
    mode <- input$ex6_answer_mode

    # Determine what friends said
    if (mode == "random") {
      state <- random_state()
      f1_says <- state$f1
      f2_says <- state$f2
    } else if (mode == "both_gold") {
      f1_says <- "gold"
      f2_says <- "gold"
    } else if (mode == "both_lead") {
      f1_says <- "lead"
      f2_says <- "lead"
    } else {
      f1_says <- "gold"
      f2_says <- "lead"
    }

    # Calculate using Peirce's formulas
    both_correct <- r * s
    f1_correct_f2_wrong <- r * (1 - s)
    f1_wrong_f2_correct <- (1 - r) * s
    both_wrong <- (1 - r) * (1 - s)

    n_both_correct <- round(both_correct * 100)
    n_both_wrong <- round(both_wrong * 100)
    n_f1c_f2w <- round(f1_correct_f2_wrong * 100)
    n_f1w_f2c <- round(f1_wrong_f2_correct * 100)

    if ((f1_says == "gold" && f2_says == "gold") || (f1_says == "lead" && f2_says == "lead")) {
      # They agree
      total_agree <- n_both_correct + n_both_wrong
      prob_correct <- n_both_correct / total_agree

      agree_text <- if (f1_says == "gold") "both say 'gold'" else "both say 'lead'"

      div(
        p(class = "key-insight",
          sprintf("When your friends agree and %s, there are only two possibilities:", agree_text)
        ),
        tags$ul(
          tags$li(sprintf("Both are correct (%d out of 100 times)", n_both_correct)),
          tags$li(sprintf("Both are wrong (%d out of 100 times)", n_both_wrong))
        ),
        div(class = "formula-box",
            p(strong("Probability they're both correct when they agree:")),
            p(withMathJax(sprintf("$$\\frac{%d}{%d + %d} = \\frac{%d}{%d} = %.4f \\text{ (or %.2f\\%%)}$$",
                                  n_both_correct, n_both_correct, n_both_wrong,
                                  n_both_correct, total_agree, prob_correct, prob_correct * 100)))
        ),
        p("This is Peirce's key insight: when two independent inference rules agree, the probability they're both correct is much higher than either one alone!"),
        p(style = "font-size: 0.9em; font-style: italic;",
          "Note: In this case, the ",
          span(class = "hl-antecedent", "antecedents"),
          " are the facts that each inference rule gave a particular result, and the ",
          span(class = "hl-consequent", "consequent"),
          " is that that result is correct."
        )
      )
    } else {
      # They disagree
      total_disagree <- n_f1c_f2w + n_f1w_f2c

      if (f1_says == "gold") {
        prob_f1_correct <- n_f1c_f2w / total_disagree
        explanation_text <- sprintf("Friend 1 is correct and the metal is gold (%d out of %d disagreement cases, or %.2f%%)",
                                   n_f1c_f2w, total_disagree, prob_f1_correct * 100)
      } else {
        prob_f2_correct <- n_f1w_f2c / total_disagree
        explanation_text <- sprintf("Friend 2 is correct and the metal is gold (%d out of %d disagreement cases, or %.2f%%)",
                                   n_f1w_f2c, total_disagree, prob_f2_correct * 100)
      }

      div(
        p(class = "key-insight",
          "When your friends disagree, one must be correct and one must be wrong. The question is: which one?"
        ),
        tags$ul(
          tags$li(sprintf("Friend 1 correct, Friend 2 wrong: %d out of 100 times", n_f1c_f2w)),
          tags$li(sprintf("Friend 1 wrong, Friend 2 correct: %d out of 100 times", n_f1w_f2c))
        ),
        p(explanation_text)
      )
    }
  })
}

shinyApp(ui = ui, server = server)
