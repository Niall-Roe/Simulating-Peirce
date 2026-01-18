library(shiny)
library(shinyjs)

# Helper function for binomial coefficient
binom_coef <- function(n, k) {
  choose(n, k)
}

# Helper function to calculate total possible outcomes using binomial expansion
# For ratio w:b and n draws, total = (w + b)^n
calculate_total_outcomes <- function(white_weight, black_weight, n_draws) {
  (white_weight + black_weight)^n_draws
}

# Helper function to calculate frequency for k white balls
# Frequency = C(n,k) * w^k * b^(n-k)
calculate_frequency <- function(n_draws, k_white, white_weight, black_weight) {
  binom_coef(n_draws, k_white) * (white_weight^k_white) * (black_weight^(n_draws - k_white))
}

# Helper function for pluralization
pluralize_ball <- function(n) {
  if (n == 1) return("ball")
  return("balls")
}

# Helper function for number words
number_word <- function(n) {
  words <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
             "eight", "nine", "ten")
  if (n >= 0 && n <= 10) return(words[n + 1])
  return(as.character(n))
}

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
      .click-info {
        padding: 15px;
        background-color: #fff3cd;
        border: 1px solid #ffc107;
        border-radius: 5px;
        margin-top: 15px;
        font-size: 0.95em;
      }
      .sequence-box {
        display: inline-block;
        padding: 3px 6px;
        margin: 2px;
        background-color: #e9ecef;
        border: 1px solid #adb5bd;
        border-radius: 3px;
        font-family: monospace;
        font-size: 0.9em;
      }
    "))
  ),

  div(class = "article-container",
      h3("Example 14: The Binomial Expansion"),

      p("Suppose we had an immense granary filled with black and white balls well mixed up; and we know that a man is going to take out a sample of a hundred. ",
        span(class = "example-trigger", id = "ex14-trigger",
             onclick = "Shiny.setInputValue('toggle_ex14', Math.random());",
             "[Click to see interactive example]")
      ),

      div(id = "example-14", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Binomial Expansion"),

          p("Explore how the binomial expansion determines the relative frequencies of different outcomes when drawing balls from an urn. The expansion ",
            withMathJax("$$(w + b)^n$$"),
            " gives us all possible outcomes and their frequencies."),

          fluidRow(
            column(4,
              sliderInput("ex14_white_weight", "White balls ratio (w):",
                         min = 1, max = 10, value = 1, step = 1),
              sliderInput("ex14_black_weight", "Black balls ratio (b):",
                         min = 1, max = 10, value = 2, step = 1),
              sliderInput("ex14_n_draws", "Number of draws (n):",
                         min = 2, max = 8, value = 4, step = 1),
              hr(),
              checkboxInput("ex14_show_sequences", "Show individual sequences", FALSE),
              hr(),
              actionButton("ex14_reset", "Reset to Peirce's example", class = "btn-primary")
            ),
            column(8,
              uiOutput("ex14_summary"),
              plotOutput("ex14_plot", height = "400px", click = "ex14_plot_click"),
              uiOutput("ex14_click_text")
            )
          ),

          hr(),

          uiOutput("ex14_table")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex14, { shinyjs::toggle("example-14") })

  # Reset to Peirce's example: 1 white, 2 black, 4 draws
  observeEvent(input$ex14_reset, {
    updateSliderInput(session, "ex14_white_weight", value = 1)
    updateSliderInput(session, "ex14_black_weight", value = 2)
    updateSliderInput(session, "ex14_n_draws", value = 4)
    updateCheckboxInput(session, "ex14_show_sequences", value = FALSE)
  })

  # Store clicked bar
  clicked_k <- reactiveVal(NULL)

  observeEvent(input$ex14_plot_click, {
    n <- input$ex14_n_draws
    click <- input$ex14_plot_click
    if (is.null(click)) return()

    # Calculate which bar was clicked
    # barplot returns bar centers, need to map click to actual bar index
    # Bar centers are at positions 1, 2, 3, ... for bars 0, 1, 2, ...
    clicked_white <- round(click$x - 1)

    if (clicked_white >= 0 && clicked_white <= n) {
      clicked_k(clicked_white)
    }
  })

  # Summary text
  output$ex14_summary <- renderUI({
    w <- input$ex14_white_weight
    b <- input$ex14_black_weight
    n <- input$ex14_n_draws

    total <- calculate_total_outcomes(w, b, n)

    p(strong("Setup: "),
      "The granary has ", strong(paste0(w, ":", b)), " ratio of white to black balls. ",
      "Drawing ", strong(n), " balls with replacement yields ",
      strong(format(total, big.mark = ",")), " total possible outcomes (weighted by frequency).")
  })

  # Plot
  output$ex14_plot <- renderPlot({
    w <- input$ex14_white_weight
    b <- input$ex14_black_weight
    n <- input$ex14_n_draws

    # Calculate frequencies for each number of white balls
    k_values <- 0:n
    frequencies <- sapply(k_values, function(k) {
      calculate_frequency(n, k, w, b)
    })

    # Convert to probabilities
    total <- sum(frequencies)
    probabilities <- frequencies / total

    # Plot
    barplot_result <- barplot(frequencies,
           names.arg = k_values,
           xlab = "Number of white balls",
           ylab = "Frequency (number of sets)",
           main = "Binomial Expansion: Frequency Distribution",
           col = "#2c7fb8",
           border = "white",
           ylim = c(0, max(frequencies) * 1.2))

    # Highlight clicked bar
    if (!is.null(clicked_k())) {
      k_clicked <- clicked_k()
      barplot(frequencies,
              col = ifelse(k_values == k_clicked, "purple", "#2c7fb8"),
              border = "white",
              add = FALSE,
              names.arg = k_values,
              xlab = "Number of white balls",
              ylab = "Frequency (number of sets)",
              main = "Binomial Expansion: Frequency Distribution",
              ylim = c(0, max(frequencies) * 1.2))
    }

    # Add grid
    grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
  })

  # Click text
  output$ex14_click_text <- renderUI({
    w <- input$ex14_white_weight
    b <- input$ex14_black_weight
    n <- input$ex14_n_draws

    total <- calculate_total_outcomes(w, b, n)

    # Pre-click state
    if (is.null(clicked_k())) {
      return(div(class = "click-info",
                 p(em("Click on any bar to see the detailed calculation for that outcome.")),
                 p("Suppose ", strong(number_word(w)), " ", pluralize_ball(w), " out of ",
                   strong(w + b), " is white and the rest black, and that ",
                   strong(number_word(n)), " balls are drawn. ",
                   "This represents the relative frequency of the different ways in which these balls might be drawn.")))
    }

    # Post-click state
    k <- clicked_k()

    # Calculate this specific frequency
    combinations <- binom_coef(n, k)
    frequency <- calculate_frequency(n, k, w, b)
    probability <- frequency / total

    # Build explanation
    div(class = "click-info",
        p(strong("Clicked outcome: "), k, " white balls out of ", n, " draws"),
        p(strong("Number of ways to arrange: "),
          combinations,
          " (combinations of ", k, " white in ", n, " positions)"),
        p(strong("Weight per sequence: "),
          w, "^", k, " × ", b, "^", n - k, " = ",
          w^k * b^(n - k)),
        p(strong("Total frequency (number of sets): "),
          combinations, " × ", w^k * b^(n - k), " = ",
          strong(frequency)),
        p(strong("Probability: "),
          frequency, " / ", total, " = ",
          strong(sprintf("%.4f", probability))),
        p("This means that if we judge by these ", strong(n), " balls, ",
          strong(frequency), " times out of ", strong(total),
          " we would find the proportion to be ",
          strong(sprintf("%.3f", k/n)), " (", k, "/", n, ").")
    )
  })

  # Detailed table
  output$ex14_table <- renderUI({
    w <- input$ex14_white_weight
    b <- input$ex14_black_weight
    n <- input$ex14_n_draws
    show_sequences <- input$ex14_show_sequences

    total <- calculate_total_outcomes(w, b, n)

    # Build table data
    k_values <- 0:n
    table_rows <- lapply(k_values, function(k) {
      combinations <- binom_coef(n, k)
      weight <- w^k * b^(n - k)
      frequency <- combinations * weight
      probability <- frequency / total
      proportion <- k / n

      list(
        k = k,
        combinations = combinations,
        weight = weight,
        frequency = frequency,
        probability = probability,
        proportion = proportion
      )
    })

    # Create HTML table
    table_html <- '<table class="table table-striped table-hover table-bordered" style="margin-top: 20px;">'
    table_html <- paste0(table_html, '<thead><tr>')
    table_html <- paste0(table_html, '<th>White Balls (k)</th>')
    table_html <- paste0(table_html, '<th>Combinations C(n,k)</th>')
    table_html <- paste0(table_html, '<th>Weight (w^k × b^(n-k))</th>')
    table_html <- paste0(table_html, '<th>Frequency (Sets)</th>')
    table_html <- paste0(table_html, '<th>Probability</th>')
    table_html <- paste0(table_html, '<th>Proportion (k/n)</th>')
    table_html <- paste0(table_html, '</tr></thead><tbody>')

    for (row in table_rows) {
      # Highlight clicked row
      row_class <- ''
      if (!is.null(clicked_k()) && row$k == clicked_k()) {
        row_class <- ' style="background-color: rgba(128, 0, 128, 0.15);"'
      }

      table_html <- paste0(table_html, '<tr', row_class, '>')
      table_html <- paste0(table_html, '<td>', row$k, '</td>')
      table_html <- paste0(table_html, '<td>', row$combinations, '</td>')
      table_html <- paste0(table_html, '<td>', row$weight, '</td>')
      table_html <- paste0(table_html, '<td><strong>', row$frequency, '</strong></td>')
      table_html <- paste0(table_html, '<td>', sprintf("%.4f", row$probability), '</td>')
      table_html <- paste0(table_html, '<td>', sprintf("%.3f", row$proportion), ' (', row$k, '/', n, ')</td>')
      table_html <- paste0(table_html, '</tr>')
    }

    table_html <- paste0(table_html, '<tr style="font-weight: bold; background-color: #e9ecef;">')
    table_html <- paste0(table_html, '<td colspan="3">Total</td>')
    table_html <- paste0(table_html, '<td>', total, '</td>')
    table_html <- paste0(table_html, '<td>1.0000</td>')
    table_html <- paste0(table_html, '<td>—</td>')
    table_html <- paste0(table_html, '</tr>')

    table_html <- paste0(table_html, '</tbody></table>')

    # Add sequence display if toggled
    sequence_html <- ""
    if (show_sequences && n <= 5) {
      sequence_html <- "<h5 style='margin-top: 30px;'>Individual Sequences (like Peirce's table):</h5>"

      for (k in n:0) {
        frequency <- calculate_frequency(n, k, w, b)
        combinations <- binom_coef(n, k)

        sequence_html <- paste0(sequence_html,
                               "<p><strong>", k, " white, ", n - k, " black</strong> — ",
                               combinations, " arrangement(s) × ", w^k * b^(n - k), " sets each = ",
                               frequency, " total sets:</p>")

        # Generate all sequences for this k
        if (combinations <= 20) {  # Only show if not too many
          # Generate binary representations
          all_positions <- combn(n, k, simplify = FALSE)

          sequence_html <- paste0(sequence_html, "<div style='margin-left: 20px; margin-bottom: 15px;'>")

          for (positions in all_positions) {
            # Create sequence string
            seq_str <- rep("b", n)
            seq_str[positions] <- "w"

            # Show this sequence repeated (weight) times
            weight <- w^k * b^(n - k)
            for (rep in 1:min(weight, 10)) {  # Limit display to 10 repetitions
              sequence_html <- paste0(sequence_html,
                                     '<span class="sequence-box">',
                                     paste(seq_str, collapse = ""),
                                     '</span> ')
            }
            if (weight > 10) {
              sequence_html <- paste0(sequence_html, " ... (", weight - 10, " more)")
            }
            sequence_html <- paste0(sequence_html, "<br/>")
          }

          sequence_html <- paste0(sequence_html, "</div>")
        } else {
          sequence_html <- paste0(sequence_html,
                                 "<div style='margin-left: 20px; margin-bottom: 15px;'>",
                                 "<em>(Too many combinations to display individually)</em></div>")
        }
      }
    } else if (show_sequences && n > 5) {
      sequence_html <- "<p style='margin-top: 20px;'><em>Individual sequences are only shown for 5 or fewer draws to keep the display manageable.</em></p>"
    }

    HTML(paste0(table_html, sequence_html))
  })
}

shinyApp(ui = ui, server = server)
