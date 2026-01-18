library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)

# Read the full text
full_text <- readLines("full_text.txt", warn = FALSE)
full_text <- paste(full_text, collapse = "\n")

# UI
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Georgia', serif;
        line-height: 1.8;
        max-width: 900px;
        margin: 0 auto;
        padding: 20px;
        font-size: 16px;
        background-color: #fafafa;
      }

      .article-container {
        background-color: white;
        padding: 40px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        border-radius: 4px;
      }

      h3 {
        text-align: center;
        font-variant: small-caps;
        margin-top: 40px;
        margin-bottom: 30px;
        letter-spacing: 1px;
      }

      h4 {
        text-align: center;
        font-weight: normal;
        margin-bottom: 10px;
      }

      .section-number {
        text-align: center;
        font-size: 1.2em;
        margin: 40px 0 20px 0;
        font-weight: bold;
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

      .example-header {
        font-weight: bold;
        font-size: 1.1em;
        margin-bottom: 20px;
        color: #1565c0;
        cursor: pointer;
        user-select: none;
        display: flex;
        align-items: center;
      }

      .example-header:before {
        content: '▼';
        margin-right: 10px;
        transition: transform 0.2s;
      }

      .example-header.collapsed:before {
        content: '▶';
      }

      .example-content {
        margin-top: 20px;
      }

      /* Highlighting for probability concepts */
      .hl-antecedent {
        background-color: #fff3cd;
        padding: 2px 4px;
        border-radius: 2px;
        font-weight: 500;
      }
      .hl-consequent {
        background-color: #d1ecf1;
        padding: 2px 4px;
        border-radius: 2px;
        font-weight: 500;
      }
      .hl-consequence {
        background-color: #d4edda;
        padding: 2px 4px;
        border-radius: 2px;
        font-weight: 600;
      }

      /* For addition/multiplication rules */
      .hl-event-a { background-color: #fee5d9; padding: 2px 4px; }
      .hl-event-b { background-color: #deebf7; padding: 2px 4px; }
      .hl-combined { background-color: #d4e4c4; padding: 2px 4px; font-weight: 500; }

      .formula {
        text-align: center;
        margin: 20px 0;
        font-family: 'Courier New', monospace;
        font-size: 1.1em;
        background-color: #f5f5f5;
        padding: 15px;
        border-radius: 4px;
      }

      .arrow-diagram {
        text-align: center;
        font-size: 1.2em;
        margin: 20px 0;
        padding: 15px;
        background-color: #f0f0f0;
        border-radius: 4px;
      }

      p {
        text-align: justify;
        margin-bottom: 15px;
      }

      .indent {
        margin-left: 40px;
      }

      .plot-container {
        margin: 20px 0;
        padding: 15px;
        background-color: white;
        border-radius: 4px;
        border: 1px solid #ddd;
      }

      .calc-output {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 4px;
        border-left: 4px solid #6c757d;
        font-family: 'Courier New', monospace;
        margin-top: 15px;
        white-space: pre-wrap;
      }

      .control-panel {
        background-color: #fff;
        padding: 15px;
        border-radius: 4px;
        border: 1px solid #dee2e6;
        margin-bottom: 15px;
      }

      .key-insight {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
    "))
  ),

  div(class = "article-container",
    h3("ILLUSTRATIONS OF THE LOGIC OF SCIENCE."),
    h4("By C. S. PEIRCE,"),
    h4("ASSISTANT IN THE UNITED STATES COAST SURVEY."),
    h4("FOURTH PAPER.—THE PROBABILITY OF INDUCTION."),

    div(class = "section-number", "I."),

    # EXAMPLE 1: Antecedent, Consequent, Consequence
    p("We have found that every argument derives its force from the general truth of the class of inferences to which it belongs; and that probability is the proportion of arguments carrying truth with them among those of any ", em("genus"), ". This is most conveniently expressed in the nomenclature of the mediæval logicians. ",
      span(class = "example-trigger", id = "ex1-trigger",
           onclick = "Shiny.setInputValue('toggle_ex1', Math.random());",
           "They called the fact expressed by a premise an ", em("antecedent"), ", and that which follows from it its ", em("consequent"), "; while the leading principle, that every (or almost every) such antecedent is followed by such a ", em("consequent"), ", they termed the ", em("consequence"), ". Using this language, we may say that probability belongs exclusively to consequences, and the probability of any consequence is the number of times in which antecedent and consequent both occur divided by the number of all the times in which the antecedent occurs."
      )
    ),

    div(id = "example-1", class = "example-container", style = "display: none;"),

    p("From this definition are deduced the following rules for the addition and multiplication of probabilities:"),

    # EXAMPLE 2: Addition Rule
    p(span(class = "example-trigger", id = "ex2-trigger",
           onclick = "Shiny.setInputValue('toggle_ex2', Math.random());",
           em("Rule for the Addition of Probabilities"), ".—Given the separate probabilities of two consequences having the same antecedent and incompatible consequents. Then the sum of these two numbers is the probability of the consequence, that from the same antecedent one or other of those consequents follows."
    )),

    div(id = "example-2", class = "example-container", style = "display: none;"),

    # EXAMPLE 3: Multiplication Rule
    p(span(class = "example-trigger", id = "ex3-trigger",
           onclick = "Shiny.setInputValue('toggle_ex3', Math.random());",
           em("Rule for the Multiplication of Probabilities"), ".—Given the separate probabilities of the two consequences, \"If A then B,\" and \"If both A and B, then C.\" Then the product of these two numbers is the probability of the consequence, \"If A, then both B and C.\""
    )),

    div(id = "example-3", class = "example-container", style = "display: none;"),

    # EXAMPLE 4: Special Rule for Independent Probabilities
    p(span(class = "example-trigger", id = "ex4-trigger",
           onclick = "Shiny.setInputValue('toggle_ex4', Math.random());",
           em("Special Rule for the Multiplication of Independent Probabilities"), ".—Given the separate probabilities of two consequences having the same antecedents, \"If A, then B,\" and \"If A, then C.\" Suppose that these consequences are such that the probability of the second is equal to the probability of the consequence, \"If both A and B, then C.\" Then the product of the two given numbers is equal to the probability of the consequence, \"If A, then both B and C.\""
    )),

    div(id = "example-4", class = "example-container", style = "display: none;"),

    p("To show the working of these rules we may examine the probabilities in regard to throwing dice. What is the probability of throwing a six with one die? The antecedent here is the event of throwing a die; the consequent, its turning up a six. As the die has six sides, all of which are turned up with equal frequency, the probability of turning up any one is 1/6. Suppose two dice are thrown, what is the probability of throwing sixes? The probability of either coming up six is obviously the same when both are thrown as when one is thrown—namely, 1/6. The probability that either will come up six when the other does is also the same as that of its coming up six whether the other does or not. The probabilities are, therefore, independent; and, by our rule, the probability that both events will happen together is the product of their several probabilities, 1/6 × 1/6. What is the probability of throwing deuce-ace? The probability that the first die will turn up ace and the second deuce is the same as the probability that both will turn up sixes—namely, 1/36; the probability that the second will turn up ace and the first deuce is likewise 1/36; these two events—first, ace; second, deuce; and, second, ace; first, deuce—are incompatible. Hence the rule for addition holds, and the probability that either will come up ace and the other deuce is 1/36 + 1/36, or 1/18."),

    p(span(class = "example-trigger", id = "dice-trigger",
           onclick = "Shiny.setInputValue('toggle_dice', Math.random());",
           "In this way all problems about dice, etc., may be solved."), " When the number of dice thrown is supposed very large, mathematics (which may be defined as the art of making groups to facilitate numeration) comes to our aid with certain devices to reduce the difficulties."),

    div(id = "example-dice", class = "example-container", style = "display: none;",
        uiOutput("dice_ui")
    ),

    div(class = "section-number", "II."),

    p("The conception of probability as a matter of fact, i.e., as the proportion of times in which an occurrence of one kind is accompanied by an occurrence of another kind, is termed by Mr. Venn the ", em("materialistic"), " view of the subject. But probability has often been regarded as being simply the degree of belief which ought to attach to a proposition; and this mode of explaining the idea is termed by Venn the ", em("conceptualistic"), " view. Most writers have mixed the two conceptions together. They, first, define the probability of an event as the reason we have to believe that it has taken place, which is conceptualistic; but shortly after they state that it is the ratio of the number of cases favorable to the event to the total number of cases favorable or contrary, and all equally possible. Except that this introduces the thoroughly unclear idea of cases equally possible in place of cases equally frequent, this is a tolerable statement of the materialistic view. The pure conceptualistic theory has been best expounded by Mr. De Morgan in his \"Formal Logic: or, the Calculus of Inference, Necessary and Probable.\""),

    p("The great difference between the two analyses is, that the conceptualists refer probability to an event, while the materialists make it the ratio of frequency of events of a species to those of a genus over that species, thus giving it two terms instead of one. The opposition may be made to appear as follows:"),

    p(span(class = "example-trigger", id = "inference-trigger",
           onclick = "Shiny.setInputValue('toggle_inference', Math.random());",
           "Suppose that we have two rules of inference,"), " such that, of all the questions to the solution of which both can be applied, the first yields correct answers to 81/100, and incorrect answers to the remaining 19/100; while the second yields correct answers to 93/100, and incorrect answers to the remaining 7/100 Suppose, further, that the two rules are entirely independent as to their truth, so that the second answers correctly the same proportion of the questions which the first answers correctly, and also the same proportion of the questions which the first answers incorrectly, and answers incorrectly the remaining proportion of the questions which the first answers correctly, and also the remaining proportion of the questions which the first answers incorrectly."),

    div(id = "example-inference", class = "example-container", style = "display: none;",
        uiOutput("inference_ui")
    ),

    p("Then, of all the questions to the solution of which both rules can be applied: both answer correctly a certain proportion; the second answers correctly and the first incorrectly a certain proportion; the second answers incorrectly and the first correctly a certain proportion; and both answer incorrectly a certain proportion."),

    p("Suppose, now, that, in reference to any question, both give the same answer. Then (the questions being always such as are to be answered by yes or no), those in reference to which their answers agree are the same as those which both answer correctly together with those which both answer falsely. The proportion of those which both answer correctly out of those their answers to which agree is, therefore, a calculable quantity."),

    p("This is, therefore, the probability that, if both modes of inference yield the same result, that result is correct. We may here conveniently make use of another mode of expression. Probability is the ratio of the favorable cases to all the cases. Instead of expressing our result in terms of this ratio, we may make use of another—the ratio of favorable to unfavorable cases. This last ratio may be called the ", em("chance"), " of an event."),

    hr(style = "margin: 40px 0; border: none; border-top: 1px solid #ccc;"),

    p(em(style = "color: #666;", "Additional sections to be formatted..."))
  )
)

# Server
server <- function(input, output, session) {

  # ==== GENERIC GRID FRAMEWORK ====

  # Create grid structure based on antecedent type
  create_grid <- function(antecedent_type, params = list()) {
    if (antecedent_type == "single_die") {
      return(list(
        cells = data.frame(
          id = 1:6,
          label = as.character(1:6),
          x = 1:6,
          y = rep(1, 6),
          prob = rep(1/6, 6)
        ),
        dims = c(x = 6, y = 1),
        x_labels = as.character(1:6),
        y_labels = "Die"
      ))
    } else if (antecedent_type == "two_dice") {
      cells <- expand.grid(die1 = 1:6, die2 = 1:6)
      return(list(
        cells = data.frame(
          id = 1:36,
          label = paste0(cells$die1, ",", cells$die2),
          x = cells$die1,
          y = cells$die2,
          die1 = cells$die1,
          die2 = cells$die2,
          prob = rep(1/36, 36)
        ),
        dims = c(x = 6, y = 6),
        x_labels = as.character(1:6),
        y_labels = as.character(1:6)
      ))
    } else if (antecedent_type == "card_deck") {
      deck_type <- params$deck_type %||% "shuffled_standard"

      if (deck_type == "shuffled_face") {
        suits <- c("H", "D", "C", "S")
        ranks <- c("J", "Q", "K")
      } else if (deck_type == "shuffled_piquet") {
        suits <- c("H", "D", "C", "S")
        ranks <- c("7", "8", "9", "10", "J", "Q", "K", "A")
      } else {
        suits <- c("H", "D", "C", "S")
        ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
      }

      cells <- expand.grid(suit = suits, rank = ranks, stringsAsFactors = FALSE)
      n_cards <- nrow(cells)

      # Handle new deck (Ace of Spades on top)
      if (deck_type == "new_standard") {
        probs <- ifelse(cells$suit == "S" & cells$rank == "A", 1, 0)
      } else {
        probs <- rep(1/n_cards, n_cards)
      }

      return(list(
        cells = data.frame(
          id = 1:n_cards,
          label = paste0(cells$rank, cells$suit),
          x = match(cells$rank, ranks),
          y = match(cells$suit, suits),
          suit = cells$suit,
          rank = cells$rank,
          prob = probs
        ),
        dims = c(x = length(ranks), y = length(suits)),
        x_labels = ranks,
        y_labels = c("Hearts", "Diamonds", "Clubs", "Spades")
      ))
    } else if (antecedent_type == "trials_100") {
      # 10x10 grid for x/100 trials
      n_success <- params$n_success %||% 50
      cells <- expand.grid(x = 1:10, y = 1:10)
      probs <- rep(0, 100)
      probs[1:n_success] <- 1/n_success

      return(list(
        cells = data.frame(
          id = 1:100,
          label = "",
          x = cells$x,
          y = cells$y,
          prob = probs
        ),
        dims = c(x = 10, y = 10),
        x_labels = NULL,
        y_labels = NULL
      ))
    }
  }

  # Evaluate which cells match consequent
  evaluate_consequent <- function(grid, consequent_rule) {
    cells <- grid$cells
    matched <- rep(FALSE, nrow(cells))

    if (consequent_rule$type == "card") {
      if (!is.null(consequent_rule$operator)) {
        operator <- consequent_rule$operator
        rank_val <- consequent_rule$rank
        property <- consequent_rule$property
        suit_val <- if(is.null(consequent_rule$suit)) "any" else consequent_rule$suit

        rank_order <- c("A" = 14, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6,
                        "7" = 7, "8" = 8, "9" = 9, "10" = 10, "J" = 11, "Q" = 12, "K" = 13)

        for (i in 1:nrow(cells)) {
          suit <- cells$suit[i]
          rank <- cells$rank[i]

          suit_match <- if (suit_val == "any") {
            TRUE
          } else if (suit_val == "red") {
            suit %in% c("H", "D")
          } else if (suit_val == "black") {
            suit %in% c("C", "S")
          } else {
            suit == suit_val
          }

          if (!suit_match) {
            matched[i] <- FALSE
            next
          }

          matched[i] <- if (operator == "any") {
            if (property == "even") rank %in% c("2", "4", "6", "8", "10")
            else if (property == "odd") rank %in% c("A", "3", "5", "7", "9")
            else if (property == "face") rank %in% c("J", "Q", "K")
            else if (property == "non_face") !(rank %in% c("J", "Q", "K"))
            else if (property == "any_card") TRUE
            else FALSE
          } else if (operator == "exactly") {
            rank == rank_val
          } else if (operator == "higher_than") {
            rank_order[rank] > rank_order[rank_val]
          } else if (operator == "lower_than") {
            rank_order[rank] < rank_order[rank_val]
          } else if (operator == "anything_other_than") {
            if (!is.null(property)) {
              if (property == "even") !(rank %in% c("2", "4", "6", "8", "10"))
              else if (property == "odd") !(rank %in% c("A", "3", "5", "7", "9"))
              else if (property == "face") !(rank %in% c("J", "Q", "K"))
              else FALSE
            } else rank != rank_val
          } else FALSE
        }
      } else {
        target <- consequent_rule$target
        for (i in 1:nrow(cells)) {
          suit <- cells$suit[i]
          rank <- cells$rank[i]
          matched[i] <- if (target == "red") suit %in% c("H", "D")
          else if (target == "ace_spades") suit == "S" && rank == "A"
          else if (target == "even") rank %in% c("2", "4", "6", "8", "10")
          else if (target == "face") rank %in% c("J", "Q", "K")
          else if (target == "less_10") rank %in% c("2", "3", "4", "5", "6", "7", "8", "9")
          else if (target == "higher_8") rank %in% c("9", "10", "J", "Q", "K", "A")
          else FALSE
        }
      }
    } else if (consequent_rule$type == "die") {
      target <- consequent_rule$target
      for (i in 1:nrow(cells)) matched[i] <- cells$label[i] == as.character(target)
    } else if (consequent_rule$type == "dice_sum") {
      target <- consequent_rule$target
      for (i in 1:nrow(cells)) matched[i] <- (cells$die1[i] + cells$die2[i]) == target
    }

    return(matched)
  }

  # Build UI for flexible consequent selection
  build_consequent_ui <- function(id_prefix, label = "Consequent", css_class = "hl-consequent") {
    div(
      selectInput(paste0(id_prefix, "_operator"), span(class = css_class, paste0(label, " - Operator:")),
                  choices = c("Exactly" = "exactly", "Higher than" = "higher_than",
                            "Lower than" = "lower_than", "Any" = "any",
                            "Anything other than" = "anything_other_than"),
                  selected = "exactly"),
      conditionalPanel(
        condition = sprintf("input['%s_operator'] == 'exactly' || input['%s_operator'] == 'higher_than' || input['%s_operator'] == 'lower_than' || input['%s_operator'] == 'anything_other_than'",
                           id_prefix, id_prefix, id_prefix, id_prefix),
        selectInput(paste0(id_prefix, "_rank"), "Rank:",
                   choices = c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), selected = "A")
      ),
      conditionalPanel(
        condition = sprintf("input['%s_operator'] == 'any' || input['%s_operator'] == 'anything_other_than'", id_prefix, id_prefix),
        selectInput(paste0(id_prefix, "_property"), "Property:",
                   choices = c("Any card" = "any_card", "Even" = "even", "Odd" = "odd",
                             "Face" = "face", "Non-face" = "non_face"),
                   selected = "any_card")
      ),
      selectInput(paste0(id_prefix, "_suit"), "Suit:",
                 choices = c("Any suit" = "any",
                           "Red (Hearts or Diamonds)" = "red",
                           "Black (Clubs or Spades)" = "black",
                           "Hearts" = "H",
                           "Diamonds" = "D",
                           "Clubs" = "C",
                           "Spades" = "S"),
                 selected = "any")
    )
  }

  # Get consequent rule from inputs
  get_consequent_rule <- function(input, id_prefix) {
    operator <- input[[paste0(id_prefix, "_operator")]]
    rule <- list(type = "card", operator = operator, suit = input[[paste0(id_prefix, "_suit")]])
    if (operator %in% c("exactly", "higher_than", "lower_than", "anything_other_than")) {
      rule$rank <- input[[paste0(id_prefix, "_rank")]]
    } else {
      rule$property <- input[[paste0(id_prefix, "_property")]]
    }
    return(rule)
  }

  # Render grid visualization
  render_grid_plot <- function(grid, matched_cells, color_scheme = "single", title = "", matched_cells_2 = NULL) {
    cells <- grid$cells
    dims <- grid$dims

    par(mar = c(4, 4, 3, 2))
    plot(NULL, xlim = c(0.5, dims["x"] + 0.5), ylim = c(0.5, dims["y"] + 0.5),
         xlab = "", ylab = "", axes = FALSE, asp = 1)

    if (title != "") {
      title(title, cex.main = 1.1, font.main = 2)
    }

    # Draw background shapes for antecedent and consequent spaces
    if (color_scheme == "single") {
      # Antecedent space (all cells) - light yellow background
      rect(0.5, 0.5, dims["x"] + 0.5, dims["y"] + 0.5,
           col = rgb(1, 0.95, 0.8, alpha = 0.3), border = NA)

      # Consequent space (matched cells) - light blue background
      if (sum(matched_cells) > 0) {
        matched_coords <- cells[matched_cells, ]
        x_range <- range(matched_coords$x)
        y_range <- range(matched_coords$y)
        rect(x_range[1] - 0.45, y_range[1] - 0.45,
             x_range[2] + 0.45, y_range[2] + 0.45,
             col = rgb(0.82, 0.93, 0.96, alpha = 0.4), border = NA)
      }
    } else if (color_scheme == "double" && !is.null(matched_cells_2)) {
      # Antecedent space - light yellow
      rect(0.5, 0.5, dims["x"] + 0.5, dims["y"] + 0.5,
           col = rgb(1, 0.95, 0.8, alpha = 0.3), border = NA)

      # Consequent B space - light orange
      if (sum(matched_cells) > 0) {
        matched_b <- cells[matched_cells, ]
        x_range_b <- range(matched_b$x)
        y_range_b <- range(matched_b$y)
        rect(x_range_b[1] - 0.45, y_range_b[1] - 0.45,
             x_range_b[2] + 0.45, y_range_b[2] + 0.45,
             col = rgb(1, 0.9, 0.87, alpha = 0.5), border = NA)
      }

      # Consequent C space - light blue
      if (sum(matched_cells_2) > 0) {
        matched_c <- cells[matched_cells_2, ]
        x_range_c <- range(matched_c$x)
        y_range_c <- range(matched_c$y)
        rect(x_range_c[1] - 0.45, y_range_c[1] - 0.45,
             x_range_c[2] + 0.45, y_range_c[2] + 0.45,
             col = rgb(0.87, 0.92, 0.97, alpha = 0.5), border = NA)
      }
    }

    # Draw cells
    count_match <- sum(matched_cells)
    count_match_2 <- if (!is.null(matched_cells_2)) sum(matched_cells_2) else 0
    count_both <- if (!is.null(matched_cells_2)) sum(matched_cells & matched_cells_2) else 0

    for (i in 1:nrow(cells)) {
      x <- cells$x[i]
      y <- cells$y[i]

      col <- if (color_scheme == "single") {
        if (matched_cells[i]) "#d4edda" else "white"
      } else if (color_scheme == "double") {
        if (!is.null(matched_cells_2)) {
          if (matched_cells[i] && matched_cells_2[i]) "#c8b2d8"
          else if (matched_cells[i]) "#fee5d9"
          else if (matched_cells_2[i]) "#deebf7"
          else "white"
        } else "white"
      } else "white"

      rect(x - 0.4, y - 0.4, x + 0.4, y + 0.4, col = col, border = "black", lwd = 0.5)
      text(x, y, cells$label[i], cex = 0.6)
    }

    # Add labels
    if (!is.null(grid$x_labels)) {
      axis(3, at = 1:dims["x"], labels = grid$x_labels, tick = FALSE, line = -1)
    }
    if (!is.null(grid$y_labels)) {
      axis(2, at = 1:dims["y"], labels = grid$y_labels, tick = FALSE, las = 1, line = -1)
    }

    # Legend
    total_cells <- nrow(cells)
    if (color_scheme == "single") {
      prob <- sum(cells$prob[matched_cells])
      text(dims["x"]/2, -0.3,
           paste0("Green cells match consequent: ", count_match, "/", total_cells,
                  " = P(A → C) = ", round(prob, 4)),
           cex = 0.9)
    } else if (color_scheme == "double" && !is.null(matched_cells_2)) {
      p_b <- sum(cells$prob[matched_cells])
      p_c <- sum(cells$prob[matched_cells_2])
      p_both <- sum(cells$prob[matched_cells & matched_cells_2])
      text(dims["x"]/2, -0.5,
           paste0("B: ", count_match, " | C: ", count_match_2, " | B∧C: ", count_both,
                  " | P(A→B)×P(A→C) = ", round(p_b * p_c, 3),
                  " vs P(A→[B∧C]) = ", round(p_both, 3)),
           cex = 0.85)
    }
  }

  # TODO: Implement Hypothetical/Empirical modes for all examples
  # - Hypothetical: Shows expected long-run frequencies with grid highlighting
  # - Empirical: Shows heat map of actual trial results
  # - Add mode toggle, restructure bars (3 bars: Antecedent, Consequent, Consequence)

  # Example 1: Antecedent, Consequent, Consequence
  observeEvent(input$toggle_ex1, {
    toggle("example-1")

    if (is.null(input$ex1_initialized)) {
      output$ex1_ui <- renderUI({
        div(
          div(class = "example-header",
              onclick = "this.classList.toggle('collapsed'); $('#ex1-content').toggle();",
              "Interactive Example: Antecedent, Consequent, Consequence"),
          div(id = "ex1-content", class = "example-content",
              p(strong("Peirce's Framework:")),
              tags$ul(
                tags$li(span(class = "hl-antecedent", "ANTECEDENT"), " = Experimental conditions (\"draw top card from well-shuffled deck\")"),
                tags$li(span(class = "hl-consequent", "CONSEQUENT"), " = Target outcome (\"the card is red\")"),
                tags$li(span(class = "hl-consequence", "CONSEQUENCE"), " = The inference rule relating them")
              ),

              div(class = "arrow-diagram",
                  HTML("<span class='hl-antecedent'>ANTECEDENT</span> <span style='color: #28a745; font-weight: bold;'>→</span> <span class='hl-consequent'>CONSEQUENT</span><br><span style='font-size: 0.9em;'>The arrow (<span class='hl-consequence'>CONSEQUENCE</span>) has the probability!</span>")
              ),

              p(strong("Formula:"), " P(A → C) = (# times A and C both occur) / (# times A occurs)"),

              fluidRow(
                column(6,
                  div(class = "control-panel",
                      selectInput("ex1_deck_type",
                                  span(class = "hl-antecedent", "Antecedent - Draw top card from:"),
                                  choices = c("Well-shuffled standard deck" = "shuffled_standard",
                                            "New standard deck (Ace♠ on top)" = "new_standard",
                                            "Well-shuffled Piquet pack (7-A)" = "shuffled_piquet",
                                            "Well-shuffled face cards (J,Q,K)" = "shuffled_face"),
                                  selected = "shuffled_standard"),

                      build_consequent_ui("ex1", "Consequent - The card is")
                  )
                ),
                column(6,
                  div(style = "margin-top: 20px;",
                      h5("Formula with values:", style = "text-align: center;"),
                      uiOutput("ex1_formula_display")
                  )
                )
              ),

              div(class = "plot-container",
                  plotOutput("ex1_grid", height = "400px")
              ),

              div(class = "calc-output",
                  verbatimTextOutput("ex1_calc")
              )
          )
        )
      })

      session$sendCustomMessage("ex1_initialized", TRUE)

      insertUI(
        selector = "#example-1",
        ui = uiOutput("ex1_ui")
      )
    }
  })

  output$ex1_formula_display <- renderUI({
    req(input$ex1_deck_type, input$ex1_operator)

    grid <- create_grid("card_deck", list(deck_type = input$ex1_deck_type))
    rule <- get_consequent_rule(input, "ex1")
    matched <- evaluate_consequent(grid, rule)

    successes <- sum(matched)
    total <- nrow(grid$cells)

    div(style = "text-align: center; font-size: 18px; margin-top: 20px;",
      div(style = "margin-bottom: 10px; font-weight: bold;",
        paste0("P(A → C) = ", successes, "/", total, " = ", round(successes/total, 4)))
    )
  })

  output$ex1_grid <- renderPlot({
    req(input$ex1_deck_type, input$ex1_operator)

    grid <- create_grid("card_deck", list(deck_type = input$ex1_deck_type))
    rule <- get_consequent_rule(input, "ex1")
    matched <- evaluate_consequent(grid, rule)
    render_grid_plot(grid, matched, "single", "Possibility Space: Each Card in the Deck")
  })

  output$ex1_calc <- renderText({
    req(input$ex1_deck_type, input$ex1_operator)

    antecedent_desc <- if (input$ex1_deck_type == "new_standard") {
      "Draw top card from NEW STANDARD deck (Ace♠ on top)"
    } else if (input$ex1_deck_type == "shuffled_face") {
      "Draw top card from WELL-SHUFFLED FACE CARD deck (J,Q,K)"
    } else if (input$ex1_deck_type == "shuffled_piquet") {
      "Draw top card from WELL-SHUFFLED PIQUET PACK (7-A)"
    } else {
      "Draw top card from WELL-SHUFFLED STANDARD deck"
    }

    grid <- create_grid("card_deck", list(deck_type = input$ex1_deck_type))
    rule <- get_consequent_rule(input, "ex1")
    matched <- evaluate_consequent(grid, rule)

    successes <- sum(matched)
    total <- nrow(grid$cells)
    prob <- successes / total

    paste0(
      "ANTECEDENT: ", antecedent_desc, "\n",
      "CONSEQUENT: [See selections above]\n\n",
      "CONSEQUENCE: \"IF ", antecedent_desc, ", THEN [consequent]\"\n\n",
      "THEORETICAL PROBABILITY:\n",
      "  P(A → C) = ", successes, " / ", total, " = ", round(prob, 4), "\n\n",
      "NOTE: Probability belongs to the CONSEQUENCE (the arrow),\n",
      "not to individual facts. There is no P(E), only P(A → C)."
    )
  })

  # Dice Example
  observeEvent(input$toggle_dice, {
    toggle("example-dice")

    if (is.null(input$dice_initialized)) {
      output$dice_ui <- renderUI({
        div(
          div(class = "example-header",
              onclick = "this.classList.toggle('collapsed'); $('#dice-content').toggle();",
              "Interactive Example: Peirce's Dice Problems"),
          div(id = "dice-content", class = "example-content",
              fluidRow(
                column(6,
                  div(class = "control-panel",
                      selectInput("dice_mode",
                                 "Select Mode:",
                                 choices = c("Addition Rule (Deuce-Ace)" = "add",
                                           "Multiplication Rule (Double Sixes)" = "multiply",
                                           "Custom (Choose Your Own)" = "custom"),
                                 selected = "add"),

                      conditionalPanel(
                        condition = "input.dice_mode == 'add'",
                        p(strong("Addition Rule Example:")),
                        p("\"What is the probability of throwing deuce-ace?\""),
                        p("Two incompatible ways:", tags$ul(
                          tags$li("First die = Ace (1), Second die = Deuce (2)"),
                          tags$li("First die = Deuce (2), Second die = Ace (1)")
                        ))
                      ),

                      conditionalPanel(
                        condition = "input.dice_mode == 'multiply'",
                        p(strong("Multiplication Rule Example:")),
                        p("\"What is the probability of throwing double sixes?\""),
                        p("Two independent events:", tags$ul(
                          tags$li("First die = 6"),
                          tags$li("Second die = 6")
                        ))
                      ),

                      conditionalPanel(
                        condition = "input.dice_mode == 'custom'",
                        p(strong("Custom Mode:")),
                        p("Select the values you want to highlight:"),
                        fluidRow(
                          column(6,
                            selectInput("dice_custom_die1",
                                       span(class = "hl-event-a", "First Die:"),
                                       choices = c("Any" = "any", "1" = "1", "2" = "2",
                                                 "3" = "3", "4" = "4", "5" = "5", "6" = "6"),
                                       selected = "any")
                          ),
                          column(6,
                            selectInput("dice_custom_die2",
                                       span(class = "hl-event-b", "Second Die:"),
                                       choices = c("Any" = "any", "1" = "1", "2" = "2",
                                                 "3" = "3", "4" = "4", "5" = "5", "6" = "6"),
                                       selected = "any")
                          )
                        )
                      )
                  )
                ),
                column(6,
                  div(class = "plot-container",
                      plotOutput("dice_grid", height = "300px")
                  )
                )
              ),

              div(class = "formula", verbatimTextOutput("dice_calc"))
          )
        )
      })
      updateTextInput(session, "dice_initialized", value = "1")
    }
  })

  output$dice_grid <- renderPlot({
    req(input$dice_mode)

    grid <- create_grid("two_dice", list())

    if (input$dice_mode == "add") {
      # Deuce-ace: (1,2) or (2,1)
      matched <- (grid$cells$die1 == 1 & grid$cells$die2 == 2) |
                 (grid$cells$die1 == 2 & grid$cells$die2 == 1)
    } else if (input$dice_mode == "multiply") {
      # Double sixes: (6,6)
      matched <- (grid$cells$die1 == 6 & grid$cells$die2 == 6)
    } else {
      # Custom mode
      req(input$dice_custom_die1, input$dice_custom_die2)
      die1_val <- input$dice_custom_die1
      die2_val <- input$dice_custom_die2

      matched <- rep(TRUE, nrow(grid$cells))
      if (die1_val != "any") {
        matched <- matched & (grid$cells$die1 == as.numeric(die1_val))
      }
      if (die2_val != "any") {
        matched <- matched & (grid$cells$die2 == as.numeric(die2_val))
      }
    }

    render_grid_plot(grid, matched, "single", "Two Dice: All Possible Outcomes")
  })

  output$dice_calc <- renderText({
    req(input$dice_mode)

    if (input$dice_mode == "add") {
      paste0(
        "ADDITION RULE (Incompatible Events)\n\n",
        "Question: What is the probability of throwing deuce-ace?\n\n",
        "Event B: First die = 1, Second die = 2\n",
        "  P(A → B) = 1/36 = ", round(1/36, 4), "\n\n",
        "Event C: First die = 2, Second die = 1\n",
        "  P(A → C) = 1/36 = ", round(1/36, 4), "\n\n",
        "These are INCOMPATIBLE (can't both happen).\n",
        "By the Addition Rule:\n",
        "  P(A → [B or C]) = P(A → B) + P(A → C)\n",
        "  P(deuce-ace) = 1/36 + 1/36 = 2/36 = 1/18 = ", round(2/36, 4), "\n\n",
        "Peirce's words: \"these two events—first, ace; second, deuce;\n",
        "and, second, ace; first, deuce—are incompatible. Hence the rule\n",
        "for addition holds, and the probability that either will come up\n",
        "ace and the other deuce is 1/36 + 1/36, or 1/18.\""
      )
    } else if (input$dice_mode == "multiply") {
      paste0(
        "MULTIPLICATION RULE (Independent Events)\n\n",
        "Question: What is the probability of throwing double sixes?\n\n",
        "Event B: First die = 6\n",
        "  P(A → B) = 6/36 = 1/6 = ", round(1/6, 4), "\n\n",
        "Event C: Second die = 6\n",
        "  P(A → C) = 6/36 = 1/6 = ", round(1/6, 4), "\n\n",
        "These are INDEPENDENT (one doesn't affect the other).\n",
        "By the Multiplication Rule:\n",
        "  P(A → [B and C]) = P(A → B) × P(A → C)\n",
        "  P(double sixes) = 1/6 × 1/6 = 1/36 = ", round(1/36, 4), "\n\n",
        "Peirce's words: \"the probability that both events will happen\n",
        "together is the product of their several probabilities, 1/6 × 1/6.\""
      )
    } else {
      # Custom mode
      req(input$dice_custom_die1, input$dice_custom_die2)
      die1_val <- input$dice_custom_die1
      die2_val <- input$dice_custom_die2

      grid <- create_grid("two_dice", list())
      matched <- rep(TRUE, nrow(grid$cells))
      if (die1_val != "any") {
        matched <- matched & (grid$cells$die1 == as.numeric(die1_val))
      }
      if (die2_val != "any") {
        matched <- matched & (grid$cells$die2 == as.numeric(die2_val))
      }

      count <- sum(matched)
      total <- nrow(grid$cells)

      die1_desc <- if (die1_val == "any") "Any value" else paste0("Value = ", die1_val)
      die2_desc <- if (die2_val == "any") "Any value" else paste0("Value = ", die2_val)

      paste0(
        "CUSTOM MODE\n\n",
        "First Die: ", die1_desc, "\n",
        "Second Die: ", die2_desc, "\n\n",
        "Matching outcomes: ", count, " / ", total, "\n",
        "Probability: ", count, "/", total, " = ", round(count/total, 4), "\n\n",
        if (die1_val == "any" && die2_val == "any") {
          "All outcomes match (probability = 1.0000)"
        } else if (die1_val != "any" && die2_val != "any") {
          paste0("Specific outcome: (", die1_val, ", ", die2_val, ")\n",
                 "This is a single outcome with probability 1/36 = ", round(1/36, 4))
        } else {
          paste0("One die fixed, one die varies.\n",
                 "Probability = 6/36 = 1/6 = ", round(1/6, 4))
        }
      )
    }
  })

  # Inference Rules Example
  observeEvent(input$toggle_inference, {
    toggle("example-inference")

    if (is.null(input$inference_initialized)) {
      output$inference_ui <- renderUI({
        div(
          div(class = "example-header",
              onclick = "this.classList.toggle('collapsed'); $('#inference-content').toggle();",
              "Interactive Example: Two Methods of Inference"),
          div(id = "inference-content", class = "example-content",
              p(strong("Question:"), " Is there a bean under this thimble?"),
              p("We have two methods for testing this, and we want to know how reliable they are."),

              fluidRow(
                column(6,
                  sliderInput("inference_method1_accuracy",
                             span(class = "hl-event-a", "Method 1 Accuracy:"),
                             min = 0, max = 100, value = 81, step = 1,
                             post = "%")
                ),
                column(6,
                  sliderInput("inference_method2_accuracy",
                             span(class = "hl-event-b", "Method 2 Accuracy:"),
                             min = 0, max = 100, value = 93, step = 1,
                             post = "%")
                )
              ),

              actionButton("inference_randomize", "Randomize 100 Tests", class = "btn-primary"),

              div(class = "plot-container", style = "margin-top: 20px;",
                  plotOutput("inference_grid", height = "400px")
              ),

              div(class = "formula", verbatimTextOutput("inference_calc"))
          )
        )
      })
      updateTextInput(session, "inference_initialized", value = "1")
    }
  })

  # Reactive values for inference grid
  inference_data <- reactiveVal(NULL)

  # Initialize or randomize the inference grid
  observeEvent(c(input$inference_randomize, input$inference_method1_accuracy, input$inference_method2_accuracy), {
    if (!is.null(input$inference_method1_accuracy) && !is.null(input$inference_method2_accuracy)) {
      # Use a valid seed based on current time
      set.seed(as.integer(Sys.time()) %% 100000)

      grid_size <- 10
      n_cells <- grid_size * grid_size

      # Generate ground truth: is there actually a bean?
      has_bean <- sample(c(TRUE, FALSE), n_cells, replace = TRUE)

      # Method 1 accuracy
      acc1 <- input$inference_method1_accuracy / 100
      method1_correct <- runif(n_cells) < acc1
      method1_answer <- ifelse(method1_correct, has_bean, !has_bean)

      # Method 2 accuracy
      acc2 <- input$inference_method2_accuracy / 100
      method2_correct <- runif(n_cells) < acc2
      method2_answer <- ifelse(method2_correct, has_bean, !has_bean)

      # Create grid data
      grid_data <- expand.grid(x = 1:grid_size, y = 1:grid_size)
      grid_data$has_bean <- has_bean
      grid_data$method1_correct <- method1_correct
      grid_data$method2_correct <- method2_correct
      grid_data$method1_answer <- method1_answer
      grid_data$method2_answer <- method2_answer

      inference_data(grid_data)
    }
  }, ignoreNULL = FALSE)

  output$inference_grid <- renderPlot({
    data <- inference_data()
    if (is.null(data)) return(NULL)

    # Create display: show has_bean status and method correctness
    data$display <- paste0(
      ifelse(data$has_bean, "B", "—"),
      "\n",
      ifelse(data$method1_correct, "✓", "✗"),
      " ",
      ifelse(data$method2_correct, "✓", "✗")
    )

    # Color by bean presence
    data$bean_color <- ifelse(data$has_bean, "Bean", "No Bean")

    ggplot(data, aes(x = x, y = y)) +
      geom_tile(aes(fill = bean_color), color = "black", linewidth = 0.8) +
      geom_text(aes(label = display), size = 3, lineheight = 0.8) +
      scale_fill_manual(values = c("Bean" = "#c8e6c9", "No Bean" = "#ffccbc"),
                       name = "Ground Truth") +
      coord_fixed() +
      theme_void() +
      theme(legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            plot.margin = margin(10, 10, 10, 10)) +
      labs(caption = "B = Bean present | ✓ = Method correct | ✗ = Method incorrect")
  })

  output$inference_calc <- renderText({
    data <- inference_data()
    if (is.null(data)) return("Click 'Randomize 100 Tests' to begin")

    m1_correct <- sum(data$method1_correct)
    m2_correct <- sum(data$method2_correct)
    both_correct <- sum(data$method1_correct & data$method2_correct)
    both_incorrect <- sum(!data$method1_correct & !data$method2_correct)
    both_agree <- sum(data$method1_answer == data$method2_answer)
    agree_and_correct <- sum((data$method1_answer == data$method2_answer) &
                             data$method1_correct & data$method2_correct)

    paste0(
      "RESULTS FROM 100 TESTS\n\n",
      "Method 1: ", m1_correct, "/100 correct\n",
      "Method 2: ", m2_correct, "/100 correct\n\n",
      "Both methods correct: ", both_correct, "/100\n",
      "Both methods incorrect: ", both_incorrect, "/100\n\n",
      "WHEN BOTH METHODS AGREE:\n",
      "Times they agreed: ", both_agree, "/100\n",
      "Times they agreed AND were correct: ", agree_and_correct, "/", both_agree, "\n",
      "Probability correct when agreeing: ", round(agree_and_correct/both_agree, 4)
    )
  })

  # Example 2: Addition Rule
  observeEvent(input$toggle_ex2, {
    toggle("example-2")

    if (is.null(input$ex2_initialized)) {
      output$ex2_ui <- renderUI({
        div(
          div(class = "example-header",
              onclick = "this.classList.toggle('collapsed'); $('#ex2-content').toggle();",
              "Interactive Example: Addition Rule for Consequences"),
          div(id = "ex2-content", class = "example-content",
              p(strong("The Rule:"), " Given two consequences with the ", em("same antecedent"),
                " but ", em("incompatible consequents"), ", we add their probabilities:"),
              p("P(A → C₁) + P(A → C₂) = P(A → [C₁ or C₂])"),

              div(class = "control-panel",
                  selectInput("ex2_deck_type",
                              span(class = "hl-antecedent", "Antecedent - Draw top card from:"),
                              choices = c("Well-shuffled standard deck" = "shuffled_standard",
                                        "New standard deck (Ace♠ on top)" = "new_standard",
                                        "Well-shuffled Piquet pack (7-A)" = "shuffled_piquet",
                                        "Well-shuffled face cards (J,Q,K)" = "shuffled_face"),
                              selected = "shuffled_standard"),

                  selectInput("ex2_target1",
                              span(class = "hl-event-a", "CONSEQUENT A - Card is:"),
                              choices = c("Ace of Spades" = "ace_spades",
                                        "Red (hearts or diamonds)" = "red",
                                        "Face card (J,Q,K)" = "face",
                                        "Even (2,4,6,8,10)" = "even",
                                        "Less than 10 (A-9)" = "less_10",
                                        "Higher than 8 (9,10,J,Q,K,A)" = "higher_8"),
                              selected = "ace_spades"),

                  selectInput("ex2_target2",
                              span(class = "hl-event-b", "CONSEQUENT B - Card is:"),
                              choices = c("Ace of Spades" = "ace_spades",
                                        "Red (hearts or diamonds)" = "red",
                                        "Face card (J,Q,K)" = "face",
                                        "Even (2,4,6,8,10)" = "even",
                                        "Less than 10 (A-9)" = "less_10",
                                        "Higher than 8 (9,10,J,Q,K,A)" = "higher_8"),
                              selected = "even"),

                  sliderInput("ex2_n_trials",
                              "Number of trials (n):",
                              min = 10, max = 10000, value = 520, step = 10)
              ),

              div(class = "plot-container",
                  plotOutput("ex2_grid", height = "400px")
              ),

              div(class = "calc-output",
                  verbatimTextOutput("ex2_calc")
              )
          )
        )
      })

      session$sendCustomMessage("ex2_initialized", TRUE)

      insertUI(
        selector = "#example-2",
        ui = uiOutput("ex2_ui")
      )
    }
  })

  output$ex2_grid <- renderPlot({
    req(input$ex2_deck_type, input$ex2_target1, input$ex2_target2, input$ex2_n_trials)

    deck_type <- input$ex2_deck_type
    t1 <- input$ex2_target1
    t2 <- input$ex2_target2
    n <- input$ex2_n_trials

    # Define deck based on type
    if (deck_type == "shuffled_face") {
      suits <- c("H", "D", "C", "S")
      ranks <- c("J", "Q", "K")
      n_suits <- 4
      n_ranks <- 3
    } else if (deck_type == "shuffled_piquet") {
      suits <- c("H", "D", "C", "S")
      ranks <- c("7", "8", "9", "10", "J", "Q", "K", "A")
      n_suits <- 4
      n_ranks <- 8
    } else if (deck_type == "new_standard") {
      # For new deck, show all cards but only Ace of Spades is possible
      suits <- c("H", "D", "C", "S")
      ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
      n_suits <- 4
      n_ranks <- 13
    } else {
      # shuffled_standard
      suits <- c("H", "D", "C", "S")
      ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
      n_suits <- 4
      n_ranks <- 13
    }

    # Function to check matches
    card_matches <- function(suit, rank, target) {
      if (target == "red") return(suit %in% c("H", "D"))
      if (target == "ace_spades") return(suit == "S" && rank == "A")
      if (target == "even") return(rank %in% c("2", "4", "6", "8", "10"))
      if (target == "face") return(rank %in% c("J", "Q", "K"))
      if (target == "less_10") return(rank %in% c("2", "3", "4", "5", "6", "7", "8", "9"))
      if (target == "higher_8") return(rank %in% c("9", "10", "J", "Q", "K", "A"))
      return(FALSE)
    }

    # Create grid and classify each card
    par(mar = c(4, 4, 3, 2))
    plot(NULL, xlim = c(0.5, n_ranks + 0.5), ylim = c(0.5, n_suits + 0.5),
         xlab = "", ylab = "", axes = FALSE, asp = 1)
    title("Possibility Space: Each Card in the Deck", cex.main = 1.1, font.main = 2)

    # Draw grid and color cells
    for (i in 1:n_suits) {
      for (j in 1:n_ranks) {
        suit <- suits[i]
        rank <- ranks[j]

        matches1 <- card_matches(suit, rank, t1)
        matches2 <- card_matches(suit, rank, t2)

        # Determine color
        if (matches1 && matches2) {
          col <- "#9370DB"  # Purple for overlap
        } else if (matches1) {
          col <- "#fee5d9"  # Orange for C1
        } else if (matches2) {
          col <- "#deebf7"  # Blue for C2
        } else {
          col <- "white"
        }

        rect(j - 0.4, i - 0.4, j + 0.4, i + 0.4, col = col, border = "black", lwd = 0.5)
        text(j, i, paste0(rank, suit), cex = 0.6)
      }
    }

    # Add axis labels
    axis(1, at = 1:n_ranks, labels = ranks, tick = FALSE, line = -1)
    axis(2, at = 1:n_suits, labels = c("Hearts", "Diamonds", "Clubs", "Spades"),
         tick = FALSE, las = 1, line = -1)

    # Count cards
    count1 <- 0
    count2 <- 0
    count_overlap <- 0
    count_either <- 0

    for (i in 1:n_suits) {
      for (j in 1:n_ranks) {
        m1 <- card_matches(suits[i], ranks[j], t1)
        m2 <- card_matches(suits[i], ranks[j], t2)
        if (m1 && m2) count_overlap <- count_overlap + 1
        if (m1) count1 <- count1 + 1
        if (m2) count2 <- count2 + 1
        if (m1 || m2) count_either <- count_either + 1
      }
    }

    total_cards <- n_suits * n_ranks
    p1 <- count1 / total_cards
    p2 <- count2 / total_cards
    p_combined <- count_either / total_cards

    # Legend
    legend_y <- n_suits + 0.8
    text(n_ranks/2, legend_y + 0.6,
         paste0("P(C₁) = ", count1, "/", total_cards, " = ", round(p1, 3),
                "   P(C₂) = ", count2, "/", total_cards, " = ", round(p2, 3)),
         cex = 0.9)

    if (count_overlap > 0) {
      text(n_ranks/2, legend_y + 0.2,
           paste0("OVERLAP: ", count_overlap, " cards → P(C₁) + P(C₂) = ", round(p1 + p2, 3),
                  " ≠ P(C₁ or C₂) = ", round(p_combined, 3)),
           cex = 0.85, col = "red")
    } else {
      text(n_ranks/2, legend_y + 0.2,
           paste0("NO OVERLAP → P(C₁) + P(C₂) = ", round(p1 + p2, 3),
                  " = P(C₁ or C₂) = ", round(p_combined, 3)),
           cex = 0.85, col = "#28a745")
    }

    # Color legend
    rect(1, -0.2, 2, -0.5, col = "#fee5d9", border = "black")
    text(2.5, -0.35, "C₁ only", adj = 0, cex = 0.8)

    rect(4, -0.2, 5, -0.5, col = "#deebf7", border = "black")
    text(5.5, -0.35, "C₂ only", adj = 0, cex = 0.8)

    if (count_overlap > 0) {
      rect(7, -0.2, 8, -0.5, col = "#9370DB", border = "black")
      text(8.5, -0.35, "Both (overlap)", adj = 0, cex = 0.8)
    }
  })

  output$ex2_calc <- renderText({
    req(input$ex2_deck_type, input$ex2_target1, input$ex2_target2, input$ex2_n_trials)

    deck_type <- input$ex2_deck_type
    t1 <- input$ex2_target1
    t2 <- input$ex2_target2
    n <- input$ex2_n_trials

    # Helper function
    card_matches <- function(suit, rank, target) {
      if (target == "red") return(suit %in% c("hearts", "diamonds"))
      if (target == "ace_spades") return(suit == "spades" && rank == "A")
      if (target == "even") return(rank %in% c("2", "4", "6", "8", "10"))
      if (target == "face") return(rank %in% c("J", "Q", "K"))
      if (target == "less_10") return(rank %in% c("2", "3", "4", "5", "6", "7", "8", "9"))
      if (target == "higher_8") return(rank %in% c("9", "10", "J", "Q", "K", "A"))
      return(FALSE)
    }

    # Descriptions
    antecedent_desc <- if (deck_type == "new_standard") {
      "Draw top card from NEW STANDARD deck (Ace♠ on top)"
    } else if (deck_type == "shuffled_face") {
      "Draw top card from WELL-SHUFFLED FACE CARD deck (J,Q,K)"
    } else if (deck_type == "shuffled_piquet") {
      "Draw top card from WELL-SHUFFLED PIQUET PACK (7-A)"
    } else {
      "Draw top card from WELL-SHUFFLED STANDARD deck"
    }

    consequent1_desc <- switch(t1,
      "red" = "Card is RED",
      "ace_spades" = "Card is ACE OF SPADES",
      "even" = "Card is EVEN (2,4,6,8,10)",
      "face" = "Card is FACE CARD (J,Q,K)",
      "less_10" = "Card is LESS THAN 10 (A-9)",
      "higher_8" = "Card is HIGHER THAN 8 (9,10,J,Q,K,A)",
      t1
    )

    consequent2_desc <- switch(t2,
      "red" = "Card is RED",
      "ace_spades" = "Card is ACE OF SPADES",
      "even" = "Card is EVEN (2,4,6,8,10)",
      "face" = "Card is FACE CARD (J,Q,K)",
      "less_10" = "Card is LESS THAN 10 (A-9)",
      "higher_8" = "Card is HIGHER THAN 8 (9,10,J,Q,K,A)",
      t2
    )

    # Define deck based on type
    if (deck_type == "shuffled_face") {
      suits <- rep(c("hearts", "diamonds", "clubs", "spades"), each = 3)
      ranks <- rep(c("J", "Q", "K"), 4)
      deck_size <- 12
    } else if (deck_type == "shuffled_piquet") {
      suits <- rep(c("hearts", "diamonds", "clubs", "spades"), each = 8)
      ranks <- rep(c("7", "8", "9", "10", "J", "Q", "K", "A"), 4)
      deck_size <- 32
    } else {
      suits <- rep(c("hearts", "diamonds", "clubs", "spades"), each = 13)
      ranks <- rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4)
      deck_size <- 52
    }

    set.seed(42)
    if (deck_type == "new_standard") {
      results1 <- sapply(1:n, function(i) card_matches("spades", "A", t1))
      results2 <- sapply(1:n, function(i) card_matches("spades", "A", t2))
      results_either <- results1 | results2
    } else {
      results1 <- sapply(1:n, function(i) {
        idx <- sample(1:deck_size, 1)
        card_matches(suits[idx], ranks[idx], t1)
      })
      results2 <- sapply(1:n, function(i) {
        idx <- sample(1:deck_size, 1)
        card_matches(suits[idx], ranks[idx], t2)
      })
      results_either <- sapply(1:n, function(i) {
        idx <- sample(1:deck_size, 1)
        card_matches(suits[idx], ranks[idx], t1) || card_matches(suits[idx], ranks[idx], t2)
      })
    }

    count1 <- sum(results1)
    count2 <- sum(results2)
    count_either <- sum(results_either)

    p1 <- count1 / n
    p2 <- count2 / n
    p_combined <- count_either / n

    paste0(
      "SAME ANTECEDENT: ", antecedent_desc, "\n\n",
      "CONSEQUENCE A: \"IF ", antecedent_desc, ", THEN ", consequent1_desc, "\"\n",
      "  - C₁ occurs: ", count1, " / ", n, " times\n",
      "  - P(A → C₁) = ", round(p1, 4), "\n\n",
      "CONSEQUENCE B: \"IF ", antecedent_desc, ", THEN ", consequent2_desc, "\"\n",
      "  - C₂ occurs: ", count2, " / ", n, " times\n",
      "  - P(A → C₂) = ", round(p2, 4), "\n\n",
      "COMBINED CONSEQUENCE: \"IF A, THEN (C₁ OR C₂)\"\n",
      "  - Either consequent: ", count_either, " / ", n, " times\n",
      "  - P(A → [C₁ or C₂]) = ", round(p_combined, 4), "\n\n",
      "ADDITION RULE:\n",
      "  P(A → C₁) + P(A → C₂) = ", round(p1, 4), " + ", round(p2, 4), " = ", round(p1 + p2, 4), "\n",
      "  P(A → [C₁ or C₂]) = ", round(p_combined, 4), "\n\n",
      "This works when C₁ and C₂ are INCOMPATIBLE (no card matches both)."
    )
  })

  # Example 3: Multiplication Rule
  observeEvent(input$toggle_ex3, {
    toggle("example-3")

    if (is.null(input$ex3_initialized)) {
      output$ex3_ui <- renderUI({
        div(
          div(class = "example-header",
              onclick = "this.classList.toggle('collapsed'); $('#ex3-content').toggle();",
              "Interactive Example: Multiplication Rule for Consequences"),
          div(id = "ex3-content", class = "example-content",
              p(strong("The Rule:"), " Given two consequences where the antecedent of the second includes the consequent of the first:"),
              p("P(A → B) × P(A∧B → C) = P(A → [B ∧ C])"),

              div(class = "arrow-diagram",
                  HTML("<span class='hl-antecedent'>A</span> <span style='color: #28a745; font-weight: bold;'>→</span> <span class='hl-consequent'>B</span> (prob P₁)<br>",
                       "<span class='hl-antecedent'>A ∧ B</span> <span style='color: #28a745; font-weight: bold;'>→</span> <span class='hl-consequent'>C</span> (prob P₂)<br>",
                       "<span style='color: #666;'>Therefore:</span> <span class='hl-antecedent'>A</span> <span style='color: #28a745; font-weight: bold;'>→</span> <span class='hl-combined'>B ∧ C</span> (prob P₁ × P₂)")
              ),

              div(class = "control-panel",
                  selectInput("ex3_deck_type",
                              span(class = "hl-antecedent", "Antecedent A - Draw top card from:"),
                              choices = c("Well-shuffled standard deck" = "shuffled_standard",
                                        "New standard deck (Ace♠ on top)" = "new_standard",
                                        "Well-shuffled Piquet pack (7-A)" = "shuffled_piquet",
                                        "Well-shuffled face cards (J,Q,K)" = "shuffled_face"),
                              selected = "shuffled_standard"),

                  selectInput("ex3_b",
                              span(class = "hl-event-a", "Consequent B - First condition:"),
                              choices = c("Ace of Spades" = "ace_spades",
                                        "Red (hearts or diamonds)" = "red",
                                        "Face card (J,Q,K)" = "face",
                                        "Even (2,4,6,8,10)" = "even",
                                        "Less than 10 (A-9)" = "less_10",
                                        "Higher than 8 (9,10,J,Q,K,A)" = "higher_8"),
                              selected = "red"),

                  selectInput("ex3_c",
                              span(class = "hl-event-b", "Consequent C - Second condition (given B):"),
                              choices = c("Ace of Spades" = "ace_spades",
                                        "Red (hearts or diamonds)" = "red",
                                        "Face card (J,Q,K)" = "face",
                                        "Even (2,4,6,8,10)" = "even",
                                        "Less than 10 (A-9)" = "less_10",
                                        "Higher than 8 (9,10,J,Q,K,A)" = "higher_8"),
                              selected = "face"),

                  sliderInput("ex3_n_trials",
                              "Number of trials (n):",
                              min = 10, max = 10000, value = 520, step = 10)
              ),

              div(class = "plot-container",
                  plotOutput("ex3_plot", height = "300px")
              ),

              div(class = "plot-container",
                  plotOutput("ex3_grid", height = "400px")
              ),

              div(class = "calc-output",
                  verbatimTextOutput("ex3_calc")
              )
          )
        )
      })

      session$sendCustomMessage("ex3_initialized", TRUE)

      insertUI(
        selector = "#example-3",
        ui = uiOutput("ex3_ui")
      )
    }
  })

  output$ex3_plot <- renderPlot({
    req(input$ex3_deck_type, input$ex3_b, input$ex3_c, input$ex3_n_trials)

    deck_type <- input$ex3_deck_type
    b_cond <- input$ex3_b
    c_cond <- input$ex3_c
    n <- input$ex3_n_trials

    # Define deck based on type
    if (deck_type == "shuffled_face") {
      suits <- rep(c("hearts", "diamonds", "clubs", "spades"), each = 3)
      ranks <- rep(c("J", "Q", "K"), 4)
      deck_size <- 12
    } else if (deck_type == "shuffled_piquet") {
      suits <- rep(c("hearts", "diamonds", "clubs", "spades"), each = 8)
      ranks <- rep(c("7", "8", "9", "10", "J", "Q", "K", "A"), 4)
      deck_size <- 32
    } else {
      suits <- rep(c("hearts", "diamonds", "clubs", "spades"), each = 13)
      ranks <- rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4)
      deck_size <- 52
    }

    matches_b <- function(suit, rank) {
      if (b_cond == "red") return(suit %in% c("hearts", "diamonds"))
      if (b_cond == "ace_spades") return(suit == "spades" && rank == "A")
      if (b_cond == "even") return(rank %in% c("2", "4", "6", "8", "10"))
      if (b_cond == "face") return(rank %in% c("J", "Q", "K"))
      if (b_cond == "less_10") return(rank %in% c("A", "2", "3", "4", "5", "6", "7", "8", "9"))
      if (b_cond == "higher_8") return(rank %in% c("9", "10", "J", "Q", "K", "A"))
      return(FALSE)
    }

    matches_c <- function(suit, rank) {
      if (c_cond == "red") return(suit %in% c("hearts", "diamonds"))
      if (c_cond == "ace_spades") return(suit == "spades" && rank == "A")
      if (c_cond == "even") return(rank %in% c("2", "4", "6", "8", "10"))
      if (c_cond == "face") return(rank %in% c("J", "Q", "K"))
      if (c_cond == "less_10") return(rank %in% c("A", "2", "3", "4", "5", "6", "7", "8", "9"))
      if (c_cond == "higher_8") return(rank %in% c("9", "10", "J", "Q", "K", "A"))
      return(FALSE)
    }

    matches_c_given_b <- function(suit, rank) {
      return(matches_b(suit, rank) && matches_c(suit, rank))
    }

    # Simulate - use same draws for both calculations
    set.seed(42)
    if (deck_type == "new_standard") {
      card_suit <- rep("spades", n)
      card_rank <- rep("A", n)
    } else {
      sampled_indices <- sample(1:deck_size, n, replace = TRUE)
      card_suit <- suits[sampled_indices]
      card_rank <- ranks[sampled_indices]
    }

    results_b <- sapply(1:n, function(i) matches_b(card_suit[i], card_rank[i]))
    results_both <- sapply(1:n, function(i) matches_c_given_b(card_suit[i], card_rank[i]))

    count_b <- sum(results_b)
    count_both <- sum(results_both)

    p_b <- count_b / n
    p_c_given_b <- if (count_b > 0) count_both / count_b else 0
    p_both <- count_both / n

    # Visualization
    par(mar = c(3, 1, 3, 1))
    plot(NULL, xlim = c(0, 1), ylim = c(0, 4.5), axes = FALSE, xlab = "", ylab = "")

    title(paste0("Multiplication Rule: n = ", n, " trials"), cex.main = 1.2, font.main = 2)

    # Bar 1: P(A → B)
    text(0, 4.0, "P(A → B):", cex = 0.9, adj = 0)
    rect(0.2, 3.8, 1, 4.1, col = "white", border = "black", lwd = 1.5)
    rect(0.2, 3.8, 0.2 + p_b * 0.8, 4.1, col = "#fee5d9", border = NA)
    text(0.6, 3.95, paste0(round(p_b, 4)), cex = 1, font = 2)

    # Bar 2: P(A∧B → C) [calculated from those where B holds]
    text(0, 3.3, "P(A∧B → C):", cex = 0.9, adj = 0)
    rect(0.2, 3.1, 1, 3.4, col = "white", border = "black", lwd = 1.5)
    rect(0.2, 3.1, 0.2 + p_c_given_b * 0.8, 3.4, col = "#deebf7", border = NA)
    text(0.6, 3.25, paste0(round(p_c_given_b, 4)), cex = 1, font = 2)

    # Product line
    text(0.5, 2.7, paste0("Product: ", round(p_b * p_c_given_b, 4)), cex = 1, font = 1, col = "#666666")

    # Bar 3: P(A → [B∧C])
    text(0, 2.2, "P(A → [B∧C]):", cex = 0.9, adj = 0)
    rect(0.2, 2.0, 1, 2.3, col = "white", border = "black", lwd = 1.5)
    rect(0.2, 2.0, 0.2 + p_both * 0.8, 2.3, col = "#d4e4c4", border = NA)
    text(0.6, 2.15, paste0(round(p_both, 4)), cex = 1, font = 2)

    # Note
    text(0.5, 1.2, paste0("Chaining: ", count_b, " times B holds, of those ", count_both, " also have C"),
         cex = 0.85, col = "#666666", font = 3)
  })

  output$ex3_grid <- renderPlot({
    req(input$ex3_deck_type, input$ex3_b, input$ex3_c)

    deck_type <- input$ex3_deck_type
    b_cond <- input$ex3_b
    c_cond <- input$ex3_c

    # Define deck based on type
    if (deck_type == "shuffled_face") {
      suits <- c("H", "D", "C", "S")
      ranks <- c("J", "Q", "K")
      n_suits <- 4
      n_ranks <- 3
    } else if (deck_type == "shuffled_piquet") {
      suits <- c("H", "D", "C", "S")
      ranks <- c("7", "8", "9", "10", "J", "Q", "K", "A")
      n_suits <- 4
      n_ranks <- 8
    } else {
      suits <- c("H", "D", "C", "S")
      ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
      n_suits <- 4
      n_ranks <- 13
    }

    # Function to check matches
    matches_b <- function(suit, rank) {
      if (b_cond == "red") return(suit %in% c("H", "D"))
      if (b_cond == "ace_spades") return(suit == "S" && rank == "A")
      if (b_cond == "even") return(rank %in% c("2", "4", "6", "8", "10"))
      if (b_cond == "face") return(rank %in% c("J", "Q", "K"))
      if (b_cond == "less_10") return(rank %in% c("2", "3", "4", "5", "6", "7", "8", "9"))
      if (b_cond == "higher_8") return(rank %in% c("9", "10", "J", "Q", "K", "A"))
      return(FALSE)
    }

    matches_c <- function(suit, rank) {
      if (c_cond == "red") return(suit %in% c("H", "D"))
      if (c_cond == "ace_spades") return(suit == "S" && rank == "A")
      if (c_cond == "even") return(rank %in% c("2", "4", "6", "8", "10"))
      if (c_cond == "face") return(rank %in% c("J", "Q", "K"))
      if (c_cond == "less_10") return(rank %in% c("2", "3", "4", "5", "6", "7", "8", "9"))
      if (c_cond == "higher_8") return(rank %in% c("9", "10", "J", "Q", "K", "A"))
      return(FALSE)
    }

    # Create grid
    par(mar = c(4, 4, 3, 2))
    plot(NULL, xlim = c(0.5, n_ranks + 0.5), ylim = c(0.5, n_suits + 0.5),
         xlab = "", ylab = "", axes = FALSE, asp = 1)
    title("Possibility Space: Chaining B and C", cex.main = 1.1, font.main = 2)

    # Draw grid and color cells
    count_b <- 0
    count_both <- 0
    for (i in 1:n_suits) {
      for (j in 1:n_ranks) {
        suit <- suits[i]
        rank <- ranks[j]

        has_b <- matches_b(suit, rank)
        has_c <- matches_c(suit, rank)

        if (has_b) count_b <- count_b + 1
        if (has_b && has_c) count_both <- count_both + 1

        # Color: B only = orange, B and C = purprle, neither = white
        col <- if (has_b && has_c) "#9370DB" else if (has_b) "#fee5d9" else "white"

        rect(j - 0.4, i - 0.4, j + 0.4, i + 0.4, col = col, border = "black", lwd = 0.5)
        text(j, i, paste0(rank, suit), cex = 0.6)
      }
    }

    # Add axis labels at top
    axis(3, at = 1:n_ranks, labels = ranks, tick = FALSE, line = -1)
    axis(2, at = 1:n_suits, labels = c("Hearts", "Diamonds", "Clubs", "Spades"),
         tick = FALSE, las = 1, line = -1)

    # Legend
    total_cards <- n_suits * n_ranks
    p_b <- count_b / total_cards
    p_both <- count_both / total_cards
    p_c_given_b <- if (count_b > 0) count_both / count_b else 0

    text(n_ranks/2, -0.5,
         paste0("Orange (B only): ", count_b - count_both, " | Green (B∧C): ", count_both,
                " | P(A→B)×P(A∧B→C) = ", round(p_b, 3), "×", round(p_c_given_b, 3),
                " = ", round(p_b * p_c_given_b, 3)),
         cex = 0.85)
  })

  output$ex3_calc <- renderText({
    req(input$ex3_deck_type, input$ex3_b, input$ex3_c, input$ex3_n_trials)

    deck_type <- input$ex3_deck_type
    b_cond <- input$ex3_b
    c_cond <- input$ex3_c
    n <- input$ex3_n_trials

    # Descriptions
    antecedent_desc <- if (deck_type == "new_standard") {
      "Draw from NEW STANDARD deck (Ace♠ on top)"
    } else if (deck_type == "shuffled_face") {
      "Draw from SHUFFLED FACE CARD deck (J,Q,K)"
    } else if (deck_type == "shuffled_piquet") {
      "Draw from SHUFFLED PIQUET PACK (7-A)"
    } else {
      "Draw from SHUFFLED STANDARD deck"
    }

    b_desc <- switch(b_cond,
      "red" = "Card is RED",
      "ace_spades" = "Card is ACE OF SPADES",
      "even" = "Card is EVEN (2,4,6,8,10)",
      "face" = "Card is FACE CARD (J,Q,K)",
      "less_10" = "Card is LESS THAN 10 (A-9)",
      "higher_8" = "Card is HIGHER THAN 8 (9,10,J,Q,K,A)",
      b_cond
    )

    c_desc <- switch(c_cond,
      "red" = "Card is RED",
      "ace_spades" = "Card is ACE OF SPADES",
      "even" = "Card is EVEN (2,4,6,8,10)",
      "face" = "Card is FACE CARD (J,Q,K)",
      "less_10" = "Card is LESS THAN 10 (A-9)",
      "higher_8" = "Card is HIGHER THAN 8 (9,10,J,Q,K,A)",
      c_cond
    )

    # Define deck based on type
    if (deck_type == "shuffled_face") {
      suits <- rep(c("hearts", "diamonds", "clubs", "spades"), each = 3)
      ranks <- rep(c("J", "Q", "K"), 4)
      deck_size <- 12
    } else if (deck_type == "shuffled_piquet") {
      suits <- rep(c("hearts", "diamonds", "clubs", "spades"), each = 8)
      ranks <- rep(c("7", "8", "9", "10", "J", "Q", "K", "A"), 4)
      deck_size <- 32
    } else {
      suits <- rep(c("hearts", "diamonds", "clubs", "spades"), each = 13)
      ranks <- rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4)
      deck_size <- 52
    }

    matches_b <- function(suit, rank) {
      if (b_cond == "red") return(suit %in% c("hearts", "diamonds"))
      if (b_cond == "ace_spades") return(suit == "spades" && rank == "A")
      if (b_cond == "even") return(rank %in% c("2", "4", "6", "8", "10"))
      if (b_cond == "face") return(rank %in% c("J", "Q", "K"))
      if (b_cond == "less_10") return(rank %in% c("A", "2", "3", "4", "5", "6", "7", "8", "9"))
      if (b_cond == "higher_8") return(rank %in% c("9", "10", "J", "Q", "K", "A"))
      return(FALSE)
    }

    matches_c <- function(suit, rank) {
      if (c_cond == "red") return(suit %in% c("hearts", "diamonds"))
      if (c_cond == "ace_spades") return(suit == "spades" && rank == "A")
      if (c_cond == "even") return(rank %in% c("2", "4", "6", "8", "10"))
      if (c_cond == "face") return(rank %in% c("J", "Q", "K"))
      if (c_cond == "less_10") return(rank %in% c("A", "2", "3", "4", "5", "6", "7", "8", "9"))
      if (c_cond == "higher_8") return(rank %in% c("9", "10", "J", "Q", "K", "A"))
      return(FALSE)
    }

    matches_c_given_b <- function(suit, rank) {
      return(matches_b(suit, rank) && matches_c(suit, rank))
    }

    # Simulate - use same draws for both calculations
    set.seed(42)
    if (deck_type == "new_standard") {
      card_suit <- rep("spades", n)
      card_rank <- rep("A", n)
    } else {
      sampled_indices <- sample(1:deck_size, n, replace = TRUE)
      card_suit <- suits[sampled_indices]
      card_rank <- ranks[sampled_indices]
    }

    results_b <- sapply(1:n, function(i) matches_b(card_suit[i], card_rank[i]))
    results_both <- sapply(1:n, function(i) matches_c_given_b(card_suit[i], card_rank[i]))

    count_b <- sum(results_b)
    count_both <- sum(results_both)

    p_b <- count_b / n
    p_c_given_b <- if (count_b > 0) count_both / count_b else 0
    p_both <- count_both / n

    paste0(
      "ANTECEDENT A: ", antecedent_desc, "\n\n",
      "CONSEQUENCE 1: \"IF A, THEN B\" where B = ", b_desc, "\n",
      "  - B occurs: ", count_b, " / ", n, " times\n",
      "  - P(A → B) = ", round(p_b, 4), "\n\n",
      "CONSEQUENCE 2: \"IF (A AND B), THEN C\" where C = ", c_desc, "\n",
      "  - Among ", count_b, " cases where B holds,\n",
      "    C also holds: ", count_both, " times\n",
      "  - P(A∧B → C) = ", count_both, " / ", count_b, " = ", round(p_c_given_b, 4), "\n\n",
      "COMBINED CONSEQUENCE: \"IF A, THEN (B AND C)\"\n",
      "  - Both B and C: ", count_both, " / ", n, " times\n",
      "  - P(A → [B∧C]) = ", round(p_both, 4), "\n\n",
      "MULTIPLICATION RULE:\n",
      "  P(A → B) × P(A∧B → C) = ", round(p_b, 4), " × ", round(p_c_given_b, 4), " = ", round(p_b * p_c_given_b, 4), "\n",
      "  P(A → [B∧C]) = ", round(p_both, 4), "\n\n",
      "The consequences CHAIN: first A leads to B, then among cases\n",
      "where we have both A and B, we check if C follows."
    )
  })

  # Example 4: Special Rule for Independent Probabilities
  observeEvent(input$toggle_ex4, {
    toggle("example-4")

    if (is.null(input$ex4_initialized)) {
      output$ex4_ui <- renderUI({
        div(
          div(class = "example-header",
              onclick = "this.classList.toggle('collapsed'); $('#ex4-content').toggle();",
              "Interactive Example: Independent Probabilities"),
          div(id = "ex4-content", class = "example-content",
              p(strong("The Special Rule:"), " When B and C are ", em("independent"),
                " (meaning P(A→C) = P(A∧B→C)), we can multiply:"),
              p("P(A → B) × P(A → C) = P(A → [B ∧ C])"),

              p(strong("Independence means:"), " Knowing B doesn't change the probability of C"),

              div(class = "control-panel",
                  selectInput("ex4_deck_type",
                              span(class = "hl-antecedent", "Antecedent A - Draw top card from:"),
                              choices = c("Well-shuffled standard deck" = "shuffled_standard",
                                        "New standard deck (Ace♠ on top)" = "new_standard",
                                        "Well-shuffled Piquet pack (7-A)" = "shuffled_piquet",
                                        "Well-shuffled face cards (J,Q,K)" = "shuffled_face"),
                              selected = "shuffled_standard"),

                  build_consequent_ui("ex4_b", "Consequent B", "hl-event-a"),
                  build_consequent_ui("ex4_c", "Consequent C", "hl-event-b")
              ),

              div(class = "plot-container",
                  plotOutput("ex4_grid", height = "400px")
              ),

              div(class = "calc-output",
                  verbatimTextOutput("ex4_calc")
              )
          )
        )
      })

      session$sendCustomMessage("ex4_initialized", TRUE)

      insertUI(
        selector = "#example-4",
        ui = uiOutput("ex4_ui")
      )
    }
  })

  output$ex4_grid <- renderPlot({
    req(input$ex4_deck_type, input$ex4_b_operator, input$ex4_c_operator)

    grid <- create_grid("card_deck", list(deck_type = input$ex4_deck_type))
    rule_b <- get_consequent_rule(input, "ex4_b")
    rule_c <- get_consequent_rule(input, "ex4_c")
    matched_b <- evaluate_consequent(grid, rule_b)
    matched_c <- evaluate_consequent(grid, rule_c)
    render_grid_plot(grid, matched_b, "double", "Possibility Space: Independent B and C", matched_c)
  })

  output$ex4_calc <- renderText({
    req(input$ex4_deck_type, input$ex4_b_operator, input$ex4_c_operator)

    deck_type <- input$ex4_deck_type

    # Create grid and rules using generic framework
    grid <- create_grid("card_deck", list(deck_type = deck_type))
    rule_b <- get_consequent_rule(input, "ex4_b")
    rule_c <- get_consequent_rule(input, "ex4_c")

    # Evaluate consequents
    matched_b <- evaluate_consequent(grid, rule_b)
    matched_c <- evaluate_consequent(grid, rule_c)
    matched_both <- matched_b & matched_c

    # Calculate counts
    count_b <- sum(matched_b)
    count_c <- sum(matched_c)
    count_both <- sum(matched_both)
    deck_size <- nrow(grid$cells)

    # Calculate probabilities
    p_b <- count_b / deck_size
    p_c <- count_c / deck_size
    p_both <- count_both / deck_size
    p_c_given_b <- if (count_b > 0) count_both / count_b else 0

    # Independence test
    is_independent <- abs(p_c - p_c_given_b) < 0.001

    # Create descriptions
    antecedent_desc <- if (deck_type == "new_standard") {
      "Draw from NEW STANDARD deck (Ace♠ on top)"
    } else if (deck_type == "shuffled_face") {
      "Draw from SHUFFLED FACE CARD deck (J,Q,K)"
    } else if (deck_type == "shuffled_piquet") {
      "Draw from SHUFFLED PIQUET PACK (7-A)"
    } else {
      "Draw from SHUFFLED STANDARD deck"
    }

    # Build rule descriptions
    build_rule_desc <- function(rule) {
      operator <- rule$operator
      suit_desc <- if (rule$suit == "any") "" else paste0(" of ",
        switch(rule$suit, "H" = "Hearts", "D" = "Diamonds", "C" = "Clubs", "S" = "Spades"))

      if (operator == "exactly") {
        paste0("Card is exactly ", rule$rank, suit_desc)
      } else if (operator == "higher_than") {
        paste0("Card is higher than ", rule$rank, suit_desc)
      } else if (operator == "lower_than") {
        paste0("Card is lower than ", rule$rank, suit_desc)
      } else if (operator == "any") {
        prop_desc <- switch(rule$property,
          "any_card" = "Any card",
          "even" = "Even",
          "odd" = "Odd",
          "face" = "Face card",
          "non_face" = "Non-face",
          "red" = "Red",
          "black" = "Black"
        )
        paste0("Card is ", prop_desc, suit_desc)
      } else if (operator == "anything_other_than") {
        if (!is.null(rule$property)) {
          prop_desc <- switch(rule$property,
            "even" = "Even",
            "odd" = "Odd",
            "face" = "Face card",
            "red" = "Red",
            "black" = "Black"
          )
          paste0("Card is anything other than ", prop_desc, suit_desc)
        } else {
          paste0("Card is anything other than ", rule$rank, suit_desc)
        }
      }
    }

    b_desc <- build_rule_desc(rule_b)
    c_desc <- build_rule_desc(rule_c)

    paste0(
      "ANTECEDENT A: ", antecedent_desc, "\n\n",
      "CONSEQUENCE 1: \"IF A, THEN B\" where B = ", b_desc, "\n",
      "  - P(A → B) = ", count_b, "/", deck_size, " = ", round(p_b, 4), "\n\n",
      "CONSEQUENCE 2: \"IF A, THEN C\" where C = ", c_desc, "\n",
      "  - P(A → C) = ", count_c, "/", deck_size, " = ", round(p_c, 4), "\n\n",
      "INDEPENDENCE TEST:\n",
      "  - P(A → C) = ", round(p_c, 4), "\n",
      "  - P(A∧B → C) = ", count_both, "/", count_b, " = ", round(p_c_given_b, 4), "\n",
      "  - ", if (is_independent) "✓ INDEPENDENT" else "✗ NOT INDEPENDENT", "\n\n",
      "SPECIAL RULE (only works if independent):\n",
      "  P(A → B) × P(A → C) = ", round(p_b, 4), " × ", round(p_c, 4), " = ", round(p_b * p_c, 4), "\n",
      "  P(A → [B∧C]) = ", count_both, "/", deck_size, " = ", round(p_both, 4), "\n\n",
      if (is_independent) {
        paste0("✓ Rule works! Products match exactly.\n",
               "This is because knowing B doesn't change the probability of C.")
      } else {
        paste0("✗ Rule doesn't work here. B and C are NOT independent.\n",
               "Knowing B changes the probability of C.\n",
               "Difference: |", round(p_b * p_c, 4), " - ", round(p_both, 4), "| = ", round(abs(p_b * p_c - p_both), 4))
      }
    )
  })
}

shinyApp(ui = ui, server = server)
