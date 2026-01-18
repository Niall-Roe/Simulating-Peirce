# This file contains the updated evaluate_consequent function with flexible builder support
# To be inserted into app.R at line 355

evaluate_consequent_NEW <- function(grid, consequent_rule) {
  cells <- grid$cells
  matched <- rep(FALSE, nrow(cells))

  if (consequent_rule$type == "card") {
    # New flexible system
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

        suit_match <- (suit_val == "any") || (suit == suit_val)
        if (!suit_match) {
          matched[i] <- FALSE
          next
        }

        matched[i] <- if (operator == "any") {
          if (property == "even") rank %in% c("2", "4", "6", "8", "10")
          else if (property == "odd") rank %in% c("A", "3", "5", "7", "9")
          else if (property == "face") rank %in% c("J", "Q", "K")
          else if (property == "non_face") !(rank %in% c("J", "Q", "K"))
          else if (property == "red") suit %in% c("H", "D")
          else if (property == "black") suit %in% c("C", "S")
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
            else if (property == "red") !(suit %in% c("H", "D"))
            else if (property == "black") !(suit %in% c("C", "S"))
            else FALSE
          } else {
            rank != rank_val
          }
        } else {
          FALSE
        }
      }
    } else {
      # Legacy support for old format
      target <- consequent_rule$target
      for (i in 1:nrow(cells)) {
        suit <- cells$suit[i]
        rank <- cells$rank[i]
        matched[i] <- if (target == "red") {
          suit %in% c("H", "D")
        } else if (target == "ace_spades") {
          suit == "S" && rank == "A"
        } else if (target == "even") {
          rank %in% c("2", "4", "6", "8", "10")
        } else if (target == "face") {
          rank %in% c("J", "Q", "K")
        } else if (target == "less_10") {
          rank %in% c("2", "3", "4", "5", "6", "7", "8", "9")
        } else if (target == "higher_8") {
          rank %in% c("9", "10", "J", "Q", "K", "A")
        } else {
          FALSE
        }
      }
    }
  } else if (consequent_rule$type == "die") {
    target <- consequent_rule$target
    for (i in 1:nrow(cells)) {
      matched[i] <- cells$label[i] == as.character(target)
    }
  } else if (consequent_rule$type == "dice_sum") {
    target <- consequent_rule$target
    for (i in 1:nrow(cells)) {
      matched[i] <- (cells$die1[i] + cells$die2[i]) == target
    }
  }

  return(matched)
}

# Helper functions to add after evaluate_consequent

build_consequent_ui <- function(id_prefix, label = "Consequent") {
  div(
    selectInput(paste0(id_prefix, "_operator"),
                paste0(label, " - Operator:"),
                choices = c("Exactly" = "exactly",
                          "Higher than" = "higher_than",
                          "Lower than" = "lower_than",
                          "Any" = "any",
                          "Anything other than" = "anything_other_than"),
                selected = "exactly"),

    conditionalPanel(
      condition = sprintf("input['%s_operator'] == 'exactly' || input['%s_operator'] == 'higher_than' || input['%s_operator'] == 'lower_than' || input['%s_operator'] == 'anything_other_than'",
                         id_prefix, id_prefix, id_prefix, id_prefix),
      selectInput(paste0(id_prefix, "_rank"),
                 "Rank:",
                 choices = c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"),
                 selected = "A")
    ),

    conditionalPanel(
      condition = sprintf("input['%s_operator'] == 'any' || input['%s_operator'] == 'anything_other_than'",
                         id_prefix, id_prefix),
      selectInput(paste0(id_prefix, "_property"),
                 "Property:",
                 choices = c("Any card" = "any_card",
                           "Even" = "even",
                           "Odd" = "odd",
                           "Face" = "face",
                           "Non-face" = "non_face",
                           "Red" = "red",
                           "Black" = "black"),
                 selected = "any_card")
    ),

    selectInput(paste0(id_prefix, "_suit"),
               "Suit:",
               choices = c("Any suit" = "any",
                         "Hearts" = "H",
                         "Diamonds" = "D",
                         "Clubs" = "C",
                         "Spades" = "S"),
               selected = "any")
  )
}

get_consequent_rule <- function(input, id_prefix) {
  operator <- input[[paste0(id_prefix, "_operator")]]

  rule <- list(
    type = "card",
    operator = operator,
    suit = input[[paste0(id_prefix, "_suit")]]
  )

  if (operator %in% c("exactly", "higher_than", "lower_than", "anything_other_than")) {
    rule$rank <- input[[paste0(id_prefix, "_rank")]]
    rule$property <- NULL
  } else {
    rule$property <- input[[paste0(id_prefix, "_property")]]
    rule$rank <- NULL
  }

  return(rule)
}
