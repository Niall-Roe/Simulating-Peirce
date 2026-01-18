library(shiny)
library(shinyjs)

#notes 
# - this is taking ex 19 and making it fit for another purpose. 
# have updated starting values to represented target paragraph, which is:

# As we cannot have an urn with an infinite number of balls to
# represent the inexhaustibleness of Nature, let us suppose one with a
# finite number, each ball being thrown back into the urn after being
# drawn out, so that there is no exhaustion of them. Suppose one ball
# out of three is white and the rest black, and that four balls are drawn.
# Then the table on [another page] represents the relative frequency of the
# different ways in which these balls might be drawn. It will be seen
# that if we should judge by these four balls of the proportion in the
# urn, 32 times out of 81 we should find it 1/4, and 24 times out of 81 we
# should find it 1/2, the truth being 1/3 To extend this table to high num-
#   bers would be great labor, but the mathematicians have found some
# ingenious ways of reckoning what the numbers would be.

# to do:
#   
#   1 set the CI sliders to be off by default. further, lets not display the blue distribution unless that CI toggle is clicked. Make those toggles not appear unless a further toggle is hit "eEable Confidence Intervals (not part of this example)"
#   2 count the possible outcomes given the initial settings. here, there should be 81. this is based on: one way to get all white: wwww. 4 ways to get 3w1b, but there are two sets. 6 ways to get 2b2w, but there are 4 sets. etc... this reasoning may be subtle, and requires teasing out. here is how he puts it for your reference:
# 
# Suppose we had an immense granary filled with black and
# white balls well mixed up; and suppose each urn were filled by taking
# a fixed number of balls from this granary quite at random. The
# relative number of white balls in the granary might be anything, say
# one in three. Then in one-third of the urns the first ball would be
# white, and in two-thirds black. In one-third of those urns of which the
# first ball was white, and also in one-third of those in which the first
# ball was black, the second ball would be white. In this way, we should
# have a distribution like that shown in the following table, where w stands for a white ball and b for a black one. The reader can, if he
# chooses, verify the table for himself. 
# 
# wwww.
# 
# wwwb. wwbw. wbww. bwww.
# wwwb. wwbw. wbww. bwww.
# 
# wwbb. wbwb. bwwb. wbbw. bwbw. bbww.
# wwbb. wbwb. bwwb. wbbw. bwbw. bbww.
# wwbb. wbwb. bwwb. wbbw. bwbw. bbww.
# wwbb. wbwb. bwwb. wbbw. bwbw. bbww.
# 
# wbbb. bwbb. bbwb. bbbw.
# wbbb. bwbb. bbwb. bbbw.
# wbbb. bwbb. bbwb. bbbw.
# wbbb. bwbb. bbwb. bbbw.
# wbbb. bwbb. bbwb. bbbw.
# wbbb. bwbb. bbwb. bbbw.
# wbbb. bwbb. bbwb. bbbw.
# wbbb. bwbb. bbwb. bbbw.
# 
# bbbb. bbbb. bbbb. bbbb. bbbb. bbbb. bbbb. bbbb.
# bbbb. bbbb. bbbb. bbbb. bbbb. bbbb. bbbb. bbbb.
# 
# In the second group, where there is one b, there are
# two sets just alike; in the third there are 4, in the fourth
# 8, and in the fifth 16, doubling every time. This is be-
#   cause we have supposed twice as many black balls in the
# granary as white ones; had we supposed 10 times as
# many, instead of
# 
# 1, 2, 4, 8, 16
# 
# sets we should have had
# 
# 1, 10, 100, 1000, 10000
# 
# sets; on the other hand, had the numbers of black and
# white balls in the granary been even, there would have
# been but one set in each group. ...
# 
# As we cannot have an urn with an infinite number of balls to
# represent the inexhaustibleness of Nature, let us suppose one with a
# finite number, each ball being thrown back into the urn after being
# drawn out, so that there is no exhaustion of them. Suppose one ball
# out of three is white and the rest black, and that four balls are drawn.
# Then the table on page 299 represents the relative frequency of the
# different ways in which these balls might be drawn. 

# the point is that it is not just the ways they can be combined... but the frequency too. This is the binomial expansion. here is how it works:


# To represent the relative frequencies of draws from a "granary" with a specific ratio, a programmer should use the **Binomial Expansion**.
# 
# ### The Logic
# 
# When drawing  balls where the ratio of White to Black is :
#   
#   1. **Combinations:** The number of ways to arrange the balls (e.g.,  vs ) is determined by the **Binomial Coefficient**.
# 2. **Weights:** Each ball drawn acts as a multiplier based on its frequency in the granary. A "set" is simply the weight assigned to a specific sequence.
# 
# ---
#   
#   ### The Variables
#   
#   *  = Number of draws (e.g., 4).
# *  = Weight of the first type (e.g., White = 1).
# *  = Weight of the second type (e.g., Black = 2).
# 
# ### The Formulas
# 
# **1. Total Relative Outcomes:**
#   To get the total number of outcomes (like the 81 in your example), use:
#   
#   
#   **2. Frequency of a specific outcome:**
#   To find the frequency for exactly  white balls:
#   
#   
#   
#   *(Where  is the number of ways to arrange  items in  slots).*
#   
#   ---
#   
#   ### Programmer’s Reference Table ()
#   
#   This is how the "81 outcomes" are calculated programmatically:
#   
#   | White Balls () | Permutations  | Weight () | Total Freq |
#   | --- | --- | --- | --- |
#   | 4 |  |  | **1** |
#   | 3 |  |  | **8** |
#   | 2 |  |  | **24** |
#   | 1 |  |  | **32** |
#   | 0 |  |  | **16** |
#   | **Sum** |  |  | **81** |
#   

#   3 reconfigure the text to use that number as the denominator. as in the above paragraph where it is in terms of 81. we can keep the /1000 version, but then say, after "= [decimal]": " = x/total]"
#   4 in the always on text, it should read "Suppose [one] ball out of [three] is white and the rest black, and that [four] balls are drawn.Thus there are [denominator] possible outcomes"
#.  5 when you click an outcome, that text can add: "It will be seen that if we should judge by these [four] balls of the proportion in the urn, [32] times out of [81] we should find it to be [clicked proportion, either 0/4, 1/4, ... 4/4] [=[decimal]]"
#   6 we will need a way to make sure this does not get bonkers for large numbers of possible combinations. maybe if the number of combinations is high, we can still display the number (if it is easy to calculate) but if not, just say "more than a thousand" or something. then, the above can be modified so that it says "more than a thousand" for the always present text, and for the clicked text, gives your probabilities with 1000 as the denominator. 


# Helper function to calculate total possible outcomes using binomial expansion
# For ratio w:b and n draws, total = (w + b)^n
calculate_total_outcomes <- function(p, s) {
  # Convert p to ratio (e.g., p=1/3 means 1 white : 2 black)
  # We'll use the binomial expansion formula
  # For sampling with replacement, calculate (1/p + (1-p)/p)^s simplified
  # But for display purposes, we want the actual count based on the ratio

  # Extract numerator and denominator from p
  p_frac <- decimal_to_fraction(p)
  white_weight <- p_frac$num
  black_weight <- p_frac$den - p_frac$num

  total <- (white_weight + black_weight)^s
  return(list(total = total, w = white_weight, b = black_weight, base = white_weight + black_weight))
}

# Helper function to convert decimal to simple fraction
# Prefers clean denominators (especially 1000)
decimal_to_fraction <- function(x, max_denom = 1000) {
  # Handle common fractions first for cleaner display
  common_fractions <- c(
    "1/2" = 0.5, "1/3" = 1/3, "2/3" = 2/3,
    "1/4" = 0.25, "3/4" = 0.75,
    "1/5" = 0.2, "2/5" = 0.4, "3/5" = 0.6, "4/5" = 0.8,
    "1/10" = 0.1, "1/100" = 0.01, "1/1000" = 0.001
  )

  for (frac_name in names(common_fractions)) {
    if (abs(x - common_fractions[frac_name]) < 0.001) {
      parts <- strsplit(frac_name, "/")[[1]]
      return(list(num = as.numeric(parts[1]), den = as.numeric(parts[2])))
    }
  }

  # Try to express as x/1000 first (cleaner for most cases)
  num_1000 <- round(x * 1000)
  if (abs(x - num_1000/1000) < 0.001) {
    return(list(num = num_1000, den = 1000))
  }

  # Otherwise find best approximation with smaller denominators
  for (den in c(100, 500, 200, 250)) {
    num <- round(x * den)
    if (abs(x - num/den) < 0.001) {
      return(list(num = num, den = den))
    }
  }

  # Final fallback to 1000
  return(list(num = num_1000, den = 1000))
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
    "))
  ),

  div(class = "article-container",
      h3("Example 19: Probability and Extreme Values"),

      p("As we cannot have an urn with an infinite number of balls to represent the inexhaustibleness of Nature, let us suppose one with a finite number, each ball being thrown back into the urn after being drawn out, so that there is no exhaustion of them. ",
        span(class = "example-trigger", id = "ex19-trigger",
             onclick = "Shiny.setInputValue('toggle_ex19', Math.random());",
             "[Click to see interactive example]")
      ),

      div(id = "example-19", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Binomial Distribution and Extreme Probabilities"),

          p("Explore how the shape of the sampling distribution changes when the true proportion is very small or very large. Click on any bar to see the exact probability. Try exploring how the size of the confidence interval changes when p is an extreme value as compared to when it is closer to 0.5."),

          fluidRow(
            column(4,
              sliderInput("ex19_p", "True proportion (p):",
                         min = 0.001, max = 0.999, value = 0.333, step = 0.001),
              sliderInput("ex19_s", "Sample size (s):",
                         min = 4, max = 500, value = 4, step = 1),
              checkboxInput("ex19_rescale", "Rescale chart (zoom to ±5σ)", FALSE),
              checkboxInput("ex19_xaxis_balls", "X-axis: Number of balls", TRUE),
              hr(),
              checkboxInput("ex19_enable_ci", "Enable Confidence Intervals (not part of this example)", FALSE),
              conditionalPanel(
                condition = "input.ex19_enable_ci == true",
                checkboxInput("ex19_show_prediction", "Show prediction interval (true p)", FALSE),
                conditionalPanel(
                  condition = "input.ex19_show_prediction == true",
                  sliderInput("ex19_pred_cl", "Prediction confidence level:",
                             min = 0.50, max = 0.99, value = 0.50, step = 0.01)
                ),
                hr(),
                checkboxInput("ex19_show_ci", "Show confidence interval (observed)", FALSE),
                conditionalPanel(
                  condition = "input.ex19_show_ci == true",
                  sliderInput("ex19_cl", "Confidence level:",
                             min = 0.50, max = 0.99, value = 0.50, step = 0.01)
                )
              ),
              hr(),
              actionButton("ex19_reset", "Reset conditions", class = "btn-primary")
            ),
            column(8,
              plotOutput("ex19_plot", height = "450px", click = "ex19_plot_click"),
              uiOutput("ex19_click_text")
            )
          )
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex19, { shinyjs::toggle("example-19") })

  # Reset to 1-in-3 urn example (Peirce's example)
  observeEvent(input$ex19_reset, {
    updateSliderInput(session, "ex19_p", value = 1/3)
    updateSliderInput(session, "ex19_s", value = 4)
    updateCheckboxInput(session, "ex19_rescale", value = FALSE)
    updateCheckboxInput(session, "ex19_xaxis_balls", value = TRUE)
    updateCheckboxInput(session, "ex19_show_prediction", value = FALSE)
    updateSliderInput(session, "ex19_pred_cl", value = 0.50)
    updateCheckboxInput(session, "ex19_show_ci", value = FALSE)
    updateSliderInput(session, "ex19_cl", value = 0.50)
  })

  # Snap to sticky values
  observeEvent(input$ex19_p, {
    p <- input$ex19_p
    sticky_values <- c(0.01, 0.2, 0.25, 1/3, 0.4, 0.5, 0.6, 2/3, 0.75, 0.8)
    tolerance <- 0.015

    for (sticky in sticky_values) {
      if (abs(p - sticky) < tolerance && abs(p - sticky) > 0.0001) {
        updateSliderInput(session, "ex19_p", value = sticky)
        break
      }
    }
  })

  # Store clicked bar
  clicked_bar <- reactiveVal(NULL)

  observeEvent(input$ex19_plot_click, {
    p <- input$ex19_p
    s <- input$ex19_s
    use_balls <- input$ex19_xaxis_balls

    click <- input$ex19_plot_click
    if (is.null(click)) return()

    # Calculate which bar was clicked
    if (use_balls) {
      clicked_k <- round(click$x)
    } else {
      clicked_k <- round(click$x * s)
    }

    if (clicked_k >= 0 && clicked_k <= s) {
      clicked_bar(clicked_k)
    }
  })

  output$ex19_plot <- renderPlot({
    p <- input$ex19_p
    s <- input$ex19_s
    rescale <- input$ex19_rescale
    use_balls <- input$ex19_xaxis_balls

    # Discrete support
    k <- 0:s
    phat <- k / s

    # Exact binomial distribution under true p
    probs <- dbinom(k, size = s, prob = p)

    # Determine x-axis values and limits
    if (use_balls) {
      x_vals <- k
      if (rescale) {
        mu <- p * s
        sigma <- sqrt(p * (1 - p) * s)
        xlim <- c(max(0, mu - 5*sigma), min(s, mu + 5*sigma))
      } else {
        xlim <- c(0, s)
      }
      xlab_text <- "Number of white balls"
    } else {
      x_vals <- phat
      if (rescale) {
        mu <- p
        sigma <- sqrt(p * (1 - p) / s)
        xlim <- c(max(0, mu - 5*sigma), min(1, mu + 5*sigma))
      } else {
        xlim <- c(0, 1)
      }
      xlab_text <- expression(hat(p))
    }

    # Plot binomial distribution
    plot(x_vals, probs,
         type = "h", lwd = 3,
         xlim = xlim,
         ylim = c(0, max(probs) * 1.3),
         xlab = xlab_text,
         ylab = "Probability mass",
         main = "Sampling distribution")

    points(x_vals, probs, pch = 16, cex = 1.2)

    # Show normal approximation for TRUE p (green)
    mu <- p
    sigma <- sqrt(p * (1 - p) / s)
    delta <- 1 / s

    # Prediction interval under true p (shaded green area)
    enable_ci <- !is.null(input$ex19_enable_ci) && input$ex19_enable_ci
    if (enable_ci && !is.null(input$ex19_show_prediction) && input$ex19_show_prediction) {
      pred_cl <- input$ex19_pred_cl
      if (!is.null(pred_cl)) {
        # Calculate prediction interval
        pred_ci <- binom.test(round(p * s), s, conf.level = pred_cl)$conf.int

        # Shade area under green curve within the CI
        if (use_balls) {
          x_shade <- seq(max(xlim[1], pred_ci[1] * s),
                        min(xlim[2], pred_ci[2] * s), length.out = 500)
          y_shade <- dnorm(x_shade, mean = mu * s, sd = sigma * s)
          polygon(c(x_shade, rev(x_shade)), c(y_shade, rep(0, length(y_shade))),
                 col = rgb(0, 0.5, 0, 0.2), border = NA)

          # Draw CI bounds
          abline(v = pred_ci[1] * s, lty = 3, lwd = 2, col = "darkgreen")
          abline(v = pred_ci[2] * s, lty = 3, lwd = 2, col = "darkgreen")
        } else {
          x_shade <- seq(max(xlim[1], pred_ci[1]),
                        min(xlim[2], pred_ci[2]), length.out = 500)
          y_shade <- dnorm(x_shade, mean = mu, sd = sigma) * delta
          polygon(c(x_shade, rev(x_shade)), c(y_shade, rep(0, length(y_shade))),
                 col = rgb(0, 0.5, 0, 0.2), border = NA)

          # Draw CI bounds
          abline(v = pred_ci[1], lty = 3, lwd = 2, col = "darkgreen")
          abline(v = pred_ci[2], lty = 3, lwd = 2, col = "darkgreen")
        }
      }
    }

    # Draw green curve for true p
    if (use_balls) {
      x_seq <- seq(xlim[1], xlim[2], length.out = 1000)
      lines(x_seq,
            dnorm(x_seq, mean = mu * s, sd = sigma * s),
            col = "darkgreen", lwd = 2, lty = 2)
    } else {
      x_seq <- seq(xlim[1], xlim[2], length.out = 1000)
      lines(x_seq,
            dnorm(x_seq, mean = mu, sd = sigma) * delta,
            col = "darkgreen", lwd = 2, lty = 2)
    }

    # Mark true p
    if (use_balls) {
      abline(v = p * s, lty = 2, lwd = 2, col = "red")
    } else {
      abline(v = p, lty = 2, lwd = 2, col = "red")
    }

    # If a bar is clicked, show observed distribution and CI (only if CI enabled)
    if (!is.null(clicked_bar())) {
      clicked_k <- clicked_bar()
      p_hat <- clicked_k / s

      # Only show blue distribution if CI is enabled
      if (enable_ci) {
        # Overlay observed distribution in blue (dashed)
        sigma_obs <- sqrt(p_hat * (1 - p_hat) / s)

        if (use_balls) {
          x_seq_obs <- seq(xlim[1], xlim[2], length.out = 1000)
          lines(x_seq_obs,
                dnorm(x_seq_obs, mean = p_hat * s, sd = sigma_obs * s),
                col = "blue", lwd = 2, lty = 2)
        } else {
          x_seq_obs <- seq(xlim[1], xlim[2], length.out = 1000)
          lines(x_seq_obs,
                dnorm(x_seq_obs, mean = p_hat, sd = sigma_obs) * delta,
                col = "blue", lwd = 2, lty = 2)
        }
      }

      # Confidence interval for observed p_hat
      if (enable_ci && !is.null(input$ex19_show_ci) && input$ex19_show_ci) {
        cl <- input$ex19_cl
        if (!is.null(cl)) {
          ci <- binom.test(clicked_k, s, conf.level = cl)$conf.int
          if (use_balls) {
            segments(ci[1] * s, 0, ci[2] * s, 0, lwd = 4, col = "darkorange")
            points(ci * s, c(0, 0), pch = 16, col = "darkorange", cex = 1.2)
          } else {
            segments(ci[1], 0, ci[2], 0, lwd = 4, col = "darkorange")
            points(ci, c(0, 0), pch = 16, col = "darkorange", cex = 1.2)
          }
        }
      }

      # Highlight clicked bar
      clicked_x <- if (use_balls) clicked_k else clicked_k / s
      clicked_prob <- dbinom(clicked_k, size = s, prob = p)

      segments(clicked_x, 0, clicked_x, clicked_prob,
               lwd = 5, col = "purple")
      points(clicked_x, clicked_prob, pch = 16, cex = 1.5, col = "purple")
    }

    # Build legend dynamically
    legend_items <- c("Exact binomial", "Distribution under true p", "True p")
    legend_lwd <- c(3, 2, 2)
    legend_lty <- c(1, 2, 2)
    legend_col <- c("black", "darkgreen", "red")

    if (enable_ci && !is.null(input$ex19_show_prediction) && input$ex19_show_prediction && !is.null(input$ex19_pred_cl)) {
      legend_items <- c(legend_items, paste0(input$ex19_pred_cl * 100, "% prediction interval"))
      legend_lwd <- c(legend_lwd, NA)
      legend_lty <- c(legend_lty, NA)
      legend_col <- c(legend_col, rgb(0, 0.5, 0, 0.3))
      legend_pch <- c(NA, NA, NA, 15)
      legend_pt_cex <- c(NA, NA, NA, 2)
    } else {
      legend_pch <- c(NA, NA, NA)
      legend_pt_cex <- c(NA, NA, NA)
    }

    if (!is.null(clicked_bar()) && enable_ci) {
      legend_items <- c(legend_items, "Distribution under observed p-hat")
      legend_lwd <- c(legend_lwd, 2)
      legend_lty <- c(legend_lty, 2)
      legend_col <- c(legend_col, "blue")
      legend_pch <- c(legend_pch, NA)
      legend_pt_cex <- c(legend_pt_cex, NA)

      if (!is.null(input$ex19_show_ci) && input$ex19_show_ci && !is.null(input$ex19_cl)) {
        legend_items <- c(legend_items, paste0(input$ex19_cl * 100, "% CI (observed)"))
        legend_lwd <- c(legend_lwd, 4)
        legend_lty <- c(legend_lty, 1)
        legend_col <- c(legend_col, "darkorange")
        legend_pch <- c(legend_pch, NA)
        legend_pt_cex <- c(legend_pt_cex, NA)
      }
    }

    legend("topright",
           legend = legend_items,
           lwd = legend_lwd,
           lty = legend_lty,
           col = legend_col,
           pch = legend_pch,
           pt.cex = legend_pt_cex,
           bty = "n",
           cex = 0.75)
  })

  output$ex19_click_text <- renderUI({
    p <- input$ex19_p
    s <- input$ex19_s

    # Convert p to fraction
    p_frac <- decimal_to_fraction(p)

    # Calculate total outcomes using binomial expansion
    outcome_data <- calculate_total_outcomes(p, s)
    total_outcomes <- outcome_data$total

    # Determine if we should use "more than a thousand" language
    use_approx <- total_outcomes > 1000

    # Pre-click state
    if (is.null(clicked_bar())) {
      # Base text describing the setup (task 4)
      base_intro <- p("Suppose ",
        strong(number_word(p_frac$num)), " ", pluralize_ball(p_frac$num), " out of ",
        strong(p_frac$den), " is white and the rest black, and that ",
        strong(number_word(s)), " balls are drawn. Thus there are ",
        if (use_approx) {
          strong("more than a thousand")
        } else {
          strong(format(total_outcomes, big.mark = ","))
        },
        " possible outcomes.")

      # Calculate prediction interval text if toggle is on
      prediction_text <- NULL
      enable_ci <- !is.null(input$ex19_enable_ci) && input$ex19_enable_ci
      if (enable_ci && !is.null(input$ex19_show_prediction) && input$ex19_show_prediction) {
        pred_cl <- input$ex19_pred_cl
        if (!is.null(pred_cl)) {
          pred_ci <- binom.test(round(p * s), s, conf.level = pred_cl)$conf.int
          # Calculate margin in terms of balls
          margin_lower <- abs(p - pred_ci[1]) * s
          margin_upper <- abs(pred_ci[2] - p) * s
          margin <- max(margin_lower, margin_upper)

          prediction_text <- p("We should be tolerably certain of not being in error by more than ",
            strong(sprintf("%.1f", margin)),
            " ",
            pluralize_ball(round(margin)),
            " in ",
            strong(p_frac$den),
            ".")
        }
      }

      return(div(class = "click-info",
                p(em("Click on any bar in the chart to see the exact probability for that outcome.")),
                base_intro,
                prediction_text))
    }

    # Post-click state
    clicked_k <- clicked_bar()

    # Calculate probability using exact binomial
    prob <- dbinom(clicked_k, size = s, prob = p)

    # Calculate frequency in terms of total outcomes (task 2, 3)
    # Frequency = C(s, k) * w^k * b^(s-k)
    w <- outcome_data$w
    b <- outcome_data$b
    frequency <- choose(s, clicked_k) * (w^clicked_k) * (b^(s - clicked_k))

    # Pluralization
    num_white_text <- if (clicked_k == 1) {
      paste(number_word(clicked_k), pluralize_ball(clicked_k))
    } else {
      paste(clicked_k, pluralize_ball(clicked_k))
    }

    # Task 5: Main click text showing frequency in terms of total
    click_proportion <- paste0(clicked_k, "/", s)
    click_proportion_decimal <- sprintf("%.3f", clicked_k / s)

    # Build base text with both representations (task 3)
    if (use_approx) {
      # Use /1000 approximation
      prob_num_1000 <- round(prob * 1000)
      base_text <- p("Suppose there were in reality ",
        strong(paste0(p_frac$num, " white ", pluralize_ball(p_frac$num))),
        " in ",
        strong(p_frac$den),
        " in a certain urn, and we were to judge of the number by ",
        strong(s),
        " drawings. The probability of drawing ",
        strong(num_white_text),
        " would be ",
        strong(paste0(prob_num_1000, "/1000")),
        " = ",
        strong(sprintf("%.3f", prob)),
        ".")

      # Task 5 text
      proportion_text <- p("It will be seen that if we should judge by these ",
        strong(number_word(s)), " balls of the proportion in the urn, approximately ",
        strong(prob_num_1000), " times out of ", strong("1000"),
        " we should find it to be ",
        strong(click_proportion),
        " = ",
        strong(click_proportion_decimal),
        ".")
    } else {
      # Use exact total outcomes
      base_text <- p("Suppose there were in reality ",
        strong(paste0(p_frac$num, " white ", pluralize_ball(p_frac$num))),
        " in ",
        strong(p_frac$den),
        " in a certain urn, and we were to judge of the number by ",
        strong(s),
        " drawings. The probability of drawing ",
        strong(num_white_text),
        " would be ",
        strong(paste0(round(prob * 1000), "/1000")),
        " = ",
        strong(sprintf("%.3f", prob)),
        " = ",
        strong(paste0(frequency, "/", total_outcomes)),
        ".")

      # Task 5 text with exact counts
      proportion_text <- p("It will be seen that if we should judge by these ",
        strong(number_word(s)), " balls of the proportion in the urn, ",
        strong(frequency), " times out of ", strong(total_outcomes),
        " we should find it to be ",
        strong(click_proportion),
        " = ",
        strong(click_proportion_decimal),
        ".")
    }

    # Prediction interval text (green toggle) - only if CI enabled
    prediction_text <- NULL
    enable_ci <- !is.null(input$ex19_enable_ci) && input$ex19_enable_ci
    if (enable_ci && !is.null(input$ex19_show_prediction) && input$ex19_show_prediction) {
      pred_cl <- input$ex19_pred_cl
      if (!is.null(pred_cl)) {
        pred_ci <- binom.test(round(p * s), s, conf.level = pred_cl)$conf.int
        # Calculate margin in terms of balls
        margin_lower <- abs(p - pred_ci[1]) * s
        margin_upper <- abs(pred_ci[2] - p) * s
        margin <- max(margin_lower, margin_upper)

        prediction_text <- p("Thus, we should be tolerably certain of not being in error by more than ",
          strong(sprintf("%.1f", margin)),
          " ",
          pluralize_ball(round(margin)),
          " in ",
          strong(p_frac$den),
          ".")
      }
    }

    # Observed CI text (blue toggle) - only if CI enabled
    ci_text <- NULL
    if (enable_ci && !is.null(input$ex19_show_ci) && input$ex19_show_ci) {
      cl <- input$ex19_cl
      if (!is.null(cl)) {
        ci <- binom.test(clicked_k, s, conf.level = cl)$conf.int
        p_hat <- clicked_k / s
        p_hat_frac <- decimal_to_fraction(p_hat)

        ci_text <- p("Assuming we were ignorant to the true proportion, tried the experiment and observed ",
          strong(paste(clicked_k, pluralize_ball(clicked_k))),
          ", we would estimate the true proportion to be approximately ",
          strong(paste0(p_hat_frac$num, "/", p_hat_frac$den)),
          " = ",
          strong(sprintf("%.3f", p_hat)),
          ". At a ",
          strong(paste0(cl * 100, "%")),
          " confidence level, the true proportion is estimated to be between ",
          strong(sprintf("%.3f", ci[1])),
          " and ",
          strong(sprintf("%.3f", ci[2])),
          ". As we know the true proportion, we also know that such a result would occur ",
          if (use_approx) {
            strong(paste0(round(prob * 1000), "/1000"))
          } else {
            strong(paste0(frequency, "/", total_outcomes))
          },
          " times in the long run.")
      }
    }

    # Build the complete output
    div(class = "click-info",
        p(strong("Clicked outcome: "), clicked_k, " white balls out of ", s, " drawings"),
        base_text,
        proportion_text,
        prediction_text,
        ci_text
    )
  })
}

shinyApp(ui = ui, server = server)
