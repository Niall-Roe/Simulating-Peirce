library(shiny)
library(shinyjs)

# Helper function to convert decimal to simple fraction
# Prefers clean denominators (especially 1000)
decimal_to_fraction <- function(x, max_denom = 1000) {
  # Handle common fractions first for cleaner display
  common_fractions <- c(
    "1/2" = 0.5, "1/3" = 1/3, "2/3" = 2/3,
    "1/4" = 0.25, "3/4" = 0.75,
    "1/5" = 0.2, "2/5" = 0.4, "3/5" = 0.6, "4/5" = 0.8,
    "1/10" = 0.1, "1/100" = 0.01
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

      p("It may be remarked that when the real value of the probability sought inductively is either very large or very small, the reasoning is more secure. ",
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
                         min = 0.001, max = 0.999, value = 0.01, step = 0.001),
              sliderInput("ex19_s", "Sample size (s):",
                         min = 5, max = 500, value = 100, step = 1),
              checkboxInput("ex19_rescale", "Rescale chart (zoom to ±5σ)", TRUE),
              checkboxInput("ex19_xaxis_balls", "X-axis: Number of balls", FALSE),
              hr(),
              checkboxInput("ex19_show_prediction", "Show prediction interval (true p)", TRUE),
              conditionalPanel(
                condition = "input.ex19_show_prediction == true",
                sliderInput("ex19_pred_cl", "Prediction confidence level:",
                           min = 0.50, max = 0.99, value = 0.50, step = 0.01)
              ),
              hr(),
              checkboxInput("ex19_show_ci", "Show confidence interval (observed)", TRUE),
              conditionalPanel(
                condition = "input.ex19_show_ci == true",
                sliderInput("ex19_cl", "Confidence level:",
                           min = 0.50, max = 0.99, value = 0.50, step = 0.01)
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

  # Reset to 1-in-100 urn example
  observeEvent(input$ex19_reset, {
    updateSliderInput(session, "ex19_p", value = 0.01)
    updateSliderInput(session, "ex19_s", value = 100)
    updateCheckboxInput(session, "ex19_rescale", value = TRUE)
    updateCheckboxInput(session, "ex19_xaxis_balls", value = FALSE)
    updateCheckboxInput(session, "ex19_show_prediction", value = TRUE)
    updateSliderInput(session, "ex19_pred_cl", value = 0.50)
    updateCheckboxInput(session, "ex19_show_ci", value = TRUE)
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
    if (!is.null(input$ex19_show_prediction) && input$ex19_show_prediction) {
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

    # If a bar is clicked, show observed distribution and CI
    if (!is.null(clicked_bar())) {
      clicked_k <- clicked_bar()
      p_hat <- clicked_k / s

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

      # Confidence interval for observed p_hat
      if (!is.null(input$ex19_show_ci) && input$ex19_show_ci) {
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

    if (!is.null(input$ex19_show_prediction) && input$ex19_show_prediction && !is.null(input$ex19_pred_cl)) {
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

    if (!is.null(clicked_bar())) {
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

    # Pre-click state
    if (is.null(clicked_bar())) {
      # Calculate prediction interval text if toggle is on
      prediction_text <- NULL
      if (!is.null(input$ex19_show_prediction) && input$ex19_show_prediction) {
        pred_cl <- input$ex19_pred_cl
        if (!is.null(pred_cl)) {
          pred_ci <- binom.test(round(p * s), s, conf.level = pred_cl)$conf.int
          # Calculate margin in terms of balls
          margin_lower <- abs(p - pred_ci[1]) * s
          margin_upper <- abs(pred_ci[2] - p) * s
          margin <- max(margin_lower, margin_upper)

          prediction_text <- p("Suppose there were in reality ",
            strong(paste0(p_frac$num, " white ", pluralize_ball(p_frac$num))),
            " in ",
            strong(p_frac$den),
            " in a certain urn, and we were to judge of the number by ",
            strong(s),
            " drawings. Thus, we should be tolerably certain of not being in error by more than ",
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
                prediction_text))
    }

    # Post-click state
    clicked_k <- clicked_bar()

    # Calculate probability
    prob <- dbinom(clicked_k, size = s, prob = p)

    # Express probability as fraction out of 1000
    prob_num <- round(prob * 1000)

    # Pluralization
    num_white_text <- if (clicked_k == 1) {
      paste(number_word(clicked_k), pluralize_ball(clicked_k))
    } else {
      paste(clicked_k, pluralize_ball(clicked_k))
    }

    # Base text (always shown)
    base_text <- p("Suppose there were in reality ",
      strong(paste0(p_frac$num, " white ", pluralize_ball(p_frac$num))),
      " in ",
      strong(p_frac$den),
      " in a certain urn, and we were to judge of the number by ",
      strong(s),
      " drawings. The probability of drawing ",
      strong(num_white_text),
      " would be ",
      strong(paste0(prob_num, "/1000")),
      " = ",
      strong(sprintf("%.3f", prob)),
      ".")

    # Prediction interval text (green toggle)
    prediction_text <- NULL
    if (!is.null(input$ex19_show_prediction) && input$ex19_show_prediction) {
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

    # Observed CI text (blue toggle)
    ci_text <- NULL
    if (!is.null(input$ex19_show_ci) && input$ex19_show_ci) {
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
          strong(paste0(prob_num, "/1000")),
          " times in the long run.")
      }
    }

    # Build the complete output
    div(class = "click-info",
        p(strong("Clicked outcome: "), clicked_k, " white balls out of ", s, " drawings"),
        base_text,
        prediction_text,
        ci_text
    )
  })
}

shinyApp(ui = ui, server = server)
