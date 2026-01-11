# app.R
library(shiny)
library(bslib)
library(tidyverse)
library(mixtools)
library(DT)
library(scales)

# --- Helper: color mixing for blended bars ---
mix_colors <- function(cols, weights) {
  if (length(cols) == 0) return("#CCCCCC")
  if (sum(weights) == 0) weights <- rep(1/length(weights), length(weights))
  weights <- weights / sum(weights)
  rgbm <- col2rgb(cols) %*% weights
  rgb(rgbm[1], rgbm[2], rgbm[3], maxColorValue = 255)
}

# Default Peirce parameters
peirce_standards_default <- c(139.2, 142.2, 144.7, 146.95, 149.7)
peirce_PE_default <- 5/8  # 0.625
bin_start_fixed <- 136.7
bin_width_fixed <- 0.5  # Changed to 0.5 as discussed earlier

# Hardcoded Kets data (actual filtered data - 142 weights)
kets_data <- c(136.8, 137.2, 137.6, 137.7, 138, 138, 138.1, 138.2, 138.3, 138.4, 138.5, 138.7, 138.8, 138.9, 138.9, 139, 139, 139.1, 139.2, 139.4, 139.4, 139.4, 139.5, 139.5, 139.7, 139.9, 139.9, 139.9, 140, 140, 140, 140, 140.2, 140.3, 140.5, 140.6, 140.8, 140.8, 140.9, 140.9, 141, 141.1, 141.4, 141.4, 141.5, 141.6, 141.7, 142, 142, 142, 142.1, 142.2, 142.2, 142.3, 142.3, 142.4, 142.7, 142.8, 142.8, 142.9, 142.9, 143, 143, 143.1, 143.1, 143.3, 143.5, 143.6, 143.8, 143.9, 143.9, 144, 144, 144.1, 144.2, 144.2, 144.5, 144.5, 144.7, 144.7, 144.9, 144.9, 144.9, 145, 145, 145.1, 145.4, 145.4, 145.4, 145.4, 145.4, 145.5, 145.6, 145.7, 145.8, 145.8, 146, 146.2, 146.3, 146.3, 146.5, 146.5, 146.7, 146.9, 147.1, 147.2, 147.2, 147.5, 147.6, 147.6, 147.8, 147.8, 147.8, 147.8, 148, 148.1, 148.2, 148.2, 148.4, 148.4, 148.6, 148.8, 148.8, 148.8, 149.1, 149.1, 149.1, 149.2, 149.4, 149.8, 149.8, 149.8, 149.8, 150, 150, 150.5, 150.6, 150.6, 150.8, 150.8, 151.3, 153)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Peirce Kets — Mixture Decomposition (Interactive)"),
  sidebarLayout(
    sidebarPanel(
      h4("Analysis Controls"),
      actionButton("reset_peirce", "Reset to Peirce's Assumptions", 
                   class = "btn-primary", width = "100%"),
      hr(),
      sliderInput("ncomps", "Number of Standards (Components)", 
                  min = 1, max = 6, value = 5, step = 1),
      checkboxInput("fix_ncomps", "Fix number of standards (otherwise find optimal)", 
                    value = TRUE),
      checkboxInput("equal_var", "Enforce equal variance across components", 
                    value = TRUE),
      checkboxInput("use_peirce_pe", "Use Peirce's historical PE = 5/8 grain (otherwise estimate from data)", 
                    value = FALSE),
      actionButton("find_standards", "Calculate Best-Fit Standards", 
                   class = "btn-success", width = "100%"),
      hr(),
      h5("Display Options"),
      checkboxInput("show_peirce_chart", "Show Peirce's Original Chart Style", value = FALSE),
      conditionalPanel(
        condition = "!input.show_peirce_chart",
        checkboxInput("show_curves", "Show component bell curves", value = TRUE),
        checkboxInput("show_sum_curve", "Show sum curve", value = TRUE),
        checkboxInput("show_bar_coloring", "Show bar coloring (blended)", value = TRUE)
      ),
      hr(),
      h5(""),
      helpText(""),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", br(),
                 plotOutput("mainPlot", height = "600px"),
                 br(),
                 uiOutput("legend_ui"),
                 br(),
                 uiOutput("counts_ui")
        ),
        tabPanel("Decomposition Table", br(),
                 DTOutput("peirce_table"),
                 downloadButton("download_table", "Download CSV")
        )
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  
  # Store computation state
  comp_state <- reactiveVal(NULL)
  
  # Initialize with Peirce's assumptions
  observe({
    df <- tibble(Weight = kets_data)
    initialize_comp(df, use_peirce = TRUE)
  })
  
  # Reset to Peirce's assumptions
  observeEvent(input$reset_peirce, {
    df <- tibble(Weight = kets_data)
    initialize_comp(df, use_peirce = TRUE)
    updateSliderInput(session, "ncomps", value = 5)
    updateCheckboxInput(session, "use_peirce_pe", value = TRUE)
    showNotification("Reset to Peirce's assumptions", type = "message")
  })
  
  # Initialize computation with data
  initialize_comp <- function(df, use_peirce = FALSE) {
    breaks <- seq(bin_start_fixed, ceiling(max(df$Weight)) + bin_width_fixed, 
                  by = bin_width_fixed)
    bin_midpoints <- breaks[-length(breaks)] + bin_width_fixed / 2
    
    df$bin <- cut(df$Weight, breaks = breaks, include.lowest = TRUE, right = FALSE)
    observed_counts <- as.numeric(table(df$bin))
    if (length(observed_counts) < length(bin_midpoints)) {
      observed_counts <- c(observed_counts, rep(0, length(bin_midpoints) - length(observed_counts)))
    }
    
    if (use_peirce) {
      # Use Peirce's exact assumptions
      PE <- peirce_PE_default
      sd_val <- PE / 0.6745
      mus <- peirce_standards_default
      k <- length(mus)
      lambdas <- c(36, 25, 26, 23, 34) / 144  # Peirce's proportions
      sigmas <- rep(sd_val, k)
      
      em_fit <- list(lambda = lambdas, mu = mus, sigma = sigmas)
      
      comp_state(list(
        df = df,
        breaks = breaks,
        bin_midpoints = bin_midpoints,
        observed_counts = observed_counts,
        use_em = FALSE,
        em_fit = em_fit
      ))
    }
  }
  
  # Find best-fit standards using EM
  observeEvent(input$find_standards, {
    df <- tibble(Weight = kets_data)
    
    breaks <- seq(bin_start_fixed, ceiling(max(df$Weight)) + bin_width_fixed, 
                  by = bin_width_fixed)
    bin_midpoints <- breaks[-length(breaks)] + bin_width_fixed / 2
    
    df$bin <- cut(df$Weight, breaks = breaks, include.lowest = TRUE, right = FALSE)
    observed_counts <- as.numeric(table(df$bin))
    if (length(observed_counts) < length(bin_midpoints)) {
      observed_counts <- c(observed_counts, rep(0, length(bin_midpoints) - length(observed_counts)))
    }
    
    # Determine number of components
    if (input$fix_ncomps) {
      n <- input$ncomps
    } else {
      # Find optimal k using BIC
      bic_vals <- numeric(6)
      for (k_test in 1:6) {
        if (k_test == 1) {
          # Special case: single normal distribution
          mu_est <- mean(df$Weight)
          sd_est <- sd(df$Weight)
          loglik <- sum(dnorm(df$Weight, mean = mu_est, sd = sd_est, log = TRUE))
          bic_vals[k_test] <- -2 * loglik + 2 * log(nrow(df))
        } else {
          fit_test <- tryCatch({
            if (k_test == 5) {
              normalmixEM(df$Weight, k = k_test, mu = peirce_standards_default, 
                          arbvar = !input$equal_var, maxit = 500, epsilon = 1e-6)
            } else {
              km <- kmeans(df$Weight, centers = k_test)
              normalmixEM(df$Weight, k = k_test, mu = sort(km$centers[,1]), 
                          arbvar = !input$equal_var, maxit = 500, epsilon = 1e-6)
            }
          }, error = function(e) NULL)
          
          if (!is.null(fit_test)) {
            bic_vals[k_test] <- -2 * fit_test$loglik + k_test * log(nrow(df))
          } else {
            bic_vals[k_test] <- Inf
          }
        }
      }
      n <- which.min(bic_vals)
      updateSliderInput(session, "ncomps", value = n)
      showNotification(paste0("Optimal number of standards found: ", n), type = "message")
    }
    
    # Special case: k=1 (single normal distribution)
    if (n == 1) {
      mu_est <- mean(df$Weight)
      if (input$use_peirce_pe) {
        sd_est <- peirce_PE_default / 0.6745
      } else {
        sd_est <- sd(df$Weight)
      }
      
      fit <- list(
        lambda = 1,
        mu = mu_est,
        sigma = sd_est,
        loglik = sum(dnorm(df$Weight, mean = mu_est, sd = sd_est, log = TRUE))
      )
      
      comp_state(list(
        df = df,
        breaks = breaks,
        bin_midpoints = bin_midpoints,
        observed_counts = observed_counts,
        use_em = TRUE,
        em_fit = fit
      ))
      showNotification("Single standard fitted", type = "message")
      return(invisible())
    }
    
    # Initialize means for k > 1
    if (n == 5) {
      init_mu <- peirce_standards_default
    } else {
      km <- tryCatch(kmeans(df$Weight, centers = n), 
                     error = function(e) NULL)
      if (!is.null(km)) {
        init_mu <- sort(km$centers[,1])
      } else {
        init_mu <- seq(min(df$Weight), max(df$Weight), length.out = n)
      }
    }
    
    # Determine if we should fix variance
    if (input$use_peirce_pe) {
      # Use Peirce's PE, equal variance
      arbvar_setting <- FALSE
    } else {
      # Use setting from checkbox
      arbvar_setting <- !input$equal_var
    }
    
    # Run EM
    fit <- tryCatch({
      normalmixEM(df$Weight, k = n, mu = init_mu, 
                  arbvar = arbvar_setting, maxit = 500, epsilon = 1e-6)
    }, error = function(e) {
      showNotification(paste0("EM failed: ", e$message), type = "error")
      NULL
    })
    
    if (!is.null(fit)) {
      # If using Peirce's PE, override the estimated sigmas
      if (input$use_peirce_pe) {
        fit$sigma <- rep(peirce_PE_default / 0.6745, n)
      }
      
      comp_state(list(
        df = df,
        breaks = breaks,
        bin_midpoints = bin_midpoints,
        observed_counts = observed_counts,
        use_em = TRUE,
        em_fit = fit
      ))
      showNotification("Best-fit standards calculated", type = "message")
    }
  })
  
  # Main reactive for all computations
  fit_reactive <- reactive({
    comp_now <- comp_state()
    req(comp_now)
    
    df <- comp_now$df
    breaks <- comp_now$breaks
    mid <- comp_now$bin_midpoints
    observed_counts <- comp_now$observed_counts
    
    fit <- comp_now$em_fit
    lambdas <- fit$lambda
    mus <- fit$mu
    sigmas <- fit$sigma
    k <- length(mus)
    
    # Posterior assignment matrix
    weights <- df$Weight
    dens <- sapply(1:k, function(j) dnorm(weights, mean = mus[j], sd = sigmas[j]))
    weighted <- sweep(dens, 2, lambdas, `*`)
    row_sums <- rowSums(weighted)
    row_sums[row_sums == 0] <- 1
    post <- sweep(weighted, 1, row_sums, `/`)
    
    # Per-bin assignments
    n_bins <- length(mid)
    bin_assignments <- matrix(0, nrow = n_bins, ncol = k)
    for (i in seq_len(n_bins)) {
      in_bin <- weights >= breaks[i] & weights < breaks[i+1]
      if (any(in_bin)) {
        bin_assignments[i, ] <- colSums(post[in_bin, , drop = FALSE])
      }
    }
    
    # Total assignments per standard
    estimated_counts <- colSums(post)
    
    # Component curves
    x_seq <- seq(min(df$Weight) - 1, max(df$Weight) + 1, by = 0.1)
    curves_df <- map_dfr(seq_len(k), function(j) {
      tibble(
        x = x_seq, 
        component = j, 
        mu = mus[j], 
        sigma = sigmas[j], 
        density = dnorm(x_seq, mean = mus[j], sd = sigmas[j]) * estimated_counts[j] * bin_width_fixed
      )
    })
    
    # Sum curve
    sum_density <- numeric(length(x_seq))
    for (j in 1:k) {
      sum_density <- sum_density + dnorm(x_seq, mean = mus[j], sd = sigmas[j]) * estimated_counts[j] * bin_width_fixed
    }
    sum_curve_df <- tibble(x = x_seq, density = sum_density)
    
    list(
      df = df,
      breaks = breaks,
      mid = mid,
      observed_counts = observed_counts,
      k = k,
      lambdas = lambdas,
      mus = mus,
      sigmas = sigmas,
      post = post,
      bin_assignments = bin_assignments,
      estimated_counts = estimated_counts,
      curves_df = curves_df,
      sum_curve_df = sum_curve_df
    )
  })
  
  # Component colors
  comp_colors <- reactive({
    k <- fit_reactive()$k
    colors <- hue_pal()(k)
    # Replace second color with a nicer yellow
    if (k >= 2) {
      colors[2] <- "#FFD700"  # Gold/yellow
    }
    colors
  })
  
  # Legend UI
  output$legend_ui <- renderUI({
    fit <- fit_reactive()
    req(fit)
    k <- fit$k
    mus <- round(fit$mus, 2)
    sigs <- round(fit$sigmas, 3)
    cols <- comp_colors()
    
    tagList(
      h4("Component Standards:"),
      fluidRow(
        lapply(seq_len(k), function(j) {
          column(2,
                 div(style = sprintf("background:%s; padding:8px; border-radius:6px; color: #fff; text-align:center; margin:4px", cols[j]),
                     HTML(sprintf("<strong>Std %d</strong><br/>μ = %.2f<br/>σ = %.3f", j, mus[j], sigs[j])))
          )
        })
      )
    )
  })
  
  # Counts UI
  output$counts_ui <- renderUI({
    fit <- fit_reactive()
    req(fit)
    k <- fit$k
    mus <- round(fit$mus, 2)
    counts <- round(fit$estimated_counts, 1)
    cols <- comp_colors()
    
    tagList(
      h4("Estimated Weights per Standard:"),
      fluidRow(
        lapply(seq_len(k), function(j) {
          column(2,
                 div(style = sprintf("background:%s; padding:8px; border-radius:6px; color: #fff; text-align:center; margin:4px", cols[j]),
                     HTML(sprintf("<strong>%.1f weights</strong><br/>@ %.2f gr", counts[j], mus[j])))
          )
        })
      ),
      p(style = "margin-top:10px", strong(sprintf("Total: %.1f weights", sum(counts))))
    )
  })
  
  # Main plot
  output$mainPlot <- renderPlot({
    fit <- fit_reactive()
    req(fit)
    
    mid <- fit$mid
    observed <- fit$observed_counts
    curves_df <- fit$curves_df
    sum_curve_df <- fit$sum_curve_df
    k <- fit$k
    colors <- comp_colors()
    
    bins_df <- tibble(
      BinStart = fit$breaks[-length(fit$breaks)],
      BinEnd = fit$breaks[-1],
      Mid = mid,
      Observed = observed
    )
    
    if (input$show_peirce_chart) {
      # Peirce's Original Chart Style
      # Red dots with circles = observations
      # Blue curves = component distributions
      # Brown curve = sum
      
      p <- ggplot() +
        # Component curves in blue shades
        geom_line(data = curves_df, aes(x = x, y = density, group = factor(component)), 
                  color = "steelblue", size = 1.5, alpha = 0.7)
      
      # Sum curve in brown
      if (input$show_sum_curve || TRUE) {  # Always show in Peirce mode
        p <- p + geom_line(data = sum_curve_df, aes(x = x, y = density), 
                           color = "#8B4513", size = 2)
      }
      
      # Red dots with circles for observations
      obs_points <- tibble(x = mid, y = observed) %>% filter(y > 0)
      p <- p + 
        geom_point(data = obs_points, aes(x = x, y = y), 
                   color = "red", size = 4, shape = 16) +
        geom_point(data = obs_points, aes(x = x, y = y), 
                   color = "red", size = 6, shape = 1, stroke = 1.5)
      
      # Styling for Peirce chart
      p <- p +
        labs(title = "Peirce's Original Chart: Egyptian Ket Weights from Naucratis",
             subtitle = "Red circles = observed counts | Blue curves = component standards | Brown curve = theoretical sum",
             x = "Weight (grains)", y = "Number of Weights") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey30"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#FFF9F0", color = NA),
          plot.background = element_rect(fill = "#FFF9F0", color = NA)
        )
      
    } else {
      # Modern interactive view
      bin_props <- sweep(fit$bin_assignments, 1, rowSums(fit$bin_assignments), FUN = "/")
      bin_props[is.nan(bin_props)] <- 0
      
      # Create base plot
      if (input$show_bar_coloring) {
        blended_cols <- apply(bin_props, 1, function(wts) mix_colors(colors, wts))
        bins_df$fill <- blended_cols
        p <- ggplot() +
          geom_rect(data = bins_df, aes(xmin = BinStart, xmax = BinEnd, ymin = 0, ymax = Observed), 
                    fill = bins_df$fill, color = "black", size = 0.3, alpha = 0.9)
      } else {
        p <- ggplot() +
          geom_rect(data = bins_df, aes(xmin = BinStart, xmax = BinEnd, ymin = 0, ymax = Observed),
                    fill = "lightblue", color = "black", size = 0.3, alpha = 0.8)
      }
      
      # Add component curves if requested - make them more visible
      if (input$show_curves) {
        # Draw curves with black outline for visibility
        for (j in 1:k) {
          curve_j <- curves_df %>% filter(component == j)
          # Black outline for contrast
          p <- p + geom_line(data = curve_j, aes(x = x, y = density), 
                             color = "black", size = 2.5, alpha = 0.6)
        }
        # Actual colored curves on top
        p <- p + geom_line(data = curves_df, aes(x = x, y = density, color = factor(component)), 
                           size = 2, alpha = 0.9)
      }
      
      # Add sum curve if requested - make it very visible and on top
      if (input$show_sum_curve) {
        p <- p + 
          geom_line(data = sum_curve_df, aes(x = x, y = density), 
                    color = "black", size = 2.5, linetype = "solid", alpha = 0.95)
      }
      
      # Add vertical lines at means
      p <- p + geom_vline(xintercept = fit$mus, linetype = "dashed", 
                          color = "grey30", alpha = 0.6, size = 0.8)
      
      # Styling
      p <- p +
        scale_color_manual(values = colors, name = "Standard") +
        labs(title = "Peirce Kets: Weight Distribution and Mixture Decomposition",
             subtitle = sprintf("Bins = %.1f grains | PE = %s | SD = %.3f gr | %d standard%s", 
                                bin_width_fixed, 
                                ifelse(input$use_peirce_pe, "5/8 gr (Peirce)", "estimated"),
                                fit$sigmas[1],
                                k, ifelse(k == 1, "", "s")),
             x = "Weight (grains)", y = "Count") +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          panel.grid.minor = element_blank()
        )
    }
    
    print(p)
  }, res = 96)
  
  # Decomposition table
  output$peirce_table <- renderDT({
    fit <- fit_reactive()
    req(fit)
    
    mid <- fit$mid
    observed <- fit$observed_counts
    bin_assign <- fit$bin_assignments
    k <- fit$k
    
    df_out <- tibble(Bin_Midpoint = round(mid, 2), Observed = observed)
    for (j in seq_len(k)) {
      df_out[[sprintf("Std_%d_(%.2f_gr)", j, fit$mus[j])]] <- round(bin_assign[, j], 3)
    }
    df_out$Predicted_Total <- round(rowSums(bin_assign), 3)
    df_out$Residual <- round(observed - rowSums(bin_assign), 3)
    
    datatable(df_out, 
              extensions = 'Buttons', 
              options = list(
                dom = 'Bfrtip', 
                buttons = c('copy', 'csv', 'excel'), 
                pageLength = 20,
                scrollX = TRUE
              ),
              rownames = FALSE) %>%
      formatRound(columns = 2:ncol(df_out), digits = 2)
  })
  
  # Download table
  output$download_table <- downloadHandler(
    filename = function() paste0("peirce_decomposition_", Sys.Date(), ".csv"),
    content = function(file) {
      fit <- fit_reactive()
      req(fit)
      
      mid <- fit$mid
      observed <- fit$observed_counts
      bin_assign <- fit$bin_assignments
      k <- fit$k
      
      df_out <- tibble(Bin_Midpoint = round(mid, 2), Observed = observed)
      for (j in seq_len(k)) {
        df_out[[sprintf("Std_%d_(%.2f_gr)", j, fit$mus[j])]] <- round(bin_assign[, j], 3)
      }
      df_out$Predicted_Total <- round(rowSums(bin_assign), 3)
      df_out$Residual <- round(observed - rowSums(bin_assign), 3)
      
      write_csv(df_out, file)
    }
  )
}

shinyApp(ui, server)
