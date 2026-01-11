# ==================================================
# Induction vs Deduction: Understanding Statistical Inference
# Enhanced Version with Improved Features
# ==================================================
library(shiny)
library(ggplot2)
library(dplyr)

#dont need the save / load functionality here now. 


ui <- fluidPage(
  titlePanel("Induction vs Deduction: Understanding Statistical Inference"),
  
  tags$head(
    tags$style(HTML("
      .info-box {
        background: #e3f2fd;
        border-left: 4px solid #2196F3;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .warning-box {
        background: #fff3e0;
        border-left: 4px solid #ff9800;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .success-box {
        background: #e8f5e9;
        border-left: 4px solid #4caf50;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .error-box {
        background: #ffebee;
        border-left: 4px solid #f44336;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .key-concept {
        background: #f3e5f5;
        border: 2px solid #9c27b0;
        padding: 12px;
        margin: 10px 0;
        border-radius: 8px;
        font-weight: 500;
      }
      .btn-reset {
        background-color: #9e9e9e;
        color: white;
      }
      @media (max-width: 768px) {
        .col-sm-6 { width: 100%; }
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      
      # Quick Actions (visible on all tabs)
      div(style = "margin-bottom: 15px;",
          actionButton("reset_all", "🔄 Reset to Defaults", 
                       class = "btn-reset", style = "width: 100%; margin: 5px 0;"),
          actionButton("save_scenario", "💾 Save Scenario", 
                       class = "btn-info", style = "width: 100%; margin: 5px 0;"),
          actionButton("load_scenario", "📂 Load Scenario", 
                       class = "btn-info", style = "width: 100%; margin: 5px 0;")
      ),
      
      hr(),
      
      # OVERVIEW TAB
      conditionalPanel(condition = "input.tabs == 'overview'",
                       div(class = "info-box",
                           HTML("<b>The True Process (Hidden in Induction):</b><br>
           Observations = True Value + Random Noise")
                       ),
                       sliderInput("true_mu", "True Value (μ)", 
                                   min = -10, max = 30, value = 10, step = 0.5),
                       selectInput("dist", "Noise Distribution:", 
                                   choices = c("Normal (Gaussian)" = "normal", 
                                               "Uniform" = "uniform",
                                               "Right-Skewed (Gamma)" = "skewed"),
                                   selected = "normal"),
                       numericInput("noise_sigma", "Noise Scale (σ)", 
                                    value = 2, min = 0.1, max = 10, step = 0.1),
                       actionButton("randomize_overview", "🎲 Randomize Parameters", 
                                    class = "btn-warning", style = "width: 100%; margin: 10px 0;")
      ),
      
      # INDUCTION TAB
      conditionalPanel(condition = "input.tabs == 'induction'",
                       div(class = "warning-box",
                           HTML("<b>Induction Mode:</b> We're estimating unknown parameters from data")
                       ),
                       h4("Data Collection"),
                       sliderInput("n", "Sample Size (n)", 
                                   min = 5, max = 500, value = 50, step = 5),
                       actionButton("resample_induction", "📊 Collect New Sample", 
                                    class = "btn-primary", style = "width: 100%; margin: 10px 0;"),
                       actionButton("randomize_induction", "🎲 Randomize Parameters", 
                                    class = "btn-warning", style = "width: 100%; margin: 5px 0;"),
                       hr(),
                       h4("Inference Settings"),
                       sliderInput("conf_level", "Confidence Level (%)", 
                                   min = 50, max = 99, value = 95, step = 1),
                       numericInput("n_sim", "Simulation Repetitions:", 
                                    value = 1000, min = 100, max = 10000, step = 100),
                       actionButton("run_sim", "▶ Run Repeated Sampling Simulation", 
                                    class = "btn-info", style = "width: 100%; margin: 10px 0;"),
                       hr(),
                       h4("Download Options"),
                       downloadButton("download_sample", "Download Sample Data", 
                                      style = "width: 100%; margin: 5px 0;"),
                       downloadButton("download_sim", "Download Sim Results", 
                                      style = "width: 100%; margin: 5px 0;")
      ),
      
      # DEDUCTION TAB
      conditionalPanel(condition = "input.tabs == 'deduction'",
                       div(class = "success-box",
                           HTML("<b>Deduction Mode:</b> We know the parameters perfectly")
                       ),
                       h4("Known Parameters"),
                       sliderInput("ded_mu", "True Value (μ)", 
                                   min = -10, max = 30, value = 10, step = 0.5),
                       selectInput("ded_dist", "Noise Distribution:", 
                                   choices = c("Normal (Gaussian)" = "normal", 
                                               "Uniform" = "uniform",
                                               "Right-Skewed (Gamma)" = "skewed"),
                                   selected = "normal"),
                       numericInput("ded_sigma", "Noise Scale (σ)", 
                                    value = 2, min = 0.1, max = 10, step = 0.1),
                       hr(),
                       h4("Probability Calculations"),
                       numericInput("threshold", "Threshold Value:", 
                                    value = 11, step = 0.5),
                       numericInput("m", "Future Observations:", 
                                    value = 20, min = 1, max = 100, step = 1),
                       numericInput("k", "At least k above threshold:", 
                                    value = 10, min = 0, step = 1),
                       hr(),
                       actionButton("verify_deduction", "🔬 Generate Data & Verify", 
                                    class = "btn-success", style = "width: 100%; margin: 10px 0;"),
                       downloadButton("download_verification", "Download Verification Data", 
                                      style = "width: 100%; margin: 5px 0;")
      ),
      
      # COMPARISON TAB
      conditionalPanel(condition = "input.tabs == 'comparison'",
                       div(class = "info-box",
                           HTML("<b>Comparison Mode:</b> Settings are inherited from other tabs")
                       ),
                       p("Use the Overview, Induction, or Deduction tabs to adjust parameters."),
                       hr(),
                       h4("Quick Actions"),
                       actionButton("resample_comparison", "📊 Collect New Sample", 
                                    class = "btn-primary", style = "width: 100%; margin: 5px 0;"),
                       actionButton("randomize_comparison", "🎲 Randomize Parameters", 
                                    class = "btn-warning", style = "width: 100%; margin: 5px 0;")
      ),
      
      # HELP TAB
      conditionalPanel(condition = "input.tabs == 'help'",
                       div(class = "info-box",
                           HTML("<b>Need help?</b><br>Navigate through the tabs to explore different concepts.")
                       )
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Overview", value = "overview",
                           br(),
                           div(class = "key-concept",
                               h3("Key Distinction", style = "margin-top: 0;"),
                               HTML("<b>INDUCTION:</b> We observe data and try to learn about unknown parameters. 
                  We quantify the <i>reliability of our method</i>, not the probability our specific conclusion is correct.<br><br>
                  <b>DEDUCTION:</b> We know the parameters perfectly and can calculate exact probabilities 
                  for future events using mathematical laws.")
                           ),
                           
                           hr(),
                           h3("Current Data-Generating Process"),
                           uiOutput("process_description"),
                           plotOutput("process_plot", height = "300px"),
                           
                           hr(),
                           h3("Quick Start Guide"),
                           div(class = "info-box",
                               HTML("<ol>
                  <li>Go to the <b>Induction</b> tab to see how we estimate unknown parameters from data</li>
                  <li>Observe how confidence intervals behave across repeated samples</li>
                  <li>Go to the <b>Deduction</b> tab to see exact probability calculations when parameters are known</li>
                  <li>Try the <b>Comparison</b> tab to see both side-by-side</li>
                  </ol>")
                           )
                  ),
                  
                  tabPanel("Induction", value = "induction",
                           br(),
                           div(class = "warning-box",
                               HTML("<b>INDUCTION MODE:</b> Pretend we don't know μ = ", 
                                    textOutput("hidden_mu_text", inline = TRUE), 
                                    ". We must estimate it from noisy data.")
                           ),
                           
                           fluidRow(
                             column(6,
                                    h4("Current Sample Statistics"),
                                    verbatimTextOutput("sample_stats")
                             ),
                             column(6,
                                    h4("What Does the CI Mean?"),
                                    uiOutput("ci_interpretation_box")
                             )
                           ),
                           
                           hr(),
                           h3("Visualizations"),
                           
                           fluidRow(
                             column(6,
                                    plotOutput("sample_hist", height = "350px")
                             ),
                             column(6,
                                    plotOutput("ci_illustration", height = "350px")
                             )
                           ),
                           
                           hr(),
                           h3("Testing the Method: Repeated Sampling"),
                           div(class = "info-box",
                               "Click 'Run Repeated Sampling Simulation' to see how often our CI method captures the true value."
                           ),
                           
                           uiOutput("simulation_results"),
                           plotOutput("repeated_ci_plot", height = "400px"),
                           
                           hr(),
                           div(class = "key-concept",
                               HTML("<b>Remember:</b> The CI tells us about <i>method reliability</i>, not the probability 
                  that this specific interval contains μ. After we compute one specific interval, 
                  it either does or doesn't contain μ - there's no probability about it anymore!")
                           )
                  ),
                  
                  tabPanel("Deduction", value = "deduction",
                           br(),
                           div(class = "success-box",
                               HTML("<b>DEDUCTION MODE:</b> Now we KNOW μ = ", 
                                    textOutput("known_mu_text", inline = TRUE), 
                                    " and σ = ", textOutput("known_sigma_text", inline = TRUE),
                                    ". We can calculate exact probabilities!")
                           ),
                           
                           h3("Probability Calculations"),
                           fluidRow(
                             column(6,
                                    h4("Single Observation"),
                                    verbatimTextOutput("single_prob"),
                                    plotOutput("single_obs_plot", height = "300px")
                             ),
                             column(6,
                                    h4("Multiple Observations"),
                                    verbatimTextOutput("multiple_prob"),
                                    plotOutput("binomial_plot", height = "300px")
                             )
                           ),
                           
                           hr(),
                           h3("Empirical Verification"),
                           div(class = "info-box",
                               "Let's verify our deductive calculations by actually generating random data:"
                           ),
                           actionButton("verify_deduction2", "🔬 Generate Data & Verify", 
                                        class = "btn-success", style = "margin: 10px 0;"),
                           uiOutput("verification_results"),
                           plotOutput("verification_plot", height = "300px")
                  ),
                  
                  tabPanel("Comparison", value = "comparison",
                           br(),
                           h3("Side-by-Side Comparison"),
                           
                           fluidRow(
                             column(6,
                                    div(class = "warning-box",
                                        h4("INDUCTION", style = "margin-top: 0;"),
                                        HTML("Parameters: <b>UNKNOWN</b><br>
                      We estimate from data<br>
                      We quantify method reliability")
                                    ),
                                    verbatimTextOutput("comparison_induction"),
                                    plotOutput("comparison_induction_plot", height = "300px")
                             ),
                             column(6,
                                    div(class = "success-box",
                                        h4("DEDUCTION", style = "margin-top: 0;"),
                                        HTML("Parameters: <b>KNOWN</b><br>
                      We calculate exact probabilities<br>
                      We make definite predictions")
                                    ),
                                    verbatimTextOutput("comparison_deduction"),
                                    plotOutput("comparison_deduction_plot", height = "300px")
                             )
                           ),
                           
                           hr(),
                           div(class = "key-concept",
                               h4("The Fundamental Difference"),
                               HTML("<p><b>Induction:</b> 'Based on this sample, our method gives us [CI]. 
                  If we used this method many times, it would capture the true value X% of the time.'</p>
                  <p><b>Deduction:</b> 'Given these exact parameters, the probability of [event] is exactly Y.'</p>
                  <p>This is why we can never say 'there's a 95% probability μ is in this interval' - 
                  that would be mixing inductive inference (what we learned from data) with deductive 
                  probability (which requires known parameters).</p>")
                           )
                  ),
                  
                  tabPanel("Help", value = "help",
                           br(),
                           h3("Understanding This App"),
                           
                           div(class = "info-box",
                               h4("The Data-Generating Model"),
                               HTML("<p>This app uses a simple model where each observation equals:</p>
                  <p style='text-align: center; font-size: 18px;'><b>Observation = μ + Noise</b></p>
                  <p>Where μ is the true value and Noise comes from a chosen distribution (Normal, Uniform, or Skewed).</p>")
                           ),
                           
                           hr(),
                           h4("Tab Descriptions"),
                           
                           tags$ul(
                             tags$li(tags$b("Overview:"), " See the current data-generating process and get started"),
                             tags$li(tags$b("Induction:"), " Estimate unknown parameters from data. Learn about confidence intervals and method reliability"),
                             tags$li(tags$b("Deduction:"), " Calculate exact probabilities when parameters are known"),
                             tags$li(tags$b("Comparison:"), " See both approaches side-by-side")
                           ),
                           
                           hr(),
                           h4("Key Concepts"),
                           
                           div(class = "key-concept",
                               h5("Confidence Intervals"),
                               HTML("<p>A 95% CI means: If we repeated our sampling procedure many times and computed a 95% CI 
                  each time, about 95% of those intervals would contain the true parameter.</p>
                  <p><b>It does NOT mean:</b> 'There's a 95% probability the true value is in this specific interval.'</p>")
                           ),
                           
                           div(class = "key-concept",
                               h5("Why This Matters"),
                               HTML("<p>This distinction is crucial in science and statistics:</p>
                  <ul>
                  <li>We rarely know true parameters, so we use induction (estimation from data)</li>
                  <li>Confidence intervals quantify our method's reliability, not our certainty about this specific result</li>
                  <li>Only when we know parameters perfectly can we make probability statements (deduction)</li>
                  </ul>")
                           ),
                           
                           hr(),
                           h4("Suggestions for Exploration"),
                           
                           tags$ol(
                             tags$li("Start with a small sample size (n=10) and watch how CIs vary widely"),
                             tags$li("Increase sample size to see CIs become narrower and more reliable"),
                             tags$li("Run the repeated sampling simulation to verify the CI coverage rate"),
                             tags$li("Try different noise distributions (especially skewed) to see how methods adapt"),
                             tags$li("In Deduction mode, verify that empirical frequencies match calculated probabilities")
                           )
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Default values for reset
  defaults <- list(
    mu = 10,
    sigma = 2,
    dist = "normal",
    n = 50,
    conf_level = 95,
    n_sim = 1000,
    threshold = 11,
    m = 20,
    k = 10
  )
  
  # Reactive values
  rv <- reactiveValues(
    mu = 10,
    sigma = 2,
    sample = NULL,
    sim_run = FALSE,
    sim_results = NULL,
    verification_data = NULL,
    saved_scenarios = list(),
    computing = FALSE
  )
  
  # Generate sample function - improved skewed distribution
  make_sample <- function(n, mu, sigma, dist) {
    tryCatch({
      if (dist == "normal") {
        rnorm(n, mean = mu, sd = sigma)
      } else if (dist == "uniform") {
        runif(n, min = mu - sigma * sqrt(3), max = mu + sigma * sqrt(3))
      } else { # skewed - using gamma distribution
        shape <- 4
        scale <- sigma / 2
        mu + rgamma(n, shape = shape, scale = scale) - shape * scale
      }
    }, error = function(e) {
      showNotification("Error generating sample. Using normal distribution.", type = "error")
      rnorm(n, mean = mu, sd = sigma)
    })
  }
  
  # Initialize with default sample
  observe({
    req(input$n)
    if (is.null(rv$sample)) {
      rv$sample <- make_sample(input$n, rv$mu, rv$sigma, input$dist)
    }
  })
  
  # Sync Overview tab params with reactive values
  observe({
    rv$mu <- input$true_mu
    rv$sigma <- input$noise_sigma
  })
  
  # Sync Deduction tab params when changed
  observe({
    req(input$ded_mu)
    updateSliderInput(session, "true_mu", value = input$ded_mu)
  })
  
  observe({
    req(input$ded_sigma)
    updateNumericInput(session, "noise_sigma", value = input$ded_sigma)
  })
  
  observe({
    req(input$ded_dist)
    updateSelectInput(session, "dist", selected = input$ded_dist)
  })
  
  # Reset to defaults
  observeEvent(input$reset_all, {
    updateSliderInput(session, "true_mu", value = defaults$mu)
    updateNumericInput(session, "noise_sigma", value = defaults$sigma)
    updateSelectInput(session, "dist", selected = defaults$dist)
    updateSliderInput(session, "n", value = defaults$n)
    updateSliderInput(session, "conf_level", value = defaults$conf_level)
    updateNumericInput(session, "n_sim", value = defaults$n_sim)
    updateNumericInput(session, "threshold", value = defaults$threshold)
    updateNumericInput(session, "m", value = defaults$m)
    updateNumericInput(session, "k", value = defaults$k)
    
    # Also update deduction tab
    updateSliderInput(session, "ded_mu", value = defaults$mu)
    updateNumericInput(session, "ded_sigma", value = defaults$sigma)
    updateSelectInput(session, "ded_dist", selected = defaults$dist)
    
    rv$sample <- make_sample(defaults$n, defaults$mu, defaults$sigma, defaults$dist)
    rv$sim_run <- FALSE
    rv$verification_data <- NULL
    
    showNotification("All parameters reset to defaults", type = "message")
  })
  
  # Save scenario
  observeEvent(input$save_scenario, {
    scenario_name <- paste("Scenario", length(rv$saved_scenarios) + 1)
    rv$saved_scenarios[[scenario_name]] <- list(
      mu = rv$mu,
      sigma = rv$sigma,
      dist = input$dist,
      n = input$n,
      conf_level = input$conf_level,
      threshold = input$threshold,
      m = input$m,
      k = input$k
    )
    showNotification(paste("Saved as", scenario_name), type = "message")
  })
  
  # Load scenario (simplified - loads most recent)
  observeEvent(input$load_scenario, {
    if (length(rv$saved_scenarios) > 0) {
      scenario <- rv$saved_scenarios[[length(rv$saved_scenarios)]]
      updateSliderInput(session, "true_mu", value = scenario$mu)
      updateNumericInput(session, "noise_sigma", value = scenario$sigma)
      updateSelectInput(session, "dist", selected = scenario$dist)
      updateSliderInput(session, "n", value = scenario$n)
      updateSliderInput(session, "conf_level", value = scenario$conf_level)
      updateNumericInput(session, "threshold", value = scenario$threshold)
      updateNumericInput(session, "m", value = scenario$m)
      updateNumericInput(session, "k", value = scenario$k)
      showNotification("Scenario loaded", type = "message")
    } else {
      showNotification("No saved scenarios available", type = "warning")
    }
  })
  
  # Resample - unified handler for all resample buttons
  resample_handler <- function() {
    rv$sample <- make_sample(input$n, rv$mu, rv$sigma, input$dist)
    rv$sim_run <- FALSE
  }
  
  observeEvent(input$resample_induction, resample_handler())
  observeEvent(input$resample_comparison, resample_handler())
  
  # Randomize - unified handler for all randomize buttons
  randomize_handler <- function() {
    new_mu <- round(runif(1, -5, 25), 2)
    new_sigma <- round(runif(1, 0.5, 5), 2)
    rv$mu <- new_mu
    rv$sigma <- new_sigma
    updateSliderInput(session, "true_mu", value = new_mu)
    updateNumericInput(session, "noise_sigma", value = new_sigma)
    updateSliderInput(session, "ded_mu", value = new_mu)
    updateNumericInput(session, "ded_sigma", value = new_sigma)
    rv$sample <- make_sample(input$n, rv$mu, rv$sigma, input$dist)
    rv$sim_run <- FALSE
    showNotification("Parameters randomized", type = "message")
  }
  
  observeEvent(input$randomize_overview, randomize_handler())
  observeEvent(input$randomize_induction, randomize_handler())
  observeEvent(input$randomize_comparison, randomize_handler())
  
  # Compute CI - with error handling
  compute_ci <- function(x, conf_level, dist) {
    tryCatch({
      n <- length(x)
      if (n < 2) stop("Need at least 2 observations")
      
      xbar <- mean(x)
      s <- sd(x)
      conf <- conf_level / 100
      
      if (dist == "normal") {
        se <- s / sqrt(n)
        tcrit <- qt(1 - (1 - conf) / 2, df = n - 1)
        ci <- xbar + c(-1, 1) * tcrit * se
        list(ci = ci, se = se, method = "t-based")
      } else {
        # Adaptive bootstrap iterations
        B <- min(5000, max(1000, n * 20))
        boot_means <- replicate(B, mean(sample(x, size = n, replace = TRUE)))
        ci <- quantile(boot_means, probs = c((1 - conf) / 2, 1 - (1 - conf) / 2))
        se <- sd(boot_means)
        list(ci = ci, se = se, method = paste0("bootstrap (B=", B, ")"))
      }
    }, error = function(e) {
      showNotification(paste("CI computation error:", e$message), type = "error")
      list(ci = c(NA, NA), se = NA, method = "error")
    })
  }
  
  # OVERVIEW TAB
  output$process_description <- renderUI({
    dist_name <- switch(input$dist,
                        "normal" = "Normal(0, σ)",
                        "uniform" = "Uniform(-σ√3, σ√3)",
                        "skewed" = "Right-Skewed Gamma")
    
    div(class = "success-box",
        h4("True Data-Generating Process:"),
        HTML(sprintf("<p style='font-size: 16px;'><b>Observation = %.2f + %s</b></p>
                    <p>Where noise has scale σ = %.2f</p>", 
                     rv$mu, dist_name, rv$sigma))
    )
  })
  
  output$process_plot <- renderPlot({
    x_range <- seq(rv$mu - 4 * rv$sigma, rv$mu + 4 * rv$sigma, length.out = 200)
    
    if (input$dist == "normal") {
      y <- dnorm(x_range, rv$mu, rv$sigma)
    } else if (input$dist == "uniform") {
      spread <- rv$sigma * sqrt(3)
      y <- dunif(x_range, rv$mu - spread, rv$mu + spread)
    } else {
      shape <- 4
      scale <- rv$sigma / 2
      y <- dgamma(x_range - rv$mu + shape * scale, shape = shape, scale = scale)
      y[x_range < rv$mu - shape * scale] <- 0
    }
    
    df <- data.frame(x = x_range, y = y)
    ggplot(df, aes(x = x, y = y)) +
      geom_line(size = 1.5, color = "#2196F3") +
      geom_area(alpha = 0.3, fill = "#2196F3") +
      geom_vline(xintercept = rv$mu, linetype = "dashed", color = "red", size = 1) +
      annotate("text", x = rv$mu, y = max(y) * 0.9, label = paste("μ =", rv$mu), 
               color = "red", fontface = "bold", hjust = -0.2) +
      labs(title = "True Distribution of Observations",
           x = "Value", y = "Density") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # INDUCTION TAB
  output$hidden_mu_text <- renderText({
    paste(rv$mu)
  })
  
  output$sample_stats <- renderPrint({
    req(rv$sample)
    x <- rv$sample
    xbar <- mean(x)
    s <- sd(x)
    ci_info <- compute_ci(x, input$conf_level, input$dist)
    
    cat("Sample Statistics:\n")
    cat("─────────────────\n")
    cat(sprintf("Sample size (n): %d\n", length(x)))
    cat(sprintf("Sample mean (x̄): %.4f\n", xbar))
    cat(sprintf("Sample SD (s): %.4f\n", s))
    cat(sprintf("Standard error: %.4f\n", ci_info$se))
    cat("\n")
    cat(sprintf("%d%% Confidence Interval:\n", input$conf_level))
    cat(sprintf("[%.4f, %.4f]\n", ci_info$ci[1], ci_info$ci[2]))
    cat(sprintf("Width: %.4f\n", ci_info$ci[2] - ci_info$ci[1]))
    cat(sprintf("Method: %s\n", ci_info$method))
    cat("\n")
    cat("True value (hidden): ***\n")
    cat(sprintf("Does CI contain μ? %s\n", 
                ifelse(rv$mu >= ci_info$ci[1] & rv$mu <= ci_info$ci[2], "YES ✓", "NO ✗")))
  })
  
  output$ci_interpretation_box <- renderUI({
    req(rv$sample)
    ci_info <- compute_ci(rv$sample, input$conf_level, input$dist)
    ci <- ci_info$ci
    
    tagList(
      div(class = "success-box",
          h5("✓ CORRECT Interpretation:", style = "margin-top: 0;"),
          HTML(sprintf("<p>If we repeated our sampling method many times, 
               approximately <b>%d%%</b> of the resulting CIs would contain the true μ.</p>
               <p>This is a statement about the <i>method's reliability</i>, not about this specific interval.</p>", 
                       input$conf_level))
      ),
      div(class = "error-box", style = "margin-top: 10px;",
          h5("✗ INCORRECT Interpretation:", style = "margin-top: 0;"),
          HTML(sprintf("<p>'There is a %d%% probability that μ is in [%.2f, %.2f].'</p>
               <p><b>Why wrong?</b> After computing this specific interval, it either contains μ or it doesn't—
               there's no probability involved! We can't make probability statements about fixed parameters 
               unless we know them (that would be deduction, not induction).</p>",
                       input$conf_level, ci[1], ci[2]))
      )
    )
  })
  
  output$sample_hist <- renderPlot({
    req(rv$sample)
    x <- rv$sample
    xbar <- mean(x)
    ci_info <- compute_ci(x, input$conf_level, input$dist)
    ci <- ci_info$ci
    
    df <- data.frame(x = x)
    
    ggplot(df, aes(x = x)) +
      annotate("rect", xmin = ci[1], xmax = ci[2], ymin = 0, ymax = Inf,
               fill = "#4CAF50", alpha = 0.2) +
      geom_histogram(bins = 30, fill = "#64B5F6", color = "white", alpha = 0.7) +
      geom_vline(xintercept = xbar, color = "darkblue", size = 1.5, linetype = "solid") +
      geom_vline(xintercept = rv$mu, color = "red", size = 1.5, linetype = "dashed", alpha = 0.7) +
      geom_vline(xintercept = ci[1], color = "#4CAF50", size = 1, linetype = "dotted") +
      geom_vline(xintercept = ci[2], color = "#4CAF50", size = 1, linetype = "dotted") +
      annotate("text", x = xbar, y = Inf, label = paste("x̄ =", round(xbar, 2)), 
               color = "darkblue", fontface = "bold", vjust = 2, hjust = -0.1) +
      annotate("text", x = rv$mu, y = Inf, label = paste("μ =", rv$mu, "(true)"), 
               color = "red", fontface = "bold", vjust = 4, hjust = -0.1) +
      annotate("text", x = mean(ci), y = Inf, 
               label = sprintf("%d%% CI", input$conf_level),
               color = "#4CAF50", fontface = "bold", vjust = 6, hjust = 0.5) +
      labs(title = "Current Sample Distribution with Confidence Interval",
           subtitle = "Green shaded region = confidence interval",
           x = "Observed Value", y = "Count") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  output$ci_illustration <- renderPlot({
    req(rv$sample)
    x <- rv$sample
    xbar <- mean(x)
    ci_info <- compute_ci(x, input$conf_level, input$dist)
    ci <- ci_info$ci
    contains_mu <- rv$mu >= ci[1] & rv$mu <= ci[2]
    
    df <- data.frame(
      y = 1,
      x = xbar,
      xmin = ci[1],
      xmax = ci[2]
    )
    
    ggplot(df, aes(x = x, y = y)) +
      geom_point(size = 8, color = "darkblue") +
      geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.2, size = 1.5,
                     color = ifelse(contains_mu, "#4CAF50", "#f44336")) +
      geom_vline(xintercept = rv$mu, linetype = "dashed", color = "red", size = 1.2) +
      annotate("text", x = rv$mu, y = 1.3, label = paste("True μ =", rv$mu),
               color = "red", fontface = "bold") +
      annotate("text", x = xbar, y = 0.7, 
               label = sprintf("[%.2f, %.2f]", ci[1], ci[2]),
               color = ifelse(contains_mu, "#4CAF50", "#f44336"), fontface = "bold") +
      labs(title = ifelse(contains_mu, 
                          "✓ This CI Captures the True Value", 
                          "✗ This CI Misses the True Value"),
           subtitle = sprintf("%d%% Confidence Interval", input$conf_level),
           x = "Value", y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  # Run simulation with improved progress tracking
  observeEvent(input$run_sim, {
    n_reps <- input$n_sim
    n <- input$n
    mu <- rv$mu
    sigma <- rv$sigma
    dist <- input$dist
    conf <- input$conf_level
    
    if (n < 2) {
      showNotification("Sample size must be at least 2", type = "error")
      return()
    }
    
    rv$computing <- TRUE
    
    withProgress(message = 'Running simulation...', value = 0, {
      ci_l <- numeric(n_reps)
      ci_u <- numeric(n_reps)
      xbar_vals <- numeric(n_reps)
      contains_true <- logical(n_reps)
      
      for (i in 1:n_reps) {
        if (i %% 100 == 0) {
          incProgress(100/n_reps, detail = paste("Sample", i, "of", n_reps))
        }
        
        x <- make_sample(n, mu, sigma, dist)
        xbar_vals[i] <- mean(x)
        ci_info <- compute_ci(x, conf, dist)
        ci_l[i] <- ci_info$ci[1]
        ci_u[i] <- ci_info$ci[2]
        contains_true[i] <- (ci_l[i] <= mu) & (ci_u[i] >= mu)
      }
      
      rv$sim_results <- data.frame(
        rep = 1:n_reps,
        mean = xbar_vals,
        ci_l = ci_l,
        ci_u = ci_u,
        contains = contains_true
      )
      rv$sim_run <- TRUE
      rv$computing <- FALSE
    })
    
    showNotification("Simulation complete!", type = "message")
  })
  
  output$simulation_results <- renderUI({
    if (!rv$sim_run) {
      return(div(class = "warning-box",
                 HTML("<p>Click 'Run Repeated Sampling Simulation' to test the method.</p>
                      <p>This will show how often the CI method captures the true value across many samples.</p>")))
    }
    
    n_contain <- sum(rv$sim_results$contains)
    n_total <- nrow(rv$sim_results)
    pct_contain <- 100 * n_contain / n_total
    expected <- input$conf_level
    diff <- abs(pct_contain - expected)
    
    margin <- 3
    is_good <- diff < margin
    
    div(class = if(is_good) "success-box" else "warning-box",
        h4("Simulation Results", style = "margin-top: 0;"),
        HTML(sprintf("<p style='font-size: 16px;'><b>%d out of %d CIs</b> contained the true value</p>
                    <p style='font-size: 18px; font-weight: bold;'>Observed Coverage: %.2f%%</p>
                    <p>Expected Coverage: ~%d%% (based on confidence level)</p>
                    <p>Difference: %.2f percentage points</p>
                    <p>%s</p>",
                     n_contain, n_total, pct_contain, expected, diff,
                     if(is_good) {
                       "✓ <b>The method is working as expected!</b> The small difference is due to random variation."
                     } else {
                       "The difference from expected is larger than typical random variation. This could be due to the sample distribution or finite simulation size."
                     }))
    )
  })
  
  output$repeated_ci_plot <- renderPlot({
    if (!rv$sim_run) return(NULL)
    
    df <- rv$sim_results[1:min(100, nrow(rv$sim_results)), ]
    
    ggplot(df, aes(x = rep, y = mean, color = contains)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_errorbar(aes(ymin = ci_l, ymax = ci_u), alpha = 0.4, width = 0) +
      geom_hline(yintercept = rv$mu, color = "red", linetype = "dashed", size = 1.2) +
      scale_color_manual(values = c("FALSE" = "#f44336", "TRUE" = "#4CAF50"),
                         labels = c("Missed μ", "Captured μ"),
                         name = "") +
      labs(title = sprintf("First %d CIs from Repeated Sampling", nrow(df)),
           subtitle = sprintf("Red line = true μ = %.2f", rv$mu),
           x = "Sample Number", y = "Sample Mean with CI") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  # DEDUCTION TAB
  output$known_mu_text <- renderText({
    paste(input$ded_mu)
  })
  
  output$known_sigma_text <- renderText({
    paste(input$ded_sigma)
  })
  
  # Calculate single observation probability - using deduction tab params
  single_prob_calc <- reactive({
    mu <- input$ded_mu
    sigma <- input$ded_sigma
    thresh <- input$threshold
    dist <- input$ded_dist
    
    if (input$k > input$m) {
      updateNumericInput(session, "k", value = input$m)
    }
    
    tryCatch({
      if (dist == "normal") {
        p <- 1 - pnorm(thresh, mu, sigma)
      } else if (dist == "uniform") {
        spread <- sigma * sqrt(3)
        a <- mu - spread
        b <- mu + spread
        p <- if(thresh <= a) 1 else if(thresh >= b) 0 else (b - thresh) / (b - a)
      } else {
        shape <- 4
        scale <- sigma / 2
        shifted_thresh <- thresh - mu + shape * scale
        p <- if(shifted_thresh < 0) 1 else pgamma(shifted_thresh, shape = shape, 
                                                  scale = scale, lower.tail = FALSE)
      }
      max(0, min(1, p))
    }, error = function(e) {
      showNotification(paste("Probability calculation error:", e$message), type = "error")
      0.5
    })
  })
  
  output$single_prob <- renderPrint({
    p <- single_prob_calc()
    cat("Single Observation Probability:\n")
    cat("═══════════════════════════════\n\n")
    cat(sprintf("P(X > %.3f) = %.4f\n\n", input$threshold, p))
    cat(sprintf("In other words: %.1f%% of observations\n", p * 100))
    cat(sprintf("will exceed %.3f\n", input$threshold))
  })
  
  output$single_obs_plot <- renderPlot({
    mu <- input$ded_mu
    sigma <- input$ded_sigma
    thresh <- input$threshold
    dist <- input$ded_dist
    
    x_range <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 200)
    
    if (dist == "normal") {
      y <- dnorm(x_range, mu, sigma)
    } else if (dist == "uniform") {
      spread <- sigma * sqrt(3)
      y <- dunif(x_range, mu - spread, mu + spread)
    } else {
      shape <- 4
      scale <- sigma / 2
      y <- dgamma(x_range - mu + shape * scale, shape = shape, scale = scale)
      y[x_range < mu - shape * scale] <- 0
    }
    
    df <- data.frame(x = x_range, y = y)
    df$above <- df$x > thresh
    
    ggplot(df, aes(x = x, y = y)) +
      geom_area(data = subset(df, above), fill = "#4CAF50", alpha = 0.5) +
      geom_line(size = 1.2, color = "#2196F3") +
      geom_vline(xintercept = thresh, linetype = "dashed", color = "red", size = 1.2) +
      annotate("text", x = thresh, y = max(y) * 0.9, 
               label = paste("Threshold =", round(thresh, 2)),
               color = "red", fontface = "bold", hjust = -0.1) +
      annotate("text", x = thresh + sigma, y = max(y) * 0.5,
               label = sprintf("P(X > %.1f) = %.3f", thresh, single_prob_calc()),
               color = "#4CAF50", fontface = "bold", size = 5) +
      labs(title = "Probability of Exceeding Threshold",
           subtitle = "Green area = probability mass above threshold",
           x = "Value", y = "Density") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  output$multiple_prob <- renderPrint({
    p <- single_prob_calc()
    m <- input$m
    k <- input$k
    
    expected_count <- m * p
    prob_at_least_k <- 1 - pbinom(k - 1, size = m, prob = p)
    
    cat("Multiple Observations:\n")
    cat("═════════════════════\n\n")
    cat(sprintf("Future observations: m = %d\n", m))
    cat(sprintf("P(single obs > threshold) = %.4f\n\n", p))
    cat(sprintf("Expected count > threshold: %.2f\n", expected_count))
    cat(sprintf("P(at least k = %d exceed) = %.4f\n", k, prob_at_least_k))
    cat(sprintf("                          = %.2f%%\n", prob_at_least_k * 100))
  })
  
  output$binomial_plot <- renderPlot({
    p <- single_prob_calc()
    m <- input$m
    k <- input$k
    
    counts <- 0:m
    probs <- dbinom(counts, size = m, prob = p)
    
    df <- data.frame(counts = counts, probs = probs)
    df$highlight <- df$counts >= k
    
    ggplot(df, aes(x = counts, y = probs, fill = highlight)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      scale_fill_manual(values = c("FALSE" = "#64B5F6", "TRUE" = "#4CAF50"),
                        labels = c(paste("< ", k), paste("≥", k)),
                        name = "Count") +
      geom_vline(xintercept = m * p, linetype = "dashed", color = "red", size = 1) +
      annotate("text", x = m * p, y = max(probs) * 0.9,
               label = sprintf("Expected = %.1f", m * p),
               color = "red", fontface = "bold", hjust = -0.1) +
      labs(title = "Distribution of Count Above Threshold",
           subtitle = sprintf("Binomial(m = %d, p = %.3f)", m, p),
           x = "Number of Observations Above Threshold",
           y = "Probability") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  # Verify deduction with improved progress
  verify_deduction_handler <- function() {
    n_sims <- 10000
    mu <- input$ded_mu
    sigma <- input$ded_sigma
    dist <- input$ded_dist
    thresh <- input$threshold
    m <- input$m
    
    rv$computing <- TRUE
    
    withProgress(message = 'Generating verification data...', value = 0, {
      counts <- numeric(n_sims)
      
      for (i in 1:n_sims) {
        if (i %% 100 == 0) {
          incProgress(100/n_sims, detail = paste("Simulation", i, "of", n_sims))
        }
        
        sample <- make_sample(m, mu, sigma, dist)
        counts[i] <- sum(sample > thresh)
      }
      
      rv$verification_data <- counts
      rv$computing <- FALSE
    })
    
    showNotification("Verification complete!", type = "message")
  }
  
  observeEvent(input$verify_deduction, verify_deduction_handler())
  observeEvent(input$verify_deduction2, verify_deduction_handler())
  
  output$verification_results <- renderUI({
    if (is.null(rv$verification_data)) {
      return(NULL)
    }
    
    p <- single_prob_calc()
    m <- input$m
    k <- input$k
    
    empirical_mean <- mean(rv$verification_data)
    theoretical_mean <- m * p
    
    empirical_prob_k <- mean(rv$verification_data >= k)
    theoretical_prob_k <- 1 - pbinom(k - 1, size = m, prob = p)
    
    div(class = "success-box",
        h4("Verification Results (10,000 simulations)", style = "margin-top: 0;"),
        HTML(sprintf("<p><b>Expected count > threshold:</b><br>
                    Theoretical: %.2f | Empirical: %.2f (difference: %.3f)</p>
                    <p><b>P(at least %d exceed):</b><br>
                    Theoretical: %.4f | Empirical: %.4f (difference: %.4f)</p>
                    <p>✓ The close match confirms our deductive calculations are correct!</p>",
                     theoretical_mean, empirical_mean, abs(theoretical_mean - empirical_mean),
                     k, theoretical_prob_k, empirical_prob_k, 
                     abs(theoretical_prob_k - empirical_prob_k)))
    )
  })
  
  output$verification_plot <- renderPlot({
    if (is.null(rv$verification_data)) {
      return(NULL)
    }
    
    p <- single_prob_calc()
    m <- input$m
    
    empirical_df <- data.frame(count = rv$verification_data)
    
    theoretical_df <- data.frame(
      count = 0:m,
      prob = dbinom(0:m, size = m, prob = p)
    )
    
    ggplot() +
      geom_histogram(data = empirical_df, aes(x = count, y = ..density..), 
                     bins = m + 1, fill = "#64B5F6", alpha = 0.5, color = "white") +
      geom_line(data = theoretical_df, aes(x = count, y = prob), 
                color = "red", size = 1.5) +
      geom_point(data = theoretical_df, aes(x = count, y = prob),
                 color = "red", size = 3) +
      labs(title = "Empirical vs Theoretical Distribution",
           subtitle = "Histogram = simulated data | Red line = theoretical probabilities",
           x = "Count Above Threshold", y = "Density / Probability") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  # COMPARISON TAB
  output$comparison_induction <- renderPrint({
    req(rv$sample)
    x <- rv$sample
    ci_info <- compute_ci(x, input$conf_level, input$dist)
    
    cat("INDUCTION: Estimating Unknown μ\n")
    cat("═══════════════════════════════\n\n")
    cat(sprintf("We collected n = %d observations\n", length(x)))
    cat(sprintf("Sample mean: %.3f\n", mean(x)))
    cat(sprintf("%d%% CI: [%.3f, %.3f]\n\n", input$conf_level, ci_info$ci[1], ci_info$ci[2]))
    cat("Interpretation:\n")
    cat(sprintf("'Our METHOD will produce CIs that\n"))
    cat(sprintf("contain μ about %d%% of the time.'\n\n", input$conf_level))
    cat("We CANNOT say:\n")
    cat(sprintf("'μ has a %d%% probability of being\n", input$conf_level))
    cat("in this specific interval.'\n")
  })
  
  output$comparison_induction_plot <- renderPlot({
    req(rv$sample)
    x <- rv$sample
    xbar <- mean(x)
    ci_info <- compute_ci(x, input$conf_level, input$dist)
    ci <- ci_info$ci
    
    df <- data.frame(
      y = 1,
      x = xbar,
      xmin = ci[1],
      xmax = ci[2]
    )
    
    ggplot(df, aes(x = x, y = y)) +
      geom_point(size = 10, color = "darkblue") +
      geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.3, size = 2,
                     color = "darkblue") +
      annotate("text", x = xbar, y = 0.6, 
               label = sprintf("x̄ = %.2f", xbar),
               fontface = "bold", size = 5) +
      annotate("text", x = xbar, y = 1.4,
               label = sprintf("%d%% CI", input$conf_level),
               fontface = "bold", size = 4, color = "darkblue") +
      labs(title = "Estimated Parameter with Uncertainty",
           x = "Estimated Value", y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })
  
  output$comparison_deduction <- renderPrint({
    p <- single_prob_calc()
    m <- input$m
    k <- input$k
    thresh <- input$threshold
    mu_ded <- input$ded_mu
    sigma_ded <- input$ded_sigma
    
    cat("DEDUCTION: Using Known Parameters\n")
    cat("═════════════════════════════════\n\n")
    cat(sprintf("We KNOW: μ = %.2f, σ = %.2f\n\n", mu_ded, sigma_ded))
    cat(sprintf("P(X > %.2f) = %.4f\n\n", thresh, p))
    cat(sprintf("For m = %d future observations:\n", m))
    cat(sprintf("Expected count > %.2f: %.2f\n", thresh, m * p))
    cat(sprintf("P(at least %d exceed): %.4f\n\n", k, 1 - pbinom(k - 1, m, p)))
    cat("These are EXACT probabilities\n")
    cat("calculated from known parameters.\n")
  })
  
  output$comparison_deduction_plot <- renderPlot({
    p <- single_prob_calc()
    m <- input$m
    
    counts <- 0:m
    probs <- dbinom(counts, size = m, prob = p)
    
    df <- data.frame(counts = counts, probs = probs)
    
    ggplot(df, aes(x = counts, y = probs)) +
      geom_bar(stat = "identity", fill = "#4CAF50", alpha = 0.7) +
      geom_vline(xintercept = m * p, linetype = "dashed", color = "red", size = 1.5) +
      labs(title = "Exact Probability Distribution",
           subtitle = "Calculated from known parameters",
           x = "Future Count Above Threshold",
           y = "Probability") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  # Download handlers
  output$download_sample <- downloadHandler(
    filename = function() {
      paste("sample_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(rv$sample)) {
        df <- data.frame(
          observation = 1:length(rv$sample),
          value = rv$sample,
          true_mu = rv$mu,
          noise_sigma = rv$sigma,
          distribution = input$dist
        )
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  output$download_sim <- downloadHandler(
    filename = function() {
      paste("simulation_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(rv$sim_results)) {
        write.csv(rv$sim_results, file, row.names = FALSE)
      }
    }
  )
  
  output$download_verification <- downloadHandler(
    filename = function() {
      paste("verification_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(rv$verification_data)) {
        df <- data.frame(
          simulation = 1:length(rv$verification_data),
          count_above_threshold = rv$verification_data,
          true_mu = input$ded_mu,
          noise_sigma = input$ded_sigma,
          distribution = input$ded_dist,
          threshold = input$threshold,
          m = input$m
        )
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)