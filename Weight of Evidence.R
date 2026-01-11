library(shiny)
library(ggplot2)
library(gridExtra)

ui <- fluidPage(
  titlePanel("Four Concepts of Weight of Evidence"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Scenario: Bean Bag"),
      p("Drawing beans from a bag with replacement. What proportion are white?"),
      
      sliderInput("n_draws", 
                  "Number of beans drawn:",
                  min = 0, max = 1000, value = 100, step = 10),
      
      sliderInput("prop_white",
                  "Proportion observed to be white:",
                  min = 0, max = 1, value = 0.9, step = 0.05),
      
      hr(),
      
      h4("Prior Beliefs (for Bayesians only)"),
      sliderInput("prior_uniform",
                  "Prior belief in uniform distribution:",
                  min = 0, max = 1, value = 0.5, step = 0.05),
      
      hr(),
      
      h4("Population (for Keynes)"),
      radioButtons("population_type",
                   "Population size:",
                   choices = list("Finite (1000 beans)" = "finite",
                                  "Infinite (sampling with replacement)" = "infinite"),
                   selected = "finite"),
      
      hr(),
      
      checkboxInput("show_peirce", "Show Peirce's Concepts", value = TRUE),
      checkboxInput("show_modern", "Show Modern Concepts", value = TRUE),
      
      hr(),
      
      actionButton("reset", "Reset to Defaults"),
      
      br(), br(),
      
      wellPanel(
        style = "background-color: #E8F8F5;",
        h5("Quick Scenarios:"),
        actionButton("scenario1", "Little evidence, balanced"),
        actionButton("scenario2", "Much evidence, balanced"),
        actionButton("scenario3", "Little evidence, strong"),
        actionButton("scenario4", "Much evidence, strong")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("All Four Concepts",
                 br(),
                 
                 conditionalPanel(
                   condition = "input.show_peirce",
                   h3("Peirce's Two Concepts (Frequentist)", style = "color: #8E44AD;"),
                   fluidRow(
                     column(6,
                            wellPanel(
                              style = "background-color: #F4ECF7;",
                              h4("1. Balance of Reasons", style = "color: #6C3483;"),
                              plotOutput("peirce_balance_plot", height = "180px"),
                              p(textOutput("peirce_balance_explain")),
                              p(em("Like Good's, but frequentist—no prior needed"))
                            )
                     ),
                     column(6,
                            wellPanel(
                              style = "background-color: #F4ECF7;",
                              h4("2. Amount of Knowledge", style = "color: #6C3483;"),
                              plotOutput("peirce_amount_plot", height = "180px"),
                              p(textOutput("peirce_amount_explain")),
                              p(em("Like Keynes', but uses precision/CI width"))
                            )
                     )
                   ),
                   hr()
                 ),
                 
                 conditionalPanel(
                   condition = "input.show_modern",
                   h3("Modern Concepts (Bayesian)", style = "color: #1F618D;"),
                   fluidRow(
                     column(6,
                            wellPanel(
                              style = "background-color: #EBF5FB;",
                              h4("Good's Weight", style = "color: #154360;"),
                              plotOutput("good_plot", height = "180px"),
                              p(textOutput("good_explanation")),
                              p(em("Bayesian version of Peirce's balance"))
                            )
                     ),
                     column(6,
                            wellPanel(
                              style = "background-color: #EBF5FB;",
                              h4("Keynes' Weight", style = "color: #154360;"),
                              plotOutput("keynes_plot", height = "180px"),
                              p(textOutput("keynes_explanation")),
                              p(em("Completeness of evidence, regardless of direction"))
                            )
                     )
                   )
                 ),
                 
                 hr(),
                 
                 h4("Distribution and Confidence Interval"),
                 plotOutput("comparison_plot", height = "400px")
        ),
        
        tabPanel("Peirce's Philosophy",
                 br(),
                 h4("C.S. Peirce (1878): 'The Probability of Induction'"),
                 
                 wellPanel(
                   style = "background-color: #FEF5E7;",
                   h5("Context: Frequentist Framework"),
                   p("Peirce was explicitly frequentist—probabilities are relative frequencies in the long run."),
                   p("He rejected subjective priors, seeking", em("objective"), 
                     "methods based only on the data.")
                 ),
                 
                 hr(),
                 
                 h4("1. The 'Method of Balancing Reasons' (Net Weight)"),
                 
                 p("From 'The Fixation of Belief', Peirce describes a method where you:",
                   tags$ol(
                     tags$li("List all arguments for and against a proposition"),
                     tags$li("Express each as a probability (frequency-based)"),
                     tags$li("Convert to odds and take logarithms"),
                     tags$li(strong("Sum the log-odds"), "to get the 'proper intensity' of belief")
                   )),
                 
                 p("This gives you the", strong("net weight"), "of evidence—the balance after 
            considering all arguments."),
                 
                 p(strong("Formula:"), "W = Σ log(pᵢ/(1-pᵢ)) where pᵢ are the probabilities 
            of arguments for/against"),
                 
                 p(em("This is remarkably similar to Good's later weight of evidence (log Bayes factor), 
            but without requiring a Bayesian prior—it's purely frequentist.")),
                 
                 hr(),
                 
                 h4("2. The 'Amount of Knowledge' (Sample Size Weight)"),
                 
                 p("Peirce's famous bean example:"),
                 
                 wellPanel(
                   style = "background-color: #E8F8F5;",
                   p(em("\"Suppose we have drawn 10 beans from a bag, and 9 are white and 1 is black. 
              Compare this to drawing 10,000 beans where 9,000 are white and 1,000 are black. 
              Both give us 0.9 proportion white, but which estimate should we trust more?\"")),
                 ),
                 
                 p("Peirce's answer: The second has more", strong("weight"), 
                   "because it's based on more knowledge."),
                 
                 p("He proposes calculating this using what we'd now call a confidence interval approach—
            the larger sample gives a narrower interval, hence more weight."),
                 
                 p(strong("Modern interpretation:"), "Weight ∝ √n (standard error decreases with √n)"),
                 p("Or: Weight = 1 / (width of confidence interval)"),
                 
                 p(em("This anticipates Keynes' 'amount of evidence' but is more precise—Peirce 
            quantifies it through confidence interval width, making it a frequentist alternative 
            to Keynes' more qualitative notion.")),
                 
                 hr(),
                 
                 h4("Key Distinction in Peirce"),
                 
                 wellPanel(
                   style = "background-color: #FADBD8;",
                   p(strong("Balance of Reasons"), "(Net Weight): Answers 'What should I believe?' 
              The net evidential force after all arguments."),
                   p(strong("Amount of Knowledge"), "(Sample Size Weight): Answers 'How confident 
              should I be in this estimate?' The precision/reliability of the inference.")
                 )
        ),
        
        tabPanel("Comparison Table",
                 br(),
                 h4("The Four Concepts Compared"),
                 
                 wellPanel(
                   style = "background-color: #FEF9E7;",
                   HTML("<table style='width:100%; border-collapse: collapse;'>
              <tr style='background-color: #F4ECF7;'>
                <th style='border: 1px solid #ddd; padding: 12px;'>Concept</th>
                <th style='border: 1px solid #ddd; padding: 12px;'>Type</th>
                <th style='border: 1px solid #ddd; padding: 12px;'>What It Measures</th>
                <th style='border: 1px solid #ddd; padding: 12px;'>Formula/Approach</th>
              </tr>
              <tr>
                <td style='border: 1px solid #ddd; padding: 8px;'><strong>Peirce's Balance of Reasons</strong></td>
                <td style='border: 1px solid #ddd; padding: 8px;'>Net Weight (Frequentist)</td>
                <td style='border: 1px solid #ddd; padding: 8px;'>Strength of evidence for/against hypothesis</td>
                <td style='border: 1px solid #ddd; padding: 8px;'>Σ log-odds of arguments</td>
              </tr>
              <tr style='background-color: #F4ECF7;'>
                <td style='border: 1px solid #ddd; padding: 8px;'><strong>Peirce's Amount of Knowledge</strong></td>
                <td style='border: 1px solid #ddd; padding: 8px;'>Sample Size Weight (Frequentist)</td>
                <td style='border: 1px solid #ddd; padding: 8px;'>Precision/reliability of estimate</td>
                <td style='border: 1px solid #ddd; padding: 8px;'>1/CI width, or √n</td>
              </tr>
              <tr>
                <td style='border: 1px solid #ddd; padding: 8px;'><strong>Good's Weight</strong></td>
                <td style='border: 1px solid #ddd; padding: 8px;'>Net Weight (Bayesian)</td>
                <td style='border: 1px solid #ddd; padding: 8px;'>How much evidence shifts beliefs</td>
                <td style='border: 1px solid #ddd; padding: 8px;'>log(Bayes factor)</td>
              </tr>
              <tr style='background-color: #EBF5FB;'>
                <td style='border: 1px solid #ddd; padding: 8px;'><strong>Keynes' Weight</strong></td>
                <td style='border: 1px solid #ddd; padding: 8px;'>Gross Weight (Bayesian)</td>
                <td style='border: 1px solid #ddd; padding: 8px;'>Completeness/amount of evidence</td>
                <td style='border: 1px solid #ddd; padding: 8px;'>n/n_max (qualitative)</td>
              </tr>
            </table>")
                 ),
                 
                 hr(),
                 
                 h4("Key Philosophical Differences"),
                 
                 fluidRow(
                   column(6,
                          wellPanel(
                            h5("Net Weight (Balance/Strength)", style = "color: #8E44AD;"),
                            p("• Measures", strong("direction and magnitude"), "of evidence"),
                            p("• Can be positive (favors H) or negative (against H)"),
                            p("• Related to", em("what to believe")),
                            p("• Peirce: Frequentist (no prior)"),
                            p("• Good: Bayesian (needs prior)")
                          )
                   ),
                   column(6,
                          wellPanel(
                            h5("Gross Weight (Amount/Precision)", style = "color: #E74C3C;"),
                            p("• Measures", strong("quantity"), "of evidence"),
                            p("• Always non-negative"),
                            p("• Related to", em("confidence in estimate")),
                            p("• Peirce: Via confidence interval width"),
                            p("• Keynes: Via completeness of information")
                          )
                   )
                 ),
                 
                 hr(),
                 
                 h4("Historical Connections"),
                 
                 p("1. Peirce (1878) articulates BOTH distinctions in his frequentist framework"),
                 p("2. Good (1950) formalizes the 'net weight' idea in Bayesian terms"),
                 p("3. Keynes (1921) emphasizes the 'gross weight' idea philosophically"),
                 p("4. Modern confusion often stems from conflating net vs. gross weight")
        ),
        
        tabPanel("Interactive Examples",
                 br(),
                 h4("Explore the Differences:"),
                 
                 wellPanel(
                   h5("Scenario 1: Balanced Evidence, Small Sample"),
                   p("10 draws, 50% white"),
                   p(strong("Peirce's Balance:"), "Near zero (no net evidence either way)"),
                   p(strong("Peirce's Amount:"), "Low (small sample, wide CI)"),
                   p(strong("Good's Weight:"), "Low (little change in beliefs)"),
                   p(strong("Keynes' Weight:"), "Low (little evidence collected)")
                 ),
                 
                 wellPanel(
                   h5("Scenario 2: Balanced Evidence, Large Sample"),
                   p("1000 draws, 50% white"),
                   p(strong("Peirce's Balance:"), "Near zero (still no net evidence)"),
                   p(strong("Peirce's Amount:"), "HIGH (large sample, narrow CI)"),
                   p(strong("Good's Weight:"), "Low (still little change in beliefs)"),
                   p(strong("Keynes' Weight:"), "HIGH (much evidence collected)"),
                   p(em("Key: Net weight distinguishes this from Scenario 1, but gross weight doesn't!"))
                 ),
                 
                 wellPanel(
                   h5("Scenario 3: Strong Evidence, Small Sample"),
                   p("10 draws, 100% white"),
                   p(strong("Peirce's Balance:"), "High (strong net evidence for H)"),
                   p(strong("Peirce's Amount:"), "Low (small sample, wide CI)"),
                   p(strong("Good's Weight:"), "High (large belief change)"),
                   p(strong("Keynes' Weight:"), "Low (little evidence collected)"),
                   p(em("Net weight is high, but we shouldn't be too confident (low gross weight)"))
                 ),
                 
                 wellPanel(
                   h5("Scenario 4: Strong Evidence, Large Sample"),
                   p("1000 draws, 90% white"),
                   p(strong("Peirce's Balance:"), "High (strong net evidence)"),
                   p(strong("Peirce's Amount:"), "HIGH (large sample, narrow CI)"),
                   p(strong("Good's Weight:"), "High (large belief change)"),
                   p(strong("Keynes' Weight:"), "HIGH (much evidence collected)"),
                   p(em("All weights high—this is our most reliable conclusion!"))
                 ),
                 
                 hr(),
                 
                 h4("The Central Question"),
                 
                 wellPanel(
                   style = "background-color: #FADBD8;",
                   p(strong("When someone says 'this evidence has weight', what do they mean?")),
                   tags$ul(
                     tags$li("Does it have weight in", em("favor"), "of something? (Net/Balance)"),
                     tags$li("Or does it have weight because there's", em("a lot"), "of it? (Gross/Amount)")
                   ),
                   p("Peirce was the first to clearly distinguish these two meanings in 1878."),
                   p("Good formalized the first in Bayesian terms (1950)."),
                   p("Keynes emphasized the second philosophically (1921)."),
                   p("We still confuse them today!")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSliderInput(session, "n_draws", value = 100)
    updateSliderInput(session, "prop_white", value = 0.9)
    updateSliderInput(session, "prior_uniform", value = 0.5)
  })
  
  observeEvent(input$scenario1, {
    updateSliderInput(session, "n_draws", value = 10)
    updateSliderInput(session, "prop_white", value = 0.5)
  })
  
  observeEvent(input$scenario2, {
    updateSliderInput(session, "n_draws", value = 1000)
    updateSliderInput(session, "prop_white", value = 0.5)
  })
  
  observeEvent(input$scenario3, {
    updateSliderInput(session, "n_draws", value = 10)
    updateSliderInput(session, "prop_white", value = 1.0)
  })
  
  observeEvent(input$scenario4, {
    updateSliderInput(session, "n_draws", value = 1000)
    updateSliderInput(session, "prop_white", value = 0.9)
  })
  
  # Peirce's Balance of Reasons (Net Weight) - Frequentist
  peirce_balance <- reactive({
    n <- input$n_draws
    if (n == 0) return(0)
    
    p_white <- input$prop_white
    
    # Log-odds of the observed proportion vs. 0.5 (uniform hypothesis)
    # This is the frequentist "balance" without a prior
    if (p_white == 0 || p_white == 1) {
      # Extreme cases
      weight <- sign(p_white - 0.5) * 5
    } else {
      # Log odds ratio: log(p_white / (1-p_white)) - log(0.5/0.5)
      weight <- log(p_white / (1 - p_white))
    }
    
    # Scale by sample size somewhat (larger samples give more confident balance)
    weight * sqrt(n) / 10
  })
  
  # Peirce's Amount of Knowledge (Sample Size Weight) - based on CI width
  peirce_amount <- reactive({
    n <- input$n_draws
    if (n == 0) return(0)
    
    p_white <- input$prop_white
    
    # Standard error for proportion
    se <- sqrt(p_white * (1 - p_white) / n)
    
    # CI width (95% CI)
    ci_width <- 2 * 1.96 * se
    
    # Weight is inverse of CI width
    weight <- 1 / (ci_width + 0.01)  # Add small constant to avoid division by zero
    
    # Normalize to 0-1 scale
    min(weight / 5, 1)
  })
  
  # Keynes' weight (amount of evidence)
  keynes_weight <- reactive({
    n <- input$n_draws
    if (input$population_type == "infinite") {
      return(NA)  # Cannot achieve completeness with infinite population
    }
    n / 1000  # Normalized by population size
  })
  
  keynes_completeness_possible <- reactive({
    input$population_type == "finite"
  })
  
  # Good's weight (strength of evidence) - Bayesian
  good_weight <- reactive({
    n <- input$n_draws
    if (n == 0) return(0)
    
    white <- round(n * input$prop_white)
    black <- n - white
    
    # Prior odds (uniform vs some specific alternative, e.g., 70% white)
    prior_uniform <- input$prior_uniform
    prior_alt <- 1 - prior_uniform
    if (prior_uniform == 0 || prior_alt == 0) return(0)
    
    prior_odds <- prior_uniform / prior_alt
    
    # Likelihood ratio
    p_uniform <- 0.5
    p_alt <- 0.7
    
    likelihood_uniform <- dbinom(white, n, p_uniform)
    likelihood_alt <- dbinom(white, n, p_alt)
    
    if (likelihood_uniform == 0 || likelihood_alt == 0) return(0)
    
    likelihood_ratio <- likelihood_alt / likelihood_uniform
    posterior_odds <- prior_odds * likelihood_ratio
    
    # Weight of evidence (log odds change)
    weight <- abs(log(posterior_odds / prior_odds))
    
    # Normalize for display
    min(weight / 5, 1)
  })
  
  # Output text for each weight
  output$peirce_balance <- renderText({
    sprintf("%.2f", peirce_balance())
  })
  
  output$peirce_amount <- renderText({
    sprintf("%.2f", peirce_amount())
  })
  
  output$keynes_weight <- renderText({
    if (!keynes_completeness_possible()) {
      "N/A (infinite population)"
    } else {
      sprintf("%.2f", keynes_weight())
    }
  })
  
  output$good_weight <- renderText({
    sprintf("%.2f", good_weight())
  })
  
  # Visual representations
  
  # Peirce's Balance - as a scale
  output$peirce_balance_plot <- renderPlot({
    w <- peirce_balance()
    
    # Create a scale from -1 to 1
    df <- data.frame(x = c(-1, 1), y = c(0, 0))
    
    ggplot(df, aes(x = x, y = y)) +
      geom_segment(aes(x = -1, xend = 1, y = 0, yend = 0), 
                   linewidth = 3, color = "gray60") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 1) +
      geom_point(x = max(-1, min(1, w)), y = 0, size = 12, color = "#8E44AD") +
      annotate("text", x = -0.75, y = -0.15, label = "Against\nH", 
               size = 3, color = "gray30") +
      annotate("text", x = 0.75, y = -0.15, label = "For\nH", 
               size = 3, color = "gray30") +
      annotate("text", x = 0, y = 0.15, label = "Neutral", 
               size = 3, color = "gray30") +
      scale_x_continuous(limits = c(-1.2, 1.2), breaks = c(-1, 0, 1),
                         labels = c("Strong Against", "Neutral", "Strong For")) +
      theme_void() +
      theme(axis.text.x = element_text(size = 9, color = "gray30"),
            plot.margin = margin(10, 10, 10, 10))
  }, height = 180)
  
  # Peirce's Amount of Knowledge - CI width visualization
  output$peirce_amount_plot <- renderPlot({
    n <- input$n_draws
    if (n == 0) {
      plot.new()
      text(0.5, 0.5, "No observations yet", cex = 1.2, col = "gray50")
      return()
    }
    
    p_obs <- input$prop_white
    se <- sqrt(p_obs * (1 - p_obs) / n)
    ci_lower <- max(0, p_obs - 1.96 * se)
    ci_upper <- min(1, p_obs + 1.96 * se)
    ci_width <- ci_upper - ci_lower
    
    # Visual representation: CI on a line, plus sample size indicator
    df <- data.frame(x = c(0, 1), y = c(0, 0))
    
    ggplot(df, aes(x = x, y = y)) +
      # Full range line
      geom_segment(aes(x = 0, xend = 1, y = 0.5, yend = 0.5), 
                   linewidth = 2, color = "gray80") +
      # Confidence interval
      geom_segment(aes(x = ci_lower, xend = ci_upper, y = 0.5, yend = 0.5),
                   linewidth = 6, color = "#8E44AD", alpha = 0.7) +
      # Point estimate
      geom_point(x = p_obs, y = 0.5, size = 8, color = "#6C3483") +
      # Sample size indicator (height of bar)
      annotate("rect", xmin = -0.05, xmax = 0, ymin = 0, ymax = min(1, n/1000),
               fill = "#A569BD", alpha = 0.6) +
      annotate("text", x = -0.025, y = min(1, n/1000) + 0.05, 
               label = sprintf("n=%d", n), size = 3, color = "#6C3483") +
      annotate("text", x = 0.5, y = 0.7, 
               label = sprintf("CI width: %.3f", ci_width), 
               size = 3.5, color = "#6C3483", fontface = "bold") +
      scale_x_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.5, 1)) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_void() +
      theme(axis.text.x = element_text(size = 9, color = "gray30"),
            plot.margin = margin(10, 10, 10, 10))
  }, height = 180)
  
  # Good's Weight - as a scale like Peirce's Balance
  output$good_plot <- renderPlot({
    w <- good_weight()
    # Good's weight is always positive, but we can show it on a scale
    # from 0 (no change) to 1 (maximum change)
    
    df <- data.frame(x = c(0, 1), y = c(0, 0))
    
    ggplot(df, aes(x = x, y = y)) +
      geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), 
                   linewidth = 3, color = "gray60") +
      geom_point(x = w, y = 0, size = 12, color = "#3498DB") +
      annotate("text", x = 0.2, y = -0.15, label = "Weak\nShift", 
               size = 3, color = "gray30") +
      annotate("text", x = 0.8, y = -0.15, label = "Strong\nShift", 
               size = 3, color = "gray30") +
      scale_x_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.5, 1),
                         labels = c("No Change", "Moderate", "Strong")) +
      theme_void() +
      theme(axis.text.x = element_text(size = 9, color = "gray30"),
            plot.margin = margin(10, 10, 10, 10))
  }, height = 180)
  
  # Keynes' Weight - as a loading/progress bar
  output$keynes_plot <- renderPlot({
    if (!keynes_completeness_possible()) {
      plot.new()
      text(0.5, 0.5, "Completeness impossible:\nInfinite population", 
           cex = 1.1, col = "#E74C3C", font = 2)
      text(0.5, 0.3, "(Sampling with replacement means\ndenominator is infinite)", 
           cex = 0.9, col = "gray40")
      return()
    }
    
    w <- keynes_weight()
    n <- input$n_draws
    
    df <- data.frame(x = c(0, 1), y = c(0, 0))
    
    ggplot(df, aes(x = x, y = y)) +
      # Background bar
      annotate("rect", xmin = 0, xmax = 1, ymin = 0.3, ymax = 0.7,
               fill = "gray90", color = "gray60", linewidth = 1) +
      # Progress bar
      annotate("rect", xmin = 0, xmax = w, ymin = 0.3, ymax = 0.7,
               fill = "#E74C3C", alpha = 0.7) +
      # Text
      annotate("text", x = 0.5, y = 0.5, 
               label = sprintf("%d / 1000 beans\n(%.1f%% complete)", n, w * 100),
               size = 4, color = "white", fontface = "bold") +
      annotate("text", x = 0, y = 0.85, label = "Empty", 
               size = 3, color = "gray30", hjust = 0) +
      annotate("text", x = 1, y = 0.85, label = "Complete", 
               size = 3, color = "gray30", hjust = 1) +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_void() +
      theme(plot.margin = margin(10, 10, 10, 10))
  }, height = 180)
  
  # Explanations
  output$peirce_balance_explain <- renderText({
    w <- peirce_balance()
    n <- input$n_draws
    if (n == 0) {
      "No observations yet"
    } else if (abs(w) < 0.2) {
      "Weak evidence—observations roughly balanced"
    } else if (abs(w) < 0.5) {
      "Moderate imbalance in observations"
    } else {
      "Strong imbalance—clear preponderance"
    }
  })
  
  output$peirce_amount_explain <- renderText({
    w <- peirce_amount()
    n <- input$n_draws
    if (n == 0) {
      "No observations yet"
    } else if (n < 50) {
      sprintf("Small sample (wide CI: ±%.2f)", 1.96 * sqrt(input$prop_white * (1-input$prop_white) / n))
    } else if (n < 300) {
      sprintf("Moderate sample (CI: ±%.2f)", 1.96 * sqrt(input$prop_white * (1-input$prop_white) / n))
    } else {
      sprintf("Large sample (narrow CI: ±%.2f)", 1.96 * sqrt(input$prop_white * (1-input$prop_white) / n))
    }
  })
  
  output$keynes_explanation <- renderText({
    if (!keynes_completeness_possible()) {
      "Cannot measure completeness with infinite population"
    } else {
      n <- input$n_draws
      if (n == 0) {
        "No evidence yet"
      } else if (n < 200) {
        "Limited amount of evidence"
      } else if (n < 600) {
        "Moderate amount of evidence"
      } else {
        "Substantial amount of evidence"
      }
    }
  })
  
  output$good_explanation <- renderText({
    w <- good_weight()
    if (input$n_draws == 0) {
      "No evidence yet"
    } else if (w < 0.2) {
      "Weak evidence—small belief shift"
    } else if (w < 0.5) {
      "Moderate evidence—noticeable shift"
    } else {
      "Strong evidence—substantial shift"
    }
  })
  
  output$comparison_plot <- renderPlot({
    n <- input$n_draws
    p_obs <- input$prop_white
    
    if (n == 0) {
      plot.new()
      text(0.5, 0.5, "No observations yet - adjust sliders to see distribution", 
           cex = 1.5, col = "gray50")
      return()
    }
    
    # For Bayesian posterior
    x <- seq(0, 1, length.out = 200)
    white <- round(n * p_obs)
    y_bayes <- dbeta(x, white + 1, n - white + 1)
    
    df2 <- data.frame(p = x, density = y_bayes)
    
    # Add frequentist CI
    se <- sqrt(p_obs * (1 - p_obs) / n)
    ci_lower <- max(0, p_obs - 1.96 * se)
    ci_upper <- min(1, p_obs + 1.96 * se)
    
    ggplot(df2, aes(x = p, y = density)) +
      geom_area(fill = "#3498DB", alpha = 0.3) +
      geom_line(color = "#3498DB", linewidth = 1) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = 1) +
      geom_vline(xintercept = p_obs, linetype = "solid", color = "black", linewidth = 1) +
      geom_segment(aes(x = ci_lower, xend = ci_upper, y = max(y_bayes) * 0.05, 
                       yend = max(y_bayes) * 0.05),
                   color = "#8E44AD", linewidth = 4, alpha = 0.7) +
      geom_point(aes(x = ci_lower, y = max(y_bayes) * 0.05), 
                 color = "#8E44AD", size = 3) +
      geom_point(aes(x = ci_upper, y = max(y_bayes) * 0.05), 
                 color = "#8E44AD", size = 3) +
      annotate("text", x = ci_lower, y = max(y_bayes) * 0.12, 
               label = sprintf("%.3f", ci_lower), size = 3.5, color = "#6C3483") +
      annotate("text", x = ci_upper, y = max(y_bayes) * 0.12, 
               label = sprintf("%.3f", ci_upper), size = 3.5, color = "#6C3483") +
      annotate("text", x = 0.5, y = max(y_bayes) * 0.95, label = "Uniform (0.5)", 
               color = "red", size = 4, hjust = -0.1) +
      annotate("text", x = p_obs, y = max(y_bayes) * 0.85, label = sprintf("Observed\n(%.2f)", p_obs), 
               color = "black", size = 4, hjust = -0.1) +
      annotate("text", x = mean(c(ci_lower, ci_upper)), y = max(y_bayes) * 0.18,
               label = sprintf("95%% CI (Peirce's Amount)\nwidth = %.3f", ci_upper - ci_lower),
               color = "#6C3483", size = 4, fontface = "bold") +
      labs(title = "Distribution of Proportion White",
           subtitle = sprintf("Blue curve: Bayesian posterior (Good's Weight) | Purple bar: Frequentist 95%% CI (Peirce's Amount) | n = %d", n),
           x = "Proportion White",
           y = "Density") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray30"))
  }, height = 400)
}

shinyApp(ui = ui, server = server)