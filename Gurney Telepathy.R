# app.R
# Enhanced Shiny app version of the Peirce-Gurney telepathy analysis

library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(scales)

# --- Helper: compute core numbers given parameters
compute_analysis <- function(
    census_size = 5705,
    hallucinations_12yr = 21,
    cases_analyzed = 31,
    time_window_hours = 12,
    annual_death_rate = 0.025,
    prob_remember_noncoincidental = 0.3,
    prob_remember_coincidental = 0.95,
    death_cat_h = c(0.4, 0.35, 0.2, 0.05),
    death_cat_annual = c(0.15, 0.08, 0.01, 0.02)
){
  daily_death_rate <- annual_death_rate / 365
  death_prob_12hr <- daily_death_rate * (time_window_hours/24)
  
  memory_correction_factor <- prob_remember_coincidental / prob_remember_noncoincidental
  
  death_categories <- tibble(
    type = c("Elderly/Sick", "Middle-aged ill", "Young healthy", "Children"),
    h_prob = death_cat_h,
    annual_death_rate = death_cat_annual
  ) %>%
    mutate(
      daily_death_rate = annual_death_rate / 365,
      prob_death_12hr = daily_death_rate * 0.5,
      weighted_contribution = h_prob * prob_death_12hr
    )
  
  peirce_conditional_prob <- sum(death_categories$weighted_contribution)
  peirce_base_prob <- peirce_conditional_prob * memory_correction_factor
  
  list(
    census_size = census_size,
    hallucinations_12yr = hallucinations_12yr,
    cases_analyzed = cases_analyzed,
    time_window_hours = time_window_hours,
    daily_death_rate = daily_death_rate,
    death_prob_12hr = death_prob_12hr,
    memory_correction_factor = memory_correction_factor,
    death_categories = death_categories,
    peirce_conditional_prob = peirce_conditional_prob,
    peirce_base_prob = peirce_base_prob,
    gurney_prob = death_prob_12hr
  )
}

# Calculate hallucinations needed
calc_hallucinations_needed <- function(death_prob) {
  if (death_prob >= 1) return(0)
  if (death_prob <= 0) return(Inf)
  target_prob <- 0.5
  n <- log(1 - target_prob) / log(1 - death_prob)
  return(ceiling(n))
}

# --- UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .info-box {
        background-color: #e3f2fd;
        border-left: 4px solid #2196F3;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .warning-box {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .danger-box {
        background-color: #f8d7da;
        border-left: 4px solid #dc3545;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .metric-box {
        background-color: #f8f9fa;
        border: 2px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin: 10px 0;
        text-align: center;
      }
      .metric-value {
        font-size: 32px;
        font-weight: bold;
        color: #2196F3;
      }
      .metric-label {
        font-size: 14px;
        color: #6c757d;
        margin-top: 5px;
      }
    "))
  ),
  
  titlePanel("Peirce vs. Gurney: The Telepathy Debate (1887)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Parameters"),
      numericInput("annual_death_rate", "General annual death rate", 
                   value = 0.025, step = 0.001, min = 0.001, max = 0.5),
      sliderInput("time_window_hours", "Time window (hours)", 
                  min = 1, max = 48, value = 12, step = 1),
      numericInput("hallucinations_12yr", "Hallucinations in 12 years", 
                   value = 21, min = 1),
      numericInput("census_size", "Census size", 
                   value = 5705, min = 1),
      
      hr(),
      h4("Memory Bias"),
      sliderInput("prob_remember_non", "Remember non-coincidental", 
                  min = 0.01, max = 1, value = 0.3, step = 0.01),
      sliderInput("prob_remember_co", "Remember coincidental", 
                  min = 0.01, max = 1, value = 0.95, step = 0.01),
      
      hr(),
      h4("Death Category Mix (h)"),
      numericInput("h_elderly", "Elderly/Sick", value = 0.40, step = 0.01, min = 0, max = 1),
      numericInput("h_middle", "Middle-aged ill", value = 0.35, step = 0.01, min = 0, max = 1),
      numericInput("h_young", "Young healthy", value = 0.20, step = 0.01, min = 0, max = 1),
      numericInput("h_children", "Children", value = 0.05, step = 0.01, min = 0, max = 1),
      
      hr(),
      h4("Simulation"),
      numericInput("n_sims", "Number of simulations", 
                   value = 10000, min = 100, step = 100),
      numericInput("seed", "Random seed (0 = random)", value = 1887),
      actionButton("run", "Run Simulation", class = "btn-primary btn-lg btn-block"),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        # OVERVIEW TAB
        tabPanel("Key Insights",
                 br(),
                 h3("The Central Question"),
                 p("In 1886, Edmund Gurney published a census showing 31 hallucinations that coincided with deaths. 
                   Was this evidence of telepathy, or could it be explained by chance alone?"),
                 
                 fluidRow(
                   column(6,
                          div(class = "metric-box",
                              div(class = "metric-value", textOutput("gurney_prob_pct")),
                              div(class = "metric-label", "Gurney's probability per case"),
                              div(style = "margin-top: 10px; font-size: 12px;", 
                                  textOutput("gurney_expected"))
                          )
                   ),
                   column(6,
                          div(class = "metric-box",
                              div(class = "metric-value", textOutput("peirce_prob_pct")),
                              div(class = "metric-label", "Peirce's corrected probability"),
                              div(style = "margin-top: 10px; font-size: 12px;", 
                                  textOutput("peirce_expected"))
                          )
                   )
                 ),
                 
                 div(class = "warning-box",
                     h4("Why Peirce's Probability is Higher"),
                     uiOutput("key_differences")
                 ),
                 
                 div(class = "info-box",
                     h4("What Makes 31 Coincidences Surprising?"),
                     uiOutput("surprise_analysis")
                 ),
                 
                 uiOutput("probability_warning")
        ),
        
        # SIMULATION TAB
        tabPanel("Simulation",
                 br(),
                 uiOutput("simulation_warning"),
                 
                 div(class = "info-box",
                     h4("Understanding the Simulation"),
                     p("This runs ", textOutput("n_sims_display", inline = TRUE), 
                       " trials of 31 hallucinations each, showing how many would coincide with deaths 
                       by pure chance under both models. The red line shows the observed value (31)."),
                     p(strong("Key insight:"), " The width and position of each distribution shows how 
                       likely different outcomes are under each set of assumptions.")
                 ),
                 
                 plotOutput("sim_plot", height = "450px"),
                 
                 fluidRow(
                   column(6,
                          div(class = "metric-box", style = "background-color: #e3f2fd;",
                              h4("Gurney's Model Results"),
                              tableOutput("gurney_sim_stats")
                          )
                   ),
                   column(6,
                          div(class = "metric-box", style = "background-color: #f3e5f5;",
                              h4("Peirce's Model Results"),
                              tableOutput("peirce_sim_stats")
                          )
                   )
                 ),
                 
                 div(class = "warning-box",
                     h4("Interpretation"),
                     uiOutput("simulation_interpretation")
                 )
        ),
        
        # CALCULATOR TAB
        tabPanel("Hallucinations Calculator",
                 br(),
                 div(class = "info-box",
                     h4("Understanding 'Hallucinations Needed'"),
                     p("This is the number of independent hallucinations required for there to be a 
                       50% chance that at least one would coincide with a death, given specific circumstances."),
                     p("The formula is: ", tags$code("n = log(0.5) / log(1 - p)")),
                     p("where ", tags$code("p"), " = probability of death within the time window")
                 ),
                 
                 wellPanel(
                   h4("Interactive Calculator"),
                   sliderInput("calc_death_prob", 
                               "Death probability for this case:", 
                               min = 0.00001, max = 1, value = 0.001, step = 0.00001),
                   h3(textOutput("calc_result"), style = "color: #2196F3;"),
                   p("hallucinations needed for 50% chance of ≥1 coincidence")
                 ),
                 
                 h4("Example Cases from Peirce's Analysis"),
                 p("Click a row to load its probability into the calculator above."),
                 DTOutput("example_cases_table"),
                 
                 div(class = "warning-box",
                     h4("What This Reveals"),
                     p("Cases with very low death probabilities (like Case 238 at 0.0035%) would require 
                       an enormous number of hallucinations (20,000) to make coincidence likely. 
                       Conversely, when someone was already known to be dying (Case 231), coincidence 
                       becomes highly probable with just a single hallucination.")
                 )
        ),
        
        # DEATH CATEGORIES TAB
        tabPanel("Death Categories",
                 br(),
                 h3("Peirce's Key Insight: Death Rates Vary by Category"),
                 p("Peirce argued that the people who appeared in hallucinations weren't a random 
                   sample - they were disproportionately elderly or sick, and thus more likely to die."),
                 
                 DTOutput("death_table"),
                 
                 plotOutput("death_category_plot", height = "350px"),
                 
                 div(class = "info-box",
                     p(strong("The weighted average:"), " By combining the composition of hallucinations (h) 
                       with category-specific death rates, Peirce calculated a higher baseline probability 
                       than Gurney's population average.")
                 )
        ),
        
        # CASES TAB
        tabPanel("Case Analysis",
                 br(),
                 h3("Peirce's Case-by-Case Critique"),
                 p("Peirce examined all 31 cases individually, identifying specific factors that would 
                   increase the probability of coincidence beyond the baseline."),
                 
                 DTOutput("peirce_cases"),
                 
                 div(class = "warning-box",
                     h4("Common Factors that Increase Coincidence Probability"),
                     tags$ul(
                       tags$li(strong("Date uncertainty:"), " If timing is vague, the 'window' expands"),
                       tags$li(strong("Illness/anxiety:"), " Both increase hallucination likelihood AND death probability"),
                       tags$li(strong("Recognition issues:"), " Was the person actually identified correctly?"),
                       tags$li(strong("Alternative explanations:"), " Some cases had mundane explanations (pranks, dreams)")
                     )
                 )
        )
      ),
      width = 9
    )
  )
)

# --- Server
server <- function(input, output, session) {
  
  # Reactive analysis
  analysis <- reactive({
    h_vec <- c(input$h_elderly, input$h_middle, input$h_young, input$h_children)
    if(sum(h_vec) <= 0) h_vec <- c(0.4, 0.35, 0.2, 0.05)
    h_vec <- h_vec / sum(h_vec)
    
    compute_analysis(
      census_size = input$census_size,
      hallucinations_12yr = input$hallucinations_12yr,
      cases_analyzed = 31,
      time_window_hours = input$time_window_hours,
      annual_death_rate = input$annual_death_rate,
      prob_remember_noncoincidental = input$prob_remember_non,
      prob_remember_coincidental = input$prob_remember_co,
      death_cat_h = h_vec,
      death_cat_annual = c(0.15, 0.08, 0.01, 0.02)
    )
  })
  
  # Check if probabilities are too extreme
  output$probability_warning <- renderUI({
    a <- analysis()
    expected_gurney <- 31 * a$gurney_prob
    expected_peirce <- 31 * a$peirce_base_prob
    
    if (expected_gurney < 0.01 && expected_peirce < 0.1) {
      div(class = "danger-box",
          h4("⚠️ Warning: Probabilities Too Low for Meaningful Simulation"),
          p(sprintf("Expected coincidences under Gurney: %.4f", expected_gurney)),
          p(sprintf("Expected coincidences under Peirce: %.4f", expected_peirce)),
          p(strong("Suggestion:"), " To see meaningful distributions in the simulation, try:"),
          tags$ul(
            tags$li("Increasing the time window to 24-48 hours"),
            tags$li("Increasing the annual death rate (historical rates were often 3-5%)"),
            tags$li("Increasing memory correction factor (make Remember coincidental closer to 100%)")
          )
      )
    } else if (expected_gurney < 0.1) {
      div(class = "warning-box",
          h4("Note: Low Probability Under Gurney's Model"),
          p(sprintf("Expected coincidences: %.4f", expected_gurney)),
          p("This reflects the original debate - Gurney's assumptions made 31 coincidences extremely improbable.")
      )
    }
  })
  
  # --- OVERVIEW TAB OUTPUTS ---
  output$gurney_prob_pct <- renderText({
    sprintf("%.4f%%", analysis()$gurney_prob * 100)
  })
  
  output$peirce_prob_pct <- renderText({
    sprintf("%.4f%%", analysis()$peirce_base_prob * 100)
  })
  
  output$gurney_expected <- renderText({
    sprintf("Expected: %.2f coincidences", 31 * analysis()$gurney_prob)
  })
  
  output$peirce_expected <- renderText({
    sprintf("Expected: %.2f coincidences", 31 * analysis()$peirce_base_prob)
  })
  
  output$key_differences <- renderUI({
    a <- analysis()
    ratio <- a$peirce_base_prob / a$gurney_prob
    
    tagList(
      p(sprintf("Peirce's probability is %.1f× higher than Gurney's because:", ratio)),
      tags$ul(
        tags$li(sprintf("People who died were more likely elderly/sick (%.0f%% of hallucinations)", 
                        a$death_categories$h_prob[1] * 100)),
        tags$li(sprintf("These groups have %.1f× higher death rates than the general population", 
                        a$death_categories$annual_death_rate[1] / input$annual_death_rate)),
        tags$li(sprintf("Coincidental hallucinations are %.1f× more memorable", 
                        a$memory_correction_factor))
      )
    )
  })
  
  output$surprise_analysis <- renderUI({
    a <- analysis()
    gurney_prob_all_31 <- a$gurney_prob^31
    peirce_expected <- 31 * a$peirce_base_prob
    
    tagList(
      p(strong("Under Gurney's assumptions:")),
      p(sprintf("Probability all 31 coincide: %s", 
                format(gurney_prob_all_31, scientific = TRUE, digits = 2)),
        style = "font-family: monospace; margin-left: 20px;"),
      p("This would be vanishingly unlikely by pure chance."),
      br(),
      p(strong("Under Peirce's corrections:")),
      if (peirce_expected < 1) {
        p(sprintf("Expected coincidences: %.2f. Still low, but %.0f times more likely than Gurney's model.", 
                  peirce_expected, a$peirce_base_prob / a$gurney_prob))
      } else {
        p(sprintf("Expected coincidences: %.2f. Having many coincidences becomes much more plausible.", 
                  peirce_expected))
      }
    )
  })
  
  # --- SIMULATION TAB OUTPUTS ---
  output$simulation_warning <- renderUI({
    req(sims())
    g_max <- max(sims()$gurney)
    p_max <- max(sims()$peirce)
    
    if (g_max == 0 && p_max == 0) {
      div(class = "danger-box",
          h4("⚠️ No Coincidences in Simulation"),
          p("With current parameters, the probabilities are so low that no coincidences occurred 
            in any of the ", format(input$n_sims, big.mark = ","), " simulations."),
          p(strong("This is historically accurate!"), " Under Gurney's assumptions, 31 coincidences 
            would be astronomically unlikely. However, to see the distributions, adjust parameters as suggested 
            in the Key Insights tab.")
      )
    } else if (g_max == 0) {
      div(class = "warning-box",
          h4("Note: No Coincidences Under Gurney's Model"),
          p("Gurney's model produced zero coincidences in all simulations, reflecting how improbable 
            31 coincidences would be under his assumptions.")
      )
    }
  })
  
  sims <- eventReactive(input$run, {
    a <- analysis()
    if(input$seed != 0) set.seed(input$seed)
    n_sims <- input$n_sims
    
    gurney_sims <- rbinom(n_sims, 31, a$gurney_prob)
    peirce_sims <- rbinom(n_sims, 31, a$peirce_base_prob)
    
    list(
      gurney = gurney_sims,
      peirce = peirce_sims,
      data = tibble(
        Model = rep(c("Gurney", "Peirce"), each = n_sims),
        Coincidences = c(gurney_sims, peirce_sims)
      )
    )
  })
  
  output$n_sims_display <- renderText({
    format(input$n_sims, big.mark = ",")
  })
  
  output$sim_plot <- renderPlot({
    req(sims())
    
    # Determine appropriate x-axis range
    max_val <- max(max(sims()$gurney), max(sims()$peirce), 31)
    
    ggplot(sims()$data, aes(x = Coincidences, fill = Model)) +
      geom_histogram(alpha = 0.6, position = "identity", binwidth = 1) +
      geom_vline(xintercept = 31, linetype = "dashed", color = "red", linewidth = 1) +
      annotate("text", x = 31, y = Inf, label = "Observed: 31", 
               vjust = 1.5, hjust = -0.1, color = "red", size = 5, fontface = "bold") +
      scale_fill_manual(values = c("Gurney" = "#2196F3", "Peirce" = "#9C27B0")) +
      scale_x_continuous(limits = c(-0.5, max(max_val + 1, 5))) +
      labs(
        title = "Distribution of Coincidences: Gurney vs Peirce Models",
        subtitle = paste(format(input$n_sims, big.mark = ","), 
                         "simulations of 31 hallucinations each"),
        x = "Number of Death Coincidences",
        y = "Frequency"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "gray40")
      )
  })
  
  output$gurney_sim_stats <- renderTable({
    req(sims())
    g <- sims()$gurney
    tibble(
      Metric = c("Mean coincidences", "Standard deviation", "Maximum observed", "All 31 coincidences", "≥20 coincidences"),
      Value = c(
        sprintf("%.3f", mean(g)),
        sprintf("%.3f", sd(g)),
        sprintf("%d", max(g)),
        sprintf("%d (%.4f%%)", sum(g == 31), 100 * mean(g == 31)),
        sprintf("%d (%.4f%%)", sum(g >= 20), 100 * mean(g >= 20))
      )
    )
  }, colnames = FALSE, width = "100%")
  
  output$peirce_sim_stats <- renderTable({
    req(sims())
    p <- sims()$peirce
    tibble(
      Metric = c("Mean coincidences", "Standard deviation", "Maximum observed", "All 31 coincidences", "≥20 coincidences"),
      Value = c(
        sprintf("%.3f", mean(p)),
        sprintf("%.3f", sd(p)),
        sprintf("%d", max(p)),
        sprintf("%d (%.4f%%)", sum(p == 31), 100 * mean(p == 31)),
        sprintf("%d (%.4f%%)", sum(p >= 20), 100 * mean(p >= 20))
      )
    )
  }, colnames = FALSE, width = "100%")
  
  output$simulation_interpretation <- renderUI({
    req(sims())
    g <- sims()$gurney
    p <- sims()$peirce
    
    g_mean <- mean(g)
    p_mean <- mean(p)
    g_max <- max(g)
    p_max <- max(p)
    
    gurney_ge_20 <- mean(g >= 20)
    peirce_ge_20 <- mean(p >= 20)
    
    if (g_max == 0 && p_max == 0) {
      tagList(
        p(strong("Both models produced zero coincidences in all simulations.")),
        p("This demonstrates that with these parameters, even a single coincidence would be noteworthy, 
          let alone 31. This reflects the core of Gurney's original argument for telepathy.")
      )
    } else if (g_max == 0) {
      tagList(
        p(sprintf("Gurney's model never produced any coincidences (mean: %.3f).", g_mean)),
        p(sprintf("Peirce's model averaged %.2f coincidences per simulation, with a maximum of %d.", 
                  p_mean, p_max)),
        if (peirce_ge_20 > 0) {
          p(sprintf("Under Peirce's corrections, 20+ coincidences occurred in %.2f%% of simulations.", 
                    100 * peirce_ge_20))
        } else {
          p("Even under Peirce's corrections, 20+ coincidences remain very rare, showing that 
            31 is still surprising - just not impossibly so.")
        }
      )
    } else {
      tagList(
        p(sprintf("Gurney's model averaged %.2f coincidences (max: %d).", g_mean, g_max)),
        p(sprintf("Peirce's model averaged %.2f coincidences (max: %d).", p_mean, p_max)),
        if (peirce_ge_20 > 0 || gurney_ge_20 > 0) {
          tagList(
            p(sprintf("Under Gurney's model, 20+ coincidences occurred in %.2f%% of simulations.", 
                      100 * gurney_ge_20)),
            p(sprintf("Under Peirce's corrections, this rose to %.2f%%.", 100 * peirce_ge_20))
          )
        } else {
          p("Even with corrections, 20+ coincidences remain extremely rare in both models.")
        },
        p(strong("Conclusion:"), " The key insight isn't whether 31 is 'likely' - it isn't - but whether 
          it's 'impossibly unlikely' (suggesting telepathy) or merely 'very unlikely' (suggesting 
          we should look for methodological issues).")
      )
    }
  })
  
  # --- CALCULATOR TAB OUTPUTS ---
  output$calc_result <- renderText({
    n <- calc_hallucinations_needed(input$calc_death_prob)
    if (is.infinite(n)) {
      "∞ (impossible)"
    } else if (n == 0) {
      "0 (certain death)"
    } else {
      format(n, big.mark = ",")
    }
  })
  
  example_cases <- reactive({
    tibble(
      Case = c(26, 27, 28, 231, 238, 240, 350, 355),
      `Death Prob` = c(0.00104, 0.00443, 0.000347, 0.693, 0.0000347, 0.0173, 1.0, 1.0),
      `Halluc. Needed` = sapply(`Death Prob`, calc_hallucinations_needed),
      `Main Objection` = c(
        "Date uncertain, in bed",
        "Not hallucination, anxiety",
        "Ill, anxious, explained by fever",
        "Anxiety, 2-day gap, fatigue",
        "Real person? Extremely unlikely",
        "Reconciliation, knew near death",
        "Maid's prank - complete explanation",
        "Wrong date - excluded"
      )
    ) %>%
      mutate(`Halluc. Needed` = ifelse(is.infinite(`Halluc. Needed`), "N/A", 
                                       ifelse(`Halluc. Needed` == 0, "N/A",
                                              format(`Halluc. Needed`, big.mark = ","))),
             `Death Prob` = sprintf("%.5f%%", `Death Prob` * 100))
  })
  
  output$example_cases_table <- renderDT({
    datatable(
      example_cases(),
      selection = 'single',
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE
    )
  })
  
  observeEvent(input$example_cases_table_rows_selected, {
    selected <- input$example_cases_table_rows_selected
    if (length(selected) > 0) {
      # Extract the numeric death probability
      prob_str <- example_cases()$`Death Prob`[selected]
      prob <- as.numeric(gsub("%", "", prob_str)) / 100
      updateSliderInput(session, "calc_death_prob", value = prob)
    }
  })
  
  # --- DEATH CATEGORIES TAB OUTPUTS ---
  output$death_table <- renderDT({
    analysis()$death_categories %>%
      mutate(
        h_prob = percent(h_prob, accuracy = 0.1),
        annual_death_rate = percent(annual_death_rate, accuracy = 0.1),
        prob_death_12hr = percent(prob_death_12hr, accuracy = 0.01),
        weighted_contribution = percent(weighted_contribution, accuracy = 0.001)
      ) %>%
      rename(
        `Category` = type,
        `Composition (h)` = h_prob,
        `Annual Death Rate` = annual_death_rate,
        `12-hr Death Prob` = prob_death_12hr,
        `Weighted Contribution` = weighted_contribution
      ) %>%
      datatable(rownames = FALSE, options = list(dom = 't'))
  })
  
  output$death_category_plot <- renderPlot({
    analysis()$death_categories %>%
      ggplot(aes(x = type, y = weighted_contribution, fill = type)) +
      geom_col() +
      geom_text(aes(label = percent(weighted_contribution, accuracy = 0.01)), 
                vjust = -0.5, fontface = "bold") +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = "Weighted Contribution to Coincidence Probability",
        subtitle = "= (Composition in hallucinations) × (Death probability)",
        x = NULL,
        y = "Contribution"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold"))
  })
  
  # --- CASES TAB OUTPUTS ---
  peirce_cases <- reactive({
    tibble(
      Case = c(26, 27, 28, 29, 170, 172, 173, 174, 175, 182, 184, 195, 197, 
               199, 201, 202, 214, 231, 236, 237, 238, 240, 249, 298, 300, 
               350, 355, 695, 697, 702),
      `Halluc. Needed` = c(667, 156, 2000, 1050, 0, 0, 0, 0, 48, 400, 25, 
                           200, 0, 0, 0, 0, 100, 1, 700, 1500, 20000, 4, 
                           100, 1000, 0, 0, 0, 100, 800, 0),
      `Main Objection` = c(
        "Date uncertain, in bed",
        "Not hallucination, anxiety",
        "Ill, anxious, explained by fever",
        "Drunk? Insufficient inquiry",
        "Multiple objections - excluded",
        "In bed, anxious - excluded",
        "Dream on ship - complete explanation",
        "Not in good health - excluded",
        "Dream continued into waking",
        "Multiple hallucinations, date issues",
        "Constant hallucinations, certainty re child",
        "Anxious, imagination, 2 people counted",
        "Date wrong, in bed - excluded",
        "Wrong year - excluded",
        "Not recognized, date uncertain - excluded",
        "Near-sighted, not in health - excluded",
        "Delirium, date uncertain",
        "Anxiety, 2-day gap, fatigue",
        "Brain disease, dark figure only",
        "Mother thought dream, date uncertain",
        "Real person? But 1/20000 needed",
        "Reconciliation, knew near death",
        "Hat and head only - not real identification",
        "Real person seen? Date wrong",
        "Sailor's yarn - excluded",
        "Maid's prank - complete explanation",
        "Wrong date - excluded",
        "Anxious, meagre, son had fever",
        "Recognition after death news",
        "Fever, date altered 4 days - excluded"
      )
    ) %>%
      mutate(`Halluc. Needed` = ifelse(`Halluc. Needed` == 0, "N/A", 
                                       format(`Halluc. Needed`, big.mark = ",")))
  })
  
  output$peirce_cases <- renderDT({
    datatable(
      peirce_cases(),
      options = list(pageLength = 15),
      rownames = FALSE
    )
  })
}

# --- Run the app
shinyApp(ui = ui, server = server)