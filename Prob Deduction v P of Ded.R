library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Probable vs Necessary Deduction Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Population Parameters"),
      numericInput("n_population", 
                   "Total number of As:", 
                   value = 100, 
                   min = 10, 
                   max = 10000),
      
      sliderInput("true_prop", 
                  "True proportion of As that are Bs (p):", 
                  min = 0, 
                  max = 1, 
                  value = 0.5, 
                  step = 0.01),
      
      hr(),
      
      h4("Argument Parameters"),
      sliderInput("probable_claim", 
                  "Probable Deduction: Claimed proportion:", 
                  min = 0, 
                  max = 1, 
                  value = 0.75, 
                  step = 0.01),
      
      hr(),
      
      h4("Sampling Parameters"),
      numericInput("n_samples", 
                   "Number of samples to draw:", 
                   value = 1000, 
                   min = 100, 
                   max = 100000),
      
      actionButton("run_simulation", "Run Simulation", class = "btn-primary"),
      
      hr(),
      
      helpText("The app compares three argument forms:",
               "1. Probable Deduction (PD): Strong argument for guarded conclusion",
               "2. Necessary Deduction (ND): Weak argument for strong conclusion",
               "3. Modified PD: Using a different claimed proportion")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Arguments",
                 h3("Argument Forms"),
                 wellPanel(
                   h4("1. Probable Deduction (True)"),
                   uiOutput("pd_true_text"),
                   textOutput("pd_true_accuracy")
                 ),
                 wellPanel(
                   h4("2. Necessary Deduction"),
                   uiOutput("nd_text"),
                   textOutput("nd_accuracy")
                 ),
                 wellPanel(
                   h4("3. Probable Deduction (Modified)"),
                   uiOutput("pd_modified_text"),
                   textOutput("pd_modified_accuracy")
                 )
        ),
        
        tabPanel("Convergence Plot",
                 plotOutput("convergence_plot", height = "600px"),
                 textOutput("convergence_summary")
        ),
        
        tabPanel("Accuracy Comparison",
                 plotOutput("accuracy_plot", height = "500px"),
                 tableOutput("accuracy_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  simulation_results <- reactiveVal(NULL)
  
  observeEvent(input$run_simulation, {
    n_pop <- input$n_population
    true_p <- input$true_prop
    claimed_p <- input$probable_claim
    n_samp <- input$n_samples
    
    # Create population: TRUE if B, FALSE if not B
    population <- c(rep(TRUE, round(n_pop * true_p)), 
                    rep(FALSE, n_pop - round(n_pop * true_p)))
    
    # Draw samples with replacement
    samples <- replicate(n_samp, sample(population, 1, replace = TRUE))
    
    # Calculate cumulative proportions
    cumulative_prop <- cumsum(samples) / seq_along(samples)
    
    # Calculate accuracy for each argument type
    # PD (True): Correct if the probability matches reality
    pd_true_correct <- samples == TRUE
    pd_true_cumulative <- cumsum(pd_true_correct) / seq_along(pd_true_correct)
    
    # ND: "All As are Bs" - correct only when sample is B
    nd_correct <- samples == TRUE
    nd_cumulative <- cumsum(nd_correct) / seq_along(nd_correct)
    
    # PD (Modified): Using claimed proportion
    # This is "correct" when our claimed probability reflects reality
    # We're right in proportion to how close our claim is to truth
    pd_modified_correct <- samples == TRUE
    pd_modified_cumulative <- cumsum(pd_modified_correct) / seq_along(pd_modified_correct)
    
    results <- list(
      samples = samples,
      cumulative_prop = cumulative_prop,
      pd_true_cumulative = pd_true_cumulative,
      nd_cumulative = nd_cumulative,
      pd_modified_cumulative = pd_modified_cumulative,
      true_p = true_p,
      claimed_p = claimed_p,
      n_samples = n_samp
    )
    
    simulation_results(results)
  })
  
  output$pd_true_text <- renderUI({
    true_p <- input$true_prop
    HTML(sprintf("<strong>Premise 1:</strong> %.0f%% of As are Bs<br>
                  <strong>Premise 2:</strong> x is an A<br>
                  <strong>Conclusion:</strong> Therefore, there is a %.0f%% chance that x is a B",
                 true_p * 100, true_p * 100))
  })
  
  output$nd_text <- renderUI({
    true_p <- input$true_prop
    HTML(sprintf("<strong>Premise 1:</strong> All As are Bs<br>
                  <strong>Premise 2:</strong> x is an A<br>
                  <strong>Conclusion:</strong> Therefore, x is a B<br>
                  <em>(This argument is correct %.0f%% of the time)</em>",
                 true_p * 100))
  })
  
  output$pd_modified_text <- renderUI({
    claimed_p <- input$probable_claim
    HTML(sprintf("<strong>Premise 1:</strong> %.0f%% of As are Bs<br>
                  <strong>Premise 2:</strong> x is an A<br>
                  <strong>Conclusion:</strong> Therefore, there is a %.0f%% chance that x is a B",
                 claimed_p * 100, claimed_p * 100))
  })
  
  output$pd_true_accuracy <- renderText({
    res <- simulation_results()
    if (is.null(res)) return("Run simulation to see results")
    
    final_accuracy <- tail(res$pd_true_cumulative, 1)
    sprintf("After %d samples: This argument leads you right %.2f%% of the time (approaches %.0f%%)",
            res$n_samples, final_accuracy * 100, res$true_p * 100)
  })
  
  output$nd_accuracy <- renderText({
    res <- simulation_results()
    if (is.null(res)) return("Run simulation to see results")
    
    final_accuracy <- tail(res$nd_cumulative, 1)
    sprintf("After %d samples: This argument leads you right %.2f%% of the time (approaches %.0f%%)",
            res$n_samples, final_accuracy * 100, res$true_p * 100)
  })
  
  output$pd_modified_accuracy <- renderText({
    res <- simulation_results()
    if (is.null(res)) return("Run simulation to see results")
    
    final_accuracy <- tail(res$pd_modified_cumulative, 1)
    claimed_p <- input$probable_claim
    true_p <- input$true_prop
    
    sprintf("After %d samples: This argument leads you right %.2f%% of the time (approaches %.0f%%, not %.0f%%)",
            res$n_samples, final_accuracy * 100, true_p * 100, claimed_p * 100)
  })
  
  output$convergence_plot <- renderPlot({
    res <- simulation_results()
    if (is.null(res)) return(NULL)
    
    df <- data.frame(
      sample_num = 1:res$n_samples,
      pd_true = res$pd_true_cumulative,
      nd = res$nd_cumulative,
      pd_modified = res$pd_modified_cumulative
    )
    
    df_long <- data.frame(
      sample_num = rep(df$sample_num, 3),
      proportion = c(df$pd_true, df$nd, df$pd_modified),
      argument = rep(c("PD (True Proportion)", "ND (All As are Bs)", "PD (Modified Claim)"), 
                     each = res$n_samples)
    )
    
    ggplot(df_long, aes(x = sample_num, y = proportion, color = argument)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = res$true_p, linetype = "dashed", color = "black", linewidth = 1) +
      geom_hline(yintercept = res$claimed_p, linetype = "dotted", color = "red", linewidth = 0.8) +
      annotate("text", x = res$n_samples * 0.8, y = res$true_p + 0.03, 
               label = sprintf("True proportion (%.0f%%)", res$true_p * 100)) +
      annotate("text", x = res$n_samples * 0.8, y = res$claimed_p + 0.03, 
               label = sprintf("Claimed proportion (%.0f%%)", res$claimed_p * 100), 
               color = "red") +
      labs(title = "Convergence of Argument Accuracy Over Samples",
           subtitle = "How often each argument leads you to the correct conclusion",
           x = "Number of Samples",
           y = "Proportion Correct",
           color = "Argument Type") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      scale_color_manual(values = c("PD (True Proportion)" = "#2E86AB",
                                    "ND (All As are Bs)" = "#A23B72",
                                    "PD (Modified Claim)" = "#F18F01"))
  })
  
  output$convergence_summary <- renderText({
    res <- simulation_results()
    if (is.null(res)) return("")
    
    sprintf("All three arguments converge to the same accuracy rate (%.0f%%) because they all succeed when x is actually a B. The difference is in the form: PD with true proportion is a strong argument for a guarded conclusion, while ND is a weak argument (only %.0f%% reliable) for a strong conclusion. PD with modified claim %.0f%% shows what happens when the premise is incorrect.",
            res$true_p * 100, res$true_p * 100, res$claimed_p * 100)
  })
  
  output$accuracy_plot <- renderPlot({
    res <- simulation_results()
    if (is.null(res)) return(NULL)
    
    final_accuracies <- data.frame(
      Argument = c("PD (True)", "ND", "PD (Modified)"),
      Accuracy = c(tail(res$pd_true_cumulative, 1),
                   tail(res$nd_cumulative, 1),
                   tail(res$pd_modified_cumulative, 1)),
      Expected = c(res$true_p, res$true_p, res$claimed_p)
    )
    
    ggplot(final_accuracies, aes(x = Argument)) +
      geom_col(aes(y = Accuracy, fill = "Observed"), alpha = 0.7, position = "dodge") +
      geom_point(aes(y = Expected, color = "Expected"), size = 4) +
      geom_text(aes(y = Accuracy, label = sprintf("%.1f%%", Accuracy * 100)), 
                vjust = -0.5, size = 5) +
      labs(title = "Final Accuracy Rates After All Samples",
           y = "Proportion Correct",
           x = "Argument Type") +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("Observed" = "#2E86AB")) +
      scale_color_manual(values = c("Expected" = "#A23B72")) +
      theme(legend.position = "bottom")
  })
  
  output$accuracy_table <- renderTable({
    res <- simulation_results()
    if (is.null(res)) return(NULL)
    
    data.frame(
      Argument = c("PD (True Proportion)", 
                   "ND (All As are Bs)", 
                   "PD (Modified Claim)"),
      `Claimed Probability` = c(sprintf("%.0f%%", res$true_p * 100),
                                "100%",
                                sprintf("%.0f%%", res$claimed_p * 100)),
      `Observed Accuracy` = c(sprintf("%.2f%%", tail(res$pd_true_cumulative, 1) * 100),
                              sprintf("%.2f%%", tail(res$nd_cumulative, 1) * 100),
                              sprintf("%.2f%%", tail(res$pd_modified_cumulative, 1) * 100)),
      `Converges To` = c(sprintf("%.0f%%", res$true_p * 100),
                         sprintf("%.0f%%", res$true_p * 100),
                         sprintf("%.0f%%", res$true_p * 100)),
      check.names = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)