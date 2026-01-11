# ==================================================
# Progressive Forecast Evaluation Explorer
# ==================================================
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(htmltools)

ui <- fluidPage(
  titlePanel("Assessing Our Methods"),
  
  tags$head(
    tags$style(HTML("
      .forecaster-table { display: inline-block; margin: 10px 20px 20px 10px; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .forecaster-title { font-weight: bold; font-size: 16px; margin-bottom: 10px; text-align: center; }
      .metric-box { background: #f8f9fa; padding: 10px; margin-top: 10px; border-radius: 5px; font-size: 13px; }
      .cell-label { font-size: 11px; color: #666; font-style: italic; }
      .formula-highlight { background: #fff3cd; padding: 8px; margin: 5px 0; border-left: 3px solid #ffc107; border-radius: 3px; font-family: monospace; }
      .secret-box { display: inline-block; width: 40px; height: 40px; background: #6c757d; border: 2px solid #495057; border-radius: 5px; cursor: help; text-align: center; line-height: 40px; font-size: 24px; color: white; margin: 0 10px; }
      .prediction-badge { display: inline-block; padding: 5px 12px; border-radius: 15px; font-weight: bold; margin: 0 5px; }
      .matrix-viz { display: inline-block; margin: 20px; text-align: center; vertical-align: top; }
      .matrix-grid { display: inline-grid; grid-template-columns: 100px 100px; grid-gap: 3px; border: 3px solid #333; margin: 10px auto; }
      .matrix-cell { width: 100px; height: 100px; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 20px; border: 1px solid #666; }
      .formula-viz { font-size: 24px; font-weight: bold; margin: 10px 0; }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Complexity Level"),
      tabsetPanel(id = "complexity_level",
                  tabPanel("Level 1: Success Rates", value = "level1", br(),
                           checkboxInput("show_ci", "Show confidence intervals", value = FALSE),
                           conditionalPanel(condition = "input.show_ci == true",
                                            sliderInput("ci_level", "Confidence level:", min = 0.8, max = 0.99, value = 0.95, step = 0.01))),
                  tabPanel("Level 2: Confusion Matrices", value = "level2", br(),
                           checkboxInput("show_formulas", "Show formula annotations", value = FALSE),
                           checkboxInput("show_roc", "Show ROC curve", value = FALSE),
                           checkboxInput("show_distributions", "Show SDT distributions", value = FALSE),
                           conditionalPanel(condition = "input.show_distributions == true",
                                            checkboxInput("separate_dists_cm", "Separate by True State", value = FALSE))),
                  tabPanel("Level 3: Balancing Reasons", value = "level3", br(),
                           actionButton("generate_case", "Generate New Test Case", class = "btn-primary", style = "width: 100%;"), br(), br(),
                           radioButtons("mbr_method", "MBR Calculation Method:",
                                        choices = c("Overall reliability (Peirce's method)" = "peirce","Per-prediction odds (TP/FP, TN/FN)" = "prediction" ),
                                        selected = "peirce"), hr(),
                           checkboxInput("apply_threshold", "Apply certainty threshold", value = FALSE),
                           conditionalPanel(condition = "input.apply_threshold == true",
                                            sliderInput("certainty_threshold", "Certainty threshold (as % margin):", min = 1, max = 20, value = 5, step = 0.5),
                                            radioButtons("error_type", "Threshold interpretation:",
                                                         choices = c("At current CI level" = "ci", "As Probable Error (50% CI)" = "pe"), selected = "ci")), hr(),
                           helpText("MBR calculates the weight of evidence by summing log-odds from each method's prediction.")),
                  tabPanel("Help / Glossary", value = "help", br(),
                           helpText("Visual guide to confusion matrix metrics"))
      ),       hr(),
      conditionalPanel(condition = "input.complexity_level == 'help'",
                       selectInput("help_metric", "Select metric to visualize:",
                                   choices = c("True Positive (TP)" = "tp", "False Positive (FP)" = "fp", "True Negative (TN)" = "tn", "False Negative (FN)" = "fn",
                                               "True Positive Rate (TPR/Sensitivity)" = "tpr", "False Positive Rate (FPR)" = "fpr", "True Negative Rate (TNR/Specificity)" = "tnr",
                                               "Precision (PPV)" = "precision", "False Discovery Rate (FDR)" = "fdr", "Likelihood Ratio + (LR+)" = "lrplus", "Peirce Skill Score (PSS/TSS)" = "pss"),
                                   selected = "tp"),
                       checkboxInput("show_numbers", "Show actual numbers (instead of labels)", value = FALSE),
                       hr()
      ),
      selectInput("scenario_select", "Load preset scenario:",
                  choices = c("Custom" = "custom", "Finley's Tornadoes" = "finley", "Gold/Lead - Set 1" = "gold_lead_1", "Gold/Lead - Set 2" = "gold_lead_2"),
                  selected = "custom"),
      actionButton("load_scenario", "Load Scenario", class = "btn-info", style = "width: 100%; margin-bottom: 10px;"), hr(),
      conditionalPanel(condition = "input.complexity_level != 'help'",
                       numericInput("total", "Total number of past cases:", value = 1000, min = 1, step = 1),
                       sliderInput("prop_event", "Proportion of events:", min = 0.01, max = 0.99, value = 0.3, step = 0.01), hr(),
                       numericInput("n_forecasters", "Number of methods:", value = 2, min = 1, max = 5, step = 1),
                       actionButton("match_totals", "Set all methods to total cases", style = "width: 100%; margin-bottom: 10px;"),
                       uiOutput("forecaster_controls")),
      conditionalPanel(condition = "input.complexity_level == 'help'",
                       numericInput("total", "Total number of past cases:", value = 1000, min = 1, step = 1),
                       sliderInput("prop_event", "Proportion of events:", min = 0.01, max = 0.99, value = 0.3, step = 0.01), hr(),
                       h4("Method 1", style = "color: #e74c3c;"),
                       numericInput("n_trials_1", "Number of trials:", value = 1000, min = 1, step = 1),
                       sliderInput("hit_rate_1", "Hit rate (TPR):", min = 0, max = 1, value = 0.8, step = 0.001),
                       sliderInput("false_alarm_1", "False alarm rate (FPR):", min = 0, max = 1, value = 0.2, step = 0.001))
    ),
    mainPanel(
      conditionalPanel(condition = "input.complexity_level == 'level1'", h3("Level 1: Simple Success Rates"), htmlOutput("success_rates")),
      conditionalPanel(condition = "input.complexity_level == 'level2'",
                       h3("Level 2: Confusion Matrices"),
                       htmlOutput("matrices_visual"),
                       br(),
                       plotOutput("barPlot", height = "300px"),
                       conditionalPanel(condition = "input.show_roc == true",
                                        hr(),
                                        h4("ROC Space"),
                                        plotOutput("roc_plot_cm", height = "400px")),
                       conditionalPanel(condition = "input.show_distributions == true",
                                        hr(),
                                        h4("Signal Detection Theory Distributions"),
                                        plotOutput("dist_plot_cm", height = "350px"))),
      conditionalPanel(condition = "input.complexity_level == 'level3'", h3("Level 3: Method of Balancing Reasons"), htmlOutput("mbr_case_display"), br(), plotOutput("mbr_plot", height = "400px"), br(), verbatimTextOutput("mbr_details")),
      conditionalPanel(condition = "input.complexity_level == 'help'", h3("Confusion Matrix Glossary"), htmlOutput("help_display"))
    )
  )
)

server <- function(input, output, session) {
  forecaster_colors <- c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6")
  test_case <- reactiveValues(event_occurred = NULL, predictions = NULL, generated = FALSE)
  exact_values <- reactiveValues(tpr = NULL, fpr = NULL)
  scenario_info <- reactiveValues(event_name = "Event", no_event_name = "No Event", predict_event_name = "Event!", predict_no_event_name = "No Event")
  
  generate_prediction <- function(event_occurred, hits, misses, false_alarms, correct_negatives) {
    if (event_occurred) {
      prob_says_event <- hits / (hits + misses)
      says_event <- runif(1) < prob_says_event
    } else {
      prob_says_event <- false_alarms / (false_alarms + correct_negatives)
      says_event <- runif(1) < prob_says_event
    }
    return(ifelse(says_event, "event", "no_event"))
  }
  
  observeEvent(input$load_scenario, {
    scenario <- input$scenario_select
    if (scenario == "custom") {
      scenario_info$event_name <- "Event"; scenario_info$no_event_name <- "No Event"
      scenario_info$predict_event_name <- "Event!"; scenario_info$predict_no_event_name <- "No Event"
      updateNumericInput(session, "total", value = 1000); updateSliderInput(session, "prop_event", value = 0.3)
      updateNumericInput(session, "n_forecasters", value = 2); Sys.sleep(0.5)
      exact_values$tpr <- NULL; exact_values$fpr <- NULL
      updateNumericInput(session, "n_trials_1", value = 1000); updateSliderInput(session, "hit_rate_1", value = 0.8); updateSliderInput(session, "false_alarm_1", value = 0.2)
      updateNumericInput(session, "n_trials_2", value = 1000); updateSliderInput(session, "hit_rate_2", value = 0.65); updateSliderInput(session, "false_alarm_2", value = 0.3)
    } else if (scenario == "finley") {
      scenario_info$event_name <- "Tornado"; scenario_info$no_event_name <- "No Tornado"
      scenario_info$predict_event_name <- "Tornado!"; scenario_info$predict_no_event_name <- "No Tornado"
      updateNumericInput(session, "total", value = 2803); updateSliderInput(session, "prop_event", value = 51/2803); updateNumericInput(session, "n_forecasters", value = 1); Sys.sleep(0.5)
      exact_values$tpr <- list("1" = 28/51); exact_values$fpr <- list("1" = 72/2752)
      updateNumericInput(session, "n_trials_1", value = 2803); updateSliderInput(session, "hit_rate_1", value = 28/51); updateSliderInput(session, "false_alarm_1", value = 72/2752)
    } else if (scenario == "gold_lead_1") {
      scenario_info$event_name <- "Gold"; scenario_info$no_event_name <- "Lead"
      scenario_info$predict_event_name <- "Predict Gold"; scenario_info$predict_no_event_name <- "Predict Lead"
      updateNumericInput(session, "total", value = 168); updateSliderInput(session, "prop_event", value = 72/168); updateNumericInput(session, "n_forecasters", value = 2); Sys.sleep(0.5)
      exact_values$tpr <- list("1" = 50/72, "2" = 36/72); exact_values$fpr <- list("1" = 20/96, "2" = 24/96)
      updateNumericInput(session, "n_trials_1", value = 168); updateSliderInput(session, "hit_rate_1", value = 50/72); updateSliderInput(session, "false_alarm_1", value = 20/96)
      updateNumericInput(session, "n_trials_2", value = 168); updateSliderInput(session, "hit_rate_2", value = 36/72); updateSliderInput(session, "false_alarm_2", value = 24/96)
    } else if (scenario == "gold_lead_2") {
      scenario_info$event_name <- "Gold"; scenario_info$no_event_name <- "Lead"
      scenario_info$predict_event_name <- "Predict Gold"; scenario_info$predict_no_event_name <- "Predict Lead"
      updateNumericInput(session, "total", value = 70); updateSliderInput(session, "prop_event", value = 40/70); updateNumericInput(session, "n_forecasters", value = 2); Sys.sleep(0.5)
      exact_values$tpr <- list("1" = 24/40, "2" = 35/40); exact_values$fpr <- list("1" = 12/30, "2" = 25/30)
      updateNumericInput(session, "n_trials_1", value = 70); updateSliderInput(session, "hit_rate_1", value = 24/40); updateSliderInput(session, "false_alarm_1", value = 12/30)
      updateNumericInput(session, "n_trials_2", value = 70); updateSliderInput(session, "hit_rate_2", value = 35/40); updateSliderInput(session, "false_alarm_2", value = 25/30)
    }
  })
  
  output$forecaster_controls <- renderUI({
    n <- input$n_forecasters; if (is.null(n) || n < 1) n <- 1
    lapply(1:n, function(i) {
      color <- forecaster_colors[((i-1) %% 5) + 1]
      tagList(div(style = paste0("border-left: 4px solid ", color, "; padding-left: 10px; margin-bottom: 15px;"),
                  h4(paste("Method", i), style = paste0("color: ", color)),
                  numericInput(paste0("n_trials_", i), "Number of trials:", value = input$total, min = 1, step = 1),
                  sliderInput(paste0("hit_rate_", i), "Hit rate (TPR):", min = 0, max = 1, value = max(0, min(1, 0.8 - (i-1)*0.15)), step = 0.001),
                  sliderInput(paste0("false_alarm_", i), "False alarm rate (FPR):", min = 0, max = 1, value = max(0, min(1, 0.2 + (i-1)*0.1)), step = 0.001),
                  conditionalPanel(condition = "input.complexity_level == 'level3'",
                                   selectInput(paste0("prediction_", i), "Prediction for test case:", choices = c("Event!" = "event", "No Event" = "no_event"), selected = "event"))))
    })
  })
  
  observeEvent(input$match_totals, {
    n <- input$n_forecasters; if (!is.null(n) && n >= 1) for (i in 1:n) updateNumericInput(session, paste0("n_trials_", i), value = input$total)
  })
  
  data_vals <- reactive({
    n <- input$n_forecasters; if (is.null(n) || n < 1) n <- 1
    total <- input$total; prop_event <- input$prop_event; n_event <- round(total * prop_event); n_nonevent <- total - n_event
    z <- qnorm((1 + input$ci_level) / 2); threshold_pct <- input$certainty_threshold / 100
    threshold_se <- if (input$apply_threshold) { if (input$error_type == "ci") threshold_pct / z else threshold_pct / 0.6745 } else Inf
    
    forecasters <- lapply(1:n, function(i) {
      n_trials <- input[[paste0("n_trials_", i)]]
      if (!is.null(exact_values$tpr) && !is.null(exact_values$tpr[[as.character(i)]])) {
        TPR <- exact_values$tpr[[as.character(i)]]; FPR <- exact_values$fpr[[as.character(i)]]
      } else { TPR <- input[[paste0("hit_rate_", i)]]; FPR <- input[[paste0("false_alarm_", i)]] }
      if (is.null(n_trials)) n_trials <- total
      if (is.null(TPR)) TPR <- max(0, min(1, 0.8 - (i-1)*0.15))
      if (is.null(FPR)) FPR <- max(0, min(1, 0.2 + (i-1)*0.1))
      
      n_event_method <- round(n_trials * prop_event); n_nonevent_method <- n_trials - n_event_method
      hits <- round(n_event_method * TPR); misses <- n_event_method - hits
      false_alarms <- round(n_nonevent_method * FPR); correct_negatives <- n_nonevent_method - false_alarms
      success_rate <- (hits + correct_negatives) / n_trials
      se <- sqrt(success_rate * (1 - success_rate) / n_trials); pe <- 0.6745 * se; ci_margin <- z * se
      PSS <- TPR - FPR; LR_pos <- ifelse(FPR > 0, TPR / FPR, Inf); LR_neg <- ifelse(FPR < 1, (1 - TPR) / (1 - FPR), Inf)
      total_yes_predictions <- hits + false_alarms; total_no_predictions <- misses + correct_negatives
      odds_when_says_yes <- ifelse(total_yes_predictions > 0 && false_alarms > 0, hits / false_alarms, Inf)
      odds_when_says_no <- ifelse(total_no_predictions > 0 && misses > 0, correct_negatives / misses, Inf)
      total_correct <- hits + correct_negatives; total_incorrect <- misses + false_alarms
      overall_odds <- ifelse(total_incorrect > 0, total_correct / total_incorrect, Inf)
      
      list(id = i, name = paste("Method", i), color = forecaster_colors[((i-1) %% 5) + 1], n_trials = n_trials,
           n_event = n_event_method, n_nonevent = n_nonevent_method, hits = hits, misses = misses, false_alarms = false_alarms, correct_negatives = correct_negatives,
           TPR = TPR, FPR = FPR, success_rate = success_rate, se = se, pe = pe, ci_margin = ci_margin,
           PSS = PSS, LR_pos = LR_pos, LR_neg = LR_neg, odds_when_says_yes = odds_when_says_yes, odds_when_says_no = odds_when_says_no, overall_odds = overall_odds)
    })
    list(forecasters = forecasters, n_event = n_event, n_nonevent = n_nonevent, total = total, threshold_se = threshold_se)
  })
  
  output$success_rates <- renderUI({
    d <- data_vals()
    rates <- lapply(d$forecasters, function(fc) {
      correct <- fc$hits + fc$correct_negatives; rate_pct <- round(fc$success_rate * 100, 1)
      ci_text <- if (input$show_ci) paste0(" ± ", round(fc$ci_margin * 100, 1), "% (at ", round(input$ci_level * 100, 0), "% confidence)") else ""
      div(style = paste0("padding: 20px; margin: 10px; border-left: 5px solid ", fc$color, "; background: ", fc$color, "10; border-radius: 5px;"),
          h4(fc$name, style = paste0("color: ", fc$color, "; margin-top: 0;")),
          div(style = "font-size: 24px; font-weight: bold;", paste0(correct, " / ", fc$n_trials, " = ", rate_pct, "%", ci_text)),
          if (input$show_ci) div(style = "font-size: 14px; color: #666; margin-top: 10px;", paste0("SE = ", round(fc$se, 4), " | PE = ", round(fc$pe, 4))) else NULL)
    })
    do.call(tagList, rates)
  })
  
  output$matrices_visual <- renderUI({
    d <- data_vals(); show_formulas <- input$show_formulas
    matrices <- lapply(d$forecasters, function(fc) {
      max_count <- max(fc$hits, fc$misses, fc$false_alarms, fc$correct_negatives)
      hit_color <- paste0("rgba(40, 167, 69, ", (fc$hits / max_count) * 0.7, ")")
      cn_color <- paste0("rgba(40, 167, 69, ", (fc$correct_negatives / max_count) * 0.7, ")")
      miss_color <- paste0("rgba(220, 53, 69, ", (fc$misses / max_count) * 0.7, ")")
      fa_color <- paste0("rgba(220, 53, 69, ", (fc$false_alarms / max_count) * 0.7, ")")
      
      tags$div(class = "forecaster-table", style = paste0("border: 3px solid ", fc$color, "; background: ", fc$color, "08"),
               div(class = "forecaster-title", style = paste0("color: ", fc$color), paste(fc$name, " (n=", fc$n_trials, ")", sep="")),
               tags$table(style = "border-collapse: collapse; margin: 10px auto;",
                          tags$tr(tags$th(style = "padding: 8px; border: 1px solid #ddd;", ""),
                                  tags$th(style = "padding: 8px; border: 1px solid #ddd; background: #f8f9fa;", paste("Forecast:", scenario_info$event_name)),
                                  tags$th(style = "padding: 8px; border: 1px solid #ddd; background: #f8f9fa;", paste("Forecast:", scenario_info$no_event_name)),
                                  tags$th(style = "padding: 8px; border: 1px solid #ddd; background: #e9ecef;", "Total")),
                          tags$tr(tags$td(style = "padding: 8px; border: 1px solid #ddd; background: #f8f9fa; font-weight: bold;", paste(scenario_info$event_name, "Occurred")),
                                  tags$td(style = paste0("padding: 8px; border: 1px solid #ddd; background: ", hit_color, "; text-align: center;"),
                                          div(style = "font-size: 18px; font-weight: bold;", fc$hits), div(class = "cell-label", "Hits (TP)")),
                                  tags$td(style = paste0("padding: 8px; border: 1px solid #ddd; background: ", miss_color, "; text-align: center;"),
                                          div(style = "font-size: 18px; font-weight: bold;", fc$misses), div(class = "cell-label", "Misses (FN)")),
                                  tags$td(style = "padding: 8px; border: 1px solid #ddd; background: #e9ecef; text-align: center; font-weight: bold;", fc$n_event)),
                          tags$tr(tags$td(style = "padding: 8px; border: 1px solid #ddd; background: #f8f9fa; font-weight: bold;", scenario_info$no_event_name),
                                  tags$td(style = paste0("padding: 8px; border: 1px solid #ddd; background: ", fa_color, "; text-align: center;"),
                                          div(style = "font-size: 18px; font-weight: bold;", fc$false_alarms), div(class = "cell-label", "False Alarms (FP)")),
                                  tags$td(style = paste0("padding: 8px; border: 1px solid #ddd; background: ", cn_color, "; text-align: center;"),
                                          div(style = "font-size: 18px; font-weight: bold;", fc$correct_negatives), div(class = "cell-label", "Correct Neg (TN)")),
                                  tags$td(style = "padding: 8px; border: 1px solid #ddd; background: #e9ecef; text-align: center; font-weight: bold;", fc$n_nonevent)),
                          tags$tr(tags$td(style = "padding: 8px; border: 1px solid #ddd; background: #e9ecef; font-weight: bold;", "Total"),
                                  tags$td(style = "padding: 8px; border: 1px solid #ddd; background: #e9ecef; text-align: center; font-weight: bold;", fc$hits + fc$false_alarms),
                                  tags$td(style = "padding: 8px; border: 1px solid #ddd; background: #e9ecef; text-align: center; font-weight: bold;", fc$misses + fc$correct_negatives),
                                  tags$td(style = "padding: 8px; border: 1px solid #ddd; background: #dee2e6; text-align: center; font-weight: bold;", fc$n_trials))),
               div(class = "metric-box",
                   div(style = "font-weight: bold; margin-bottom: 5px;", "Metrics:"),
                   div(paste0("PSS = ", round(fc$PSS, 3))), div(paste0("LR+ = ", round(fc$LR_pos, 2))),
                   div(paste0("LR− = ", round(fc$LR_neg, 2))), div(paste0("SE = ", round(fc$se, 4)))),
               if (show_formulas) tagList(
                 div(class = "formula-highlight", strong("PSS:"), " TP/(TP+FN) − FP/(FP+TN) = ",
                     fc$hits, "/", fc$n_event, " − ", fc$false_alarms, "/", fc$n_nonevent, " = ", round(fc$PSS, 3)),
                 div(class = "formula-highlight", strong("Odds when says 'Event!':"), " TP / FP = ",
                     fc$hits, " / ", fc$false_alarms, " = ", round(fc$odds_when_says_yes, 2))) else NULL)
    })
    do.call(tagList, matrices)
  })
  
  output$barPlot <- renderPlot({
    d <- data_vals()
    df <- bind_rows(lapply(d$forecasters, function(fc) data.frame(Forecaster = fc$name, Color = fc$color, PSS = fc$PSS, logLRplus = log(fc$LR_pos), logLRminus = log(fc$LR_neg)))) %>%
      pivot_longer(cols = c(PSS, logLRplus, logLRminus), names_to = "Metric", values_to = "Value") %>%
      mutate(Metric = case_when(Metric == "PSS" ~ "Peirce Skill Score", Metric == "logLRplus" ~ "log(LR+)", Metric == "logLRminus" ~ "log(LR−)"))
    color_map <- setNames(unique(df$Color), unique(df$Forecaster))
    ggplot(df, aes(x = Forecaster, y = Value, fill = Forecaster)) + geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
      geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
      facet_wrap(~Metric, scales = "free_y") + scale_fill_manual(values = color_map) +
      labs(title = "Comparison of Metrics Across Methods", x = NULL, y = "Value") +
      theme_minimal(base_size = 14) + theme(legend.position = "none", axis.text.x = element_text(angle = 0))
  })

  output$roc_plot_cm <- renderPlot({
    d <- data_vals()

    # Create ROC curves for each method
    roc_data <- lapply(d$forecasters, function(fc) {
      # Generate points along the ROC curve for this method's d' and c values
      # For simplicity, we'll use the single point (FPR, TPR) for each method
      data.frame(
        FPR = fc$FPR,
        TPR = fc$TPR,
        Method = fc$name,
        Color = fc$color
      )
    })
    roc_df <- bind_rows(roc_data)

    ggplot(roc_df, aes(x = FPR, y = TPR, color = Method)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
      geom_point(size = 5) +
      scale_color_manual(values = setNames(roc_df$Color, roc_df$Method)) +
      coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
      labs(title = "ROC Space", x = "P(False Alarm) = FPR", y = "P(Hit) = TPR") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
  })

  output$dist_plot_cm <- renderPlot({
    d <- data_vals()

    # Use the first forecaster for visualization (or could make this selectable)
    if (length(d$forecasters) == 0) return(NULL)
    fc <- d$forecasters[[1]]

    # Calculate d' and criterion from TPR and FPR
    # d' = z(TPR) - z(FPR), where z is the inverse normal CDF
    # c = -z(FPR)
    TPR_adj <- pmin(pmax(fc$TPR, 0.001), 0.999)
    FPR_adj <- pmin(pmax(fc$FPR, 0.001), 0.999)

    c_val <- -qnorm(FPR_adj)
    d_val <- qnorm(TPR_adj) + qnorm(1 - FPR_adj)
    s_val <- 1.0  # Assume equal variance

    x <- seq(-4, 8, length.out = 400)
    df_dist <- data.frame(x = x, noise = dnorm(x, 0, 1), signal = dnorm(x, d_val, s_val))

    fill_vals <- c("Hit"="#2ca02c","Miss"="#d62728","False Alarm"="#ff7f0e","Correct Rejection"="#1f77b4")

    if (input$separate_dists_cm) {
      alpha_val <- 0.5
      p <- ggplot(df_dist, aes(x = x)) +
        geom_ribbon(data = subset(df_dist, x > c_val), aes(ymin = 0, ymax = signal, fill = "Hit"), alpha = alpha_val) +
        geom_ribbon(data = subset(df_dist, x <= c_val), aes(ymin = 0, ymax = signal, fill = "Miss"), alpha = alpha_val) +
        geom_line(aes(y = signal), color = "steelblue", size = 1) +
        geom_ribbon(data = subset(df_dist, x > c_val), aes(ymin = -0.45, ymax = -0.45 + noise, fill = "False Alarm"), alpha = alpha_val) +
        geom_ribbon(data = subset(df_dist, x <= c_val), aes(ymin = -0.45, ymax = -0.45 + noise, fill = "Correct Rejection"), alpha = alpha_val) +
        geom_line(aes(y = -0.45 + noise), linetype = "dashed", color = "steelblue", size = 1) +
        geom_vline(xintercept = c_val, color = "black", size = 1) +
        geom_hline(yintercept = -0.45, color = "gray50", linetype = "dotted") +
        geom_hline(yintercept = 0, color = "black", size = 0.3) +
        scale_fill_manual(values = fill_vals) +
        theme_minimal() +
        labs(title = paste("Evidence Distributions -", fc$name, "(Separated by True State)"),
             x = "Evidence", y = "Density", fill = "Outcome") +
        annotate("text", x = min(x) + 1, y = 0.35, label = paste(scenario_info$event_name, "PRESENT"), fontface = "bold", hjust = 0) +
        annotate("text", x = min(x) + 1, y = -0.1, label = scenario_info$no_event_name, fontface = "bold", hjust = 0) +
        expand_limits(y = c(-0.45, 0))
    } else {
      p <- ggplot(df_dist, aes(x = x)) +
        geom_ribbon(data = subset(df_dist, x > c_val), aes(ymin = 0, ymax = signal, fill = "Hit"), alpha = 0.5) +
        geom_ribbon(data = subset(df_dist, x <= c_val), aes(ymin = 0, ymax = signal, fill = "Miss"), alpha = 0.5) +
        geom_ribbon(data = subset(df_dist, x > c_val), aes(ymin = 0, ymax = noise, fill = "False Alarm"), alpha = 0.5) +
        geom_ribbon(data = subset(df_dist, x <= c_val), aes(ymin = 0, ymax = noise, fill = "Correct Rejection"), alpha = 0.5) +
        geom_line(aes(y = noise), linetype = "dashed", color = "steelblue", size = 1) +
        geom_line(aes(y = signal), color = "steelblue", size = 1) +
        geom_vline(xintercept = c_val, color = "black", size = 1) +
        geom_hline(yintercept = 0, color = "black", size = 0.3) +
        scale_fill_manual(values = fill_vals) +
        theme_minimal() +
        labs(title = paste("Evidence Distributions -", fc$name),
             x = "Evidence", y = "Density", fill = "Outcome") +
        expand_limits(y = 0)
    }
    p
  })
  
  observeEvent(input$generate_case, {
    d <- data_vals(); test_case$event_occurred <- runif(1) < input$prop_event
    for (i in 1:length(d$forecasters)) {
      fc <- d$forecasters[[i]]
      pred <- generate_prediction(test_case$event_occurred, fc$hits, fc$misses, fc$false_alarms, fc$correct_negatives)
      updateSelectInput(session, paste0("prediction_", fc$id), selected = pred)
    }
    test_case$generated <- TRUE
  })
  
  output$mbr_case_display <- renderUI({
    d <- data_vals()
    if (!test_case$generated) return(div(style = "padding: 20px; background: #f8f9fa; border-radius: 5px; text-align: center;", h4("Click 'Generate New Test Case' to begin")))
    
    predictions <- lapply(d$forecasters, function(fc) {
      pred <- input[[paste0("prediction_", fc$id)]]; if (is.null(pred)) pred <- "event"
      
      if (input$mbr_method == "peirce") {
        if (pred == "event") { log_odds <- log(fc$LR_pos) } else { log_odds <- log(fc$LR_neg) }
        odds_ratio <- fc$overall_odds
      } else {
        if (pred == "event") { log_odds <- log(fc$odds_when_says_yes); odds_ratio <- fc$odds_when_says_yes
        } else { log_odds <- -log(fc$odds_when_says_no); odds_ratio <- fc$odds_when_says_no }
      }
      
      #deleted a - above, before log(fc$LR_neg). hopefully didn't mess it up elsewhere. 
      
      passes_threshold <- fc$se < d$threshold_se
      list(method = fc$name, color = fc$color, prediction = pred, log_odds = log_odds, odds_ratio = odds_ratio, passes_threshold = passes_threshold, se = fc$se, pe = fc$pe)
    })
    
    valid_predictions <- Filter(function(p) p$passes_threshold, predictions)
    total_log_odds <- sum(sapply(valid_predictions, function(p) p$log_odds))
    
    pred_displays <- lapply(predictions, function(p) {
      badge_color <- if (p$prediction == "event") "#28a745" else "#dc3545"
      opacity <- if (p$passes_threshold) "1" else "0.3"
      excluded_text <- if (!p$passes_threshold) " (EXCLUDED)" else ""
      
      if (p$prediction == "event") {
        display_log_odds <- p$log_odds
        contribution_text <- paste0("Contribution toward ", toupper(scenario_info$event_name), ": ", round(display_log_odds, 3))
      } else {
        display_log_odds <- -p$log_odds
        contribution_text <- paste0("Contribution toward ", toupper(scenario_info$no_event_name), ": ", round(display_log_odds, 3))
      }
      
      
      
      div(style = paste0("display: inline-block; margin: 10px; padding: 15px; border: 2px solid ", p$color, "; border-radius: 8px; opacity: ", opacity),
          div(style = paste0("color: ", p$color, "; font-weight: bold; font-size: 16px;"), paste0(p$method, excluded_text)),
          div(class = "prediction-badge", style = paste0("background: ", badge_color, "; color: white; margin: 10px 0;"),
              if (p$prediction == "event") scenario_info$predict_event_name else scenario_info$predict_no_event_name),
          div(style = "font-size: 13px; font-weight: bold;", contribution_text),
          div(style = "font-size: 11px; color: #666; margin-top: 5px;", paste0("SE = ", round(p$se, 4))))
    })
    
    result_color <- if (total_log_odds > 0) "#28a745" else if (total_log_odds < 0) "#dc3545" else "#6c757d"
    result_text <- if (total_log_odds > 0) {
      paste0("Evidence supports: ", toupper(scenario_info$event_name), " (log-odds = ", round(total_log_odds, 3), ")")
    } else if (total_log_odds < 0) {
      paste0("Evidence supports: ", toupper(scenario_info$no_event_name), " (log-odds = ", round(total_log_odds, 3), ")")
    } else "Evidence is neutral (log-odds = 0)"
    
    tagList(
      div(style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
          h4("Test Case Predictions:"),
          do.call(tagList, pred_displays), hr(),
          div(style = paste0("font-size: 20px; font-weight: bold; color: ", result_color, "; text-align: center; padding: 15px;"), result_text),
          div(style = "text-align: center; margin-top: 15px;",
              span("Actual outcome: "),
              span(class = "secret-box", title = if (test_case$event_occurred) paste(scenario_info$event_name, "occurred") else scenario_info$no_event_name, "?"))))
  })
  
  output$mbr_plot <- renderPlot({
    d <- data_vals(); if (!test_case$generated) return(NULL)
    
    contributions <- lapply(d$forecasters, function(fc) {
      pred <- input[[paste0("prediction_", fc$id)]]; if (is.null(pred)) pred <- "event"
      if (input$mbr_method == "peirce") {
        if (pred == "event") log_odds <- log(fc$LR_pos) else log_odds <- -log(fc$LR_neg)*-1
      } else {
        if (pred == "event") log_odds <- log(fc$odds_when_says_yes) else log_odds <- -log(fc$odds_when_says_no)
      }
      #added a *-1 above too. 
      passes_threshold <- fc$se < d$threshold_se
      data.frame(Method = fc$name, Color = fc$color, LogOdds = log_odds, Prediction = pred, PassesThreshold = passes_threshold)
    })
    
    df <- bind_rows(contributions); df_valid <- df %>% filter(PassesThreshold)
    total_row <- data.frame(Method = "TOTAL", Color = "#000000", LogOdds = sum(df_valid$LogOdds), Prediction = "total", PassesThreshold = TRUE)
    df_plot <- bind_rows(df_valid, total_row); df_plot$Method <- factor(df_plot$Method, levels = c(df_valid$Method, "TOTAL"))
    color_map <- setNames(df_plot$Color, df_plot$Method)
    
    ggplot(df_plot, aes(x = Method, y = LogOdds, fill = Method)) + geom_col(alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_text(aes(label = round(LogOdds, 2)), vjust = ifelse(df_plot$LogOdds >= 0, -0.5, 1.5), size = 5) +
      scale_fill_manual(values = color_map) +
      labs(title = "Method of Balancing Reasons: Weight of Evidence", subtitle = "Log-odds contributions (positive = event, negative = no event)",
           x = NULL, y = "Log-Odds Contribution") +
      theme_minimal(base_size = 14) + theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust = 0.5), panel.grid.major.x = element_blank())
  })
  
  output$mbr_details <- renderPrint({
    d <- data_vals(); if (!test_case$generated) { cat("Generate a test case to see MBR calculations.\n"); return() }
    
    cat("=== Method of Balancing Reasons Details ===\n")
    if (input$mbr_method == "peirce") cat("Using Peirce's Method: Overall reliability with LR+/LR-\n\n") else cat("Using Per-Prediction Odds Method\n\n")
    
    total_log_odds <- 0; n_valid <- 0
    for (fc in d$forecasters) {
      pred <- input[[paste0("prediction_", fc$id)]]; if (is.null(pred)) pred <- "event"
      passes_threshold <- fc$se < d$threshold_se
      
      cat(fc$name, ":\n")
      cat("  Prediction: ", ifelse(pred == "event", scenario_info$predict_event_name, scenario_info$predict_no_event_name), "\n")
      
      # this has to be changed. it is double negativing when it says no event in the Peirce schema. 
      # added a *-1 to the log_contribution_raw object. 
      # but it does not graph right
      # and int he boxes at the top it has the extra negative still. 
      # needs to either say Contributes X towards No Event or
      # Contributes -X towards event
      
      if (input$mbr_method == "peirce") {
        if (pred == "event") {
          cat("  Using LR+: TPR / FPR = ", round(fc$TPR, 3), " / ", round(fc$FPR, 3), " = ", round(fc$LR_pos, 3), "\n")
          log_contribution <- log(fc$LR_pos)
          cat("  Log-odds contribution toward ", toupper(scenario_info$event_name), ": ", round(log_contribution, 3), "\n")
        } else {
          cat("  Using LR-: (1-TPR) / (1-FPR) = ", round(1-fc$TPR, 3), " / ", round(1-fc$FPR, 3), " = ", round(fc$LR_neg, 3), "\n")
          log_contribution_raw <- log(fc$LR_neg)*-1; log_contribution <- -log_contribution_raw
          cat("  Log-odds contribution toward ", toupper(scenario_info$no_event_name), ": ", round(log_contribution_raw, 3), "\n")
        }
        
      } else {
        if (pred == "event") {
          cat("  Using: Hits / False Alarms = ", fc$hits, " / ", fc$false_alarms, " = ", round(fc$odds_when_says_yes, 3), "\n")
          log_contribution <- log(fc$odds_when_says_yes)
          cat("  Log-odds contribution toward ", toupper(scenario_info$event_name), ": ", round(log_contribution, 3),
              ifelse(log_contribution < 0, " (negative = weak evidence)", ""), "\n")
        } else {
          cat("  Using: Correct Negatives / Misses = ", fc$correct_negatives, " / ", fc$misses, " = ", round(fc$odds_when_says_no, 3), "\n")
          log_contribution_raw <- log(fc$odds_when_says_no); log_contribution <- -log_contribution_raw
          cat("  Log-odds contribution toward ", toupper(scenario_info$no_event_name), ": ", round(log_contribution_raw, 3),
              ifelse(log_contribution_raw < 0, " (negative = weak evidence)", ""), "\n")
        }
      }
      
      if (passes_threshold) { total_log_odds <- total_log_odds + log_contribution; n_valid <- n_valid + 1 }
      cat("  SE = ", round(fc$se, 4), ifelse(passes_threshold, " (INCLUDED)\n", " (EXCLUDED - too uncertain)\n"), "\n")
    }
    
    cat("=======================================\n")
    cat("Total log-odds (sum): ", round(total_log_odds, 3), "\n")
    cat("Methods included: ", n_valid, " of ", length(d$forecasters), "\n")
    cat("Odds ratio: ", round(exp(total_log_odds), 3), "\n")
    
    if (total_log_odds > 0) {
      cat("\nInterpretation: The weight of evidence favors ", toupper(scenario_info$event_name), " occurring.\n")
      cat("'Proper intensity of feeling': ", round(total_log_odds, 3), " log-odds units toward ", toupper(scenario_info$event_name), "\n")
    } else if (total_log_odds < 0) {
      cat("\nInterpretation: The weight of evidence favors ", toupper(scenario_info$no_event_name), ".\n")
      cat("'Proper intensity of feeling': ", round(abs(total_log_odds), 3), " log-odds units toward ", toupper(scenario_info$no_event_name), "\n")
    } else cat("\nInterpretation: Evidence is perfectly balanced (neutral).\n")
    
    cat("\n--- Actual Outcome ---\n")
    cat("The event ", ifelse(test_case$event_occurred, "DID", "DID NOT"), " occur.\n")
    cat("Actual: ", ifelse(test_case$event_occurred, scenario_info$event_name, scenario_info$no_event_name), "\n\n")
    
    cat("Classification of Results:\n")
    for (fc in d$forecasters) {
      pred <- input[[paste0("prediction_", fc$id)]]; if (is.null(pred)) pred <- "event"
      result <- if (test_case$event_occurred && pred == "event") "True Positive (correctly predicted event)"
      else if (test_case$event_occurred && pred == "no_event") "False Negative (missed the event)"
      else if (!test_case$event_occurred && pred == "event") "False Positive (false alarm)"
      else "True Negative (correctly predicted no event)"
      cat(fc$name, ": ", result, "\n")
    }
  })
  
  output$help_display <- renderUI({
    d <- data_vals(); if (length(d$forecasters) == 0) return(div("Configure a method to see visualizations"))
    fc <- d$forecasters[[1]]
    metric <- input$help_metric
    show_nums <- input$show_numbers
    
    make_matrix <- function(tp_col, fp_col, fn_col, tn_col, label = "") {
      div(class = "matrix-grid",
          div(class = "matrix-cell", style = paste0("background: ", tp_col), 
              if(show_nums) fc$hits else "TP"),
          div(class = "matrix-cell", style = paste0("background: ", fn_col), 
              if(show_nums) fc$misses else "FN"),
          div(class = "matrix-cell", style = paste0("background: ", fp_col), 
              if(show_nums) fc$false_alarms else "FP"),
          div(class = "matrix-cell", style = paste0("background: ", tn_col), 
              if(show_nums) fc$correct_negatives else "TN"))
    }
    
    make_fraction <- function(num_matrix, denom_matrix, result) {
      div(style = "text-align: center; margin: 40px 0;",
          div(class = "fraction-container",
              num_matrix,
              div(class = "fraction-line"),
              denom_matrix),
          span(class = "equals-sign", "="),
          span(class = "result-box", result))
    }
    
    green <- "#28a745"; red <- "#dc3545"; white <- "#f8f9fa"
    
    content <- switch(metric,
                      "tp" = list(title = "True Positive (TP)", desc = "Correctly predicted the event occurred",
                                  viz = make_matrix(green, white, white, white), 
                                  formula = if(show_nums) paste0("TP = ", fc$hits) else "TP"),
                      "fp" = list(title = "False Positive (FP)", desc = "Incorrectly predicted event (false alarm)",
                                  viz = make_matrix(white, red, white, white), 
                                  formula = if(show_nums) paste0("FP = ", fc$false_alarms) else "FP"),
                      "tn" = list(title = "True Negative (TN)", desc = "Correctly predicted no event",
                                  viz = make_matrix(white, white, white, green), 
                                  formula = if(show_nums) paste0("TN = ", fc$correct_negatives) else "TN"),
                      "fn" = list(title = "False Negative (FN)", desc = "Missed the event",
                                  viz = make_matrix(white, white, red, white), 
                                  formula = if(show_nums) paste0("FN = ", fc$misses) else "FN"),
                      "tpr" = list(title = "True Positive Rate (TPR / Sensitivity)", 
                                   desc = "Proportion of actual events correctly identified",
                                   viz = make_fraction(
                                     make_matrix(green, white, white, white),
                                     make_matrix(green, white, red, white),
                                     if(show_nums) paste0(fc$hits, " / ", fc$n_event, " = ", round(fc$TPR, 3)) else round(fc$TPR, 3)),
                                   formula = "TPR = TP / (TP + FN)"),
                      "fpr" = list(title = "False Positive Rate (FPR)", 
                                   desc = "Proportion of non-events incorrectly identified as events",
                                   viz = make_fraction(
                                     make_matrix(white, red, white, white),
                                     make_matrix(white, red, white, green),
                                     if(show_nums) paste0(fc$false_alarms, " / ", fc$n_nonevent, " = ", round(fc$FPR, 3)) else round(fc$FPR, 3)),
                                   formula = "FPR = FP / (FP + TN)"),
                      "tnr" = list(title = "True Negative Rate (TNR / Specificity)", 
                                   desc = "Proportion of non-events correctly identified",
                                   viz = make_fraction(
                                     make_matrix(white, white, white, green),
                                     make_matrix(white, red, white, green),
                                     if(show_nums) paste0(fc$correct_negatives, " / ", fc$n_nonevent, " = ", round(1-fc$FPR, 3)) else round(1-fc$FPR, 3)),
                                   formula = "TNR = TN / (TN + FP)"),
                      "precision" = list(title = "Precision (Positive Predictive Value)", 
                                         desc = "Proportion of positive predictions that were correct",
                                         viz = make_fraction(
                                           make_matrix(green, white, white, white),
                                           make_matrix(green, red, white, white),
                                           if(show_nums) paste0(fc$hits, " / ", fc$hits + fc$false_alarms, " = ", round(fc$hits/(fc$hits + fc$false_alarms), 3)) 
                                           else round(fc$hits/(fc$hits + fc$false_alarms), 3)),
                                         formula = "Precision = TP / (TP + FP)"),
                      "fdr" = list(title = "False Discovery Rate (FDR)", 
                                   desc = "Proportion of positive predictions that were wrong",
                                   viz = make_fraction(
                                     make_matrix(white, red, white, white),
                                     make_matrix(green, red, white, white),
                                     if(show_nums) paste0(fc$false_alarms, " / ", fc$hits + fc$false_alarms, " = ", round(fc$false_alarms/(fc$hits + fc$false_alarms), 3))
                                     else round(fc$false_alarms/(fc$hits + fc$false_alarms), 3)),
                                   formula = "FDR = FP / (FP + TP)"),
                      "lrplus" = list(title = "Likelihood Ratio Positive (LR+)", 
                                      desc = "How much a positive prediction increases odds of event",
                                      viz = make_fraction(
                                        div(style = "text-align: center;",
                                            div("TPR:", style = "font-weight: bold; margin-bottom: 10px;"),
                                            div(class = "fraction-container", style = "display: inline-block;",
                                                make_matrix(green, white, white, white),
                                                div(class = "fraction-line"),
                                                make_matrix(green, white, red, white))),
                                        div(style = "text-align: center;",
                                            div("FPR:", style = "font-weight: bold; margin-bottom: 10px;"),
                                            div(class = "fraction-container", style = "display: inline-block;",
                                                make_matrix(white, red, white, white),
                                                div(class = "fraction-line"),
                                                make_matrix(white, red, white, green))),
                                        if(show_nums) paste0(round(fc$TPR, 3), " / ", round(fc$FPR, 3), " = ", round(fc$LR_pos, 2))
                                        else round(fc$LR_pos, 2)),
                                      formula = "LR+ = TPR / FPR"),
                      "pss" = list(title = "Peirce Skill Score (PSS / True Skill Statistic)", 
                                   desc = "Overall discrimination ability (ranges from -1 to 1)",
                                   viz = div(style = "text-align: center; margin: 40px 0;",
                                             div(class = "fraction-container",
                                                 make_matrix(green, white, white, white),
                                                 div(class = "fraction-line"),
                                                 make_matrix(green, white, red, white),
                                                 div(style = "margin-top: 10px; font-weight: bold;", 
                                                     if(show_nums) paste0(fc$hits, " / ", fc$n_event, " = ", round(fc$TPR, 3)) else paste0("TPR = ", round(fc$TPR, 3)))),
                                             span(style = "font-size: 40px; font-weight: bold; margin: 0 20px; vertical-align: middle;", "−"),
                                             div(class = "fraction-container",
                                                 make_matrix(white, red, white, white),
                                                 div(class = "fraction-line"),
                                                 make_matrix(white, red, white, green),
                                                 div(style = "margin-top: 10px; font-weight: bold;", 
                                                     if(show_nums) paste0(fc$false_alarms, " / ", fc$n_nonevent, " = ", round(fc$FPR, 3)) else paste0("FPR = ", round(fc$FPR, 3)))),
                                             span(class = "equals-sign", "="),
                                             span(class = "result-box", round(fc$PSS, 3))),
                                   formula = "PSS = TPR − FPR")
    )
    
    tagList(
      h4(content$title, style = "color: #2c3e50; margin-top: 20px;"),
      p(content$desc, style = "font-size: 16px; color: #555;"),
      div(style = "text-align: center; margin: 30px 0;", 
          if(metric %in% c("tp", "fp", "tn", "fn")) {
            tagList(content$viz, div(style = "margin-top: 20px; font-size: 20px; font-weight: bold;", content$formula))
          } else {
            tagList(div(style = "font-weight: bold; font-size: 18px; margin-bottom: 20px;", content$formula),
                    content$viz)
          }),
      p(style = "margin-top: 30px; padding: 15px; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
        strong("Note: "), "These calculations use Method 1's current values. Adjust the sliders to see how metrics change."))
  })
}

shinyApp(ui, server)