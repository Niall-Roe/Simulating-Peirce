library(shiny)

#use as reference for example 16 (dice). The values are already set for the "use urn" toggle. 
#when putting into example 16, update the style to match other examples. 
#ignore the. "integrated over bins" option. 

ui <- fluidPage(
  titlePanel("Binomial sampling, p-hat, and confidence intervals"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("p", "True proportion p",
                  min = 0.001, max = 0.999, value = 0.2, step = 0.001),
      sliderInput("s", "Sample size s",
                  min = 5, max = 500, value = 50, step = 1),
      sliderInput("cl", "Confidence level",
                  min = 0.50, max = 0.99, value = 0.90, step = 0.01),
      
      checkboxInput("show_normal", "Show normal approximation", TRUE),
      
      radioButtons("normal_mode",
                   "Normal approximation type",
                   choices = c("Midpoint density" = "mid",
                               "Integrated over bins" = "int"),
                   selected = "int"),
      
      hr(),
      
      actionButton("dice", "Use 1-in-100 urn example")
    ),
    
    mainPanel(
      plotOutput("distPlot", height = "450px"),
      verbatimTextOutput("ciText")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$dice, {
    updateSliderInput(session, "p", value = 0.01)
    updateSliderInput(session, "s", value = 100)
    updateSliderInput(session, "cl", value = 0.50)
  })
  
  output$distPlot <- renderPlot({
    
    p  <- input$p
    s  <- input$s
    cl <- input$cl
    
    # Discrete support
    k <- 0:s
    phat <- k / s
    delta <- 1 / s
    
    # Exact binomial distribution
    probs <- dbinom(k, size = s, prob = p)
    
    # Plot binomial distribution
    plot(phat, probs,
         type = "h", lwd = 3,
         xlim = c(0, 1),
         ylim = c(0, max(probs) * 1.3),
         xlab = expression(hat(p)),
         ylab = "Probability mass",
         main = "Sampling distribution of p-hat")
    
    points(phat, probs, pch = 16)
    
    # Normal approximation
    if (input$show_normal) {
      
      mu <- p
      sigma <- sqrt(p * (1 - p) / s)
      
      if (input$normal_mode == "mid") {
        # Midpoint approximation (scaled density)
        x <- seq(0, 1, length.out = 1000)
        lines(x,
              dnorm(x, mean = mu, sd = sigma) * delta,
              col = "blue", lwd = 2)
        
      } else {
        # Integrated normal over bins
        norm_probs <- pnorm(phat + delta / 2, mu, sigma) -
          pnorm(phat - delta / 2, mu, sigma)
        
        lines(phat, norm_probs,
              type = "h", col = "blue", lwd = 2)
        points(phat, norm_probs, col = "blue", pch = 1)
      }
      
      legend("topright",
             legend = c("Exact binomial",
                        "Normal approximation"),
             lwd = c(3, 2),
             col = c("black", "blue"),
             bty = "n")
    }
    
    # Mark true p
    abline(v = p, lty = 2, lwd = 2)
    
    # Exact binomial confidence interval
    alpha <- 1 - cl
    ci <- binom.test(round(p * s), s, conf.level = cl)$conf.int
    
    segments(ci[1], 0, ci[2], 0, lwd = 4, col = "red")
    points(ci, c(0, 0), pch = 16, col = "red")
    
  })
  
  output$ciText <- renderText({
    p  <- input$p
    s  <- input$s
    cl <- input$cl
    
    ci <- binom.test(round(p * s), s, conf.level = cl)$conf.int
    
    paste0(
      "Exact binomial confidence interval for p\n",
      "Confidence level: ", cl, "\n",
      "Lower bound: ", round(ci[1], 4), "\n",
      "Upper bound: ", round(ci[2], 4)
    )
  })
}

shinyApp(ui = ui, server = server)
