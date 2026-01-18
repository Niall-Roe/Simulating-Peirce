library(shiny)
library(shinyjs)

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
    "))
  ),

  div(class = "article-container",
      h3("Example 11"),

      p("In short, to express the proper state of our belief, not one number but two are requisite, the first depending on the inferred probability, the second on the amount of knowledge on which that probability is based. ",
        span(class = "example-trigger", id = "ex11-trigger",
             onclick = "Shiny.setInputValue('toggle_ex11', Math.random());",
             "[Click to see interactive example]")
      ),

      div(id = "example-11", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex11, { shinyjs::toggle("example-11") })
}

shinyApp(ui = ui, server = server)
