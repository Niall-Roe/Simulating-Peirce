library(shiny)
library(shinyjs)




# Instructions:
#   
#   This should give a way let people play with the binomial expansion. it shoudl let them create tables like Peirce's, and see how they correspond /relate to a binomial distribution. 
# 

#More context:
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

#End instructions. 


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
      h3("Example 14"),

      p("Suppose we had an immense granary filled with black and white balls well mixed up; and we know that a man is going to take out a sample of a hundred. ",
        span(class = "example-trigger", id = "ex14-trigger",
             onclick = "Shiny.setInputValue('toggle_ex14', Math.random());",
             "[Click to see interactive example]")
      ),

      div(id = "example-14", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_ex14, { shinyjs::toggle("example-14") })
}

shinyApp(ui = ui, server = server)
