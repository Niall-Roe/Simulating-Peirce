library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(tidyr)

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Georgia', serif;
        line-height: 1.8;
        max-width: 900px;
        margin: 0 auto;
        padding: 20px;
        font-size: 16px;
      }

      h3 {
        text-align: center;
        font-variant: small-caps;
        margin-top: 40px;
        margin-bottom: 30px;
      }

      h4 {
        text-align: center;
        font-weight: normal;
        margin-bottom: 10px;
      }

      .section-number {
        text-align: center;
        font-size: 1.2em;
        margin: 40px 0 20px 0;
      }

      .example-trigger {
        display: inline-block;
        background-color: #e8f4f8;
        border-left: 3px solid #2c7fb8;
        padding: 2px 6px;
        cursor: pointer;
        transition: background-color 0.2s;
      }

      .example-trigger:hover {
        background-color: #d0e8f2;
      }

      .example-container {
        margin: 20px 0;
        padding: 20px;
        background-color: #f9f9f9;
        border: 1px solid #ddd;
        border-radius: 5px;
      }

      .example-header {
        font-weight: bold;
        margin-bottom: 15px;
        color: #2c7fb8;
        cursor: pointer;
        user-select: none;
      }

      .example-header:before {
        content: '▼ ';
        display: inline-block;
        transition: transform 0.2s;
      }

      .example-header.collapsed:before {
        content: '▶ ';
      }

      .example-content {
        margin-top: 15px;
      }

      .highlight-rule1 { background-color: #fee5d9; }
      .highlight-rule2 { background-color: #deebf7; }
      .highlight-combined { background-color: #d4e4c4; }
      .highlight-error-50 { background-color: #fee5d9; }
      .highlight-error-90 { background-color: #fcbba1; }
      .highlight-error-99 { background-color: #fc9272; }
      .highlight-error-999 { background-color: #fb6a4a; }
      .highlight-prob { background-color: #c7e9c0; }
      .highlight-chance { background-color: #fff7bc; }

      .small-caps {
        font-variant: small-caps;
      }

      .formula {
        text-align: center;
        margin: 15px 0;
        font-family: 'Courier New', monospace;
      }

      .indent {
        margin-left: 40px;
      }

      p {
        text-align: justify;
        margin-bottom: 15px;
      }

      .footnote {
        font-size: 0.9em;
        margin-top: 40px;
        border-top: 1px solid #ccc;
        padding-top: 20px;
      }
    "))
  ),

  div(
    h3("ILLUSTRATIONS OF THE LOGIC OF SCIENCE."),
    h4("By C. S. PEIRCE,"),
    h4("ASSISTANT IN THE UNITED STATES COAST SURVEY."),
    h4("FOURTH PAPER.—THE PROBABILITY OF INDUCTION."),

    div(class = "section-number", "I."),

    p("WE have found that every argument derives its force from the general truth of the class of inferences to which it belongs; and that probability is the proportion of arguments carrying truth with them among those of any ", em("genus"), ". This is most conveniently expressed in the nomenclature of the mediæval logicians. They called the fact expressed by a premise an ", em("antecedent"), ", and that which follows from it its consequent; while the leading principle, that every (or almost every) such antecedent is followed by such a ", em("consequent"), ", they termed the ", em("consequence"), ". Using this language, we may say that probability belongs exclusively to consequences, and the probability of any consequence is the number of times in which antecedent and consequent both occur divided by the number of all the times in which the antecedent occurs. From this definition are deduced the following rules for the addition and multiplication of probabilities:"),

    # Example 1: Rules of Probability
    div(id = "example-1-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_1', Math.random());",
        em("Rule for the Addition of Probabilities"), ".—Given the separate probabilities of two consequences having the same antecedent and incompatible consequents. Then the sum of these two numbers is the probability of the consequence, that from the same antecedent one or other of those consequents follows."
    ),
    div(id = "example-1", class = "example-container", style = "display: none;"),

    p(em("Rule for the Multiplication of Probabilities"), ".—Given the separate probabilities of the two consequences, \"If A then B,\" and \"If both A and B, then C.\" Then the product of these two numbers is the probability of the consequence, \"If A, then both B and C.\""),

    p(em("Special Rule for the Multiplication of Independent Probabilities"), ".—Given the separate probabilities of two consequences having the same antecedents, \"If A, then B,\" and \"If A, then C.\" Suppose that these consequences are such that the probability of the second is equal to the probability of the consequence, \"If both A and B, then C.\" Then the product of the two given numbers is equal to the probability of the consequence, \"If A, then both B and C.\""),

    # Example 2: Dice Probabilities
    p("To show the working of these rules we may examine the probabilities in regard to throwing dice. What is the probability of throwing a six with one die? The antecedent here is the event of throwing a die; the consequent, its turning up a six. As the die has six sides, all of which are turned up with equal frequency, the probability of turning up any one is ",
      span(class = "formula", "1/6"), ". "),

    div(id = "example-2-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_2', Math.random());",
        "Suppose two dice are thrown, what is the probability of throwing sixes? [...] What is the probability of throwing deuce-ace?"
    ),
    div(id = "example-2", class = "example-container", style = "display: none;"),

    p("In this way all problems about dice, etc., may be solved. When the number of dice thrown is supposed very large, mathematics (which may be defined as the art of making groups to facilitate numeration) comes to our aid with certain devices to reduce the difficulties."),

    div(class = "section-number", "II."),

    p("The conception of probability as a matter of ", em("fact"), ", i. e., as the proportion of times in which an occurrence of one kind is accompanied by an occurrence of another kind, is termed by Mr. Venn the materialistic view of the subject. But probability has often been regarded as being simply the degree of belief which ought to attach to a proposition; and this mode of explaining the idea is termed by Venn the conceptualistic view. Most writers have mixed the two conceptions together."),

    # Example 3: Two rules of inference
    div(id = "example-3-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_3', Math.random());",
        "Suppose that we have two rules of inference, such that, of all the questions to the solution of which both can be applied, the first yields correct answers to 81/100, and incorrect answers to the remaining 19/100; while the second yields correct answers to 93/100..."
    ),
    div(id = "example-3", class = "example-container", style = "display: none;"),

    # Example 4: Probability vs Chance
    p("We may here conveniently make use of another mode of expression. Probability is the ratio of the favorable cases to all the cases. Instead of expressing our result in terms of this ratio, we may make use of another—the ratio of favorable to unfavorable cases. This last ratio may be called the ",
      span(id = "chance-def", class = "example-trigger",
           onclick = "Shiny.setInputValue('show_example_4', Math.random());",
           em("chance")), " of an event."),
    div(id = "example-4", class = "example-container", style = "display: none;"),

    # Example 5: Logarithm of chance as belief intensity
    p("It will be seen that a chance is a quantity which may have any magnitude, however great. An event in whose favor there is an even chance, or 1/1, has a probability of 1/2."),

    div(id = "example-5-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_5', Math.random());",
        "Now, there is one quantity which, more simply than any other, fulfills these conditions; it is the logarithm of the chance."
    ),
    div(id = "example-5", class = "example-container", style = "display: none;"),

    # Example 6: Bean sampling and confidence
    div(id = "example-6-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_6', Math.random());",
        "Let us, then, consider for a moment the formation of a belief of probability. Suppose we have a large bag of beans from which one has been secretly taken at random and hidden under a thimble..."
    ),
    div(id = "example-6", class = "example-container", style = "display: none;"),

    # Example 7: Balancing reasons with beans
    p("In the conceptualistic view of probability, complete ignorance, where the judgment ought not to swerve either toward or away from the hypothesis, is represented by the probability 1/2."),

    div(id = "example-7-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_7', Math.random());",
        "Suppose that the first bean which we drew from our bag were black. That would constitute an argument, no matter how slender, that the bean under the thimble was also black..."
    ),
    div(id = "example-7", class = "example-container", style = "display: none;"),

    # Example 8: Saturn hair color paradox
    div(id = "example-8-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_8', Math.random());",
        "But let us suppose that we are totally ignorant what colored hair the inhabitants of Saturn have..."
    ),
    div(id = "example-8", class = "example-container", style = "display: none;"),

    div(class = "section-number", "III."),

    p("All our reasonings are of two kinds: 1. ", em("Explicative, analytic"), ", or ", em("deductive"), "; 2. ", em("Amplifiative, synthetic"), ", or (loosely speaking) ", em("inductive"), ". In explicative reasoning, certain facts are first laid down in the premises. These facts are, in every case, an inexhaustible multitude, but they may often be summed up in one simple proposition by means of some regularity which runs through them all."),

    # Example 9: Rule of succession
    div(id = "example-9-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_9', Math.random());",
        "Most treatises on probability contain a very different doctrine. They state, for example, that if one of the ancient denizens of the shores of the Mediterranean, who had never heard of tides, had gone to the bay of Biscay, and had there seen the tide rise, say m times, he could know that there was a probability equal to (m+1)/(m+2) that it would rise the next time."
    ),
    div(id = "example-9", class = "example-container", style = "display: none;"),

    # Example 10: Urn example
    div(id = "example-10-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_10', Math.random());",
        "But this solution betrays its origin if we apply it to the case in which the man has never seen the tide rise at all; that is, if we put m = 0..."
    ),
    div(id = "example-10", class = "example-container", style = "display: none;"),

    div(class = "section-number", "IV."),

    # Example 11: Two types of probability questions
    div(id = "example-11-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_11', Math.random());",
        "But there is another problem in connection with this subject. It is this: Given a certain state of things, required to know what proportion of all synthetic inferences relating to it will be true within a given degree of approximation."
    ),
    div(id = "example-11", class = "example-container", style = "display: none;"),

    # Example 12: Error bounds
    div(id = "example-12-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_12', Math.random());",
        "It is found that, if the true proportion of white balls is p, and s balls are drawn, then the error of the proportion obtained by the induction will be—"
    ),
    div(id = "example-12", class = "example-container", style = "display: none;"),

    # Example 13: Census application
    div(id = "example-13-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_13', Math.random());",
        "The use of this may be illustrated by an example. By the census of 1870, it appears that the proportion of males among native white children under one year old was 0.5082, while among colored children of the same age the proportion was only 0.4977."
    ),
    div(id = "example-13", class = "example-container", style = "display: none;"),

    div(class = "section-number", "V."),

    # Example 14: Deductive vs inductive probability
    div(id = "example-14-trigger", class = "example-trigger",
        onclick = "Shiny.setInputValue('show_example_14', Math.random());",
        "It appears, then, that in one sense we can, and in another we cannot, determine the probability of synthetic inference. When I reason in this way: 'Ninety-nine Cretans in a hundred are liars...' "
    ),
    div(id = "example-14", class = "example-container", style = "display: none;"),

    p("Late in the last century, Immanuel Kant asked the question, \"How are synthetical judgments ", em("a priori"), " possible?\" [...]"),

    p("Though a synthetic inference cannot by any means be reduced to deduction, yet that the rule of induction will hold good in the long run may be deduced from the principle that reality is only the object of the final opinion to which sufficient investigation would lead. That belief gradually tends to fix itself under the influence of inquiry is, indeed, one of the facts with which logic sets out.")
  )
)

# Server
server <- function(input, output, session) {

  # Example 1: Addition and multiplication rules
  observeEvent(input$show_example_1, {
    toggle("example-1")

    output$ex1_ui <- renderUI({
      div(
        div(class = "example-header", "Interactive Example: Rules of Probability"),
        div(class = "example-content",
            p(strong("Addition Rule:"), " When events are mutually exclusive (cannot both happen)"),
            sliderInput("ex1_p1", "P(Event 1):", min = 0, max = 1, value = 0.3, step = 0.05),
            sliderInput("ex1_p2", "P(Event 2):", min = 0, max = 1, value = 0.4, step = 0.05),
            plotOutput("ex1_plot", height = "250px"),
            verbatimTextOutput("ex1_calc")
        )
      )
    })

    insertUI(
      selector = "#example-1",
      ui = uiOutput("ex1_ui")
    )
  })

  output$ex1_plot <- renderPlot({
    req(input$ex1_p1, input$ex1_p2)

    df <- data.frame(
      Event = c("Event 1", "Event 2", "Event 1 OR Event 2", "Neither"),
      Probability = c(input$ex1_p1, input$ex1_p2,
                     min(1, input$ex1_p1 + input$ex1_p2),
                     max(0, 1 - input$ex1_p1 - input$ex1_p2))
    )
    df$Event <- factor(df$Event, levels = df$Event)

    ggplot(df, aes(x = Event, y = Probability, fill = Event)) +
      geom_col() +
      scale_fill_manual(values = c("steelblue", "darkgreen", "purple", "gray70")) +
      ylim(0, 1) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$ex1_calc <- renderText({
    paste0(
      "P(Event 1) = ", input$ex1_p1, "\n",
      "P(Event 2) = ", input$ex1_p2, "\n",
      "P(Event 1 OR Event 2) = ", input$ex1_p1, " + ", input$ex1_p2, " = ",
      round(input$ex1_p1 + input$ex1_p2, 3), "\n\n",
      if(input$ex1_p1 + input$ex1_p2 > 1) {
        "Note: Sum exceeds 1, so events are not mutually exclusive!"
      } else {
        "Events are mutually exclusive (no overlap)"
      }
    )
  })

  # Example 2: Dice probabilities
  observeEvent(input$show_example_2, {
    toggle("example-2")

    output$ex2_ui <- renderUI({
      div(
        div(class = "example-header", "Interactive Example: Dice Probabilities"),
        div(class = "example-content",
            fluidRow(
              column(6,
                     h5("Throwing Sixes"),
                     sliderInput("ex2_ndice_sixes", "Number of dice:",
                                min = 1, max = 4, value = 2, step = 1)
              ),
              column(6,
                     h5("Throwing Deuce-Ace"),
                     p("(One die shows 1, the other shows 2)")
              )
            ),
            plotOutput("ex2_plot", height = "300px"),
            verbatimTextOutput("ex2_calc")
        )
      )
    })

    insertUI(selector = "#example-2", ui = uiOutput("ex2_ui"))
  })

  output$ex2_plot <- renderPlot({
    req(input$ex2_ndice_sixes)

    # Calculate probability of all sixes
    p_all_sixes <- (1/6)^input$ex2_ndice_sixes

    # For 2 dice, calculate deuce-ace
    p_deuce_ace <- if(input$ex2_ndice_sixes == 2) 2 * (1/6) * (1/6) else NA

    outcomes <- data.frame(
      Outcome = c("All Sixes", "Deuce-Ace\n(2 dice only)", "Other"),
      Probability = c(
        p_all_sixes,
        ifelse(is.na(p_deuce_ace), 0, p_deuce_ace),
        1 - p_all_sixes - ifelse(is.na(p_deuce_ace), 0, p_deuce_ace)
      )
    )

    ggplot(outcomes, aes(x = Outcome, y = Probability, fill = Outcome)) +
      geom_col() +
      scale_fill_manual(values = c("darkred", "darkblue", "gray80")) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$ex2_calc <- renderText({
    p_all_sixes <- (1/6)^input$ex2_ndice_sixes

    result <- paste0(
      "All Sixes with ", input$ex2_ndice_sixes, " dice:\n",
      "P = (1/6)^", input$ex2_ndice_sixes, " = ", round(p_all_sixes, 6), "\n",
      "Or 1 in ", round(1/p_all_sixes), "\n\n"
    )

    if(input$ex2_ndice_sixes == 2) {
      p_deuce_ace <- 2 * (1/6) * (1/6)
      result <- paste0(result,
        "Deuce-Ace (2 dice):\n",
        "Ways: (1,2) or (2,1) = 2 ways\n",
        "P = 2 × (1/6) × (1/6) = ", round(p_deuce_ace, 6), "\n",
        "Or 1 in ", round(1/p_deuce_ace)
      )
    }

    result
  })

  # Example 3: Two rules of inference
  observeEvent(input$show_example_3, {
    toggle("example-3")

    output$ex3_ui <- renderUI({
      div(
        div(class = "example-header", "Interactive Example: Combining Independent Arguments"),
        div(class = "example-content",
            sliderInput("ex3_rule1", "Rule 1 correct (%):",
                       min = 50, max = 99, value = 81, step = 1),
            sliderInput("ex3_rule2", "Rule 2 correct (%):",
                       min = 50, max = 99, value = 93, step = 1),
            plotOutput("ex3_plot", height = "300px"),
            verbatimTextOutput("ex3_calc")
        )
      )
    })

    insertUI(selector = "#example-3", ui = uiOutput("ex3_ui"))
  })

  output$ex3_plot <- renderPlot({
    req(input$ex3_rule1, input$ex3_rule2)

    r1 <- input$ex3_rule1 / 100
    r2 <- input$ex3_rule2 / 100

    # All four combinations
    both_correct <- r1 * r2
    r1_correct_r2_wrong <- r1 * (1 - r2)
    r1_wrong_r2_correct <- (1 - r1) * r2
    both_wrong <- (1 - r1) * (1 - r2)

    # When they agree
    agree_total <- both_correct + both_wrong

    df <- data.frame(
      Outcome = c("Both\nCorrect", "R1 Correct\nR2 Wrong",
                 "R1 Wrong\nR2 Correct", "Both\nWrong"),
      Probability = c(both_correct, r1_correct_r2_wrong,
                     r1_wrong_r2_correct, both_wrong),
      Agree = c("Agree", "Disagree", "Disagree", "Agree")
    )

    ggplot(df, aes(x = Outcome, y = Probability, fill = Agree)) +
      geom_col() +
      scale_fill_manual(values = c("Agree" = "darkgreen", "Disagree" = "gray70")) +
      geom_text(aes(label = sprintf("%.3f", Probability)),
               vjust = -0.5, size = 4) +
      ylim(0, 1) +
      labs(title = "Distribution of Outcomes") +
      theme_minimal()
  })

  output$ex3_calc <- renderText({
    r1 <- input$ex3_rule1 / 100
    r2 <- input$ex3_rule2 / 100

    both_correct <- r1 * r2
    both_wrong <- (1 - r1) * (1 - r2)
    agree_total <- both_correct + both_wrong
    prob_correct_when_agree <- both_correct / agree_total

    # Chance notation
    chance1 <- r1 / (1 - r1)
    chance2 <- r2 / (1 - r2)
    combined_chance <- chance1 * chance2

    paste0(
      "When both rules agree:\n",
      "P(both correct) = ", round(both_correct, 4), "\n",
      "P(both wrong) = ", round(both_wrong, 4), "\n",
      "P(agree) = ", round(agree_total, 4), "\n\n",
      "P(correct | agree) = ", round(prob_correct_when_agree, 4),
      " (", round(prob_correct_when_agree * 100, 1), "%)\n\n",
      "Using chance notation:\n",
      "Chance₁ = ", round(chance1, 2), "\n",
      "Chance₂ = ", round(chance2, 2), "\n",
      "Combined chance = ", round(combined_chance, 2)
    )
  })

  # Continue with more examples...
  # [I'll add the remaining examples in the next section]

}

shinyApp(ui = ui, server = server)
