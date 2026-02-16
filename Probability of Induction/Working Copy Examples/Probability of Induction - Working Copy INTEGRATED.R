library(shiny)
library(shinyjs)

# ============================================================================
# HELPER FUNCTIONS (from all example files)
# ============================================================================

# From ex14.R - Binomial coefficient
binom_coef <- function(n, k) {
  choose(n, k)
}

# From ex14.R - Calculate total outcomes
calculate_total_outcomes <- function(white_weight, black_weight, n_draws) {
  (white_weight + black_weight)^n_draws
}

# From ex14.R - Calculate frequency for k white balls
calculate_frequency <- function(n_draws, k_white, white_weight, black_weight) {
  binom_coef(n_draws, k_white) * (white_weight^k_white) * (black_weight^(n_draws - k_white))
}

# From ex14.R and ex19.R - Pluralization
pluralize_ball <- function(n) {
  if (n == 1) return("ball")
  return("balls")
}

# From ex14.R and ex19.R - Number words
number_word <- function(n) {
  words <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
             "eight", "nine", "ten")
  if (n >= 0 && n <= 10) return(words[n + 1])
  return(as.character(n))
}

# From ex19.R - Decimal to fraction conversion
decimal_to_fraction <- function(x, max_denom = 1000) {
  common_fractions <- c(
    "1/2" = 0.5, "1/3" = 1/3, "2/3" = 2/3,
    "1/4" = 0.25, "3/4" = 0.75,
    "1/5" = 0.2, "2/5" = 0.4, "3/5" = 0.6, "4/5" = 0.8,
    "1/10" = 0.1, "1/100" = 0.01, "1/1000" = 0.001
  )

  for (frac_name in names(common_fractions)) {
    if (abs(x - common_fractions[frac_name]) < 0.001) {
      parts <- strsplit(frac_name, "/")[[1]]
      return(list(num = as.numeric(parts[1]), den = as.numeric(parts[2])))
    }
  }

  num_1000 <- round(x * 1000)
  if (abs(x - num_1000/1000) < 0.001) {
    return(list(num = num_1000, den = 1000))
  }

  for (den in c(100, 500, 200, 250)) {
    num <- round(x * den)
    if (abs(x - num/den) < 0.001) {
      return(list(num = num, den = den))
    }
  }

  return(list(num = num_1000, den = 1000))
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  useShinyjs(),
  withMathJax(),

  tags$head(
    tags$style(HTML("
      body { background-color: #f4f1ea; font-family: 'Georgia', serif; color: #2c2c2c; }
      .article-container {
        max-width: 1100px;
        margin: 50px auto;
        padding: 60px;
        background-color: #ffffff;
        box-shadow: 0 10px 25px rgba(0,0,0,0.1);
        line-height: 1.8;
      }
      h3 { text-align: center; font-weight: bold; text-transform: uppercase; letter-spacing: 2px; margin-bottom: 5px; }
      h4 { text-align: center; font-variant: small-caps; color: #555; margin-bottom: 30px; }
      .section-num { text-align: center; font-weight: bold; margin: 40px 0 20px 0; font-size: 1.3em; }
      p { margin-bottom: 1.5em; text-align: justify; }
      .math-formula { text-align: center; font-family: 'Times New Roman', serif; margin: 20px 0; font-size: 1.1em; }
      .table-container {
        font-family: 'Courier New', monospace;
        background-color: #f9f9f9;
        padding: 20px;
        border: 1px solid #ddd;
        margin: 20px 0;
        overflow-x: auto;
        white-space: pre;
      }
      .syllogism { margin-left: 50px; border-left: 3px solid #eee; padding-left: 20px; font-style: italic; }
      .footnote { font-size: 0.9em; border-top: 1px solid #ccc; padding-top: 20px; margin-top: 50px; color: #666; }

      /* Integrated examples - blue */
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

      /* Non-integrated examples - yellow */
      .example-trigger-yellow {
        background-color: #fff3cd;
        border-left: 3px solid #ffc107;
        padding: 2px 6px;
        cursor: pointer;
        transition: all 0.2s;
        border-radius: 2px;
      }
      .example-trigger-yellow:hover {
        background-color: #ffe69c;
        border-left-color: #ff9800;
      }

      .example-container {
        margin: 25px 0;
        padding: 25px;
        background-color: #f8f9fa;
        border: 2px solid #dee2e6;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }

      /* Styles from ex4, ex5, ex14, ex16, ex19 */
      .mode-tabs {
        display: flex;
        gap: 10px;
        margin-bottom: 20px;
      }
      .mode-tab {
        padding: 10px 20px;
        background-color: #e9ecef;
        border: 2px solid #dee2e6;
        border-radius: 6px;
        cursor: pointer;
        font-weight: 600;
        transition: all 0.2s;
      }
      .mode-tab.active {
        background-color: #2c7fb8;
        color: white;
        border-color: #2c7fb8;
      }
      .preset-button {
        margin: 5px;
        padding: 8px 15px;
        background-color: #e9ecef;
        border: 2px solid #dee2e6;
        border-radius: 5px;
        cursor: pointer;
        transition: all 0.2s;
        display: inline-block;
      }
      .preset-button:hover {
        background-color: #2c7fb8;
        color: white;
        border-color: #2c7fb8;
      }
      .hl-antecedent { background-color: rgba(255, 243, 205, 0.8); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent { background-color: rgba(144, 238, 144, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-a { background-color: rgba(255, 182, 193, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-consequent-b { background-color: rgba(173, 216, 230, 0.7); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .hl-both { background-color: rgba(221, 160, 221, 0.5); padding: 2px 4px; border-radius: 2px; font-weight: 500; }
      .key-insight {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .independence-note {
        background-color: #d1ecf1;
        border-left: 4px solid #17a2b8;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .formula-box {
        text-align: center;
        font-size: 1.2em;
        margin: 20px 0;
        padding: 15px;
        background-color: #f0f0f0;
        border-radius: 6px;
        font-weight: bold;
      }
      .click-info {
        padding: 15px;
        background-color: #fff3cd;
        border: 1px solid #ffc107;
        border-radius: 5px;
        margin-top: 15px;
        font-size: 0.95em;
      }
      .sequence-box {
        display: inline-block;
        padding: 3px 6px;
        margin: 2px;
        background-color: #e9ecef;
        border: 1px solid #adb5bd;
        border-radius: 3px;
        font-family: monospace;
        font-size: 0.9em;
      }
      .group-box {
        padding: 15px;
        margin: 10px 0;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        background-color: #ffffff;
      }
      .result-box {
        padding: 20px;
        margin-top: 20px;
        background-color: #e7f3ff;
        border: 2px solid #2c7fb8;
        border-radius: 8px;
      }
    "))
  ),

  div(class = "article-container",
      h3("ILLUSTRATIONS OF THE LOGIC OF SCIENCE."),
      h4("By C. S. PEIRCE, Assistant in the United States Coast Survey."),
      h4("FOURTH PAPER. — THE PROBABILITY OF INDUCTION."),

      div(class = "section-num", "I."),
      p("We have found that every argument derives its force from the general truth of the class of inferences to which it belongs; and that probability is the proportion of arguments carrying truth with them among those of any ", em("genus"), ". This is most conveniently expressed in the nomenclature of the mediæval logicians. They called the fact expressed by a premise an ", em("antecedent"), ", and that which follows from it its ", em("consequent"), "; while the leading principle, that every (or almost every) such antecedent is followed by such a ", em("consequent"), ", they termed the ", em("consequence"), ". Using this language, we may say that probability belongs exclusively to consequences, and the probability of any consequence is the number of times in which antecedent and consequent both occur divided by the number of all the times in which the antecedent occurs. ",
        span(class = "example-trigger-yellow", id = "ex1-trigger",
             onclick = "Shiny.setInputValue('toggle_ex1', Math.random());",
             "From this definition are deduced the following rules for the addition and multiplication of probabilities:")
      ),

      div(id = "example-1", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      p(span(class = "example-trigger-yellow", id = "ex2-trigger",
             onclick = "Shiny.setInputValue('toggle_ex2', Math.random());",
             strong("Rule for the Addition of Probabilities."), " — Given the separate probabilities of two consequences having the same antecedent and incompatible consequents. Then the sum of these two numbers is the probability of the consequence, that from the same antecedent one or other of those consequents follows.")
      ),

      div(id = "example-2", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      p(span(class = "example-trigger-yellow", id = "ex3-trigger",
             onclick = "Shiny.setInputValue('toggle_ex3', Math.random());",
             strong("Rule for the Multiplication of Probabilities."), " — Given the separate probabilities of the two consequences, \"If A then B,\" and \"If both A and B, then C.\" Then the product of these two numbers is the probability of the consequence, \"If A, then both B and C.\"")
      ),

      div(id = "example-3", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      # ==== EXAMPLE 4: INTEGRATED ====
      p(span(class = "example-trigger", id = "ex4-trigger",
             onclick = "Shiny.setInputValue('toggle_ex4', Math.random());",
             strong("Special Rule for the Multiplication of Independent Probabilities."), " — Given the separate probabilities of two consequences having the same antecedents, \"If A, then B,\" and \"If A, then C.\" Suppose that these consequences are such that the probability of the second is equal to the probability of the consequence, \"If both A and B, then C.\" Then the product of the two given numbers is equal to the probability of the consequence, \"If A, then both B and C.\"")
      ),

      div(id = "example-4", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Independent Probabilities"),

          p(strong("Independence"), " means that knowing B occurred doesn't change the probability of C:"),
          tags$ul(
            tags$li("P(A → C) = P(A∧B → C)  ", em("(C's probability is the same whether or not B occurred)")),
            tags$li("When this holds, events B and C are ", strong("independent")),
            tags$li("Then: P(A → B∧C) = P(A → B) × P(A → C)")
          ),

          div(class = "formula-box",
              "If P(A → C) = P(A∧B → C), then:",
              br(),
              "P(A → B∧C) = P(A → B) × P(A → C)"
          ),

          div(class = "independence-note",
              strong("Why does independence matter? "),
              "When B and C are independent, we can use the simpler formula above. When they're ",
              em("not"), " independent, we need the general multiplication rule from Example 3."
          ),

          hr(),

          div(class = "mode-tabs",
              div(class = "mode-tab active", id = "ex4-theoretical-tab",
                  onclick = "Shiny.setInputValue('ex4_mode', 'theoretical', {priority: 'event'});",
                  "Theoretical Space"),
              div(class = "mode-tab", id = "ex4-empirical-tab",
                  onclick = "Shiny.setInputValue('ex4_mode', 'empirical', {priority: 'event'});",
                  "Long-Run Frequency")
          ),

          uiOutput("ex4_content")
      ),

      p("To show the working of these rules we may examine the probabilities in regard to throwing dice. What is the probability of throwing a six with one die? The antecedent here is the event of throwing a die; the consequent, its turning up a six. As the die has six sides, all of which are turned up with equal frequency, the probability of turning up any one is 1/6. Suppose two dice are thrown, what is the probability of throwing sixes? The probability of either coming up six is obviously the same when both are thrown as when one is thrown — namely, 1/6. The probability that either will come up six when the other does is also the same as that of its coming up six whether the other does or not. The probabilities are, therefore, independent; and, by our rule, the probability that both events will happen together is the product of their several probabilities, 1/6 × 1/6. What is the probability of throwing deuce-ace? The probability that the first die will turn up ace and the second deuce is the same as the probability that both will turn up sixes — namely, 1/36; the probability that the second will turn up ace and the first deuce is likewise 1/36. These two events — first, ace; second, deuce; and, second, ace; first, deuce — are incompatible. Hence the rule for addition holds, and the probability that either will come up ace and the other deuce is 1/36 + 1/36 or 1/18."),

      # ==== EXAMPLE 5: INTEGRATED ====
      p(span(class = "example-trigger", id = "ex5-trigger",
             onclick = "Shiny.setInputValue('toggle_ex5', Math.random());",
             "In this way all problems about dice, etc., may be solved."), " When the number of dice thrown is supposed very large, mathematics (which may be defined as the art of making groups to facilitate numeration) comes to our aid with certain devices to reduce the difficulties."),

      div(id = "example-5", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Working with Dice Probabilities"),

          p("Explore classic dice probability problems using the rules we've learned. Select a preset or customize your own scenario."),

          div(class = "key-insight",
              strong("Important Note: "),
              "The problems we are solving here are theoretical ones. The answers to those problems give us expectations about empirical long-run frequencies, assuming our theoretical assumptions hold. But we solve the problems of dice with the mathematics, not by rolling them 10,000 times—just like we solve geometry problems with trigonometry, not a protractor."
          ),

          div(style = "margin: 20px 0; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
              h5("Peirce's Examples:"),
              div(
                actionButton("preset_single_six", "Single die shows 6", class = "btn-sm"),
                actionButton("preset_double_sixes", "Both dice show 6", class = "btn-sm"),
                actionButton("preset_deuce_ace", "Deuce-Ace (2 & 1)", class = "btn-sm")
              ),
              h5("Additional Examples:", style = "margin-top: 15px;"),
              div(
                actionButton("preset_sum_seven", "Dice sum to 7", class = "btn-sm"),
                actionButton("preset_at_least_one_six", "At least one 6", class = "btn-sm"),
                actionButton("preset_both_even", "Both dice even", class = "btn-sm"),
                actionButton("preset_doubles", "Any doubles", class = "btn-sm")
              )
          ),

          hr(),

          div(class = "mode-tabs",
              div(class = "mode-tab active", id = "ex5-theoretical-tab",
                  onclick = "Shiny.setInputValue('ex5_mode', 'theoretical', {priority: 'event'});",
                  "Theoretical Space"),
              div(class = "mode-tab", id = "ex5-empirical-tab",
                  onclick = "Shiny.setInputValue('ex5_mode', 'empirical', {priority: 'event'});",
                  "Long-Run Frequency")
          ),

          uiOutput("ex5_content")
      ),

      div(class = "section-num", "II."),
      p("The conception of probability as a matter of fact, i. e., as the proportion of times in which an occurrence of one kind is accompanied by an occurrence of another kind, is termed by Mr. Venn the ", em("materialistic"), " view of the subject. But probability has often been regarded as being simply the degree of belief which ought to attach to a proposition; and this mode of explaining the idea is termed by Venn the ", em("conceptualistic"), " view. Most writers have mixed the two conceptions together. They, first, define the probability of an event as the reason we have to believe that it has taken place, which is conceptualistic; but shortly after they state that it is the ratio of the number of cases favorable to the event to the total number of cases favorable or contrary, and all equally possible. Except that this introduces the thoroughly unclear idea of cases equally possible in place of cases equally frequent, this is a tolerable statement of the materialistic view. The pure conceptualistic theory has been best expounded by Mr. De Morgan in his \"Formal Logic: or, the Calculus of Inference, Necessary and Probable.\""),

      p("The great difference between the two analyses is, that the conceptualists refer probability to an event, while the materialists make it the ratio of frequency of events of a species to those of a genus over that species, thus giving it two terms instead of one. The opposition may be made to appear as follows:"),

      p(span(class = "example-trigger-yellow", id = "ex6-trigger",
             onclick = "Shiny.setInputValue('toggle_ex6', Math.random());",
             "Suppose that we have two rules of inference, such that, of all the questions to the solution of which both can be applied, the first yields correct answers to 81/100 and incorrect answers to the remaining 19/100; while the second yields correct answers to 93/100 and incorrect answers to the remaining 7/100. Suppose, further, that the two rules are entirely independent as to their truth, so that the second answers correctly 93/100 of the questions which the first answers correctly, and also 93/100 of the questions which the first answers incorrectly. Then, of all the questions to the solution of which both rules can be applied—")
      ),

      div(id = "example-6", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      div(class = "math-formula",
          "both answer correctly: 93/100 of 81/100 or (93 × 81) / (100 × 100);", br(),
          "the second answers correctly and the first incorrectly: 93/100 of 19/100 or (93 × 19) / (100 × 100);", br(),
          "the second answers incorrectly and the first correctly: 7/100 of 81/100 or (7 × 81) / (100 × 100);", br(),
          "and both answer incorrectly: 7/100 of 19/100 or (7 × 19) / (100 × 100)."
      ),

      p(span(class = "example-trigger-yellow", id = "ex7-trigger",
             onclick = "Shiny.setInputValue('toggle_ex7', Math.random());",
             "Suppose, now, that, in reference to any question, both give the same answer. Then those in reference to which their answers agree are the same as those which both answer correctly together with those which both answer falsely, or 93 × 81 / (100 × 100) + 7 × 19 / (100 × 100) of all. The proportion of those which both answer correctly out of those their answers to which agree is, therefore—")
      ),

      div(id = "example-7", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      div(class = "math-formula", "(93 × 81) / ((93 × 81) + (7 × 19))"),

      p("This is, therefore, the probability that, if both modes of inference yield the same result, that result is correct. We may here conveniently make use of another mode of expression. Probability is the ratio of the favorable cases to all the cases. Instead of expressing our result in terms of this ratio, we may make use of another — the ratio of favorable to unfavorable cases. ",
        span(class = "example-trigger-yellow", id = "ex8-trigger",
             onclick = "Shiny.setInputValue('toggle_ex8', Math.random());",
             "This last ratio may be called the ", em("chance"), " of an event."), " Then the chance of a true answer by the first mode of inference is 81/19 and by the second is 93/7 and the chance of a correct answer from both, when they agree, is 81/19 × 93/7, or the product of the chances of each singly yielding a true answer."),

      div(id = "example-8", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      p("It will be seen that a chance is a quantity which may have any magnitude, however great. An event in whose favor there is an even chance, or 1/1, has a probability of 1/2. An argument having an even chance can do nothing toward reënforcing others, since according to the rule its combination with another would only multiply the chance of the latter by 1."),

      p("Probability and chance undoubtedly belong primarily to consequences, and are relative to premises; but we may, nevertheless, speak of the chance of an event absolutely, meaning by that the chance of the combination of all arguments in reference to it which exist for us in the given state of our knowledge. Any quantity which varies with the chance might, therefore, it would seem, serve as a thermometer for the proper intensity of belief. Among all such quantities there is one which is peculiarly appropriate. When there is a very great chance, the feeling of belief ought to be very intense. Absolute certainty, or an infinite chance, can never be attained by mortals, and this may be represented appropriately by an infinite belief. As the chance diminishes the feeling of believing should diminish, until an even chance is reached, where it should completely vanish. Now, there is one quantity which fulfills these conditions; it is the logarithm of the chance."),

      p("There is a general law of sensibility, called Fechner's psychophysical law. It is that the intensity of any sensation is proportional to the logarithm of the external force which produces it. It is entirely in harmony with this law that the feeling of belief should be as the logarithm of the chance, this latter being the expression of the state of facts which produces the belief."),

      p("The rule for the combination of independent concurrent arguments takes a very simple form when expressed in terms of the intensity of belief, measured in the proposed way. It is this: Take the sum of all the feelings of belief which would be produced separately by all the arguments pro, subtract from that the similar sum for arguments con, and the remainder is the feeling of belief which we ought to have on the whole. ",
        span(class = "example-trigger-yellow", id = "ex9-trigger",
             onclick = "Shiny.setInputValue('toggle_ex9', Math.random());",
             "This is a proceeding which men often resort to, under the name of balancing reasons.")
      ),

      div(id = "example-9", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      p(span(class = "example-trigger-yellow", id = "ex10-trigger",
             onclick = "Shiny.setInputValue('toggle_ex10', Math.random());",
             "Suppose we have a large bag of beans from which one has been secretly taken at random and hidden under a thimble. We are now to form a probable judgment of the color of that bean, by drawing others singly from the bag. Suppose the first drawing is white and the next black. We conclude that there is not an immense preponderance of either color, and that there is something like an even chance that the bean under the thimble is black. When we have drawn a thousand times, if about half have been white, we have great confidence in this result.")
      ),

      div(id = "example-10", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      p("Now, as the whole utility of probability is to insure us in the long run, it follows that we ought not to have the same feeling of belief in reference to all events of which the chance is even. ",
        span(class = "example-trigger-yellow", id = "ex11-trigger",
             onclick = "Shiny.setInputValue('toggle_ex11', Math.random());",
             "In short, to express the proper state of our belief, not one number but two are requisite, the first depending on the inferred probability, the second on the amount of knowledge on which that probability is based."), " When our knowledge is very slight, this second number may be even more important than the probability itself; and when we have no knowledge at all this completely overwhelms the other, so that there is no sense in saying that the chance of the totally unknown event is even. We thus perceive that the conceptualistic view is quite inadequate."),

      div(id = "example-11", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      p(span(class = "example-trigger-yellow", id = "ex12-trigger",
             onclick = "Shiny.setInputValue('toggle_ex12', Math.random());",
             "Suppose that we are totally ignorant what colored hair the inhabitants of Saturn have. Let us take a color-chart in which all possible colors are shown shading into one another. In such a chart the relative areas occupied by different classes of colors are perfectly arbitrary. Let us inclose such an area and ask what is the chance that the color of the hair of the inhabitants of Saturn falls within that area? According to conceptualistic principles, the answer can only be one-half, since the judgment should neither favor nor oppose the hypothesis. What is true of this area is true of any other; and it will equally be true of a third area which embraces the other two. But the probability for each of the smaller areas being one-half, that for the larger should be at least unity, which is absurd.")
      ),

      div(id = "example-12", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      div(class = "section-num", "III."),
      p("All our reasonings are of two kinds: 1. Explicative, analytic, or deductive; 2. Amplificative, synthetic, or (loosely speaking) inductive. In explicative reasoning, certain facts are first laid down in the premises. These facts are, in every case, an inexhaustible multitude. Such a statement will be the conclusion of an analytic inference. Of this sort are all mathematical demonstrations. But synthetic reasoning is of another kind. In this case the facts summed up in the conclusion are not among those stated in the premises. They are different facts, as when one sees that the tide rises ", em("m"), " times and concludes that it will rise the next time. These are the only inferences which increase our real knowledge."),

      p("In any problem in probabilities, we have given the relative frequency of certain events, and we perceive that in these facts the relative frequency of another event is given in a hidden way. This is therefore mere explicative reasoning, and is evidently entirely inadequate to the representation of synthetic reasoning. ",
        span(class = "example-trigger-yellow", id = "ex13-trigger",
             onclick = "Shiny.setInputValue('toggle_ex13', Math.random());",
             "Most treatises on probability contain a very different doctrine."), " They state, for example, that if one had seen the tide rise, say ", em("m"), " times, he could know that there was a probability equal to (m + 1) / (m + 2) that it would rise the next time. But this solution betrays its origin if we apply it to the case in which the man has never seen the tide rise at all; that is, if we put m = 0. In this case, the probability comes out 1/2, involving the conceptualistic principle that there is an even chance of a totally unknown event. But this principle is absurd."),

      div(id = "example-13", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      # ==== EXAMPLE 14: INTEGRATED ====
      p("If there be any way of enumerating the possibilities of Nature so as to make them all equal, it is the following: ",
        span(class = "example-trigger", id = "ex14-trigger",
             onclick = "Shiny.setInputValue('toggle_ex14', Math.random());",
             "Suppose we had an immense granary filled with black and white balls well mixed up; and suppose each urn were filled by taking a fixed number of balls from this granary quite at random. The relative number of white balls might be anything, say one in three. In this way, we should have a distribution like that shown in the following table, where w stands for a white ball and b for a black one:")
      ),

      div(id = "example-14", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Binomial Expansion"),

          p("Explore how the binomial expansion determines the relative frequencies of different outcomes when drawing balls from an urn. The expansion ",
            withMathJax("$$(w + b)^n$$"),
            " gives us all possible outcomes and their frequencies."),

          fluidRow(
            column(4,
              sliderInput("ex14_white_weight", "White balls ratio (w):",
                         min = 1, max = 10, value = 1, step = 1),
              sliderInput("ex14_black_weight", "Black balls ratio (b):",
                         min = 1, max = 10, value = 2, step = 1),
              sliderInput("ex14_n_draws", "Number of draws (n):",
                         min = 2, max = 8, value = 4, step = 1),
              hr(),
              checkboxInput("ex14_show_sequences", "Show individual sequences", FALSE),
              hr(),
              actionButton("ex14_reset", "Reset to Peirce's example", class = "btn-primary")
            ),
            column(8,
              uiOutput("ex14_summary"),
              plotOutput("ex14_plot", height = "400px", click = "ex14_plot_click"),
              uiOutput("ex14_click_text")
            )
          ),

          hr(),

          uiOutput("ex14_table")
      ),

      div(class = "table-container",
          "wwww.
wwwb. wwwb. wwbw. wwbw. wbww. wbww. bwww. bwww.
wwbb. bbww.
wbwb. bwwb. wbbw. bwbw.
wwbb. bbww.
wbwb. bwwb. wbbw. bwbw.
wwbb. bbww.
wbwb. bwwb. wbbw. bwbw.
wwbb. bbww.
wbwb. bwwb. wbbw. bwbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb.
bbbb."
      ),

      p("In the second group, where there is one ", em("b"), ", there are two sets just alike; in the third there are 4, in the fourth 8, and in the fifth 16, doubling every time. This is because we have supposed twice as many black balls in the granary as white ones. Now suppose two balls were drawn from one of these urns and were found to be both white, what would be the probability of the next one being white? By inspecting the table, the reader can see that in each group all orders occur with equal frequency. ",
        span(class = "example-trigger-yellow", id = "ex15-trigger",
             onclick = "Shiny.setInputValue('toggle_ex15', Math.random());",
             "Hence the colors of the balls already drawn have no influence on the probability of any other being white or black.")
      ),

      div(id = "example-15", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      # ==== EXAMPLE 16: INTEGRATED ====
      p("The assumption that any such thing can be done leads simply to the conclusion that reasoning from past to future experience is absolutely worthless. ",
        span(class = "example-trigger", id = "ex16-trigger",
             onclick = "Shiny.setInputValue('toggle_ex16', Math.random());",
             "In fact, the moment that you assume that the chances in favor of that of which we are totally ignorant are even, the problem about the tides does not differ, in any arithmetical particular, from the case in which a penny (known to be equally likely to come up heads and tails) should turn up heads m times successively. In short, it would be to assume that Nature is a pure chaos, or chance combination of independent elements, in which reasoning from one fact to another would be impossible; and since, as we shall hereafter see, there is no judgment of pure observation without reasoning, it would be to suppose all human cognition illusory and no real knowledge possible."), " It would be to suppose that if we have found the order of Nature more or less regular in the past, this has been by a pure run of luck which we may expect is now at an end. Now, it may be we have no scintilla of proof to the contrary, but reason is unnecessary in reference to that belief which is of all the most settled, which nobody doubts or can doubt, and which he who should deny would stultify himself in so doing."),

      div(id = "example-16", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Testing if a Difference is Real or Due to Chance"),

          p("This example demonstrates Peirce's method for determining whether an observed difference is ",
            strong("real (systematic)"), " or merely ", strong("due to chance"),
            ". The key question: Can the observed difference be attributed to random variation?"),

          p("Peirce's approach: Calculate the probable error for each group, then check whether the observed difference falls ",
            strong("within"), " or ", strong("beyond"), " the error intervals. If the difference falls beyond even the largest error interval (4.77e), ",
            "it indicates a real, systematic difference rather than chance variation."),

          p("Using the formula ", withMathJax("$$e = c \\times \\sqrt{\\frac{2p(1-p)}{s}}$$"),
            " (where ", em("c"), " depends on the confidence level) we calculate the probable error, then compare the observed difference against multiples of this error."),

          fluidRow(
            column(12,
              sliderInput("ex16_p_assumed", "Assumed true proportion (p) for calculation:",
                         min = 0.001, max = 0.999, value = 0.5, step = 0.01),
              p(em("Note: Peirce assumes p = 1/2 for this calculation, as it represents the natural proportion before any difference."))
            )
          ),

          hr(),

          fluidRow(
            column(6,
              div(class = "group-box",
                h5(strong("Group 1")),
                numericInput("ex16_p1", "Observed proportion (p₁):",
                            value = 0.5082, min = 0.001, max = 0.999, step = 0.0001),
                numericInput("ex16_n1", "Sample size (n₁):",
                            value = 1000000, min = 1, max = 10000000, step = 1)
              )
            ),
            column(6,
              div(class = "group-box",
                h5(strong("Group 2")),
                numericInput("ex16_p2", "Observed proportion (p₂):",
                            value = 0.4977, min = 0.001, max = 0.999, step = 0.0001),
                numericInput("ex16_n2", "Sample size (n₂):",
                            value = 150000, min = 1, max = 10000000, step = 1)
              )
            )
          ),

          hr(),

          fluidRow(
            column(3,
              actionButton("ex16_reset", "Reset to 1870 Census", class = "btn-primary btn-block")
            ),
            column(3,
              actionButton("ex16_chance", "Example: Due to Chance", class = "btn-warning btn-block")
            ),
            column(3,
              actionButton("ex16_tea", "Lady Tasting Tea", class = "btn-info btn-block")
            ),
            column(3,
              checkboxInput("ex16_rescale", "Rescale chart (zoom)", TRUE)
            )
          ),

          hr(),

          h5("Combined Comparison"),
          checkboxInput("ex16_show_errors", "Show probable error bounds", TRUE),
          conditionalPanel(
            condition = "input.ex16_show_errors == true",
            sliderInput("ex16_confidence", "Confidence level (%):",
                       min = 50, max = 100, value = 50, step = 1)
          ),
          plotOutput("ex16_combined_plot", height = "350px"),

          uiOutput("ex16_odds_display"),

          hr(),

          uiOutput("ex16_results")
      ),

      div(class = "section-num", "IV."),
      p("We have examined the problem proposed by the conceptualists: Given a synthetic conclusion, required to know out of all possible states of things how many will accord with it. We have found that it is only an absurd attempt to reduce synthetic to analytic reason. But there is another problem: Given a certain state of things, required to know what proportion of all synthetic inferences relating to it will be true within a given degree of approximation. There is no difficulty about this problem; the answer is perfectly well known."),

      p(span(class = "example-trigger-yellow", id = "ex17-trigger",
             onclick = "Shiny.setInputValue('toggle_ex17', Math.random());",
             "It is found that, if the true proportion of white balls is ", em("p"), ", and ", em("s"), " balls are drawn, then the error of the proportion obtained by the induction will be—")
      ),

      div(id = "example-17", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      div(class = "math-formula",
          "half the time within 0.477", br(),
          "9 times out of 10 within 1.163", br(),
          "99 times out of 100 within 1.821", br(),
          "999 times out of 1,000 within 2.328", br(),
          "9,999 times out of 10,000 within 2.751", br(),
          "9,999,999,999 times out of 10,000,000,000 within 4.77"
      ),
      p("multiplied by the square-root of 2p(1-p)/s."),

      p(span(class = "example-trigger-yellow", id = "ex18-trigger",
             onclick = "Shiny.setInputValue('toggle_ex18', Math.random());",
             "The use of this may be illustrated by an example. By the census of 1870, it appears that the proportion of males among native white children under one year old was 0.5082, while among colored children of the same age the proportion was only 0.4977. The difference between these is 0.0105, or about one in a 100. Can this be attributed to chance, or would the difference always exist among a great number of white and colored children under like circumstances? Here ", em("p"), " may be taken at 1/2; hence 2p(1-p) is also 1/2. The number of white children counted was near 1,000,000; hence the fraction whose square-root is to be taken is about 1/2,000,000. The root is about 1/1,400, and this multiplied by 0.477 gives about 0.0003 as the probable error in the ratio of males among the whites as obtained from the induction. The number of black children was about 150,000, which gives 0.0008 for the probable error. We see that the actual discrepancy is ten times the sum of these, and such a result would happen, according to our table, only once out of 10,000,000,000 censuses, in the long run.")
      ),

      div(id = "example-18", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      # ==== EXAMPLE 19: INTEGRATED ====
      p(span(class = "example-trigger", id = "ex19-trigger",
             onclick = "Shiny.setInputValue('toggle_ex19', Math.random());",
             "It may be remarked that when the real value of the probability sought inductively is either very large or very small, the reasoning is more secure. Thus, suppose there were in reality one white ball in 100 in a certain urn, and we were to judge of the number by 100 drawings. The probability of drawing no white ball would be 366/1000; that of drawing one white ball would be 370/1000; that of drawing two would be 185/1000; that of drawing three would be 61/1000; that of drawing four would be 15/1000; that of drawing five would be only 3/1000, etc. Thus we should be tolerably certain of not being in error by more than one ball in 100.")
      ),

      div(id = "example-19", class = "example-container", style = "display: none;",
          h4("Interactive Demonstration: Binomial Distribution and Extreme Probabilities"),

          p("Explore how the shape of the sampling distribution changes when the true proportion is very small or very large. Click on any bar to see the exact probability. Try exploring how the size of the confidence interval changes when p is an extreme value as compared to when it is closer to 0.5."),

          fluidRow(
            column(4,
              sliderInput("ex19_p", "True proportion (p):",
                         min = 0.001, max = 0.999, value = 0.01, step = 0.001),
              sliderInput("ex19_s", "Sample size (s):",
                         min = 5, max = 500, value = 100, step = 1),
              checkboxInput("ex19_rescale", "Rescale chart (zoom to ±5σ)", TRUE),
              checkboxInput("ex19_xaxis_balls", "X-axis: Number of balls", FALSE),
              hr(),
              checkboxInput("ex19_show_prediction", "Show prediction interval (true p)", TRUE),
              conditionalPanel(
                condition = "input.ex19_show_prediction == true",
                sliderInput("ex19_pred_cl", "Prediction confidence level:",
                           min = 0.50, max = 0.99, value = 0.50, step = 0.01)
              ),
              hr(),
              checkboxInput("ex19_show_ci", "Show confidence interval (observed)", TRUE),
              conditionalPanel(
                condition = "input.ex19_show_ci == true",
                sliderInput("ex19_cl", "Confidence level:",
                           min = 0.50, max = 0.99, value = 0.50, step = 0.01)
              ),
              hr(),
              actionButton("ex19_reset", "Reset conditions", class = "btn-primary")
            ),
            column(8,
              plotOutput("ex19_plot", height = "450px", click = "ex19_plot_click"),
              uiOutput("ex19_click_text")
            )
          )
      ),

      p("It appears, then, that in one sense we can, and in another we cannot, determine the probability of synthetic inference."),

      p("When I reason in this way:"),
      div(class = "syllogism",
          p("Ninety-nine Cretans in a hundred are liars;"),
          p("But Epimenides is a Cretan;"),
          p("Therefore, Epimenides is a liar:—")
      ),
      p(span(class = "example-trigger-yellow", id = "ex20-trigger",
             onclick = "Shiny.setInputValue('toggle_ex20', Math.random());",
             "I know that reasoning similar to that would carry truth 99 times in 100."), " But when I reason in the opposite direction:"),

      div(id = "example-20", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      div(class = "syllogism",
          p("Minos, Sarpedon, Rhadamanthus, Deucalion, and Epimenides, are all the Cretans I can think of;"),
          p("But these were all atrocious liars,"),
          p("Therefore, pretty much all Cretans must have been liars;")
      ),
      p(span(class = "example-trigger-yellow", id = "ex21-trigger",
             onclick = "Shiny.setInputValue('toggle_ex21', Math.random());",
             "I do not in the least know how often such reasoning would carry me right."), " On the other hand, what I do know is that some definite proportion of Cretans must have been liars, and that this proportion can be probably approximated to by an induction from five or six instances. Even in the worst case for the probability of such an inference, that in which about half the Cretans are liars, the ratio so obtained would probably not be in error by more than 1/6. So much I know; but, then, in the present case the inference is that pretty much all Cretans are liars, and whether there may not be a special improbability in that I do not know."),

      div(id = "example-21", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Coming soon!")
      ),

      div(class = "section-num", "V."),
      p("Late in the last century, Immanuel Kant asked the question, \"How are synthetical judgments a priori possible?\" By synthetical judgments he meant such as assert positive fact and are not mere affairs of arrangement; in short, judgments of the kind which synthetical reasoning produces, and which analytic reasoning cannot yield. By a priori judgments he meant such as that all outward objects are in space, every event has a cause, etc., propositions which according to him can never be inferred from experience. Not so much by his answer to this question as by the mere asking of it, the current philosophy of that time was shattered and destroyed, and a new epoch in its history was begun. But before asking that question he ought to have asked the more general one, \"How are any synthetical judgments at all possible?\" How is it that a man can observe one fact and straightway pronounce judgment concerning another different fact not involved in the first?"),

      p("Such reasoning, as we have seen, has, at least in the usual sense of the phrase, no definite probability; how, then, can it add to our knowledge? This is a strange paradox; the Abbé Gratry says it is a miracle, and that every true induction is an immediate inspiration from on high. I respect this explanation far more than many a pedantic attempt to solve the question by some juggle with probabilities, with the forms of syllogism, or what not. I respect it because it shows an appreciation of the depth of the problem, because it assigns an adequate cause, and because it is intimately connected—as the true account should be—with a general philosophy of the universe. At the same time, I do not accept this explanation, because an explanation should tell how a thing is done, and to assert a perpetual miracle seems to be an abandonment of all hope of doing that, without sufficient justification."),

      p("It will be interesting to see how the answer which Kant gave to his question about synthetical judgments a priori will appear if extended to the question of synthetical judgments in general. That answer is, that synthetical judgments a priori are possible because whatever is universally true is involved in the conditions of experience. Let us apply this to a general synthetical reasoning. I take from a bag a handful of beans; they are all purple, and I infer that all the beans in the bag are purple. How can I do that? Why, upon the principle that whatever is universally true of my experience (which is here the appearance of these different beans) is involved in the condition of experience. The condition of this special experience is that all these beans were taken from that bag. According to Kant's principle, then, whatever is found true of all the beans drawn from the bag must find its explanation in some peculiarity of the contents of the bag. This is a satisfactory statement of the principle of induction."),

      p("When we draw a deductive or analytic conclusion, our rule of inference is that facts of a certain general character are either invariably or in a certain proportion of cases accompanied by facts of another general character. Then our premise being a fact of the former class, we infer with certainty or with the appropriate degree of probability the existence of a fact of the second class. But the rule for synthetic inference is of a different kind. When we sample a bag of beans we do not in the least assume that the fact of some beans being purple involves the necessity or even the probability of other beans being so. On the contrary, the conceptualistic method of treating probabilities, which really amounts simply to the deductive treatment of them, when rightly carried out leads to the result that a synthetic inference has just an even chance in its favor, or in other words is absolutely worthless. The color of one bean is entirely independent of that of another. But synthetic inference is founded upon a classification of facts, not according to their characters, but according to the manner of obtaining them. Its rule is, that a number of facts obtained in a given way will in general more or less resemble other facts obtained in the same way; or, experiences whose conditions are the same will have the same general characters."),

      p("In the former case, we know that premises precisely similar in form to those of the given ones will yield true conclusions, just once in a calculable number of times. In the latter case, we only know that premises obtained under circumstances similar to the given ones (though perhaps themselves very different) will yield true conclusions, at least once in a calculable number of times. We may express this by saying that in the case of analytic inference we know the probability of our conclusion (if the premises are true), but in the case of synthetic inferences we only know the degree of trustworthiness of our proceeding. As all knowledge comes from synthetic inference, we must equally infer that all human certainty consists merely in our knowing that the processes by which our knowledge has been derived are such as must generally have led to true conclusions."),

      p("Though a synthetic inference cannot by any means be reduced to deduction, yet that the rule of induction will hold good in the long run may be deduced from the principle that reality is only the object of the final opinion to which sufficient investigation would lead. That belief gradually tends to fix itself under the influence of inquiry is, indeed, one of the facts with which logic sets out."),

      div(class = "footnote",
          p("1. Strictly we should need an infinite series of numbers each depending on the probable error of the last."),
          p("2. \"Perfect indecision, belief inclining neither way, an even chance.\" — De Morgan, p. 182."),
          p("3. Logique. The same is true, according to him, of every performance of a differentiation, but not of integration. He does not tell us whether it is the supernatural assistance which makes the former process so much the easier.")
      )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  # Toggle functions for all examples
  observeEvent(input$toggle_ex1, { shinyjs::toggle("example-1") })
  observeEvent(input$toggle_ex2, { shinyjs::toggle("example-2") })
  observeEvent(input$toggle_ex3, { shinyjs::toggle("example-3") })
  observeEvent(input$toggle_ex4, { shinyjs::toggle("example-4") })
  observeEvent(input$toggle_ex5, { shinyjs::toggle("example-5") })
  observeEvent(input$toggle_ex6, { shinyjs::toggle("example-6") })
  observeEvent(input$toggle_ex7, { shinyjs::toggle("example-7") })
  observeEvent(input$toggle_ex8, { shinyjs::toggle("example-8") })
  observeEvent(input$toggle_ex9, { shinyjs::toggle("example-9") })
  observeEvent(input$toggle_ex10, { shinyjs::toggle("example-10") })
  observeEvent(input$toggle_ex11, { shinyjs::toggle("example-11") })
  observeEvent(input$toggle_ex12, { shinyjs::toggle("example-12") })
  observeEvent(input$toggle_ex13, { shinyjs::toggle("example-13") })
  observeEvent(input$toggle_ex14, { shinyjs::toggle("example-14") })
  observeEvent(input$toggle_ex15, { shinyjs::toggle("example-15") })
  observeEvent(input$toggle_ex16, { shinyjs::toggle("example-16") })
  observeEvent(input$toggle_ex17, { shinyjs::toggle("example-17") })
  observeEvent(input$toggle_ex18, { shinyjs::toggle("example-18") })
  observeEvent(input$toggle_ex19, { shinyjs::toggle("example-19") })
  observeEvent(input$toggle_ex20, { shinyjs::toggle("example-20") })
  observeEvent(input$toggle_ex21, { shinyjs::toggle("example-21") })

  # ========================================================================
  # EXAMPLE 4: Independent Probabilities
  # ========================================================================

  # Helper function to check if die roll matches consequent
  die_matches <- function(roll, cons) {
    if (cons == "even") {
      return(roll %% 2 == 0)
    } else if (cons == "odd") {
      return(roll %% 2 == 1)
    } else if (cons == "gt4") {
      return(roll > 4)
    } else {
      return(roll == as.numeric(cons))
    }
  }

  # Helper function to check if card matches consequent
  card_matches <- function(rank, suit, cons, antecedent) {
    if (cons == "ace") return(rank == 1)
    if (cons == "king") return(rank == 13)
    if (cons == "queen") return(rank == 12)
    if (cons == "jack") return(rank == 11)
    if (cons == "heart") return(suit == 2)
    if (cons == "spade") return(suit == 1)
    if (cons == "red") return(suit %in% c(2, 3))
    if (cons == "even") return(rank %in% c(2, 4, 6, 8, 10))
    if (cons == "better_than_9") return(rank %in% c(1, 10, 11, 12, 13))
    if (cons == "face") return(rank %in% c(11, 12, 13))
    return(FALSE)
  }

  # Mode state for example 4
  ex4_current_mode <- reactiveVal("theoretical")

  # Cumulative trial tracking for example 4
  ex4_cumulative_trials <- reactiveVal(0)
  ex4_cumulative_successes1 <- reactiveVal(0)
  ex4_cumulative_successes2 <- reactiveVal(0)
  ex4_cumulative_successes_both <- reactiveVal(0)
  ex4_all_frequencies1 <- reactiveVal(numeric(0))
  ex4_all_frequencies2 <- reactiveVal(numeric(0))
  ex4_all_frequencies_both <- reactiveVal(numeric(0))
  ex4_used_10000 <- reactiveVal(FALSE)

  observeEvent(input$ex4_mode, {
    ex4_current_mode(input$ex4_mode)
    runjs("$('.mode-tab').removeClass('active');")
    if (input$ex4_mode == "theoretical") {
      runjs("$('#ex4-theoretical-tab').addClass('active');")
      if (!is.null(input$ex4_antecedent_emp)) {
        updateSelectInput(session, "ex4_antecedent", selected = input$ex4_antecedent_emp)
      }
      if (!is.null(input$ex4_cons1_emp)) {
        updateSelectInput(session, "ex4_cons1", selected = input$ex4_cons1_emp)
      }
      if (!is.null(input$ex4_cons2_emp)) {
        updateSelectInput(session, "ex4_cons2", selected = input$ex4_cons2_emp)
      }
    } else {
      runjs("$('#ex4-empirical-tab').addClass('active');")
      if (!is.null(input$ex4_antecedent)) {
        updateSelectInput(session, "ex4_antecedent_emp", selected = input$ex4_antecedent)
      }
      if (!is.null(input$ex4_cons1)) {
        updateSelectInput(session, "ex4_cons1_emp", selected = input$ex4_cons1)
      }
      if (!is.null(input$ex4_cons2)) {
        updateSelectInput(session, "ex4_cons2_emp", selected = input$ex4_cons2)
      }
    }
  })

  observeEvent(input$ex4_reset, {
    ex4_cumulative_trials(0)
    ex4_cumulative_successes1(0)
    ex4_cumulative_successes2(0)
    ex4_cumulative_successes_both(0)
    ex4_all_frequencies1(numeric(0))
    ex4_all_frequencies2(numeric(0))
    ex4_all_frequencies_both(numeric(0))
    ex4_used_10000(FALSE)
  })

  output$ex4_content <- renderUI({
    mode <- ex4_current_mode()

    if (mode == "theoretical") {
      div(
        uiOutput("ex4_independence_message"),

        fluidRow(
          column(4,
                 h5("Setup:"),

                 selectInput("ex4_antecedent",
                            span(class = "hl-antecedent", "Antecedent:"),
                            choices = c(
                              "Roll a standard die" = "die",
                              "Draw from a well shuffled standard deck" = "standard",
                              "Draw from a well shuffled Piquet Pack (7-A)" = "piquet"
                            ),
                            selected = "standard"),

                 uiOutput("ex4_cons1_ui"),
                 uiOutput("ex4_cons2_ui"),

                 hr(),
                 uiOutput("ex4_theoretical_calc")
          ),

          column(8,
                 plotOutput("ex4_theoretical_plot", height = "500px")
          )
        )
      )
    } else {
      div(
        p(class = "key-insight",
          strong("Empirical View: "),
          "Run trials to see how the long-run frequencies converge. For independent events, P(Both) ≈ P(B) × P(C)."
        ),

        uiOutput("ex4_independence_message_emp"),
        uiOutput("ex4_empirical_formula_display"),

        fluidRow(
          column(4,
                 h5("Setup:"),

                 selectInput("ex4_antecedent_emp",
                            span(class = "hl-antecedent", "Antecedent:"),
                            choices = c(
                              "Roll a standard die" = "die",
                              "Draw from a well shuffled standard deck" = "standard",
                              "Draw from a well shuffled Piquet Pack (7-A)" = "piquet"
                            ),
                            selected = "standard"),

                 uiOutput("ex4_cons1_ui_emp"),
                 uiOutput("ex4_cons2_ui_emp"),

                 hr(),
                 actionButton("ex4_run_sim", "Run 10 More Trials",
                             class = "btn-primary btn-lg",
                             style = "width: 100%; margin-bottom: 10px;"),
                 actionButton("ex4_run_10000", "Run 10,000 Trials",
                             class = "btn-info btn-lg",
                             style = "width: 100%; margin-bottom: 10px;"),
                 actionButton("ex4_reset", "Reset",
                             class = "btn-warning btn-lg",
                             style = "width: 100%;"),
                 hr(),
                 uiOutput("ex4_empirical_summary")
          ),

          column(8,
                 plotOutput("ex4_empirical_plot", height = "450px")
          )
        )
      )
    }
  })

  output$ex4_independence_message <- renderUI({
    req(input$ex4_antecedent, input$ex4_cons1, input$ex4_cons2)

    p(class = "key-insight",
      strong("Theoretical View: "),
      "The ",
      span(class = "hl-consequent-a", "pink cells"),
      " and ",
      span(class = "hl-consequent-b", "blue cells"),
      " show outcomes where each consequent occurs. The ",
      span(class = "hl-both", "purple cells"),
      " show where both occur. For independent events, the purple region is proportional to the product of the individual probabilities."
    )
  })

  output$ex4_independence_message_emp <- renderUI({
    req(input$ex4_antecedent_emp, input$ex4_cons1_emp, input$ex4_cons2_emp)

    if (ex4_cumulative_trials() > 0) {
      p(class = "independence-note",
        strong("Independence Check: "),
        "The purple line (both) should converge to the product of the pink and blue lines' theoretical values, demonstrating independence."
      )
    } else {
      NULL
    }
  })

  output$ex4_cons1_ui <- renderUI({
    req(input$ex4_antecedent)

    if (input$ex4_antecedent == "die") {
      selectInput("ex4_cons1",
                 span(class = "hl-consequent-a", "First consequent:"),
                 choices = c("Shows a 6" = "6",
                           "Shows a 1" = "1",
                           "Shows a 5" = "5",
                           "Shows an even number" = "even",
                           "Shows an odd number" = "odd",
                           "Greater than 4" = "gt4"),
                 selected = "even")
    } else {
      selectInput("ex4_cons1",
                 span(class = "hl-consequent-a", "First consequent:"),
                 choices = c("Card is an Ace" = "ace",
                           "Card is a King" = "king",
                           "Card is a Heart" = "heart",
                           "Card is red" = "red",
                           "Card is even (2,4,6,8,10)" = "even",
                           "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                           "Card is a face card (J,Q,K)" = "face"),
                 selected = "red")
    }
  })

  output$ex4_cons2_ui <- renderUI({
    req(input$ex4_antecedent)

    if (input$ex4_antecedent == "die") {
      selectInput("ex4_cons2",
                 span(class = "hl-consequent-b", "Second consequent:"),
                 choices = c("Shows a 2" = "2",
                           "Shows a 3" = "3",
                           "Shows a 4" = "4",
                           "Shows an even number" = "even",
                           "Shows an odd number" = "odd",
                           "Greater than 4" = "gt4"),
                 selected = "gt4")
    } else {
      selectInput("ex4_cons2",
                 span(class = "hl-consequent-b", "Second consequent:"),
                 choices = c("Card is a Queen" = "queen",
                           "Card is a Jack" = "jack",
                           "Card is a Spade" = "spade",
                           "Card is red" = "red",
                           "Card is even (2,4,6,8,10)" = "even",
                           "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                           "Card is a face card (J,Q,K)" = "face"),
                 selected = "face")
    }
  })

  output$ex4_cons1_ui_emp <- renderUI({
    req(input$ex4_antecedent_emp)

    if (input$ex4_antecedent_emp == "die") {
      selectInput("ex4_cons1_emp",
                 span(class = "hl-consequent-a", "First consequent:"),
                 choices = c("Shows a 6" = "6",
                           "Shows a 1" = "1",
                           "Shows a 5" = "5",
                           "Shows an even number" = "even",
                           "Shows an odd number" = "odd",
                           "Greater than 4" = "gt4"),
                 selected = "even")
    } else {
      selectInput("ex4_cons1_emp",
                 span(class = "hl-consequent-a", "First consequent:"),
                 choices = c("Card is an Ace" = "ace",
                           "Card is a King" = "king",
                           "Card is a Heart" = "heart",
                           "Card is red" = "red",
                           "Card is even (2,4,6,8,10)" = "even",
                           "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                           "Card is a face card (J,Q,K)" = "face"),
                 selected = "red")
    }
  })

  output$ex4_cons2_ui_emp <- renderUI({
    req(input$ex4_antecedent_emp)

    if (input$ex4_antecedent_emp == "die") {
      selectInput("ex4_cons2_emp",
                 span(class = "hl-consequent-b", "Second consequent:"),
                 choices = c("Shows a 2" = "2",
                           "Shows a 3" = "3",
                           "Shows a 4" = "4",
                           "Shows an even number" = "even",
                           "Shows an odd number" = "odd",
                           "Greater than 4" = "gt4"),
                 selected = "gt4")
    } else {
      selectInput("ex4_cons2_emp",
                 span(class = "hl-consequent-b", "Second consequent:"),
                 choices = c("Card is a Queen" = "queen",
                           "Card is a Jack" = "jack",
                           "Card is a Spade" = "spade",
                           "Card is red" = "red",
                           "Card is even (2,4,6,8,10)" = "even",
                           "Card is better than 9 (10,J,Q,K,A)" = "better_than_9",
                           "Card is a face card (J,Q,K)" = "face"),
                 selected = "face")
    }
  })

  # Due to length constraints, I'll need to continue this in a second part...
  # This file is getting very large. Let me create a note about what's missing.

  # THE FOLLOWING SERVER LOGIC STILL NEEDS TO BE ADDED:
  # - Rest of Example 4 server logic (theoretical plot, calc, empirical sim)
  # - Example 5 complete server logic
  # - Example 14 complete server logic
  # - Example 16 complete server logic
  # - Example 19 complete server logic

  # For now, adding placeholder to make file work
  output$ex4_theoretical_plot <- renderPlot({
    plot(1, 1, main = "Example 4 - Full logic coming in next iteration")
  })
}

shinyApp(ui = ui, server = server)
