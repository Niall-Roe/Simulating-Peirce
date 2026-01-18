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
      h3("ILLUSTRATIONS OF THE LOGIC OF SCIENCE."),
      h4("By C. S. PEIRCE, Assistant in the United States Coast Survey."),
      h4("FOURTH PAPER. — THE PROBABILITY OF INDUCTION."),
      
      div(class = "section-num", "I."),
      p("We have found that every argument derives its force from the general truth of the class of inferences to which it belongs; and that probability is the proportion of arguments carrying truth with them among those of any ", em("genus"), ". This is most conveniently expressed in the nomenclature of the mediæval logicians. They called the fact expressed by a premise an ", em("antecedent"), ", and that which follows from it its ", em("consequent"), "; while the leading principle, that every (or almost every) such antecedent is followed by such a ", em("consequent"), ", they termed the ", em("consequence"), ". ",
        span(class = "example-trigger", id = "ex1-trigger",
             onclick = "Shiny.setInputValue('toggle_ex1', Math.random());",
             "Using this language, we may say that probability belongs exclusively to consequences, and the probability of any consequence is the number of times in which antecedent and consequent both occur divided by the number of all the times in which the antecedent occurs.")
      ),

      div(id = "example-1", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      p("From this definition are deduced the following rules for the addition and multiplication of probabilities:"),

      p(span(class = "example-trigger", id = "ex2-trigger",
             onclick = "Shiny.setInputValue('toggle_ex2', Math.random());",
             strong("Rule for the Addition of Probabilities."), " — Given the separate probabilities of two consequences having the same antecedent and incompatible consequents. Then the sum of these two numbers is the probability of the consequence, that from the same antecedent one or other of those consequents follows.")
      ),

      div(id = "example-2", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),

      p(span(class = "example-trigger", id = "ex3-trigger",
             onclick = "Shiny.setInputValue('toggle_ex3', Math.random());",
             strong("Rule for the Multiplication of Probabilities."), " — Given the separate probabilities of the two consequences, \"If A then B,\" and \"If both A and B, then C.\" Then the product of these two numbers is the probability of the consequence, \"If A, then both B and C.\"")
      ),

      div(id = "example-3", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),

      p(span(class = "example-trigger", id = "ex4-trigger",
             onclick = "Shiny.setInputValue('toggle_ex4', Math.random());",
             strong("Special Rule for the Multiplication of Independent Probabilities."), " — Given the separate probabilities of two consequences having the same antecedents, \"If A, then B,\" and \"If A, then C.\" Suppose that these consequences are such that the probability of the second is equal to the probability of the consequence, \"If both A and B, then C.\" Then the product of the two given numbers is equal to the probability of the consequence, \"If A, then both B and C.\"")
      ),

      div(id = "example-4", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      p("To show the working of these rules we may examine the probabilities in regard to throwing dice. What is the probability of throwing a six with one die? The antecedent here is the event of throwing a die; the consequent, its turning up a six. As the die has six sides, all of which are turned up with equal frequency, the probability of turning up any one is 1/6. Suppose two dice are thrown, what is the probability of throwing sixes? The probability of either coming up six is obviously the same when both are thrown as when one is thrown — namely, 1/6. The probability that either will come up six when the other does is also the same as that of its coming up six whether the other does or not. The probabilities are, therefore, independent; and, by our rule, the probability that both events will happen together is the product of their several probabilities, 1/6 × 1/6. What is the probability of throwing deuce-ace? The probability that the first die will turn up ace and the second deuce is the same as the probability that both will turn up sixes — namely, 1/36; the probability that the second will turn up ace and the first deuce is likewise 1/36. These two events — first, ace; second, deuce; and, second, ace; first, deuce — are incompatible. Hence the rule for addition holds, and the probability that either will come up ace and the other deuce is 1/36 + 1/36 or 1/18."),

      p(span(class = "example-trigger", id = "ex5-trigger",
             onclick = "Shiny.setInputValue('toggle_ex5', Math.random());",
             "In this way all problems about dice, etc., may be solved."), " When the number of dice thrown is supposed very large, mathematics (which may be defined as the art of making groups to facilitate numeration) comes to our aid with certain devices to reduce the difficulties."),

      div(id = "example-5", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      div(class = "section-num", "II."),
      p("The conception of probability as a matter of fact, i. e., as the proportion of times in which an occurrence of one kind is accompanied by an occurrence of another kind, is termed by Mr. Venn the ", em("materialistic"), " view of the subject. But probability has often been regarded as being simply the degree of belief which ought to attach to a proposition; and this mode of explaining the idea is termed by Venn the ", em("conceptualistic"), " view. Most writers have mixed the two conceptions together. They, first, define the probability of an event as the reason we have to believe that it has taken place, which is conceptualistic; but shortly after they state that it is the ratio of the number of cases favorable to the event to the total number of cases favorable or contrary, and all equally possible. Except that this introduces the thoroughly unclear idea of cases equally possible in place of cases equally frequent, this is a tolerable statement of the materialistic view. The pure conceptualistic theory has been best expounded by Mr. De Morgan in his \"Formal Logic: or, the Calculus of Inference, Necessary and Probable.\""),
      
      p("The great difference between the two analyses is, that the conceptualists refer probability to an event, while the materialists make it the ratio of frequency of events of a species to those of a genus over that species, thus giving it two terms instead of one. The opposition may be made to appear as follows:"),
      
      p(span(class = "example-trigger", id = "ex6-trigger",
             onclick = "Shiny.setInputValue('toggle_ex6', Math.random());",
             "Suppose that we have two rules of inference, such that, of all the questions to the solution of which both can be applied, the first yields correct answers to 81/100 and incorrect answers to the remaining 19/100; while the second yields correct answers to 93/100 and incorrect answers to the remaining 7/100. Suppose, further, that the two rules are entirely independent as to their truth, so that the second answers correctly 93/100 of the questions which the first answers correctly, and also 93/100 of the questions which the first answers incorrectly. Then, of all the questions to the solution of which both rules can be applied—")
      ),

      div(id = "example-6", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      div(class = "math-formula", 
          "both answer correctly: 93/100 of 81/100 or (93 × 81) / (100 × 100);", br(),
          "the second answers correctly and the first incorrectly: 93/100 of 19/100 or (93 × 19) / (100 × 100);", br(),
          "the second answers incorrectly and the first correctly: 7/100 of 81/100 or (7 × 81) / (100 × 100);", br(),
          "and both answer incorrectly: 7/100 of 19/100 or (7 × 19) / (100 × 100)."
      ),
      
      p(span(class = "example-trigger", id = "ex7-trigger",
             onclick = "Shiny.setInputValue('toggle_ex7', Math.random());",
             "Suppose, now, that, in reference to any question, both give the same answer. Then those in reference to which their answers agree are the same as those which both answer correctly together with those which both answer falsely, or 93 × 81 / (100 × 100) + 7 × 19 / (100 × 100) of all. The proportion of those which both answer correctly out of those their answers to which agree is, therefore—")
      ),

      div(id = "example-7", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      div(class = "math-formula", "(93 × 81) / ((93 × 81) + (7 × 19))"),
      
      p("This is, therefore, the probability that, if both modes of inference yield the same result, that result is correct. We may here conveniently make use of another mode of expression. Probability is the ratio of the favorable cases to all the cases. Instead of expressing our result in terms of this ratio, we may make use of another — the ratio of favorable to unfavorable cases. ",
        span(class = "example-trigger", id = "ex8-trigger",
             onclick = "Shiny.setInputValue('toggle_ex8', Math.random());",
             "This last ratio may be called the ", em("chance"), " of an event."), " Then the chance of a true answer by the first mode of inference is 81/19 and by the second is 93/7 and the chance of a correct answer from both, when they agree, is 81/19 × 93/7, or the product of the chances of each singly yielding a true answer."),

      div(id = "example-8", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      p("It will be seen that a chance is a quantity which may have any magnitude, however great. An event in whose favor there is an even chance, or 1/1, has a probability of 1/2. An argument having an even chance can do nothing toward reënforcing others, since according to the rule its combination with another would only multiply the chance of the latter by 1."),
      
      p("Probability and chance undoubtedly belong primarily to consequences, and are relative to premises; but we may, nevertheless, speak of the chance of an event absolutely, meaning by that the chance of the combination of all arguments in reference to it which exist for us in the given state of our knowledge. Any quantity which varies with the chance might, therefore, it would seem, serve as a thermometer for the proper intensity of belief. Among all such quantities there is one which is peculiarly appropriate. When there is a very great chance, the feeling of belief ought to be very intense. Absolute certainty, or an infinite chance, can never be attained by mortals, and this may be represented appropriately by an infinite belief. As the chance diminishes the feeling of believing should diminish, until an even chance is reached, where it should completely vanish. Now, there is one quantity which fulfills these conditions; it is the logarithm of the chance."),
      
      p("There is a general law of sensibility, called Fechner's psychophysical law. It is that the intensity of any sensation is proportional to the logarithm of the external force which produces it. It is entirely in harmony with this law that the feeling of belief should be as the logarithm of the chance, this latter being the expression of the state of facts which produces the belief."),
      
      p("The rule for the combination of independent concurrent arguments takes a very simple form when expressed in terms of the intensity of belief, measured in the proposed way. It is this: Take the sum of all the feelings of belief which would be produced separately by all the arguments pro, subtract from that the similar sum for arguments con, and the remainder is the feeling of belief which we ought to have on the whole. ",
        span(class = "example-trigger", id = "ex9-trigger",
             onclick = "Shiny.setInputValue('toggle_ex9', Math.random());",
             "This is a proceeding which men often resort to, under the name of balancing reasons.")
      ),

      div(id = "example-9", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      p(span(class = "example-trigger", id = "ex10-trigger",
             onclick = "Shiny.setInputValue('toggle_ex10', Math.random());",
             "Suppose we have a large bag of beans from which one has been secretly taken at random and hidden under a thimble. We are now to form a probable judgment of the color of that bean, by drawing others singly from the bag. Suppose the first drawing is white and the next black. We conclude that there is not an immense preponderance of either color, and that there is something like an even chance that the bean under the thimble is black. When we have drawn a thousand times, if about half have been white, we have great confidence in this result.")
      ),

      div(id = "example-10", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      p("Now, as the whole utility of probability is to insure us in the long run, it follows that we ought not to have the same feeling of belief in reference to all events of which the chance is even. ",
        span(class = "example-trigger", id = "ex11-trigger",
             onclick = "Shiny.setInputValue('toggle_ex11', Math.random());",
             "In short, to express the proper state of our belief, not one number but two are requisite, the first depending on the inferred probability, the second on the amount of knowledge on which that probability is based."), " When our knowledge is very slight, this second number may be even more important than the probability itself; and when we have no knowledge at all this completely overwhelms the other, so that there is no sense in saying that the chance of the totally unknown event is even. We thus perceive that the conceptualistic view is quite inadequate."),

      div(id = "example-11", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),

      p(span(class = "example-trigger", id = "ex12-trigger",
             onclick = "Shiny.setInputValue('toggle_ex12', Math.random());",
             "Suppose that we are totally ignorant what colored hair the inhabitants of Saturn have. Let us take a color-chart in which all possible colors are shown shading into one another. In such a chart the relative areas occupied by different classes of colors are perfectly arbitrary. Let us inclose such an area and ask what is the chance that the color of the hair of the inhabitants of Saturn falls within that area? According to conceptualistic principles, the answer can only be one-half, since the judgment should neither favor nor oppose the hypothesis. What is true of this area is true of any other; and it will equally be true of a third area which embraces the other two. But the probability for each of the smaller areas being one-half, that for the larger should be at least unity, which is absurd.")
      ),

      div(id = "example-12", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      div(class = "section-num", "III."),
      p("All our reasonings are of two kinds: 1. Explicative, analytic, or deductive; 2. Amplificative, synthetic, or (loosely speaking) inductive. In explicative reasoning, certain facts are first laid down in the premises. These facts are, in every case, an inexhaustible multitude. Such a statement will be the conclusion of an analytic inference. Of this sort are all mathematical demonstrations. But synthetic reasoning is of another kind. In this case the facts summed up in the conclusion are not among those stated in the premises. They are different facts, as when one sees that the tide rises ", em("m"), " times and concludes that it will rise the next time. These are the only inferences which increase our real knowledge."),
      
      p("In any problem in probabilities, we have given the relative frequency of certain events, and we perceive that in these facts the relative frequency of another event is given in a hidden way. This is therefore mere explicative reasoning, and is evidently entirely inadequate to the representation of synthetic reasoning. ",
        span(class = "example-trigger", id = "ex13-trigger",
             onclick = "Shiny.setInputValue('toggle_ex13', Math.random());",
             "Most treatises on probability contain a very different doctrine."), " They state, for example, that if one had seen the tide rise, say ", em("m"), " times, he could know that there was a probability equal to (m + 1) / (m + 2) that it would rise the next time. But this solution betrays its origin if we apply it to the case in which the man has never seen the tide rise at all; that is, if we put m = 0. In this case, the probability comes out 1/2, involving the conceptualistic principle that there is an even chance of a totally unknown event. But this principle is absurd."),

      div(id = "example-13", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      p("If there be any way of enumerating the possibilities of Nature so as to make them all equal, it is the following: ",
        span(class = "example-trigger", id = "ex14-trigger",
             onclick = "Shiny.setInputValue('toggle_ex14', Math.random());",
             "Suppose we had an immense granary filled with black and white balls well mixed up; and suppose each urn were filled by taking a fixed number of balls from this granary quite at random. The relative number of white balls might be anything, say one in three. In this way, we should have a distribution like that shown in the following table, where w stands for a white ball and b for a black one:")
      ),

      div(id = "example-14", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      div(class = "table-container", 
          "wwww.

wwwb. wwbw. wbww. bwww.
wwwb. wwbw. wbww. bwww.

wwbb. wbwb. bwwb. wbbw. bwbw. bbww.
wwbb. wbwb. bwwb. wbbw. bwbw. bbww.
wwbb. wbwb. bwwb. wbbw. bwbw. bbww.
wwbb. wbwb. bwwb. wbbw. bwbw. bbww.

wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.
wbbb. bwbb. bbwb. bbbw.

bbbb. bbbb. bbbb. bbbb. bbbb. bbbb. bbbb. bbbb.
bbbb. bbbb. bbbb. bbbb. bbbb. bbbb. bbbb. bbbb."
      ),
      
      p("In the second group, where there is one ", em("b"), ", there are two sets just alike; in the third there are 4, in the fourth 8, and in the fifth 16, doubling every time. This is because we have supposed twice as many black balls in the granary as white ones. Now suppose two balls were drawn from one of these urns and were found to be both white, what would be the probability of the next one being white? By inspecting the table, the reader can see that in each group all orders occur with equal frequency. ",
        span(class = "example-trigger", id = "ex15-trigger",
             onclick = "Shiny.setInputValue('toggle_ex15', Math.random());",
             "Hence the colors of the balls already drawn have no influence on the probability of any other being white or black.")
      ),

      div(id = "example-15", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),

      p("The assumption that any such thing can be done leads simply to the conclusion that reasoning from past to future experience is absolutely worthless. ",
        span(class = "example-trigger", id = "ex16-trigger",
             onclick = "Shiny.setInputValue('toggle_ex16', Math.random());",
             "In fact, the moment that you assume that the chances in favor of that of which we are totally ignorant are even, it would be to assume that Nature is a pure chaos, in which reasoning from one fact to another would be impossible."), " If we have found the order of Nature more or less regular in the past, this would be a pure run of luck which we may expect is now at an end. But reason is unnecessary in reference to that belief which is of all the most settled, which nobody doubts or can doubt."),

      div(id = "example-16", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      div(class = "section-num", "IV."),
      p("We have examined the problem proposed by the conceptualists: Given a synthetic conclusion, required to know out of all possible states of things how many will accord with it. We have found that it is only an absurd attempt to reduce synthetic to analytic reason. But there is another problem: Given a certain state of things, required to know what proportion of all synthetic inferences relating to it will be true within a given degree of approximation. There is no difficulty about this problem; the answer is perfectly well known."),
      
      p(span(class = "example-trigger", id = "ex17-trigger",
             onclick = "Shiny.setInputValue('toggle_ex17', Math.random());",
             "It is found that, if the true proportion of white balls is ", em("p"), ", and ", em("s"), " balls are drawn, then the error of the proportion obtained by the induction will be—")
      ),

      div(id = "example-17", class = "example-container", style = "display: none;",
          div(
            h4("Interactive Demonstration: Peirce's Formula for Probable Error"),

            p("Peirce's formula tells us how accurate our induction will be. If the true proportion of white balls is ",
              strong("p"), " and we draw ", strong("s"), " balls, the error will be within certain bounds with known frequencies."),

            p("The formula multiplies these constants by ", withMathJax("$$\\sqrt{\\frac{2p(1-p)}{s}}$$")),

            fluidRow(
              column(4,
                sliderInput("ex17_p", "True proportion (p):",
                           min = 0.1, max = 0.9, value = 0.5, step = 0.05),
                sliderInput("ex17_s", "Number of balls drawn (s):",
                           min = 10, max = 1000, value = 100, step = 10)
              ),
              column(8,
                tableOutput("ex17_table")
              )
            ),

            hr(),

            p(strong("Theoretical vs. Empirical Distribution:")),

            p("The curve below shows the theoretical distribution of errors according to Peirce's formula. The colored regions mark the 50%, 90%, and 99% confidence bounds."),

            plotOutput("ex17_theoretical_plot", height = "350px"),

            hr(),

            p(strong("Test with Simulation:")),
            fluidRow(
              column(6,
                numericInput("ex17_trials", "Number of trials:",
                            value = 1000, min = 100, max = 10000, step = 100),
                actionButton("ex17_simulate", "Run Simulation",
                            class = "btn-primary"),
                br(), br(),
                textOutput("ex17_results")
              ),
              column(6,
                plotOutput("ex17_empirical_plot", height = "300px")
              )
            )
          )
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
      
      p(span(class = "example-trigger", id = "ex18-trigger",
             onclick = "Shiny.setInputValue('toggle_ex18', Math.random());",
             "The use of this may be illustrated by an example. By the census of 1870, it appears that the proportion of males among native white children under one year old was 0.5082, while among colored children of the same age the proportion was only 0.4977. The difference is 0.0105, or about one in a 100. Can this be attributed to chance? Here ", em("p"), " may be taken at 1/2; hence 2p(1-p) is also 1/2. The number of white children counted was near 1,000,000; hence the fraction whose square-root is to be taken is about 1/2,000,000. The root is about 1/1,400, and this multiplied by 0.477 gives about 0.0003 as the probable error. For black children (about 150,000), the error is 0.0008. We see that the actual discrepancy is ten times the sum of these, and such a result would happen only once out of 10,000,000,000 censuses, in the long run.")
      ),

      div(id = "example-18", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      p(span(class = "example-trigger", id = "ex19-trigger",
             onclick = "Shiny.setInputValue('toggle_ex19', Math.random());",
             "It may be remarked that when the real value of the probability sought inductively is either very large or very small, the reasoning is more secure. Thus, suppose there were in reality one white ball in 100 in a certain urn, and we were to judge of the number by 100 drawings. The probability of drawing no white ball would be 366 1000; that of drawing one white ball would be 370 1000; that of drawing two would be 185 1000; that of drawing three would be 1000; that of 61 drawing four would be 15 1000; that of drawing five would be only 3 etc. Thus we should be tolerably certain of not 1000 being in error by more than one ball in 100.")
      ),

      div(id = "example-19", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      p("It appears, then, that in one sense we can, and in another we cannot, determine the probability of synthetic inference."),
      
      p("When I reason in this way:"),
      div(class = "syllogism",
          p("Ninety-nine Cretans in a hundred are liars;"),
          p("But Epimenides is a Cretan;"),
          p("Therefore, Epimenides is a liar:—")
      ),
      p(span(class = "example-trigger", id = "ex20-trigger",
             onclick = "Shiny.setInputValue('toggle_ex20', Math.random());",
             "I know that reasoning similar to that would carry truth 99 times in 100."), " But when I reason in the opposite direction:"),

      div(id = "example-20", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),

      div(class = "syllogism",
          p("Minos, Sarpedon, Rhadamanthus, Deucalion, and Epimenides, are all the Cretans I can think of;"),
          p("But these were all atrocious liars,"),
          p("Therefore, pretty much all Cretans must have been liars;")
      ),
      p(span(class = "example-trigger", id = "ex21-trigger",
             onclick = "Shiny.setInputValue('toggle_ex21', Math.random());",
             "I do not in the least know how often such reasoning would carry me right."), " On the other hand, what I do know is that some definite proportion of Cretans must have been liars, and that this proportion can be probably approximated to by an induction from five or six instances. Even in the worst case for the probability of such an inference, that in which about half the Cretans are liars, the ratio so obtained would probably not be in error by more than 1 6. So much I know;"),

      div(id = "example-21", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ), 
    
      
      div(class = "section-num", "V."),
      p("Late in the last century, Immanuel Kant asked the question, \"How are synthetical judgments a priori possible?\" By synthetical judgments he meant such as assert positive fact and are not mere affairs of arrangement. But before asking that question he ought to have asked the more general one, \"How are any synthetical judgments at all possible?\" How is it that a man can observe one fact and straightway pronounce judgment concerning another different fact not involved in the first?"),
      
      p("This is a strange paradox; the Abbé Gratry says it is a miracle, and that every true induction is an immediate inspiration from on high. I respect this explanation because it shows an appreciation of the depth of the problem. At the same time, I do not accept it, because an explanation should tell how a thing is done."),
      
      p("Synthetic inference is founded upon a classification of facts, not according to their characters, but according to the manner of obtaining them. Its rule is, that a number of facts obtained in a given way will in general more or less resemble other facts obtained in the same way; or, experiences whose conditions are the same will have the same general characters. In the case of analytic inference we know the probability of our conclusion; but in the case of synthetic inferences we only know the degree of trustworthiness of our proceeding."),
      
      p(span(class = "example-trigger", id = "ex22-trigger",
             onclick = "Shiny.setInputValue('toggle_ex22', Math.random());",
             "When we draw a deductive or analytic conclusion, our rule of inference is that facts of a certain general character are either invariably or in a certain proportion of cases accompanied by facts of another general character. Then our premise being a fact of the former class, we infer withc ertainty or with the appropriate degree of probability the existence of a fact of the second class."), " ",
        span(class = "example-trigger", id = "ex23-trigger",
             onclick = "Shiny.setInputValue('toggle_ex23', Math.random());",
             "But the rule for synthetic inference is of a different kind. When we sample a bag of beans we do not in the least assume that the fact of some beans being purple involves the necessity or even the probability of other beans being so. On the contrary, the conceptualistic method of treating probabilities, which really amounts simply to the deductive treatment of them, when rightly carried out leads to the result that a synthetic inference has just an even chance in its favor, or in other words is absolutely worthless. The color of one bean is entirely independent of that of another. But synthetic inference is founded upon a classification of facts, not according to their characters, but according to the manner of obtaining them. Its rule is, that a number of facts obtained in a given way will in general more or less resemble other facts obtained in the same way; or, experiences whose conditions are the same will have the same general characters.")
      ),

      div(id = "example-22", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),

      div(id = "example-23", class = "example-container", style = "display: none;",
          p(strong("Interactive Example"), " — Interactive coming soon!")
      ),
      
      p("Though a synthetic inference cannot by any means be reduced to deduction, yet that the rule of induction will hold good in the long run may be deduced from the principle that reality is only the object of the final opinion to which sufficient investigation would lead. That belief gradually tends to fix itself under the influence of inquiry is, indeed, one of the facts with which logic sets out."),
      
      div(class = "footnote",
          p("1. Strictly we should need an infinite series of numbers each depending on the probable error of the last."),
          p("2. \"Perfect indecision, belief inclining neither way, an even chance.\" — De Morgan, p. 182."),
          p("3. Logique. The same is true, according to him, of every performance of a differentiation, but not of integration. He does not tell us whether it is the supernatural assistance which makes the former process so much the easier.")
      )
  )
)

server <- function(input, output, session) {
  # Toggle functions for all interactive examples
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
  observeEvent(input$toggle_ex22, { shinyjs::toggle("example-22") })
  observeEvent(input$toggle_ex23, { shinyjs::toggle("example-23") })

  # Example 17: Peirce's Formula for Probable Error
  output$ex17_table <- renderTable({
    p <- input$ex17_p
    s <- input$ex17_s

    # Calculate the standard error factor
    se_factor <- sqrt(2 * p * (1 - p) / s)

    # Peirce's constants and their corresponding confidence levels
    constants <- c(0.477, 1.163, 1.821, 2.328, 2.751, 4.77)
    confidence <- c("50% (half the time)",
                   "90% (9 times out of 10)",
                   "99% (99 times out of 100)",
                   "99.9% (999 times out of 1,000)",
                   "99.99% (9,999 times out of 10,000)",
                   "99.99999999% (9,999,999,999 times out of 10,000,000,000)")

    # Calculate error bounds
    error_bounds <- constants * se_factor

    data.frame(
      "Confidence Level" = confidence,
      "Peirce's Constant" = sprintf("%.3f", constants),
      "Error Bound" = sprintf("±%.4f", error_bounds),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Theoretical distribution plot (always visible)
  output$ex17_theoretical_plot <- renderPlot({
    p <- input$ex17_p
    s <- input$ex17_s

    se_factor <- sqrt(2 * p * (1 - p) / s)

    # For binomial sampling, errors follow approximately half-normal distribution
    # Generate theoretical curve using normal approximation
    x_max <- 4 * se_factor
    x <- seq(0, x_max, length.out = 500)

    # Theoretical density (folded normal at 0)
    y <- dnorm(x, mean = 0, sd = se_factor / sqrt(2)) * 2

    # Calculate bounds
    bound_50 <- 0.477 * se_factor
    bound_90 <- 1.163 * se_factor
    bound_99 <- 1.821 * se_factor

    # Create plot
    plot(x, y, type = "l", lwd = 2,
         main = "Theoretical Distribution of Errors",
         xlab = "Absolute Error from True Proportion",
         ylab = "Density",
         col = "#2c7fb8",
         ylim = c(0, max(y) * 1.1))

    # Add shaded regions for confidence bounds
    # 50% region (lightest)
    x_50 <- x[x <= bound_50]
    y_50 <- y[1:length(x_50)]
    polygon(c(0, x_50, bound_50, 0), c(0, y_50, 0, 0),
            col = rgb(0.85, 0.3, 0.3, 0.15), border = NA)

    # 90% region
    x_90 <- x[x <= bound_90]
    y_90 <- y[1:length(x_90)]
    polygon(c(0, x_90, bound_90, 0), c(0, y_90, 0, 0),
            col = rgb(1, 0.5, 0, 0.1), border = NA)

    # 99% region
    x_99 <- x[x <= bound_99]
    y_99 <- y[1:length(x_99)]
    polygon(c(0, x_99, bound_99, 0), c(0, y_99, 0, 0),
            col = rgb(0.17, 0.63, 0.17, 0.1), border = NA)

    # Add vertical lines at bounds
    abline(v = bound_50, col = "#d62728", lwd = 2, lty = 2)
    abline(v = bound_90, col = "#ff7f0e", lwd = 2, lty = 2)
    abline(v = bound_99, col = "#2ca02c", lwd = 2, lty = 2)

    legend("topright",
           legend = c("50% bound", "90% bound", "99% bound"),
           col = c("#d62728", "#ff7f0e", "#2ca02c"),
           lwd = 2, lty = 2, cex = 0.9)
  })

  # Simulation for Example 17
  sim_data_ex17 <- eventReactive(input$ex17_simulate, {
    p <- input$ex17_p
    s <- input$ex17_s
    trials <- input$ex17_trials

    # Run simulations
    errors <- replicate(trials, {
      sample_white <- rbinom(1, s, p)
      observed_prop <- sample_white / s
      abs(observed_prop - p)  # Absolute error
    })

    list(errors = errors, p = p, s = s)
  })

  # Empirical plot overlaid on theoretical
  output$ex17_empirical_plot <- renderPlot({
    req(sim_data_ex17())
    data <- sim_data_ex17()
    errors <- data$errors
    p <- data$p
    s <- data$s

    se_factor <- sqrt(2 * p * (1 - p) / s)

    # Calculate bounds
    bound_50 <- 0.477 * se_factor
    bound_90 <- 1.163 * se_factor
    bound_99 <- 1.821 * se_factor

    # Create histogram with density scale
    hist(errors,
         breaks = 30,
         probability = TRUE,
         main = "Empirical vs. Theoretical",
         xlab = "Absolute Error",
         ylab = "Density",
         col = rgb(0.91, 0.96, 0.97, 0.7),
         border = "#2c7fb8")

    # Overlay theoretical curve
    x_max <- max(errors, 4 * se_factor)
    x <- seq(0, x_max, length.out = 500)
    y <- dnorm(x, mean = 0, sd = se_factor / sqrt(2)) * 2
    lines(x, y, col = "#2c7fb8", lwd = 2, lty = 1)

    # Add vertical lines at bounds
    abline(v = bound_50, col = "#d62728", lwd = 2, lty = 2)
    abline(v = bound_90, col = "#ff7f0e", lwd = 2, lty = 2)
    abline(v = bound_99, col = "#2ca02c", lwd = 2, lty = 2)

    legend("topright",
           legend = c("Empirical", "Theoretical", "50%", "90%", "99%"),
           col = c(rgb(0.91, 0.96, 0.97, 0.7), "#2c7fb8", "#d62728", "#ff7f0e", "#2ca02c"),
           lwd = c(10, 2, 2, 2, 2),
           lty = c(1, 1, 2, 2, 2),
           cex = 0.8)
  })

  output$ex17_results <- renderText({
    req(sim_data_ex17())
    data <- sim_data_ex17()
    errors <- data$errors
    p <- data$p
    s <- data$s

    se_factor <- sqrt(2 * p * (1 - p) / s)

    # Calculate actual percentages within each bound
    within_50 <- mean(errors <= 0.477 * se_factor) * 100
    within_90 <- mean(errors <= 1.163 * se_factor) * 100
    within_99 <- mean(errors <= 1.821 * se_factor) * 100

    sprintf("Simulation Results:\n%.1f%% within 50%% bound (expected ~50%%)\n%.1f%% within 90%% bound (expected ~90%%)\n%.1f%% within 99%% bound (expected ~99%%)",
            within_50, within_90, within_99)
  })
}

shinyApp(ui = ui, server = server)