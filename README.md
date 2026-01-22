# Simulating Peirce

Interactive Shiny applications exploring C.S. Peirce's work on probability, induction, and the logic of science.

## Applications

### Probability of Induction Interactive Reader
**Location**: `Probability of Induction/`
**Status**: Active development

Interactive reading experience of Peirce's 1878 article "The Probability of Induction" with inline mathematical examples. Presents the complete article text with clickable examples that launch visualizations using Peirce's antecedent-consequent-consequence framework.

**Purpose**: Enable readers to explore Peirce's mathematical arguments interactively while maintaining the original text and conceptual framework. Color-coordinated visualizations connect text to working examples.

**Current Progress**:
- Foundation complete with extensible architecture
- 10 examples all but complete
- 1 example in progress
- 14 examples to do

**To Run:**
```r
shiny::runApp("Probability of Induction/app.R")
```

---

### Probability of Induction (Standalone)
**Location**: `Probability of Induction.R`
**Status**: Stable

Compares induction vs deduction in statistical inference. Interactive demonstration showing how inductive reasoning moves from sample to population while deductive reasoning moves from population to sample. Includes adjustable population parameters and noise distributions.

**Purpose**: Illustrate the fundamental difference between inductive and deductive statistical reasoning using interactive visualizations.

**To Run:**
```r
source("Probability of Induction.R")
```

---

### Assessing Methods
**Location**: `Assessing Methods.R`
**Status**: Stable

Progressive exploration of forecast evaluation methods across three complexity levels: success rates, confusion matrices, and the Method of Balancing Reasons.

**Purpose**: Demonstrate Peirce's approach to evaluating predictive methods using weight of evidence, confusion matrices, and likelihood ratios. Includes presets for historical examples (Finley's tornado forecasts, Peirce's gold/lead experiments).

**To Run:**
```r
source("Assessing Methods.R")
```

---

### Gurney Telepathy
**Location**: `Gurney Telepathy.R`
**Status**: Stable (but also a mess)

Analysis of Edmund Gurney's telepathy experiments using Peirce's critique methodology.

**Purpose**: Demonstrate Peirce's statistical critique of Gurney's 1888 telepathy experiments, showing how conditional probabilities and memory biases affect evidential reasoning. Compares Gurney's simple base-rate calculation with Peirce's more sophisticated analysis accounting for death rate categories and memory correction factors.

**To Run:**
```r
source("Gurney Telepathy.R")
```

---

### Weight of Evidence
**Location**: `Weight of Evidence.R`
**Status**: Stable

Compares four concepts of weight of evidence: Peirce's Balance of Reasons, Peirce's Amount of Knowledge, Good's Weight (Bayesian), and Keynes's Weight.

**Purpose**: Illustrate how different thinkers conceptualize evidential weight using a bean-drawing scenario. Highlights distinctions between frequentist and Bayesian approaches.

**To Run:**
```r
source("Weight of Evidence.R")
```

---

### Kets Analysis
**Location**: `Kets.R`
**Status**: Stable

Mixture model decomposition of Peirce's Kets weight measurements.

**Purpose**: Analyze Peirce's 1873 gravity experiments using mixture decomposition to identify the five standard weights. Demonstrates Gaussian mixture models applied to historical metrology data with options to use Peirce's historical probable error or estimate from data.

**To Run:**
```r
source("Kets.R")
```

---

### Probable vs Necessary Deduction
**Location**: `Prob Deduction v P of Ded.R`
**Status**: Stable

Compares probable deduction (strong argument for guarded conclusion) with necessary deduction (weak argument for strong conclusion).

**Purpose**: Illustrate Peirce's distinction between different forms of deductive reasoning and their convergence properties through simulation.

**To Run:**
```r
source("Prob Deduction v P of Ded.R")
```

---

## Project Structure

```
Simulating-Peirce/
├── Probability of Induction/              # Interactive article reader (in development)
│   ├── app.R                              # Main Shiny app
│   ├── full_text.txt                      # Complete article text
│   ├── Working Copy Examples/             # Example implementations by status
│   │   ├── 1 - To Do/                     # 14 examples planned
│   │   ├── 2 - In Progress/               # 1 example under development
│   │   ├── 3 - All But Complete/          # 10 examples nearly ready
│   │   └── 4 - Incorporated/              # Completed examples
│   └── framework_updates.R                # Development utilities
├── Probability of Induction.R             # Standalone induction vs deduction
├── Assessing Methods.R                    # Forecast evaluation methods
├── Gurney Telepathy.R                     # Telepathy experiment critique
├── Weight of Evidence.R                   # Four concepts of evidential weight
├── Kets.R                                 # Mixture model decomposition
├── Prob Deduction v P of Ded.R            # Deduction comparison
├── v1 PoI read through.R                  # Early prototype of interactive reader
├── Probability of Induction - Working Copy - copy jan 18.R  # Development snapshot
└── Popular_Science_Monthly_Volume_12_April_1878_Illustrations_of_the_Logic_of_Science_IV.pdf
```

## Getting Started

### Prerequisites
```r
install.packages(c("shiny", "shinyjs", "ggplot2", "dplyr", "tidyr", "bslib", "DT", "scales", "mixtools"))
```

### Running Apps

For Shiny apps in folders:
```r
shiny::runApp("Probability of Induction/app.R")
```

For standalone R scripts:
```r
source("Probability of Induction.R")
source("Assessing Methods.R")
source("Weight of Evidence.R")
# etc.
```

## Current Focus

The **Probability of Induction Interactive Reader** is in active development. The app presents Peirce's complete 1878 article with clickable mathematical examples that launch interactive visualizations. The foundation is complete with an extensible architecture for adding examples incrementally. Of 25 planned examples, 10 are nearly complete, 1 is in progress, and 14 are planned.

## Key Concepts

### Peirce's Probability Framework

Unlike modern probability theory, Peirce defines probability as belonging to **consequences** (relationships between facts), not events:

- **Antecedent**: The premise fact(s)
- **Consequent**: The conclusion fact(s)
- **Consequence**: The relationship/rule connecting them
- **Probability**: How often the consequence holds (antecedent → consequent)


## Design Philosophy

1. **Text First**: Original texts are the primary interface
2. **Interactive Enhancement**: Math examples become explorable
3. **Visual Clarity**: Color coordination connects concepts
4. **Historical Accuracy**: Peirce's terminology and framework preserved
5. **Progressive Disclosure**: Complexity revealed as needed

## Development Status

**Probability of Induction Interactive Reader**: In development (25 examples: 10 nearly complete, 1 in progress, 14 planned)

**All Other Apps**: Stable (functional, not in active development)

## Academic Context

These applications explore C.S. Peirce's pioneering contributions to pragmatic philosophy, frequentist probability interpretation, inductive logic, statistical inference theory, and the scientific method. Peirce's "Illustrations of the Logic of Science" (1877-1878) laid foundational groundwork for American pragmatism and modern statistical theory.

## Author

Niall Roe
