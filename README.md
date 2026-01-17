# Simulating Peirce

Interactive Shiny applications exploring C.S. Peirce's work on probability, induction, and the logic of science.

## Applications

### 📖 Probability of Induction (NEW - In Development)
**Location**: `Probability of Induction/`

Interactive reading experience of Peirce's 1878 article "The Probability of Induction" with inline mathematical examples.

**Features:**
- Complete article text with clickable interactive examples
- Color-coordinated visualizations matching text
- Uses Peirce's antecedent-consequent-consequence framework
- Extensible architecture for adding examples

**To Run:**
```r
shiny::runApp("Probability of Induction/app.R")
```

**Status**: Foundation complete with 2 working examples. Ready for incremental development. See `Probability of Induction/STATUS.md` for details.

---

### 🎲 Probability of Induction (Original)
**Location**: `Probability of Induction.R`

Standalone app comparing induction vs deduction in statistical inference.

**To Run:**
```r
source("Probability of Induction.R")
```

---

### ⚖️ Assessing Methods
**Location**: `Assessing Methods.R`

Explores weight of evidence and Bayesian reasoning.

**To Run:**
```r
source("Assessing Methods.R")
```

---

### 🔮 Gurney Telepathy
**Location**: `Gurney Telepathy.R`

Analysis of Edmund Gurney's telepathy experiments using Peirce's methods.

---

### 📊 Weight of Evidence
**Location**: `Weight of Evidence.R`

Interactive exploration of evidential reasoning and likelihood ratios.

---

## Project Structure

```
Simulating-Peirce/
├── Probability of Induction/       # NEW: Interactive article reader
│   ├── app.R                       # Main Shiny app
│   ├── full_text.txt              # Complete article text
│   ├── README.md                   # App documentation
│   ├── DEVELOPMENT_GUIDE.md        # Implementation guide
│   └── STATUS.md                   # Current status
├── Probability of Induction.R      # Original standalone app
├── Assessing Methods.R             # Weight of evidence app
├── Gurney Telepathy.R              # Telepathy analysis
├── Weight of Evidence.R            # Likelihood ratios
├── Kets.R                          # Kets analysis
├── Prob Deduction v P of Ded.R     # Deduction comparison
└── README.md                       # This file
```

## Getting Started

### Prerequisites
```r
install.packages(c("shiny", "shinyjs", "ggplot2", "dplyr", "tidyr"))
```

### Running Apps

**Option 1: From R/RStudio**
```r
shiny::runApp("Probability of Induction/app.R")
```

**Option 2: Source directly**
```r
source("Probability of Induction.R")  # For older standalone apps
```

## Current Focus

The **Probability of Induction** interactive reader is the main development focus. This app:

1. Presents Peirce's complete 1878 article
2. Allows readers to click mathematical examples in the text
3. Provides interactive visualizations for each example
4. Uses color coordination to connect text with visuals
5. Maintains Peirce's conceptual framework throughout

See `Probability of Induction/DEVELOPMENT_GUIDE.md` for implementation details.

## Key Concepts

### Peirce's Probability Framework

Unlike modern probability theory, Peirce defines probability as belonging to **consequences** (relationships between facts), not events:

- **Antecedent**: The premise fact(s)
- **Consequent**: The conclusion fact(s)
- **Consequence**: The relationship/rule connecting them
- **Probability**: How often the consequence holds (antecedent → consequent)

Example:
- Antecedent: "Has disease" (50 people)
- Consequent: "Tests positive"
- Consequence: "If disease, then positive test"
- Probability: 45/50 = 0.9 (the consequence holds 90% of the time)

All examples in the new app use this framework explicitly.

## Design Philosophy

1. **Text First**: Original texts are the primary interface
2. **Interactive Enhancement**: Math examples become explorable
3. **Visual Clarity**: Color coordination connects concepts
4. **Historical Accuracy**: Peirce's terminology and framework preserved
5. **Progressive Disclosure**: Complexity revealed as needed

## Development Status

### Probability of Induction Reader
- ✅ Foundation complete
- ✅ 2 working examples
- 🚧 12+ more examples planned
- 🚧 Full text integration in progress
- 📋 See `Probability of Induction/STATUS.md` for details

### Other Apps
- ✅ All functional and complete
- Some may benefit from integration with new interactive framework

## Contributing

To add new examples to the Probability of Induction reader:

1. See `Probability of Induction/DEVELOPMENT_GUIDE.md`
2. Follow the established pattern
3. Use color coordination
4. Maintain Peirce's framework

## Resources

- **Source PDF**: `Popular_Science_Monthly_Volume_12_April_1878_Illustrations_of_the_Logic_of_Science_IV.pdf`
- **Full Text**: `Probability of Induction/full_text.txt`
- **Implementation Guide**: `Probability of Induction/IMPLEMENTATION_GUIDE.md`

## Academic Context

These apps explore C.S. Peirce's pioneering work in:

- Pragmatic philosophy
- Frequentist interpretation of probability
- Inductive logic and scientific method
- Statistical inference theory
- Philosophy of science

Peirce's "Illustrations of the Logic of Science" (1877-1878) laid groundwork for American pragmatism and modern statistical theory.

## License

Educational and research use.

## Author

Niall Roe

## Last Updated

January 17, 2026
