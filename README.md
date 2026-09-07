# easyanalysis

R helper functions for building "Table 1"-style descriptive/comparison
summary tables (via [gtsummary](https://www.danieldsjoberg.com/gtsummary/)
and [flextable](https://ardata-fr.github.io/flextable-book/)) and for
checking normality of continuous variables, aimed at clinical/research
manuscript reporting.

This package is the renovated, consolidated form of four standalone scripts
(`summary table.R` → `summary table_1.02.R`) that were iterated on directly
in the repo root between July and October 2022. See [NEWS.md](NEWS.md) for
that history and what changed in the renovation, and [ROADMAP.md](ROADMAP.md)
for where the project is headed next (an R Shiny web app built on top of
these functions).

## Installation

```r
# install.packages("devtools")
devtools::install_github("SpaRroW410/Easy_analysis")
```

## Usage

```r
library(easyanalysis)

# Unstratified summary of two continuous variables
summary_table(y = c("Sepal.Width", "Sepal.Length"), data = iris)

# Stratified by group, with custom labels, a p-value column, and a caption
lab_y <- c(Sepal.Width = "Sepal width", Sepal.Length = "Sepal length")
summary_table(
  x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris,
  p = TRUE, ylab = lab_y, caption = "**Comparison**"
)

# Check normality of each variable within the first level of a grouping variable
check_normality(x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris)
```

See `?summary_table` and `?check_normality` for full parameter documentation.

## Development status

This is an early-stage personal project. The package has not yet been run
through `R CMD check` or its `testthat` suite in an actual R environment as
part of this renovation — see the note in the commit history / roadmap
before relying on it in production.
