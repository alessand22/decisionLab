
<!-- README.md is generated from README.Rmd. Please edit that file -->

# decisionLab

<!-- badges: start -->

<!-- badges: end -->

## Overview

**decisionLab** is an R package for **Multi-Criteria Decision Analysis
(MCDA)**. It provides methods for criterion weighting and alternative
ranking, supporting researchers, students, and practitioners in solving
complex decision-making problems.

The package is designed for applications in:

- Management
- Healthcare
- Engineering
- Public Administration
- Operations Research
- Decision Support Systems

The current version implements methods for both **criterion weighting**
and **alternative ranking**, including support for **group
decision-making** through Group SWARA.

## Implemented Methods

### Ranking Methods

| Method | Description                                                    |
|--------|----------------------------------------------------------------|
| TOPSIS | Technique for Order Preference by Similarity to Ideal Solution |
| SPOTIS | Stable Preference Ordering Towards Ideal Solution              |
| SAW    | Simple Additive Weighting                                      |

### Weighting Methods

| Method  | Description                                      |
|---------|--------------------------------------------------|
| Entropy | Objective weighting based on information entropy |
| MEREC   | Method based on the Removal Effects of Criteria  |
| SWARA   | Step-wise Weight Assessment Ratio Analysis       |

### Group Weighting Methods

| Method | Description |
|----|----|
| SWARA Group | Group SWARA with support for multiple experts and aggregation of individual weights |

------------------------------------------------------------------------

## Installation

Install the development version from GitHub:

``` r
install.packages("pak")
pak::pak("alessand22/decisionLab")
```

Alternatively:

``` r
devtools::install_github("alessand22/decisionLab")
```

------------------------------------------------------------------------

## Example: TOPSIS

``` r
library(decisionLab)

data <- matrix(
  c(
    4, 7,
    2, 9,
    6, 5
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(
    c("A1", "A2", "A3"),
    c("C1", "C2")
  )
)

result <- topsis(
  data = data,
  weights = c(0.5, 0.5),
  benefit = c(TRUE, TRUE)
)

result$result
```

------------------------------------------------------------------------

## Example: SWARA

``` r
library(decisionLab)

criteria <- c(
  "Cost",
  "Quality",
  "Risk",
  "Time"
)

s <- c(
  0,
  0.20,
  0.10,
  0.30
)

result <- swara_weights(
  criteria_order = criteria,
  comparative_importance = s
)

result$result
```

------------------------------------------------------------------------

## Example: Group SWARA

``` r
library(decisionLab)

experts <- list(

  expert1 = list(
    criteria_order =
      c("Cost", "Quality", "Risk", "Time"),
    comparative_importance =
      c(0, 0.20, 0.10, 0.30)
  ),

  expert2 = list(
    criteria_order =
      c("Quality", "Cost", "Time", "Risk"),
    comparative_importance =
      c(0, 0.15, 0.20, 0.25)
  ),

  expert3 = list(
    criteria_order =
      c("Risk", "Quality", "Cost", "Time"),
    comparative_importance =
      c(0, 0.10, 0.15, 0.20)
  )

)

result <- swara_group(
  experts = experts,
  aggregation = "mean"
)

result$result
```

------------------------------------------------------------------------

## Citation

If you use **decisionLab** in academic research, please cite:

> Corrêa, A. C. (2026). *decisionLab: Multi-Criteria Decision Analysis
> Methods* (Version 0.1.0) \[R package\].
> <https://github.com/alessand22/decisionLab>

You can also obtain the citation directly in R:

``` r
citation("decisionLab")
```

------------------------------------------------------------------------

## Author

**Alessandro de Castro Corrêa**

Federal Institute of Education, Science and Technology of Pará (IFPA)

E-mail: <alessandro.correa@ifpa.com.br>

GitHub: <https://github.com/alessand22>

Project repository:

<https://github.com/alessand22/decisionLab>

------------------------------------------------------------------------

## License

This project is licensed under the MIT License.

------------------------------------------------------------------------

## Roadmap

Future releases are expected to include additional MCDA methods, such
as:

- CRITIC
- VIKOR
- COPRAS
- AHP-Gaussian
- BWM
- SAPEVO-M

The goal of **decisionLab** is to provide a comprehensive and
user-friendly environment for Multi-Criteria Decision Analysis in R.
