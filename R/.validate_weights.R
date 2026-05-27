#' @keywords internal
#' @noRd
.validate_weights <- function(weights, n_criteria) {

  if (!is.numeric(weights)) {
    stop("'weights' must be numeric")
  }

  if (length(weights) != n_criteria) {
    stop(
      "length of 'weights' must equal number of criteria"
    )
  }

  if (any(is.na(weights))) {
    stop("Weights contain NA values")
  }

  if (any(weights < 0)) {
    stop("Weights must be non-negative")
  }

  if (sum(weights) == 0) {
    stop("Sum of weights cannot be zero")
  }

  # Filosofia acadêmica do decisionLab
  if (abs(sum(weights) - 1) > 1e-8) {
    stop(
      paste(
        "Weights must sum to 1.",
        "Current sum =", round(sum(weights), 10)
      )
    )
  }
}
