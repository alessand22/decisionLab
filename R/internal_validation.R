#' @keywords internal
#' @noRd
.validate_matrix <- function(data) {

  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("'data' must be a matrix or data frame")
  }

  data <- as.matrix(data)

  if (!is.numeric(data)) {
    stop("Decision matrix must contain only numeric values")
  }

  if (nrow(data) < 2) {
    stop("Decision matrix must contain at least two alternatives")
  }

  if (ncol(data) < 2) {
    stop("Decision matrix must contain at least two criteria")
  }

  if (any(is.na(data))) {
    stop("Decision matrix contains NA values")
  }

  if (any(!is.finite(data))) {
    stop("Decision matrix contains non-finite values")
  }

  return(data)
}

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

  if (abs(sum(weights) - 1) > 1e-8) {
    stop(
      paste(
        "Weights must sum to 1.",
        "Current sum =", round(sum(weights), 10)
      )
    )
  }
}

#' @keywords internal
#' @noRd
.validate_benefit <- function(benefit, n_criteria) {

  if (!is.logical(benefit)) {
    stop("'benefit' must be a logical vector")
  }

  if (length(benefit) != n_criteria) {
    stop(
      paste(
        "length of 'benefit' must equal number of criteria.",
        "Expected", n_criteria,
        "but received", length(benefit)
      )
    )
  }

  if (any(is.na(benefit))) {
    stop("'benefit' contains NA values")
  }
}
