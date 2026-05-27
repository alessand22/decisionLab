#' @keywords internal
#' @noRd
.validate_benefit <- function(benefit, n_criteria) {

  # Verificar tipo lógico
  if (!is.logical(benefit)) {
    stop("'benefit' must be a logical vector")
  }

  # Verificar comprimento
  if (length(benefit) != n_criteria) {
    stop(
      paste(
        "length of 'benefit' must equal number of criteria.",
        "Expected", n_criteria,
        "but received", length(benefit)
      )
    )
  }

  # Verificar NA
  if (any(is.na(benefit))) {
    stop("'benefit' contains NA values")
  }
}
