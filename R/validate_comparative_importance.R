#' @keywords internal
#' @noRd

.validate_comparative_importance <- function(
    comparative_importance,
    n_criteria
) {

  # -----------------------------
  # Verificar tipo numérico
  # -----------------------------

  if (!is.numeric(comparative_importance)) {
    stop(
      "'comparative_importance' must be numeric"
    )
  }

  # -----------------------------
  # Verificar comprimento
  # -----------------------------

  if (length(comparative_importance) != n_criteria) {
    stop(
      paste(
        "length of 'comparative_importance'",
        "must equal number of criteria.",
        "Expected", n_criteria,
        "but received",
        length(comparative_importance)
      )
    )
  }

  # -----------------------------
  # Verificar NA
  # -----------------------------

  if (any(is.na(comparative_importance))) {
    stop(
      "'comparative_importance' contains NA values"
    )
  }

  # -----------------------------
  # Verificar valores finitos
  # -----------------------------

  if (any(!is.finite(comparative_importance))) {
    stop(
      "'comparative_importance' contains non-finite values"
    )
  }

  # -----------------------------
  # Verificar negatividade
  # -----------------------------

  if (any(comparative_importance < 0)) {
    stop(
      "'comparative_importance' must be non-negative"
    )
  }

  # -----------------------------
  # Verificar primeiro elemento
  # -----------------------------

  if (comparative_importance[1] != 0) {
    stop(
      "The first element of ",
      "'comparative_importance' must be 0"
    )
  }

  return(comparative_importance)
}
