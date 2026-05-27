#' @keywords internal
#' @noRd

.validate_criteria_order <- function(criteria_order) {

  # -----------------------------
  # Verificar NULL
  # -----------------------------

  if (is.null(criteria_order)) {
    stop("'criteria_order' cannot be NULL")
  }

  # -----------------------------
  # Verificar vetor
  # -----------------------------

  if (!is.vector(criteria_order)) {
    stop("'criteria_order' must be a vector")
  }

  # -----------------------------
  # Converter para character
  # -----------------------------

  criteria_order <- as.character(criteria_order)

  # -----------------------------
  # Verificar comprimento mínimo
  # -----------------------------

  if (length(criteria_order) < 2) {
    stop("At least two criteria are required")
  }

  # -----------------------------
  # Verificar valores ausentes
  # -----------------------------

  if (any(is.na(criteria_order))) {
    stop("'criteria_order' contains NA values")
  }

  # -----------------------------
  # Verificar strings vazias
  # -----------------------------

  if (any(trimws(criteria_order) == "")) {
    stop("'criteria_order' contains empty strings")
  }

  # -----------------------------
  # Verificar duplicatas
  # -----------------------------

  if (any(duplicated(criteria_order))) {
    stop("'criteria_order' contains duplicated criteria")
  }

  return(criteria_order)
}
