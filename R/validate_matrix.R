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
