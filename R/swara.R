#' Step-wise Weight Assessment Ratio Analysis (SWARA)
#'
#' Computes criterion weights using the SWARA method.
#'
#' SWARA is a subjective weighting method based on
#' expert judgment and sequential comparative importance
#' of criteria.
#'
#' @param criteria_order Character vector with criteria
#' names ordered from most important to least important.
#' @param comparative_importance Numeric vector containing
#' comparative importance coefficients (\eqn{s_j}).
#' The first element must be 0.
#'
#' @return A list containing:
#' \describe{
#'   \item{method}{Method name.}
#'   \item{criteria}{Ordered criteria names.}
#'   \item{s}{Comparative importance coefficients.}
#'   \item{k}{Recalculated coefficients.}
#'   \item{q}{Interim weights.}
#'   \item{weights}{Normalized weights.}
#'   \item{result}{Data frame with final weights.}
#' }
#'
#' @details
#' The SWARA procedure follows these steps:
#' \enumerate{
#'   \item Criteria are ordered from most important
#'   to least important.
#'   \item Comparative importance coefficients are defined.
#'   \item Recalculated coefficients are computed.
#'   \item Interim weights are obtained sequentially.
#'   \item Final normalized weights are calculated.
#' }
#'
#' @references
#' Keršuliene, V., Zavadskas, E. K., & Turskis, Z. (2010).
#' Selection of rational dispute resolution method by
#' applying new step‐wise weight assessment ratio analysis
#' (SWARA). Journal of Business Economics and Management,
#' 11(2), 243-258.
#'
#' @examples
#' criteria <- c("Cost", "Quality", "Risk", "Time")
#'
#' s <- c(0, 0.2, 0.1, 0.3)
#'
#' result <- swara_weights(
#'   criteria_order = criteria,
#'   comparative_importance = s
#' )
#'
#' result$weights
#'
#' @export

swara_weights <- function(criteria_order,
                          comparative_importance) {

  # -----------------------------
  # 0) Validações
  # -----------------------------

  criteria_order <- .validate_criteria_order(
    criteria_order
  )

  n_criteria <- length(criteria_order)

  comparative_importance <-
    .validate_comparative_importance(
      comparative_importance = comparative_importance,
      n_criteria = n_criteria
    )

  # -----------------------------
  # 1) Coeficientes recalculados
  # -----------------------------

  k <- comparative_importance + 1

  k[1] <- 1

  # -----------------------------
  # 2) Pesos intermediários
  # -----------------------------

  q <- numeric(n_criteria)

  q[1] <- 1

  for (j in 2:n_criteria) {

    q[j] <- q[j - 1] / k[j]
  }

  # -----------------------------
  # 3) Pesos normalizados
  # -----------------------------

  weights <- q / sum(q)

  # -----------------------------
  # 4) Resultado
  # -----------------------------

  result <- data.frame(
    Criterion = criteria_order,
    Comparative_Importance = comparative_importance,
    Recalculated_Coefficient = round(k, 6),
    Interim_Weight = round(q, 6),
    Weight = round(weights, 6)
  )

  # -----------------------------
  # 5) Retorno
  # -----------------------------

  return(list(
    method = "SWARA",
    criteria = criteria_order,
    s = comparative_importance,
    k = k,
    q = q,
    weights = weights,
    result = result
  ))
}
