#' Entropy Method for Objective Weights
#'
#' Entropy method for objective determination of criterion
#' weights in multi-criteria decision analysis.
#'
#' @param data Decision matrix.
#' @param benefit Logical vector indicating criterion type:
#' TRUE = benefit criterion, FALSE = cost criterion.
#'
#' @return A list containing:
#' \item{method}{Method name.}
#' \item{adjusted_data}{Adjusted decision matrix.}
#' \item{P}{Normalized probability matrix.}
#' \item{entropy}{Entropy values for each criterion.}
#' \item{divergence}{Divergence degree of each criterion.}
#' \item{weights}{Normalized criterion weights.}
#' \item{zero_adjustment}{Logical value indicating whether
#' zero probabilities were detected.}
#'
#' @references
#' Shannon, C. E. (1948).
#' A mathematical theory of communication.
#' Bell System Technical Journal, 27(3), 379-423.
#'
#' @export
entropy_weights <- function(data, benefit){
  # ---------------------------------------------------
  # Entropy Method for Objective Weights
  #
  # data     = matriz de decisão
  # benefit  = vetor lógico: TRUE = benefício, FALSE = custo
  # ---------------------------------------------------

  # -----------------------------
  # 0) Validações
  # -----------------------------

  data <- .validate_matrix(data)

  .validate_benefit(
    benefit = benefit,
    n_criteria = ncol(data)
  )

  if (any(data < 0)) {
    stop(
      "Entropy method requires non-negative data"
    )
  }

  # -----------------------------
  # 1) Ajustar critérios de custo
  # -----------------------------

  adjusted_data <- data

  if (any(!benefit & data == 0)) {

    stop(
      "Cost criteria contain zero values. ",
      "Division by zero would occur."
    )
  }

  adjusted_data[, !benefit] <- 1 / data[, !benefit]

  # -----------------------------
  # 2) Normalização
  # -----------------------------

  col_totals <- colSums(adjusted_data)

  if (any(col_totals == 0)) {

    stop(
      "At least one criterion has column sum equal to zero."
    )
  }

  P <- sweep(
    adjusted_data,
    2,
    col_totals,
    FUN = "/"
  )

  # -----------------------------
  # 3) Entropia
  # -----------------------------

  m <- nrow(data)

  zero_detected <- any(P == 0)

  if (zero_detected) {

    warning(
      paste(
        "Zero values detected in probability matrix P.",
        "The theoretical limit p*log(p)=0 was applied."
      )
    )
  }

  entropy_term <- ifelse(
    P > 0,
    P * log(P),
    0
  )

  entropy <- -colSums(entropy_term) / log(m)

  # -----------------------------
  # 4) Grau de divergência
  # -----------------------------

  divergence <- 1 - entropy

  if (sum(divergence) == 0) {

    stop(
      "All divergence values are zero. Cannot compute weights."
    )
  }

  # -----------------------------
  # 5) Pesos
  # -----------------------------

  weights <- divergence / sum(divergence)

  # -----------------------------
  # 6) Retorno
  # -----------------------------

  return(list(
    method = "Entropy",
    adjusted_data = adjusted_data,
    P = P,
    entropy = entropy,
    divergence = divergence,
    weights = weights,
    zero_adjustment = zero_detected
  ))
}
