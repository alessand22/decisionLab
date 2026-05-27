#' MEREC Method for Objective Weights
#'
#' Method based on the Removal Effects of Criteria (MEREC)
#' for objective determination of criterion weights.
#'
#' @param data Decision matrix.
#' @param benefit Logical vector indicating criterion type:
#' TRUE = benefit criterion, FALSE = cost criterion.
#'
#' @return A list containing:
#' \item{method}{Method name.}
#' \item{normalized_data}{Normalized decision matrix.}
#' \item{S}{Overall performance vector of alternatives.}
#' \item{Sprime}{Performance matrix after removing each criterion.}
#' \item{E}{Removal effect vector for each criterion.}
#' \item{weights}{Normalized criterion weights.}
#'
#' @references
#' Keshavarz-Ghorabaee, M., Amiri, M., Zavadskas, E. K.,
#' Turskis, Z., & Antucheviciene, J. (2021).
#' Determination of objective weights using a new method based on
#' the removal effects of criteria (MEREC).
#' Symmetry, 13(4), 525.
#'
#' @export
merec_weights <- function(data, benefit){
  # ---------------------------------------------------
  # MEREC - Method based on the Removal Effects of Criteria
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

  if (any(data <= 0)) {
    stop(
      "MEREC requires strictly positive data (all values > 0)"
    )
  }

  # -----------------------------
  # 1) Normalização
  # -----------------------------

  N <- matrix(
    NA,
    nrow = nrow(data),
    ncol = ncol(data)
  )

  for (j in seq_len(ncol(data))) {

    if (benefit[j]) {

      # Critério benefício
      N[, j] <- min(data[, j]) / data[, j]

    } else {

      # Critério custo
      N[, j] <- data[, j] / max(data[, j])
    }
  }

  # -----------------------------
  # 2) Verificação de segurança
  # -----------------------------

  # Em teoria:
  # N ∈ (0,1]

  if (any(N <= 0 | !is.finite(N))) {

    stop(
      "Invalid normalized values detected in N."
    )
  }

  # -----------------------------
  # 3) Desempenho total S
  # -----------------------------

  S <- numeric(nrow(N))

  for (i in seq_len(nrow(N))) {

    log_terms <- abs(log(N[i, ]))

    S[i] <- log(
      1 + sum(log_terms) / ncol(N)
    )
  }

  # -----------------------------
  # 4) Desempenho com remoção Sprime
  # -----------------------------

  Sprime <- matrix(
    NA,
    nrow = nrow(N),
    ncol = ncol(N)
  )

  for (i in seq_len(nrow(N))) {

    for (j in seq_len(ncol(N))) {

      reduced <- N[i, -j]

      log_terms <- abs(log(reduced))

      Sprime[i, j] <- log(
        1 + sum(log_terms) / ncol(N)
      )
    }
  }

  # -----------------------------
  # 5) Efeito de remoção
  # -----------------------------

  E <- numeric(ncol(N))

  for (j in seq_len(ncol(N))) {

    E[j] <- sum(
      abs(Sprime[, j] - S)
    )
  }

  if (sum(E) == 0) {

    stop(
      "All removal effects are zero. Cannot compute weights."
    )
  }

  # -----------------------------
  # 6) Pesos
  # -----------------------------

  weights <- E / sum(E)

  # -----------------------------
  # 7) Retorno
  # -----------------------------

  return(list(
    method = "MEREC",
    normalized_data = N,
    S = S,
    Sprime = Sprime,
    E = E,
    weights = weights
  ))
}
