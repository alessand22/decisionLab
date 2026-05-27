#' SAW Method
#'
#' Simple Additive Weighting (SAW) method for
#' multi-criteria decision analysis.
#'
#' @param data Decision matrix.
#' @param weights Numeric vector of criterion weights.
#' @param benefit Logical vector indicating criterion type:
#' TRUE = benefit criterion, FALSE = cost criterion.
#'
#' @return A list containing:
#' \item{method}{Method name.}
#' \item{alternatives}{Alternative names.}
#' \item{normalized_data}{Normalized decision matrix.}
#' \item{weighted_data}{Weighted normalized matrix.}
#' \item{scores}{SAW scores.}
#' \item{ranking}{Ranking of alternatives.}
#' \item{result}{Summary table with scores and rankings.}
#' \item{weights}{Criterion weights.}
#'
#' @references
#' MacCrimmon, K. R. (1968).
#' Decisionmaking among multiple-attribute alternatives:
#' A survey and consolidated approach.
#'
#' @export
saw <- function(data, weights, benefit){
  # ---------------------------------------------------
  # SAW - Simple Additive Weighting
  #
  # data     = matriz de decisão
  # weights  = pesos dos critérios
  # benefit  = vetor lógico: TRUE = benefício, FALSE = custo
  # ---------------------------------------------------

  # -----------------------------
  # 0) Validações
  # -----------------------------

  data <- .validate_matrix(data)

  n_criteria <- ncol(data)

  .validate_weights(
    weights = weights,
    n_criteria = n_criteria
  )

  .validate_benefit(
    benefit = benefit,
    n_criteria = n_criteria
  )

  # -----------------------------
  # 1) Identificação das alternativas
  # -----------------------------

  if (!is.null(rownames(data))) {
    alternatives <- rownames(data)
  } else {
    alternatives <- seq_len(nrow(data))
  }

  # -----------------------------
  # 2) Verificações para normalização
  # -----------------------------

  # Critérios benefício:
  # max não pode ser zero
  for (j in seq_len(n_criteria)) {

    if (benefit[j] && max(data[, j]) == 0) {

      stop(
        paste(
          "Criterion", j,
          "is benefit type and has maximum equal to zero."
        )
      )
    }
  }

  # Critérios custo:
  # não pode haver zero
  for (j in seq_len(n_criteria)) {

    if (!benefit[j] && any(data[, j] == 0)) {

      stop(
        paste(
          "Criterion", j,
          "is cost type and contains zero values."
        )
      )
    }
  }

  # -----------------------------
  # 3) Normalização
  # -----------------------------

  N <- matrix(
    NA,
    nrow = nrow(data),
    ncol = n_criteria
  )

  for (j in seq_len(n_criteria)) {

    if (benefit[j]) {

      # Critério benefício
      N[, j] <- data[, j] / max(data[, j])

    } else {

      # Critério custo
      N[, j] <- min(data[, j]) / data[, j]
    }
  }

  # -----------------------------
  # 4) Matriz ponderada
  # -----------------------------

  WN <- sweep(N, 2, weights, "*")

  # -----------------------------
  # 5) Scores
  # -----------------------------

  scores <- rowSums(WN)

  # -----------------------------
  # 6) Ranking
  # -----------------------------

  # maior score = melhor alternativa
  ranking <- rank(-scores, ties.method = "first")

  result <- data.frame(
    Alternative = alternatives,
    Score = round(scores, 6),
    Ranking = ranking
  )

  # -----------------------------
  # 7) Retorno
  # -----------------------------

  return(list(
    method = "SAW",
    alternatives = alternatives,
    normalized_data = N,
    weighted_data = WN,
    scores = scores,
    ranking = ranking,
    result = result,
    weights = weights
  ))
}
