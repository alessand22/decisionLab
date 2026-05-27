#' TOPSIS Method
#'
#' Technique for Order Preference by Similarity to Ideal Solution (TOPSIS).
#'
#' @param data Decision matrix.
#' @param weights Numeric vector of criterion weights.
#' @param benefit Logical vector indicating benefit criteria.
#'
#' @return A list containing:
#' \item{scores}{Closeness coefficients}
#' \item{ranking}{Ranking of alternatives}
#' \item{result}{Summary table}
#'
#' @references
#' Hwang, C. L., & Yoon, K. (1981).
#' Multiple Attribute Decision Making:
#' Methods and Applications.
#'
#' @export
topsis <- function(data, weights, benefit){
  # ---------------------------------------------------
  # TOPSIS - Technique for Order Preference by Similarity
  # to Ideal Solution
  #
  # data     = matriz de decisão
  # weights  = pesos dos critérios
  # benefit  = vetor lógico: TRUE = benefício, FALSE = custo
  # ---------------------------------------------------

  # -----------------------------
  # 0) Validações
  # -----------------------------

  data <- .validate_matrix(data)

  .validate_weights(
    weights = weights,
    n_criteria = ncol(data)
  )

  .validate_benefit(
    benefit = benefit,
    n_criteria = ncol(data)
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
  # 2) Normalização vetorial
  # -----------------------------

  den <- sqrt(colSums(data^2))

  if (any(den == 0)) {
    stop("At least one criterion has zero norm. Cannot normalize.")
  }

  R <- sweep(data, 2, den, "/")

  # -----------------------------
  # 3) Matriz ponderada
  # -----------------------------

  V <- sweep(R, 2, weights, "*")

  # -----------------------------
  # 4) Soluções ideal e anti-ideal
  # -----------------------------

  ideal <- ifelse(
    benefit,
    apply(V, 2, max),
    apply(V, 2, min)
  )

  anti_ideal <- ifelse(
    benefit,
    apply(V, 2, min),
    apply(V, 2, max)
  )

  # -----------------------------
  # 5) Distâncias
  # -----------------------------

  ideal_matrix <- matrix(
    ideal,
    nrow = nrow(V),
    ncol = ncol(V),
    byrow = TRUE
  )

  anti_matrix <- matrix(
    anti_ideal,
    nrow = nrow(V),
    ncol = ncol(V),
    byrow = TRUE
  )

  dist_ideal <- sqrt(rowSums((V - ideal_matrix)^2))

  dist_anti_ideal <- sqrt(
    rowSums((V - anti_matrix)^2)
  )

  # -----------------------------
  # 6) Coeficiente de proximidade
  # -----------------------------

  denominator <- dist_ideal + dist_anti_ideal

  if (any(denominator == 0)) {
    stop(
      "Zero denominator detected in closeness coefficient."
    )
  }

  scores <- dist_anti_ideal / denominator

  # -----------------------------
  # 7) Ranking
  # -----------------------------

  ranking <- rank(-scores, ties.method = "first")

  result <- data.frame(
    Alternative = alternatives,
    Score = round(scores, 6),
    Ranking = ranking
  )

  # -----------------------------
  # 8) Retorno
  # -----------------------------

  return(list(
    method = "TOPSIS",
    alternatives = alternatives,
    normalized_data = R,
    weighted_data = V,
    ideal = ideal,
    anti_ideal = anti_ideal,
    dist_ideal = dist_ideal,
    dist_anti_ideal = dist_anti_ideal,
    scores = scores,
    ranking = ranking,
    result = result,
    weights = weights
  ))
}
