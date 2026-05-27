#' SPOTIS Method
#'
#' Stable Preference Ordering Towards Ideal Solution (SPOTIS)
#' for multi-criteria decision analysis.
#'
#' @param data Decision matrix.
#' @param weights Numeric vector of criterion weights.
#' @param benefit Logical vector indicating criterion type:
#' TRUE = benefit criterion, FALSE = cost criterion.
#' @param lower_bounds Numeric vector containing lower bounds
#' for each criterion.
#' @param upper_bounds Numeric vector containing upper bounds
#' for each criterion.
#' @param ideal_solution Optional numeric vector representing
#' the ideal solution for each criterion.
#'
#' @return A list containing:
#' \item{method}{Method name.}
#' \item{alternatives}{Alternative names.}
#' \item{ideal_solution}{Ideal solution vector.}
#' \item{normalized_distance}{Normalized distance matrix.}
#' \item{weighted_distance}{Weighted distance matrix.}
#' \item{scores}{SPOTIS scores.}
#' \item{ranking}{Ranking of alternatives.}
#' \item{result}{Summary table with scores and rankings.}
#' \item{weights}{Criterion weights.}
#' \item{lower_bounds}{Lower bounds of criteria.}
#' \item{upper_bounds}{Upper bounds of criteria.}
#'
#' @references
#' Dezert, J., Tchamova, A., Han, D., & Tacnet, J. M. (2020).
#' The SPOTIS rank reversal free method for multi-criteria decision-making
#' support. 2020 IEEE 23rd International Conference on Information Fusion
#' (FUSION), 1-8.
#'
#' @export
spotis <- function(data,
                   weights,
                   benefit,
                   lower_bounds,
                   upper_bounds,
                   ideal_solution = NULL){
  # ---------------------------------------------------
  # SPOTIS - Stable Preference Ordering Towards Ideal Solution
  #
  # data           = matriz de decisão
  # weights        = pesos dos critérios
  # benefit        = vetor lógico: TRUE = benefício, FALSE = custo
  # lower_bounds   = limites inferiores dos critérios
  # upper_bounds   = limites superiores dos critérios
  # ideal_solution = solução ideal opcional
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

  if (length(lower_bounds) != n_criteria) {
    stop(
      "length of 'lower_bounds' must equal number of criteria"
    )
  }

  if (length(upper_bounds) != n_criteria) {
    stop(
      "length of 'upper_bounds' must equal number of criteria"
    )
  }

  if (any(is.na(lower_bounds))) {
    stop("'lower_bounds' contains NA values")
  }

  if (any(is.na(upper_bounds))) {
    stop("'upper_bounds' contains NA values")
  }

  if (any(!is.finite(lower_bounds))) {
    stop("'lower_bounds' contains non-finite values")
  }

  if (any(!is.finite(upper_bounds))) {
    stop("'upper_bounds' contains non-finite values")
  }

  if (any(upper_bounds <= lower_bounds)) {
    stop(
      "Each upper bound must be greater than lower bound"
    )
  }

  # -----------------------------
  # 1) Identificação das alternativas
  # -----------------------------

  if (!is.null(rownames(data))) {
    alternatives <- rownames(data)
  } else {
    alternatives <- seq_len(nrow(data))
  }

  # -----------------------------
  # 2) Verificar se dados estão dentro dos bounds
  # -----------------------------

  for (j in seq_len(n_criteria)) {

    if (any(
      data[, j] < lower_bounds[j] |
      data[, j] > upper_bounds[j]
    )) {

      stop(
        paste(
          "Criterion", j,
          "contains values outside specified bounds."
        )
      )
    }
  }

  # -----------------------------
  # 3) Solução ideal
  # -----------------------------

  if (is.null(ideal_solution)) {

    ideal_solution <- ifelse(
      benefit,
      upper_bounds,
      lower_bounds
    )

  } else {

    if (length(ideal_solution) != n_criteria) {
      stop(
        "length of 'ideal_solution' must equal number of criteria"
      )
    }

    if (any(is.na(ideal_solution))) {
      stop("'ideal_solution' contains NA values")
    }

    if (any(!is.finite(ideal_solution))) {
      stop("'ideal_solution' contains non-finite values")
    }

    # Verificar se solução ideal está dentro dos bounds
    if (any(
      ideal_solution < lower_bounds |
      ideal_solution > upper_bounds
    )) {
      stop(
        "Ideal solution must be within specified bounds"
      )
    }
  }

  # -----------------------------
  # 4) Distância normalizada
  # -----------------------------

  D <- matrix(
    NA,
    nrow = nrow(data),
    ncol = n_criteria
  )

  for (i in seq_len(nrow(data))) {

    for (j in seq_len(n_criteria)) {

      D[i, j] <- abs(
        data[i, j] - ideal_solution[j]
      ) / abs(
        upper_bounds[j] - lower_bounds[j]
      )
    }
  }

  # -----------------------------
  # 5) Distância ponderada
  # -----------------------------

  WD <- sweep(D, 2, weights, "*")

  scores <- rowSums(WD)

  # -----------------------------
  # 6) Ranking
  # -----------------------------

  # menor score = melhor alternativa
  ranking <- rank(scores, ties.method = "first")

  result <- data.frame(
    Alternative = alternatives,
    Score = round(scores, 6),
    Ranking = ranking
  )

  # -----------------------------
  # 7) Retorno
  # -----------------------------

  return(list(
    method = "SPOTIS",
    alternatives = alternatives,
    ideal_solution = ideal_solution,
    normalized_distance = D,
    weighted_distance = WD,
    scores = scores,
    ranking = ranking,
    result = result,
    weights = weights,
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds
  ))
}
