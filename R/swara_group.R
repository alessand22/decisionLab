#' Group Step-wise Weight Assessment Ratio Analysis (SWARA)
#'
#' Computes criterion weights using the Group SWARA method.
#'
#' In this implementation, each expert may define a different
#' criterion ordering and corresponding comparative importance
#' coefficients. Individual SWARA weights are computed for each
#' expert and aggregated into a group solution.
#'
#' @param experts List of experts. Each element must contain:
#' \describe{
#'   \item{criteria_order}{Character vector with criteria names
#'   ordered from most important to least important.}
#'   \item{comparative_importance}{Numeric vector containing
#'   comparative importance coefficients (\eqn{s_j}).}
#' }
#' @param aggregation Aggregation method used to combine
#' expert weights. Available options are:
#' \code{"mean"} and \code{"geometric"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{method}{Method name.}
#'   \item{aggregation}{Aggregation method used.}
#'   \item{n_experts}{Number of experts.}
#'   \item{individual_results}{Individual SWARA results.}
#'   \item{weight_matrix}{Matrix of individual weights.}
#'   \item{weights}{Aggregated criterion weights.}
#'   \item{result}{Data frame with final weights.}
#' }
#'
#' @details
#' The Group SWARA procedure follows these steps:
#' \enumerate{
#'   \item Each expert ranks criteria independently.
#'   \item Individual SWARA weights are computed.
#'   \item Individual weights are aggregated using the
#'   selected aggregation method.
#'   \item Final group weights are normalized.
#' }
#'
#' @references
#' Keršuliene, V., Zavadskas, E. K., & Turskis, Z. (2010).
#' Selection of rational dispute resolution method by
#' applying new step-wise weight assessment ratio analysis
#' (SWARA). Journal of Business Economics and Management,
#' 11(2), 243-258.
#'
#' @examples
#' experts <- list(
#'
#'   expert1 = list(
#'     criteria_order =
#'       c("Cost", "Quality", "Risk", "Time"),
#'     comparative_importance =
#'       c(0, 0.20, 0.10, 0.30)
#'   ),
#'
#'   expert2 = list(
#'     criteria_order =
#'       c("Quality", "Cost", "Time", "Risk"),
#'     comparative_importance =
#'       c(0, 0.15, 0.20, 0.25)
#'   ),
#'
#'   expert3 = list(
#'     criteria_order =
#'       c("Risk", "Quality", "Cost", "Time"),
#'     comparative_importance =
#'       c(0, 0.10, 0.15, 0.20)
#'   )
#'
#' )
#'
#' result <- swara_group(
#'   experts = experts,
#'   aggregation = "mean"
#' )
#'
#' result$weights
#'
#' @export

swara_group <- function(
    experts,
    aggregation = "mean"
) {

  # -----------------------------
  # 0) Validações
  # -----------------------------

  experts <- .validate_experts(
    experts
  )

  aggregation <- match.arg(
    aggregation,
    c("mean", "geometric")
  )

  # -----------------------------
  # 1) SWARA individual
  # -----------------------------

  individual_results <- lapply(
    experts,
    function(expert) {

      swara_weights(
        criteria_order =
          expert$criteria_order,
        comparative_importance =
          expert$comparative_importance
      )

    }
  )

  # -----------------------------
  # 2) Verificar critérios
  # -----------------------------

  criteria_sets <- lapply(
    individual_results,
    function(x) sort(x$criteria)
  )

  if (!all(
    vapply(
      criteria_sets,
      identical,
      logical(1),
      criteria_sets[[1]]
    )
  )) {

    stop(
      paste(
        "All experts must evaluate",
        "the same set of criteria."
      )
    )

  }

  # -----------------------------
  # 3) Critérios únicos
  # -----------------------------

  criteria <- criteria_sets[[1]]

  # -----------------------------
  # 4) Matriz de pesos
  # -----------------------------

  weight_matrix <- matrix(
    0,
    nrow = length(criteria),
    ncol = length(individual_results),
    dimnames = list(
      criteria,
      paste0(
        "Expert_",
        seq_along(individual_results)
      )
    )
  )

  for (i in seq_along(individual_results)) {

    result_i <- individual_results[[i]]

    weight_matrix[
      result_i$criteria,
      i
    ] <- result_i$weights

  }

  # -----------------------------
  # 5) Agregação
  # -----------------------------

  if (aggregation == "mean") {

    weights <- rowMeans(
      weight_matrix
    )

  } else {

    if (any(weight_matrix == 0)) {

      warning(
        paste(
          "Geometric aggregation detected one or more",
          "zero weights.",
          "These values were replaced by",
          ".Machine$double.eps to avoid",
          "numerical problems in logarithmic",
          "transformation."
        ),
        call. = FALSE
      )

    }

    weights <- apply(
      weight_matrix,
      1,
      function(x) {

        exp(
          mean(
            log(
              pmax(
                x,
                .Machine$double.eps
              )
            )
          )
        )

      }
    )

  }

  # -----------------------------
  # 6) Normalização
  # -----------------------------

  weights <- weights / sum(weights)

  # -----------------------------
  # 7) Resultado
  # -----------------------------

  result <- data.frame(
    Criterion = names(weights),
    Weight = round(
      weights,
      6
    )
  )

  result <- result[
    order(
      result$Weight,
      decreasing = TRUE
    ),
  ]

  rownames(result) <- NULL

  # -----------------------------
  # 8) Retorno
  # -----------------------------

  return(
    list(
      method = "SWARA Group",
      aggregation = aggregation,
      n_experts = length(experts),
      individual_results =
        individual_results,
      weight_matrix =
        weight_matrix,
      weights =
        weights,
      result =
        result
    )
  )

}
