# Internal engine for Group SWARA
#
# @keywords internal

.swara_group_engine <- function(
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
    c(
      "mean",
      "geometric"
    )
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
    function(x) {

      sort(
        x$criteria
      )

    }
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
        seq_along(
          individual_results
        )
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

  weights <- weights / sum(
    weights
  )

  # -----------------------------
  # 7) Resultado
  # -----------------------------

  result <- data.frame(
    Criterion =
      names(weights),
    Weight =
      round(
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

      method =
        "SWARA Group",

      aggregation =
        aggregation,

      n_experts =
        length(experts),

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
