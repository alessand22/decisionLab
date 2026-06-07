#' Validate Experts List
#'
#' Internal validation function for Group SWARA.
#'
#' Validates whether the experts object is a properly
#' structured list containing the required elements for
#' each expert.
#'
#' @param experts List of experts.
#'
#' @return Validated experts list.
#'
#' @keywords internal

.validate_experts <- function(experts) {

  # -----------------------------
  # Verificar objeto principal
  # -----------------------------

  if (!is.list(experts)) {

    stop(
      "'experts' must be a list"
    )

  }

  # -----------------------------
  # Verificar número de experts
  # -----------------------------

  if (length(experts) < 2) {

    stop(
      "Group SWARA requires at least two experts"
    )

  }

  # -----------------------------
  # Verificar cada expert
  # -----------------------------

  for (i in seq_along(experts)) {

    expert <- experts[[i]]

    # Expert deve ser lista

    if (!is.list(expert)) {

      stop(
        paste(
          "Expert",
          i,
          "must be a list"
        )
      )

    }

    # Componentes obrigatórios

    required_elements <- c(
      "criteria_order",
      "comparative_importance"
    )

    if (!all(
      required_elements %in%
      names(expert)
    )) {

      stop(
        paste(
          "Expert",
          i,
          "must contain",
          "'criteria_order' and",
          "'comparative_importance'"
        )
      )

    }

    # Validar critérios

    criteria_order <-
      .validate_criteria_order(
        expert$criteria_order
      )

    n_criteria <-
      length(criteria_order)

    # Validar coeficientes SWARA

    .validate_comparative_importance(
      comparative_importance =
        expert$comparative_importance,
      n_criteria =
        n_criteria
    )

  }

  return(experts)

}
