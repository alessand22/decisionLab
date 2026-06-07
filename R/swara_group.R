#' Group Step-wise Weight Assessment Ratio Analysis (SWARA)
#'
#' Computes criterion weights using the Group SWARA method
#' from survey or spreadsheet data where each row represents
#' one expert.
#'
#' Rankings are provided in the criterion columns
#' (1 = most important criterion), while SWARA comparative
#' importance coefficients are provided in separate columns.
#'
#' @param data Data frame containing expert responses.
#' @param criteria_columns Character vector containing
#' criterion ranking columns.
#' @param s_columns Character vector containing SWARA
#' comparative importance columns.
#' @param aggregation Aggregation method used to combine
#' expert weights. Available options are
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
#' Each row represents one expert.
#'
#' Criterion columns contain rankings:
#' \itemize{
#'   \item 1 = most important criterion
#'   \item 2 = second most important criterion
#'   \item ...
#' }
#'
#' The function reconstructs the criterion ordering for
#' each expert, computes individual SWARA weights and
#' aggregates them into a group solution.
#'
#' @examples
#' data <- data.frame(
#'   Cost    = c(1, 2, 3),
#'   Quality = c(2, 1, 2),
#'   Risk    = c(3, 4, 1),
#'   Time    = c(4, 3, 4),
#'   s2      = c(0.20, 0.15, 0.10),
#'   s3      = c(0.10, 0.20, 0.15),
#'   s4      = c(0.30, 0.25, 0.20)
#' )
#'
#' result <- swara_group(
#'   data = data,
#'   criteria_columns =
#'     c("Cost", "Quality", "Risk", "Time"),
#'   s_columns =
#'     c("s2", "s3", "s4")
#' )
#'
#' result$result
#'
#' @export

swara_group <- function(
    data,
    criteria_columns,
    s_columns,
    aggregation = "mean"
) {

  # -----------------------------
  # 0) Validações
  # -----------------------------

  if (!is.data.frame(data)) {

    stop(
      "'data' must be a data.frame"
    )

  }

  if (!is.character(criteria_columns)) {

    stop(
      "'criteria_columns' must be a character vector"
    )

  }

  if (!is.character(s_columns)) {

    stop(
      "'s_columns' must be a character vector"
    )

  }

  if (!all(criteria_columns %in% names(data))) {

    stop(
      "Some criteria columns were not found in 'data'"
    )

  }

  if (!all(s_columns %in% names(data))) {

    stop(
      "Some s_columns were not found in 'data'"
    )

  }

  n_criteria <- length(criteria_columns)

  if (length(s_columns) != (n_criteria - 1)) {

    stop(
      paste(
        "The number of s_columns must be equal",
        "to the number of criteria minus one."
      )
    )

  }

  aggregation <- match.arg(
    aggregation,
    c(
      "mean",
      "geometric"
    )
  )

  # -----------------------------
  # 1) Construir lista experts
  # -----------------------------

  experts <- vector(
    mode = "list",
    length = nrow(data)
  )

  for (i in seq_len(nrow(data))) {

    ranks <- as.numeric(
      unlist(
        data[i, criteria_columns]
      )
    )

    if (any(is.na(ranks))) {

      stop(
        paste(
          "Missing ranking value detected",
          "in row",
          i
        )
      )

    }

    if (!setequal(
      ranks,
      seq_len(n_criteria)
    )) {

      stop(
        paste(
          "Row",
          i,
          "must contain a valid ranking",
          "from 1 to",
          n_criteria
        )
      )

    }

    criteria_order <-
      criteria_columns[
        order(ranks)
      ]

    s_values <- c(
      0,
      as.numeric(
        unlist(
          data[i, s_columns]
        )
      )
    )

    experts[[i]] <- list(
      criteria_order =
        criteria_order,
      comparative_importance =
        s_values
    )

  }

  # -----------------------------
  # 2) Executar engine
  # -----------------------------

  .swara_group_engine(
    experts = experts,
    aggregation = aggregation
  )

}
