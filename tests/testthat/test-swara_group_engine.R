test_that(
  ".swara_group_engine returns valid result with mean aggregation",
  {

    experts <- list(

      expert1 = list(
        criteria_order =
          c("Cost", "Quality", "Risk", "Time"),
        comparative_importance =
          c(0, 0.20, 0.10, 0.30)
      ),

      expert2 = list(
        criteria_order =
          c("Quality", "Cost", "Time", "Risk"),
        comparative_importance =
          c(0, 0.15, 0.20, 0.25)
      )

    )

    result <- .swara_group_engine(
      experts = experts,
      aggregation = "mean"
    )

    expect_equal(
      result$method,
      "SWARA Group"
    )

    expect_equal(
      result$n_experts,
      2
    )

    expect_equal(
      sum(result$weights),
      1,
      tolerance = 1e-8
    )

  }
)

test_that(
  ".swara_group_engine returns valid result with geometric aggregation",
  {

    experts <- list(

      expert1 = list(
        criteria_order =
          c("Cost", "Quality", "Risk", "Time"),
        comparative_importance =
          c(0, 0.20, 0.10, 0.30)
      ),

      expert2 = list(
        criteria_order =
          c("Quality", "Cost", "Time", "Risk"),
        comparative_importance =
          c(0, 0.15, 0.20, 0.25)
      )

    )

    result <- .swara_group_engine(
      experts = experts,
      aggregation = "geometric"
    )

    expect_equal(
      sum(result$weights),
      1,
      tolerance = 1e-8
    )

  }
)

test_that(
  ".swara_group_engine rejects different criterion sets",
  {

    experts <- list(

      expert1 = list(
        criteria_order =
          c("Cost", "Quality", "Risk"),
        comparative_importance =
          c(0, 0.20, 0.10)
      ),

      expert2 = list(
        criteria_order =
          c("Cost", "Quality", "Time"),
        comparative_importance =
          c(0, 0.15, 0.20)
      )

    )

    expect_error(

      .swara_group_engine(
        experts = experts
      ),

      "All experts must evaluate"

    )

  }
)

test_that(
  ".swara_group_engine returns weight matrix",
  {

    experts <- list(

      expert1 = list(
        criteria_order =
          c("Cost", "Quality", "Risk"),
        comparative_importance =
          c(0, 0.20, 0.10)
      ),

      expert2 = list(
        criteria_order =
          c("Quality", "Cost", "Risk"),
        comparative_importance =
          c(0, 0.15, 0.20)
      )

    )

    result <- .swara_group_engine(
      experts = experts
    )

    expect_true(
      is.matrix(
        result$weight_matrix
      )
    )

  }
)
