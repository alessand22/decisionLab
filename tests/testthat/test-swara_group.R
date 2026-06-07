test_that("swara_group returns valid result with mean aggregation", {

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

  result <- swara_group(
    experts = experts,
    aggregation = "mean"
  )

  expect_type(
    result,
    "list"
  )

  expect_equal(
    result$method,
    "SWARA Group"
  )

  expect_equal(
    result$aggregation,
    "mean"
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

})

test_that("swara_group returns valid result with geometric aggregation", {

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

  result <- swara_group(
    experts = experts,
    aggregation = "geometric"
  )

  expect_type(
    result,
    "list"
  )

  expect_equal(
    result$aggregation,
    "geometric"
  )

  expect_equal(
    sum(result$weights),
    1,
    tolerance = 1e-8
  )

})

test_that("swara_group rejects non-list experts", {

  expect_error(

    swara_group(
      experts = c(1, 2, 3)
    ),

    "'experts' must be a list"

  )

})

test_that("swara_group requires at least two experts", {

  experts <- list(

    expert1 = list(
      criteria_order =
        c("Cost", "Quality"),
      comparative_importance =
        c(0, 0.20)
    )

  )

  expect_error(

    swara_group(
      experts = experts
    ),

    "Group SWARA requires at least two experts"

  )

})

test_that("swara_group rejects experts without criteria_order", {

  experts <- list(

    expert1 = list(
      comparative_importance =
        c(0, 0.20)
    ),

    expert2 = list(
      criteria_order =
        c("Cost", "Quality"),
      comparative_importance =
        c(0, 0.20)
    )

  )

  expect_error(

    swara_group(
      experts = experts
    ),

    "must contain"

  )

})

test_that("swara_group rejects experts without comparative_importance", {

  experts <- list(

    expert1 = list(
      criteria_order =
        c("Cost", "Quality")
    ),

    expert2 = list(
      criteria_order =
        c("Cost", "Quality"),
      comparative_importance =
        c(0, 0.20)
    )

  )

  expect_error(

    swara_group(
      experts = experts
    ),

    "must contain"

  )

})

test_that("swara_group rejects different criterion sets", {

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
        c(0, 0.20, 0.10)
    )

  )

  expect_error(

    swara_group(
      experts = experts
    ),

    "All experts must evaluate"

  )

})

test_that("swara_group returns weight matrix", {

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

  result <- swara_group(
    experts = experts
  )

  expect_true(
    is.matrix(
      result$weight_matrix
    )
  )

  expect_equal(
    ncol(result$weight_matrix),
    2
  )

})
