test_that(
  "swara_group accepts valid data frame",
  {

    data <- data.frame(

      Cost =
        c(1, 2, 3),

      Quality =
        c(2, 1, 2),

      Risk =
        c(3, 4, 1),

      Time =
        c(4, 3, 4),

      s2 =
        c(0.20, 0.15, 0.10),

      s3 =
        c(0.10, 0.20, 0.15),

      s4 =
        c(0.30, 0.25, 0.20)

    )

    result <- swara_group(
      data = data,
      criteria_columns =
        c(
          "Cost",
          "Quality",
          "Risk",
          "Time"
        ),
      s_columns =
        c(
          "s2",
          "s3",
          "s4"
        )
    )

    expect_equal(
      result$method,
      "SWARA Group"
    )

    expect_equal(
      sum(result$weights),
      1,
      tolerance = 1e-8
    )

  }
)

test_that(
  "swara_group rejects non data frame input",
  {

    expect_error(

      swara_group(
        data = matrix(1:10),
        criteria_columns =
          c("C1", "C2"),
        s_columns =
          c("s2")
      ),

      "'data' must be a data.frame"

    )

  }
)

test_that(
  "swara_group rejects missing criteria columns",
  {

    data <- data.frame(
      Cost = c(1, 2),
      s2 = c(0.20, 0.15)
    )

    expect_error(

      swara_group(
        data = data,
        criteria_columns =
          c(
            "Cost",
            "Quality"
          ),
        s_columns =
          c("s2")
      ),

      "criteria columns"

    )

  }
)

test_that(
  "swara_group rejects missing s columns",
  {

    data <- data.frame(
      Cost = c(1, 2),
      Quality = c(2, 1)
    )

    expect_error(

      swara_group(
        data = data,
        criteria_columns =
          c(
            "Cost",
            "Quality"
          ),
        s_columns =
          c("s2")
      ),

      "s_columns"

    )

  }
)

test_that(
  "swara_group rejects invalid rankings",
  {

    data <- data.frame(

      Cost =
        c(1, 1),

      Quality =
        c(2, 2),

      Risk =
        c(2, 3),

      Time =
        c(4, 4),

      s2 =
        c(0.20, 0.15),

      s3 =
        c(0.10, 0.20),

      s4 =
        c(0.30, 0.25)

    )

    expect_error(

      swara_group(
        data = data,
        criteria_columns =
          c(
            "Cost",
            "Quality",
            "Risk",
            "Time"
          ),
        s_columns =
          c(
            "s2",
            "s3",
            "s4"
          )
      ),

      "valid ranking"

    )

  }
)
