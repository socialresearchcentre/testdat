df <- data.frame(
  x = labelled::labelled(c("M", "M", "F"), c(Male = "M", Female = "F"), "Sex"),
  y = labelled::labelled(c("M", "F", "F"), c(Male = "M", Female = "F", Other = "X")),
  z = labelled::labelled(c("M", "X", "F"), c(Male = "M", Female = "F", Other = "X")),
  no_labels = c("M", "X", "F"),
  no_val_labels = labelled::labelled(c("M", "M", "F"), label = "Sex"),
  no_var_label = labelled::labelled(c("M", "M", "F"), c(Male = "M", Female = "F", Other = "X"))
)

test_that("Basic use", {
  # Sucesses
  expect_success(expect_labels(
    x,
    val_labels = c(Male = "M", Female = "F"),
    var_label = "Sex",
    data = df
  ))

  expect_success(expect_labels(
    x,
    val_labels = c("Male", "Female"),
    var_label = "Sex",
    data = df
  ))

  # Failures
  expect_failure(expect_labels(
    x,
    val_labels = c(Female = "M", Male = "F"),
    var_label = "Sex",
    data = df
  ), message = "`df` has 3 records failing label check on variable `x`")

  expect_failure(expect_labels(
    x,
    val_labels = c("Male"),
    var_label = "Sex",
    data = df
  ), "`df` has 1 records failing label check on variable `x`")
})

test_that("Weak label checks work", {
  # Sucesses
  expect_success(expect_labels(
    x,
    val_labels = TRUE,
    var_label = TRUE,
    data = df
  ))

  expect_success(expect_labels(
    no_labels,
    val_labels = FALSE,
    var_label = FALSE,
    data = df
  ))

  expect_success(expect_labels(
    no_val_labels,
    val_labels = FALSE,
    data = df
  ))

  expect_success(expect_labels(
    no_var_label,
    var_label = FALSE,
    data = df
  ))

  # Failures
  expect_failure(expect_labels(
    no_labels,
    val_labels = TRUE,
    var_label = TRUE,
    data = df
  ))

  expect_failure(expect_labels(
    no_val_labels,
    val_labels = TRUE,
    data = df
  ))

  expect_failure(expect_labels(
    no_var_label,
    var_label = TRUE,
    data = df
  ))
})


test_that("All records fail where variable labels don't match", {
  expect_failure(expect_labels(
    x,
    val_labels = c(Male = "M", Female = "F"),
    var_label = "Age",
    data = df
  ), message = "`df` has 3 records failing label check on variable `x`.")
})

test_that("Test only fails when there is a label discrepancy for values present in the data", {
  expect_success(expect_labels(
    x,
    val_labels = labelled::val_labels(df$y),
    data = df
  ))

  expect_success(expect_labels(
    y,
    val_labels = labelled::val_labels(df$z),
    data = df
  ))

  expect_success(expect_labels(
    x,
    val_labels = labelled::val_labels(df$z),
    data = df
  ))

  expect_failure(expect_labels(
    z,
    val_labels = labelled::val_labels(df$x),
    data = df
  ))
})
