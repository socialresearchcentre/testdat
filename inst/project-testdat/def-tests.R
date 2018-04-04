library(dplyr)

read_excel_allsheets <- function(filename) {
  sheets <- openxlsx::getSheetNames(filename)
  x <- lapply(sheets, function(x) suppressWarnings(openxlsx::read.xlsx(filename, sheet = x, skipEmptyCols = FALSE) %>% tbl_df))
  x <- lapply(x, function(x) { names(x) <- gsub("\\.", " ", names(x)); x })
  names(x) <- sheets
  x
}

defs <- read_excel_allsheets("definitions.xlsx")

# check all variables are in the base check and variable lists
base_missing  <- names(get_testdata())[!names(get_testdata()) %in% defs[["Base checks"]]$Variable]
if (length(base_missing) > 0)  warning("Some variables do not appear in the `Base checks` list: ",  paste0("`", base_missing,  "`", collapse = ", "))

value_missing <- names(get_testdata())[!names(get_testdata()) %in% defs[["Value checks"]]$Variable]
if (length(value_missing) > 0) warning("Some variables do not appear in the `Value checks` list: ", paste0("`", value_missing, "`", collapse = ", "))

# remove variables with no base check from list
defs[["Base checks"]] %<>% filter(!is.na(Base))

#-------------------------------------------------------------------------------

test_that("Base checks", {
  for (i in seq_len(nrow(defs[["Base checks"]]))) {
    expect_base(!!parse_quosure(defs[["Base checks"]][[i, "Variable"]]),
                !!parse_quosure(defs[["Base checks"]][[i, "Base"]]))
  }
})

# test_that("All labelled variables have no unlabelled values", {
#   for (i in names(get_testdata())[sapply(get_testdata(), is.labelled)]) {
#     expect_values(!!parse_quosure(i),
#                   attr(get_testdata()[[i]], "labels"))
#   }
# })

test_that("Value checks", {
  for (i in seq_len(nrow(defs[["Value checks"]]))) {
    # Length test
    if (!is.na(defs[["Value checks"]][[i, "Max Length"]])) {
      expect_func(!!parse_quosure(defs[["Value checks"]][[i, "Variable"]]),
                  chk_length,
                  args = list(defs[["Value checks"]][[i, "Max Length"]]))
    }
    # Pattern test
    if (!is.na(defs[["Value checks"]][[i, "Pattern"]])) {
      expect_func(!!parse_quosure(defs[["Value checks"]][[i, "Variable"]]),
                  chk_pattern,
                  args = list(defs[["Value checks"]][[i, "Pattern"]]))
    }
    # Uniqueness test
    if (defs[["Value checks"]][[i, "Unique"]] %in% TRUE) {
      expect_unique(vars(!!parse_quosure(defs[["Base checks"]][[i, "Variable"]])))
    }
    # Value checks
    values <- c()
    if (defs[["Value checks"]][[i, "Labels"]] %in% TRUE)
      values %<>% c(attr(get_testdata()[[defs[["Value checks"]][[i, "Variable"]]]], "labels"))

    for (j in names(defs[["Value checks"]])[str_detect(names(defs[["Value checks"]]), "^Range ")]) {
      if (!is.na(defs[["Value checks"]][[i, j]]))
        values %<>% c(eval(parse_expr(defs[["Value checks"]][[i, j]])))
    }

    if (length(values) > 0) {
      expect_values(!!parse_quosure(defs[["Value checks"]][[i, "Variable"]]),
                    values)
    }
  }
})

test_that("Consistency checks", {
  for (i in seq_len(nrow(defs[["Consistency checks"]]))) {
    expect_cond(!!parse_quosure(defs[["Consistency checks"]][[i, "Condition 1"]]),
                !!parse_quosure(defs[["Consistency checks"]][[i, "Condition 2"]]))
  }
})

test_that("Custom checks", {
  for (i in defs[["Custom"]][["Check"]]) eval(parse_expr(i))
})
