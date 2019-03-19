library(dplyr)
library(rlang)

read_excel_allsheets <- function(filename) {
  sheets <- openxlsx::getSheetNames(filename)
  x <- lapply(sheets, function(x) suppressWarnings(openxlsx::read.xlsx(filename, sheet = x, skipEmptyCols = FALSE) %>% tbl_df %>% mutate_all(as.character)))
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

# remove variables with no base check from list and
defs[["Base checks"]] <-
  defs[["Base checks"]] %>%
  filter(!is.na(Base)) %>%
  mutate(`Missing Values` = ifelse(`Missing Values` %in% c(NA, ""), "NULL", `Missing Values`))

#-------------------------------------------------------------------------------

test_that("Base checks", {
  for (i in seq_len(nrow(defs[["Base checks"]]))) {
    expect_base(!!parse_quo(defs[["Base checks"]][[i, "Variable"]]),
                !!parse_quo(defs[["Base checks"]][[i, "Base"]]),
                c(getOption("testdat.miss"), eval(parse_expr(defs[["Base checks"]][[i, "Missing Values"]]))),
                defs[["Base checks"]][[i, "Missing Valid"]] %in% TRUE)
  }
})

# test_that("All labelled variables have no unlabelled values", {
#   for (i in names(get_testdata())[sapply(get_testdata(), is.labelled)]) {
#     expect_values(!!parse_quo(i),
#                   attr(get_testdata()[[i]], "labels"))
#   }
# })

test_that("Value checks", {
  for (i in seq_len(nrow(defs[["Value checks"]]))) {
    # Length test
    if (!is.na(defs[["Value checks"]][[i, "Max Length"]])) {
      expect_func(!!parse_quo(defs[["Value checks"]][[i, "Variable"]]),
                  chk_length,
                  args = list(defs[["Value checks"]][[i, "Max Length"]]))
    }
    # Pattern test
    if (!is.na(defs[["Value checks"]][[i, "Pattern"]])) {
      expect_func(!!parse_quo(defs[["Value checks"]][[i, "Variable"]]),
                  chk_pattern,
                  args = list(defs[["Value checks"]][[i, "Pattern"]]))
    }
    # Uniqueness test
    if (defs[["Value checks"]][[i, "Unique"]] %in% TRUE) {
      expect_unique(vars(!!parse_quo(defs[["Base checks"]][[i, "Variable"]])))
    }
    # Value checks
    values <- c()
    if (defs[["Value checks"]][[i, "Labels"]] %in% TRUE)
      values <- c(values, attr(get_testdata()[[defs[["Value checks"]][[i, "Variable"]]]], "labels"))

    for (j in names(defs[["Value checks"]])[grepl("^ Range ", names(defs[["Value checks"]]))]) {
      if (!is.na(defs[["Value checks"]][[i, j]]))
        values <- c(values, eval(parse_expr(defs[["Value checks"]][[i, j]])))
    }

    if (length(values) > 0) {
      expect_values(!!parse_quo(defs[["Value checks"]][[i, "Variable"]]),
                    values)
    }
  }
})

test_that("Consistency checks", {
  for (i in seq_len(nrow(defs[["Consistency checks"]]))) {
    expect_cond(!!parse_quo(defs[["Consistency checks"]][[i, "Condition 1"]]),
                !!parse_quo(defs[["Consistency checks"]][[i, "Condition 2"]]))
  }
})

test_that("Custom checks", {
  for (i in defs[["Custom"]][["Check"]]) eval(parse_expr(i))
})
