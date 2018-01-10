library(testdat)
library(srcqilt)
library(xlsx)

file_name <- "Z:/Consulting/Jobs/A-K/Department of Education and Training (Cth)/2003 GOS18 November round 2017/5. Sample/3. Master cleaned/Institutions/Returned/PS Files/Raw/Issues/1019 GOS 2018 November 1019 JCU Population File JS.xlsx"

dat <- read.xlsx2(file_name, sheetName = "Population Data", stringsAsFactors = FALSE, startRow = 2) %>%
  tbl_df() %>%
  rename_all(tolower)

dat %<>%
  mutate_if(is.character, funs(ifelse(. %in% "", NA, .)))

set_reporter(ExcelReporter$new())
get_reporter()$set_output("example/Excel output test.xlsx")
context("JCU")
context_data(dat)

test_that("E306 is correct", {
  expect_true(all(dat$e306 == "1019"))
})

test_that("Base checks", {
  expect_base(email1, inscope == 0)
  expect_base(email2, inscope == 0)
})

test_that("Consistency checks", {
  expect_cond(inscope == 0, sampleframe %in% c(0,8))
})

test_that("Missing contact details", {
  expect_any(vars(email1, email2, email3), chk_nmiss, inscope == 0)
  expect_any(vars(e402, e403, e404), chk_nmiss, inscope == 0)
  expect_any(vars(phone1, phone2, phone3), chk_nmiss, inscope == 0)
})

test_that("Email checks", {
  expect_all(vars(email1, email2, email3), chk_pattern,
             args = list(pattern = "^[a-zA-Z0-9._%+'-]+@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]+$"))
  email_valid <- function(x) { !email.error(x) }
  expect_all(vars(email1, email2, email3), email_valid)
})

test_that("Range checks", {
  expect_values(inscope, 1:5)
  expect_values(sampleframe, 0:8)
  expect_func(inscope, chk_values, args = list(1:5))
  expect_func(sampleframe, chk_values, args = list(0:8))
})

test_that("Check date formats", {
  expect_func(e314, chk_date_yyyymmdd)
  expect_func(completiondate, chk_date_yyyymmdd)
})

test_that("Uniqueness checks", {
  expect_unique(vars(e313, e307))
  expect_unique(vars(e313), inscope == 0)
  expect_unique(vars(gosid))
})

test_that("HEIMS format checks", {
  # for (i in names(dat)[is_heims(names(dat))]) {
  #   expect_true(all(heims_check(dat[[i]], i)))
  # }
  for (i in names(dat)[is_heims(names(dat))]) {
    expect_func(!!parse_quosure(i), heims_check, args = list(i))
  }
})

get_reporter()$output_results()


end_data_test("JCU", dat)
x <- get_reporter()$get_results()
str(x[[1]]$results[[3]])
