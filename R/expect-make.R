expect_make <- function(func) {
  browser()
  args <- formals(func)[-1] %>% as.list
  func <- enexpr(func)

  out <- new_function(exprs(var = , `...` = , flt = TRUE, data = get_testdata()),
                      expr({
                        expect_func(ensym(var),
                                    !!func,
                                    enquo(flt),
                                    enquo(data),
                                    args = list(...))
                      }))

  out
}

expect_make_naive <- function(func) {
  out <- function(var, ..., flt = TRUE, data = get_testdata()) {
    expect_func(!!ensym(var),
                !!enquo(func),
                !!enquo(flt),
                !!enquo(data),
                args = list(...))
  }
}
