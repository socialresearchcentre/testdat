
`%==%` <- function(x, y) {
  (!(is.na(x) & !is.na(y)) &
     !(is.na(y) & !is.na(x)) &
     (is.na(x) & is.na(y) |
        x == y))
}

`%!=%` <- function(x, y) {
  !(x %==% y)
}
