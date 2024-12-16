add_class <- function(x, cls) {
  structure(x, class = c(cls, class(x)))
}

first_line <- function(x) {
  map_chr(strsplit(x, "\n", fixed = TRUE), 1L)
}
