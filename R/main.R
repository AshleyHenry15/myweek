main <- function(args = commandArgs(TRUE)) {
  from <- Sys.Date() - 7
  evts <- get_events(from)
  clev <- clean_events(evts, from = from)
  smry <- summarize_events(clev)
  md <- format_summary(smry, from = from)
  send_summary(md)
}

if (is.null(sys.call())) {
  # packages in Depends
  library(blastula)
  library(glue)
  library(httr)
  library(purrr)

  # files in R
  root <- dirname(whereami::thisfile())
  source(file.path(root, "gh.R"))
  source(file.path(root, "summary.R"))
  source(file.path(root, "format.R"))
  source(file.path(root, "send.R"))
  source(file.path(root, "utils.R"))

  main()
}
