merge_gh_pages <- function(res, res2) {
  if (!is.null(names(res2)) && identical(names(res), names(res2))) {
    res3 <- mapply(function(x, y, n) {
      z <- c(x, y)
      atm <- is.atomic(z)
      if (atm && n %in% c("total_count", "incomplete_results")) {
        y
      } else if (atm) {
        unique(z)
      } else {
        z
      }
    }, res, res2, names(res), SIMPLIFY = FALSE)
  } else {
    res3 <- c(res, res2)
  }
  attributes(res3) <- attributes(res2)
  res3
}

gh_pg <- function(..., cond) {
  res <- r1 <- gh::gh(...)
  while (cond(r1) && gh:::gh_has_next(r1)) {
    r1 <- gh::gh_next(r1)
    res <- merge_gh_pages(res, r1)
  }
  res
}

evts <- function(from = Sys.Date() - 7, until = Sys.Date()) {
  username <- Sys.getenv("GITHUB_ACTOR", "gaborcsardi")
  gh_pg("/users/{username}/events", username = username)
}

clean_events <- function(evts) {
  evts
}

main <- function(args) {
  evts <- get_events()
  clev <- clean_events(evts)
  send_summary(clev)
}

if (is.null(sys.call())) {
  main(commandArgs(TRUE))
}
