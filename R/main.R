#' Merge multiple responses for the pages of the same GitHub query
#'
#' @param res Result of the original query, or result of the previous
#'   `merge_gh_pages()` call.
#' @param res2 Response to the next page.
#' @return Merged pages, in the same format as the original response, with
#'   the attributes from `res2`.

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

#' GitHub API query with conditional pagination
#'
#' @param ... parameters to pass to [gh::gh()].
#' @param cond Callback function that is called with the response from
#'   [gh::gh()]. If it returns `TRUE` and there is a `next` page, then
#'   the next page is also requested.
#' @return The complete result, with all pages of the pagination merged.

gh_pg <- function(..., cond) {
  res <- r1 <- gh::gh(...)
  while (isTRUE(cond(r1)) && gh:::gh_has_next(r1)) {
    r1 <- gh::gh_next(r1)
    res <- merge_gh_pages(res, r1)
  }
  res
}

#' Get the GitHub activitity feed for a user
#'
#' The user taken from the `GITHUB_ACTOR` environment variable. This is
#' set on GitHub Actions runs.
#' @param from Date to start the activity from.
#' @return Result of the GitHub API query, a list of [GitHub events](
#'   https://docs.github.com/en/rest/using-the-rest-api/github-event-types).

get_events <- function(from = Sys.Date() - 1) {
  username <- Sys.getenv("GITHUB_ACTOR", "gaborcsardi")
  cond <- function(rsp) {
    last_at <- parsedate::parse_iso_8601(rsp[[length(rsp)]][["created_at"]])
    last_at >= as.POSIXct(from)
  }
  gh_pg("/users/{username}/events", username = username, cond = cond)
}

#' Remove events that I am not interested in
#'
#' Some events are from automated tasks, these are removed. These are
#' specific for me, and should be parameterized for a generic tool.
#'
#' Additionally, all events are trimmed down, by keeping only the important
#' fields. Current event types are listed at
#' https://docs.github.com/en/rest/using-the-rest-api/github-event-types.
#'
#' @param evts List of events from the GitHub API.
#' @return Cleaned list of events.

clean_events <- function(evts) {
  evts
}

#' Format the summary and send it in an email
#'
#' For now it does not actually send them, only prints them to the screen.
#'
#' @param evts List of (possibly cleaned) GitHub events.

send_summary <- function(evts) {
  print(evts)
}

main <- function(args) {
  evts <- get_events()
  clev <- clean_events(evts)
  send_summary(clev)
}

if (is.null(sys.call())) {
  main(commandArgs(TRUE))
}
