#' Merge multiple responses for the pages of the same GitHub query
#'
#' @param res Result of the original query, or result of the previous
#'   `merge_gh_pages()` call.
#' @param res2 Response to the next page.
#' @return Merged pages, in the same format as the original response, with
#'   the attributes from `res2`.

merge_gh_pages <- function(res, res2) {
  res3 <- c(res, res2)
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
  username <- get_username()
  cond <- function(rsp) {
    last_at <- parsedate::parse_iso_8601(rsp[[length(rsp)]][["created_at"]])
    last_at >= as.POSIXct(from)
  }
  # 100 is the maximum that GH will send in a single page
  gh_pg(
    "/users/{username}/events",
    username = username,
    cond = cond,
    .per_page = 100
  )
}

get_username <- function() {
  Sys.getenv("GITHUB_ACTOR", "gaborcsardi")
}

#' Remove events that I am not interested in
#'
#' First, remove events older than the requested date.
#'
#' Some events are from automated tasks, these are removed. These are
#' specific for me, and should be parameterized for a generic tool.
#'
#' @param evts List of events from the GitHub API.
#' @return Cleaned list of events. It loses the attributes, e.g. class
#'   in the cleaning process.

clean_events <- function(evts, from = NULL) {
  if (!is.null(from)) {
    crat <- parsedate::parse_iso_8601(sapply(evts, "[[", "created_at"))
    evts <- evts[crat >= as.POSIXct(from)]
  }

  drop <- function(pred) {
    evts <<- evts[!vapply(evts, pred, logical(1))]
  }

  # Automatic issue comments in the /cran org
  drop(\(x) x$type == "IssueCommentEvent" && grepl("^cran/", x$repo$name))

  # Push to the packages branch of pak, that's the nightly build
  drop(\(x) x$type == "PushEvent" && x$payload$ref == "refs/heads/packages")

  # Push to r-lib/r-lib.github.io, also nightly build
  drop(\(x) x$type == "PushEvent" && x$repo$name == "r-lib/r-lib.github.io")

  c(evts)
}

keep <- function(evts, pred) {
  evts[vapply(evts, pred, logical(1))]
}

repos_created <- function(evts) {
  rps <- keep(evts, \(x) x$type == "CreateEvent" && x$payload$ref_type == "repository")
  add_class(sort(vapply(rps, \(x) x$repo$name, "")), "repos_created")
}

tags_created <- function(evts) {
  rps <- keep(evts, \(x) x$type == "CreateEvent" && x$payload$ref_type == "tag")
  tc <- data.frame(
    repo = vapply(rps, \(x) x$repo$name, ""),
    tag = vapply(rps, \(x) x$payload$ref, "")
  )
  tc <- tc[order(tc$repo, tc$tag), ]
  add_class(tc, "tags_created")
}

branches_created <- function(evts) {
  rps <- keep(evts, \(x) x$type == "CreateEvent" && x$payload$ref_type == "branch")
  bc <- data.frame(
    repo = vapply(rps, \(x) x$repo$name, ""),
    branch = vapply(rps, \(x) x$payload$ref, "")
  )
  bc <- bc[order(bc$repo, bc$branch), ]
  add_class(bc, "branches_created")
}

commits_pushed <- function(evts) {
  pss <- keep(evts, \(x) x$type == "PushEvent")
  cms <- data.frame(
    repo = vapply(pss, \(x) x$repo$name, ""),
    count = vapply(pss, \(x) length(x$payload$commits), 1L)
  )
  cp <- dplyr::summarize(cms, count = sum(count), .by = repo)
  cp <- cp[order(cp$repo), ]
  add_class(cp, "commits_pushed")
}

summarize_events <- function(evts) {
  list(
    repos_created = repos_created(evts),
    tags_created = tags_created(evts),
    branches_created = branches_created(evts),
    commits_pushed = commits_pushed(evts)
  )
}

l_repo <- function(text, repo = text) {
  glue::glue("[{text}](https://github.com/{repo})")
}

l_tag <- function(text, repo, tag = text) {
  glue::glue("[`{text}`](https://github.com/{repo}/releases/tag/{tag})")
}

l_branch <- function(text, repo, branch = text) {
  glue::glue("[`{text}`](https://github.com/{repo}/tree/{branch})")
}

format.repos_created <- function(x, ...) {
  if (length(x) == 0) return(character())
  c("# \u2728 Repos created", "",
    glue::glue("* {l_repo(x)}"),
    "", ""
  )
}

format.tags_created <- function(x, ...) {
  if (length(x) == 0) return(character())
  perrepo <- dplyr::summarize(
    x,
    tags = paste(l_tag(sort(tag), repo), collapse = ", "),
    .by = repo
  )
  c("# \U0001f516 Tags created", "",
    glue::glue("* {l_repo(perrepo$repo)}: {perrepo$tags}"),
    "", ""
  )
}

format.branches_created <- function(x, ...) {
  if (length(x) == 0) return(character())
  perrepo <- dplyr::summarize(
    x,
    branches = paste(l_branch(sort(branch), repo), collapse = ", "),
    .by = repo
  )
  c("# \U0001f500 Branches created", "",
    glue::glue("* {l_repo(perrepo$repo)}: {perrepo$branches}"),
    "", ""
  )
}

l_commits <- function(text, repo, from, until = Sys.Date() + 1) {
  author <- get_username()
  glue::glue("[{text}](https://github.com/{repo}/commits?author={author}&since={from}&until={until})")
}

format.commits_pushed <- function(x, from, ...) {
  if (length(x) == 0) return(character())
  c("# \U0001f3c3 Commits pushed to repos", "",
    glue::glue("* {l_repo(x$repo)}: {l_commits(paste0(x$count, \" commits\"), x$repo, from)}"),
    "", ""
  )
}

format_summary <- function(summary, from) {
  c(
    format(summary$repos_created),
    format(summary$tags_created),
    format(summary$branches_created),
    format(summary$commits_pushed, from),
    NULL
  )
}

#' Format the summary and send it in an email
#'
#' @param md Markdown summary to send.

send_summary <- function(md) {
  sbjt <- glue::glue("Weekly GitHub summary ({Sys.Date()})")
  writeLines(c(sbjt, "", md))

  msg <- blastula::compose_email(
    header = sbjt,
    body = blastula::md(paste(md, collapse = "\n"))
  )

  from <- Sys.getenv("MAILGUN_FROM", "myweek@mail.gaborcsardi.org")
  rcpt <- Sys.getenv("MAILGUN_EMAIL", "csardi.gabor@gmail.com")
  url <- Sys.getenv("MAILGUN_URL", "https://api.mailgun.net/v3/mail.gaborcsardi.org/messages")
  key <- Sys.getenv("MAILGUN_API_KEY", NA_character_)
  if (is.na(key)) {
    key <- keyring::key_get("MAILGUN_API_KEY")
  }
  send_res <- blastula::send_by_mailgun(
    msg,
    subject = sbjt,
    from = from,
    recipients = rcpt,
    url = url,
    api_key = key
  )

  invisible(send_res)
}

add_class <- function(x, cls) {
  structure(x, class = c(cls, class(x)))
}

main <- function(args) {
  library(blastula)
  library(httr) # to work around a blastula bug
  from <- Sys.Date() - 7
  evts <- get_events(from)
  clev <- clean_events(evts, from = from)
  summary <- summarize_events(clev)
  md <- format_summary(summary, from = from)
  send_summary(md)
}

if (is.null(sys.call())) {
  main(commandArgs(TRUE))
}
