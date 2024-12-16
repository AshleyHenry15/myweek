l_repo <- function(text, repo = text) {
  glue("[{text}](https://github.com/{repo})")
}

l_tag <- function(text, repo, tag = text) {
  glue("[`{text}`](https://github.com/{repo}/releases/tag/{tag})")
}

l_branch <- function(text, repo, branch = text) {
  glue("[`{text}`](https://github.com/{repo}/tree/{branch})")
}

format.repos_created <- function(x, ...) {
  if (NROW(x) == 0) return(character())
  c("# \u2728 Repos created", "",
    glue("* {l_repo(x)}"),
    "", ""
  )
}

format.forks_created <- function(x, ...) {
  if (NROW(x) == 0) return(character())
  c("# \U0001f374 Forks created", "",
    glue("* {l_repo(x$repo)} \u2b05 {l_repo(x$upstream)}"),
    "", ""
  )
}

format.tags_created <- function(x, ...) {
  if (NROW(x) == 0) return(character())
  perrepo <- dplyr::summarize(
    x,
    tags = paste(l_tag(sort(tag), repo), collapse = ", "),
    .by = repo
  )
  c("# \U0001f516 Tags created", "",
    glue("* {l_repo(perrepo$repo)}: {perrepo$tags}"),
    "", ""
  )
}

format.branches_created <- function(x, ...) {
  if (NROW(x) == 0) return(character())
  byrepo <- dplyr::summarize(
    x,
    branches = paste(l_branch(sort(branch), repo), collapse = ", "),
    .by = repo
  )
  c("# \U0001f500 Branches created", "",
    glue("* {l_repo(byrepo$repo)}: {byrepo$branches}"),
    "", ""
  )
}

l_commits <- function(text, repo, from, until = Sys.Date() + 1) {
  author <- get_username()
  glue("[{text}](https://github.com/{repo}/commits?author={author}&since={from}&until={until})")
}

link_issues <- function(text, repo) {
  sub <- glue("[\\1](https://github.com/{repo}/issues/\\1)")
  gsub("(#[0-9]+)\\b", sub, text)
}

l_commit <- function(text, repo, sha) {
  text <- map2_chr(text, repo, link_issues)
  glue("{text} [\u27a1](https://github.com/{repo}/commit/{sha})")
}

format_commits_for_repo <- function(repo, x, from) {
  cmts <- x[x$repo == repo, ]
  lab <- cli::pluralize("{nrow(cmts)} commit{?s}")
  c(glue("## {l_repo(repo)} ({l_commits(lab, repo, from)})"), "",
    glue("* {l_commit(cmts$message, cmts$repo, cmts$sha)}"), ""
  )
}

format.commits_pushed <- function(x, from, ...) {
  if (NROW(x) == 0) return(character())
  rps <- unique(x$repo)
  c("# \U0001f3c3 Commits pushed to repos", "",
    unlist(map(rps, format_commits_for_repo, x, from)),
    "", ""
  )
}

format_issues_for_repo <- function(repo, x) {
  x <- x[x$repo == repo, ]
  text <- glue("{x$title} #{x$number}")
  c(glue("## {l_repo(repo)}"), "",
    glue("* {link_issues(text, repo)}"), ""
  )
}

format.issues_opened <- function(x, ...) {
  if (NROW(x) == 0) return(character())
  rps <- unique(x$repo)
  c("# \u2795 Issues opened", "",
    unlist(map(rps, format_issues_for_repo, x)),
    "", ""
  )
}

format.issues_closed <- function(x, ...) {
  if (NROW(x) == 0) return(character())
  rps <- unique(x$repo)
  c("# \u2705 Issues closed", "",
    unlist(map(rps, format_issues_for_repo, x)),
    "", ""
  )
}

format_summary <- function(smry, from) {
  c(
    format(smry$repos_created),
    format(smry$forks_created),
    format(smry$tags_created),
    format(smry$branches_created),
    format(smry$issues_opened),
    format(smry$issues_closed),
    format(smry$commits_pushed, from),
    NULL
  )
}
