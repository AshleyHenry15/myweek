#' Format the summary and send it in an email
#'
#' @param md Markdown summary to send.

send_summary <- function(md, dry_run = FALSE) {
  sbjt <- glue("Weekly GitHub summary ({Sys.Date()})")
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
  send_res <- if (dry_run) {
    cli::cli_alert_info(
      "Not sending mail to {.val {rcpt}}, subject {.val {sbjt}} "
    )
  } else {
    blastula::send_by_mailgun(
      msg,
      subject = sbjt,
      from = from,
      recipients = rcpt,
      url = url,
      api_key = key
    )
  }

  invisible(send_res)
}
