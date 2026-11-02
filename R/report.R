#' Write a self-contained HTML report of a career
#'
#' The report gathers the descriptive profile, a timeline, contributions by
#' period, transitions, every item of evidence with its origin, the sources
#' consulted and the coverage of each dimension. It has no external
#' resources.
#'
#' @param career An `ac_career`.
#' @param file Output HTML file. There is no default, so nothing is written
#'   unless a destination is given.
#' @param lang Language: `"en"` or `"es"`.
#' @param period Length of the periods in the contributions table.
#'
#' @return The path of the report, invisibly.
#' @export
#'
#' @examples
#' out <- file.path(tempdir(), "career-report.html")
#' ac_report(ac_example("software"), out)
ac_report <- function(career, file, lang = c("en", "es"), period = 5) {
  stopifnot(inherits(career, "ac_career"))
  lang <- match.arg(lang)
  s <- strings(lang)
  e <- html_escape
  prof <- ac_profile(career, lang)
  tl <- ac_timeline(career)
  contrib <- ac_contributions(career, period)
  trans <- ac_transitions(career, period)
  ev <- career$events

  table_html <- function(df) {
    if (!nrow(df)) return("<p>&mdash;</p>")
    head <- paste0("<tr>", paste0("<th>", e(names(df)), "</th>", collapse = ""), "</tr>")
    body <- vapply(seq_len(nrow(df)), function(i) {
      cells <- vapply(df[i, , drop = FALSE], function(v) {
        v <- as.character(v)
        if (is.na(v)) "" else if (grepl("^https?://", v)) sprintf('<a href="%s">%s</a>', e(v), e(sub("^https?://", "", v))) else e(v)
      }, "")
      paste0("<tr>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
    }, "")
    paste0('<div class="scroll"><table><thead>', head, "</thead><tbody>", paste(body, collapse = ""),
           "</tbody></table></div>")
  }

  years <- sort(unique(tl$year))
  level <- function(n) if (n == 0) 0 else if (n == 1) 1 else if (n <= 3) 2 else if (n <= 6) 3 else 4
  grid <- vapply(unique(tl$dimension), function(d) {
    rows <- tl[tl$dimension == d, , drop = FALSE]
    rows <- rows[order(rows$year), , drop = FALSE]
    cells <- vapply(seq_len(nrow(rows)), function(i) {
      sprintf('<td class="l%d%s" title="%d: %d"></td>', level(rows$n[i]),
              if (rows$break_year[i]) " brk" else "", rows$year[i], rows$n[i])
    }, "")
    sprintf("<tr><th>%s</th>%s</tr>", e(dimension_label(d, lang)), paste(cells, collapse = ""))
  }, "")
  axis <- vapply(years, function(y) sprintf("<td>%s</td>", if (y %% 5 == 0 || y == years[1]) y else ""), "")
  timeline_html <- paste0('<div class="scroll"><table class="tl"><tbody>', paste(grid, collapse = ""),
                          '<tr class="axis"><th></th>', paste(axis, collapse = ""), "</tr></tbody></table></div>")

  evidence <- data.frame(ev$start, dimension_label(ev$dimension, lang), ev$title,
                         ifelse(ev$declared_by == "self", s$declared, ev$source), ev$url,
                         stringsAsFactors = FALSE)
  names(evidence) <- c(s$year, s$dimension, s$title_col, s$source, "URL")
  dims <- ac_dimensions()
  coverage <- data.frame(dims[[lang]], dims$sources, dims$coverage, stringsAsFactors = FALSE)
  names(coverage) <- c(s$dimension, s$source, s$coverage)
  consulted <- career$sources
  br <- career$context$breaks
  cdf <- as.data.frame(unclass(contrib), stringsAsFactors = FALSE)

  css <- paste(
    ":root{--ink:#1c1e21;--muted:#5d636b;--bg:#fafaf9;--card:#fff;--line:#e3e4e6;--accent:#3d5a80;",
    "--l0:#efefec;--l1:#cfdcd6;--l2:#8fb3a6;--l3:#3d6b5e;--l4:#173a32;--brk:#c4673e}",
    "@media (prefers-color-scheme:dark){:root{--ink:#e8e9eb;--muted:#a8adb4;--bg:#15171a;--card:#1d2024;",
    "--line:#30343a;--accent:#9dbbe0;--l0:#24282d;--l1:#2f4a42;--l2:#467766;--l3:#7db5a2;--l4:#c4ead9;--brk:#e08a63}}",
    "body{margin:0;background:var(--bg);color:var(--ink);font:16px/1.55 system-ui,-apple-system,Segoe UI,sans-serif}",
    "main{max-width:1050px;margin:0 auto;padding:32px 16px 64px}",
    "h1{font-family:Georgia,serif;font-weight:normal;margin:0 0 4px}h2{margin:40px 0 12px;font-size:1.2em;color:var(--accent)}",
    ".muted{color:var(--muted)}.caveat{border-left:4px solid var(--accent);padding:4px 14px;background:var(--card)}",
    "ul.hl li{margin:6px 0}.scroll{overflow-x:auto}",
    "table{border-collapse:collapse;width:100%;font-size:.9em;background:var(--card)}",
    "th,td{border-bottom:1px solid var(--line);padding:6px 8px;text-align:left;vertical-align:top}th{color:var(--muted);font-weight:600}",
    "table.tl{width:auto;background:none}table.tl td{width:14px;height:18px;padding:0;border:1px solid var(--bg)}",
    "table.tl th{white-space:nowrap;padding-right:10px;border:0;font-weight:normal}",
    "td.l0{background:var(--l0)}td.l1{background:var(--l1)}td.l2{background:var(--l2)}td.l3{background:var(--l3)}td.l4{background:var(--l4)}",
    "td.brk{background-image:repeating-linear-gradient(45deg,var(--brk) 0 2px,transparent 2px 6px)}",
    "tr.axis td{font-size:.7em;color:var(--muted);border:0;overflow:visible;white-space:nowrap}",
    "footer{margin-top:48px;color:var(--muted);font-size:.85em}"
  )
  who <- if (!is.null(career$name)) career$name else s$title
  html <- c(
    "<!doctype html>", sprintf('<html lang="%s"><head><meta charset="utf-8">', lang),
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    sprintf("<title>%s: %s</title>", e(s$title), e(who)), "<style>", css, "</style></head><body><main>",
    sprintf("<h1>%s</h1>", e(who)),
    sprintf('<p class="muted">%s%s</p>', e(s$title),
            if (!is.null(career$orcid)) sprintf(' &middot; <a href="https://orcid.org/%s">orcid.org/%s</a>',
                                                career$orcid, career$orcid) else ""),
    sprintf('<div class="caveat"><p><strong>%s.</strong> %s</p></div>', e(s$caveat_title), e(s$caveat)),
    sprintf("<h2>%s</h2>", e(s$summary)),
    sprintf('<ul class="hl">%s</ul>', paste0("<li>", e(prof$statements), "</li>", collapse = "")),
    sprintf("<h2>%s</h2>", e(s$timeline)), sprintf('<p class="muted">%s</p>', e(s$legend)), timeline_html,
    sprintf("<h2>%s</h2>", e(s$contributions)), table_html(cdf),
    sprintf("<h2>%s</h2>", e(s$transitions)), table_html(as.data.frame(unclass(trans))),
    if (nrow(br) || !is.null(career$context$field) || !is.null(career$context$notes)) c(
      sprintf("<h2>%s</h2>", e(s$context)),
      if (!is.null(career$context$field)) sprintf("<p><strong>%s:</strong> %s</p>", e(s$field), e(career$context$field)),
      if (nrow(br)) c(sprintf("<h3>%s</h3>", e(s$breaks)), table_html(br)),
      if (!is.null(career$context$notes)) sprintf("<p><strong>%s:</strong> %s</p>", e(s$notes), e(career$context$notes))),
    sprintf("<h2>%s</h2>", e(s$evidence)), table_html(evidence),
    sprintf("<h2>%s</h2>", e(s$sources)),
    if (nrow(consulted)) c(sprintf("<h3>%s</h3>", e(s$consulted)), table_html(consulted)),
    table_html(coverage),
    sprintf("<footer>%s %s.</footer>", e(s$generated), format(Sys.Date())),
    "</main></body></html>"
  )
  writeLines(enc2utf8(html), file, useBytes = TRUE)
  invisible(normalizePath(file))
}
