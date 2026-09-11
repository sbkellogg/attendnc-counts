# src/build.R
#
# Assembles each public AttendNC Bright Spots page from:
#   - shared header/footer partials (src/partials/*.html)
#   - shared design tokens/reset/header/footer CSS (styles/shared.css)
#   - page-specific metadata, CSS, and body content (defined below /
#     src/pages/*.html)
#
# Re-run this script (`Rscript src/build.R` from the repo root, or
# `source("src/build.R")` from an R session already in the repo root)
# whenever a partial or a page source file changes. It overwrites the
# deployed root-level HTML files listed in each page's `output` field.

read_file <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# ---- Primary navigation ----------------------------------------------------
# `href` is either a same-site anchor on the landing page (resolved relative
# to the landing page unless we're currently building the landing page
# itself, in which case the "attendnc-bright-spots-landing-page.html" prefix
# is dropped) or an absolute path to another top-level page (`absolute = TRUE`).
nav_items <- list(
  list(key = "about",       label = "About",                href = "#about"),
  list(key = "explore",     label = "Explore the Data",      href = "explore-data.html", absolute = TRUE),
  list(key = "learning",    label = "What We're Learning",   href = "#learning"),
  list(key = "spotlights",  label = "School Spotlights",     href = "#spotlights"),
  list(key = "research",    label = "Research",              href = "attendnc-attendance-research.html", absolute = TRUE)
)

landing_page <- "attendnc-bright-spots-landing-page.html"

build_nav <- function(current_key, is_home) {
  home_prefix <- if (is_home) "" else landing_page
  lines <- vapply(nav_items, function(item) {
    href <- if (isTRUE(item$absolute)) item$href else paste0(home_prefix, item$href)
    current_attr <- if (!is.null(current_key) && identical(item$key, current_key)) {
      ' aria-current="page"'
    } else {
      ""
    }
    sprintf('      <a href="%s"%s>%s</a>', href, current_attr, item$label)
  }, character(1))
  paste(lines, collapse = "\n")
}

# ---- Footer links -----------------------------------------------------------
build_footer_links <- function(is_home) {
  home_prefix <- if (is_home) "" else landing_page
  home_href <- if (is_home) "#top" else landing_page
  items <- list(
    list(label = "Home", href = home_href),
    list(label = "Explore the Data", href = "explore-data.html"),
    list(label = "School Spotlights", href = paste0(home_prefix, "#spotlights")),
    list(label = "About the Analysis", href = "about-analysis.html"),
    list(label = "Research", href = "attendnc-attendance-research.html")
  )
  lines <- vapply(items, function(i) {
    sprintf('        <li><a href="%s">%s</a></li>', i$href, i$label)
  }, character(1))
  paste(lines, collapse = "\n")
}

# ---- Page manifest -----------------------------------------------------------
# nav_current: which primary nav item (if any) gets aria-current="page".
# is_home: TRUE only for the landing page (affects brand link + anchor prefixes).
pages <- list(
  list(
    id = "landing",
    output = landing_page,
    title = "AttendNC Bright Spots | NCDPI",
    description = "AttendNC Bright Spots highlights North Carolina schools making unusual progress in reducing chronic absenteeism and shares what educators are learning from them.",
    css = "styles/landing.css",
    body = "src/pages/landing-body.html",
    scripts = "src/pages/landing-scripts.html",
    nav_current = NULL,
    is_home = TRUE
  ),
  list(
    id = "explore-data",
    output = "explore-data.html",
    title = "Explore the Data | AttendNC Bright Spots",
    description = "Explore the AttendNC Bright Spots map, dashboard, and analysis.",
    css = "styles/explore-data.css",
    body = "src/pages/explore-data-body.html",
    scripts = NULL,
    nav_current = "explore",
    is_home = FALSE
  ),
  list(
    id = "dashboard",
    output = "bright-spots-dashboard.html",
    title = "Bright Spots Dashboard | AttendNC",
    description = "Interactive AttendNC Bright Spots dashboard for exploring statewide school-level results.",
    css = "styles/dashboard.css",
    body = "src/pages/dashboard-body.html",
    scripts = NULL,
    nav_current = "explore",
    is_home = FALSE
  ),
  list(
    id = "about-analysis",
    output = "about-analysis.html",
    title = "About the Analysis | AttendNC Bright Spots",
    description = "How AttendNC identifies Bright Spot schools using context-adjusted chronic absenteeism analysis.",
    css = "styles/about-analysis.css",
    body = "src/pages/about-analysis-body.html",
    scripts = NULL,
    nav_current = "explore",
    is_home = FALSE
  ),
  list(
    id = "research",
    output = "attendnc-attendance-research.html",
    title = "AttendNC Research | AttendNC Bright Spots",
    description = "AttendNC research summary: what North Carolina and national research says about chronic absenteeism, its consequences, causes, and promising responses.",
    css = "styles/research.css",
    body = "src/pages/research-body.html",
    scripts = NULL,
    nav_current = "research",
    is_home = FALSE
  ),
  list(
    id = "ecu-spotlight",
    output = "ecu-community-school-spotlight.html",
    title = "ECU Community School | AttendNC Counts School Spotlight",
    description = "AttendNC Bright Spots school spotlight for ECU Community School, an East Carolina University laboratory school.",
    css = "styles/ecu-spotlight.css",
    body = "src/pages/ecu-spotlight-body.html",
    scripts = NULL,
    nav_current = "spotlights",
    is_home = FALSE
  ),
  list(
    id = "northwest-spotlight",
    output = "northwest-elementary-school-spotlight-revised.html",
    title = "Northwest Elementary | AttendNC Counts School Spotlight",
    description = "AttendNC Bright Spots school spotlight for Northwest Elementary School in Pitt County Schools.",
    css = "styles/northwest-spotlight.css",
    body = "src/pages/northwest-spotlight-body.html",
    scripts = NULL,
    nav_current = "spotlights",
    is_home = FALSE
  )
)

# ---- Build --------------------------------------------------------------
header_tpl <- read_file("src/partials/header.html")
footer_tpl <- read_file("src/partials/footer.html")

for (p in pages) {
  brand_href <- if (p$is_home) "#top" else landing_page

  header_html <- sub("__BRAND_HREF__", brand_href, header_tpl, fixed = TRUE)
  header_html <- sub("__NAV_ITEMS__", build_nav(p$nav_current, p$is_home), header_html, fixed = TRUE)

  footer_html <- sub("__FOOTER_LINKS__", build_footer_links(p$is_home), footer_tpl, fixed = TRUE)

  body_html <- read_file(p$body)
  scripts_html <- if (!is.null(p$scripts) && file.exists(p$scripts)) read_file(p$scripts) else ""

  out <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    sprintf("<meta name=\"description\" content=\"%s\">\n", p$description),
    sprintf("<title>%s</title>\n", p$title),
    "<link rel=\"stylesheet\" href=\"styles/shared.css\">\n",
    sprintf("<link rel=\"stylesheet\" href=\"%s\">\n", p$css),
    "</head>\n<body>\n",
    header_html, "\n",
    body_html, "\n",
    footer_html, "\n",
    scripts_html,
    "\n</body>\n</html>\n"
  )

  writeLines(out, p$output, useBytes = TRUE)
  message("Built ", p$output)
}
