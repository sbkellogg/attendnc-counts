# src/build.R
#
# Assembles each public AttendNC Bright Spots page from:
#   - shared header/footer partials (src/partials/*.html)
#   - shared design tokens/reset/header/footer CSS (styles/shared.css)
#   - page-specific metadata, CSS, and body content (defined below /
#     src/pages/*.html)
#
# Deployed pages live in topical subfolders off the repo root (mirroring the
# innovation-leadership-council site convention), e.g.:
#   index.html                                  (landing / home)
#   explore/index.html (map + embedded dashboard), explore/about-analysis.html
#   research/index.html
#   spotlights/<school-slug>/index.html
#   reports/<report-slug>/index.html            (future; placeholder for now)
# `styles/` and `images/` stay at the repo root and are referenced with a
# relative "../" prefix computed from each page's folder depth.
#
# Re-run this script (`Rscript src/build.R` from the repo root, or
# `source("src/build.R")` from an R session already in the repo root)
# whenever a partial or a page source file changes. It overwrites the
# deployed HTML files listed in each page's `output` field.

read_file <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# ---- Path helpers -----------------------------------------------------------
# Every canonical path below (page outputs, "images/...", "styles/...") is
# expressed relative to the repo root. `up_prefix()` turns a page's output
# path into the "../" chain needed to reach the root from that page's folder,
# so any root-relative path can be resolved as `paste0(up, root_relative_path)`.
page_depth <- function(output) {
  dir <- dirname(output)
  if (identical(dir, ".")) 0L else length(strsplit(dir, "/", fixed = TRUE)[[1]])
}

up_prefix <- function(output) strrep("../", page_depth(output))

# ---- Cross-page link map ----------------------------------------------------
# Maps each page's *old* flat filename (as still referenced literally inside
# src/pages/*-body.html) to its new root-relative canonical path, so body
# content written before the restructure keeps working. Update this map (and
# the affected body files) if a page's canonical path changes again.
page_map <- c(
  "attendnc-bright-spots-landing-page.html" = "index.html",
  "explore-data.html" = "explore/index.html",
  "about-analysis.html" = "explore/about-analysis.html",
  "attendnc-attendance-research.html" = "research/index.html",
  "ecu-community-school-spotlight.html" = "spotlights/ecu-community/index.html",
  "northwest-elementary-school-spotlight-revised.html" = "spotlights/northwest-elementary/index.html",
  "bright-spots-feasibility-study.html" = "research/bright-spots-feasibility-study/index.html"
)

landing_output <- unname(page_map["attendnc-bright-spots-landing-page.html"])

# Rewrites a root-relative asset reference ("images/..." or "documents/...")
# inside `html` so it resolves correctly from a page living `up` directories
# deep.
resolve_assets <- function(html, up) {
  html <- gsub("images/", paste0(up, "images/"), html, fixed = TRUE)
  gsub("documents/", paste0(up, "documents/"), html, fixed = TRUE)
}

# Like resolve_assets(), plus rewrites any occurrence of an *old* flat page
# filename (as still referenced literally inside src/pages/*-body.html) to
# its new canonical path. Only apply this to body content — header/footer
# HTML is already built with correct new-style paths via build_nav() /
# build_footer_links(), and running the old-name substitution over those
# would corrupt any new path that happens to contain an old filename as a
# substring (e.g. "explore/about-analysis.html" contains "about-analysis.html").
resolve_links <- function(html, up) {
  for (old_name in names(page_map)) {
    html <- gsub(old_name, paste0(up, page_map[[old_name]]), html, fixed = TRUE)
  }
  resolve_assets(html, up)
}

# ---- Primary navigation ----------------------------------------------------
# The top bar is reserved for the site's main index pages (each with its own
# URL and, where useful, its own in-page quick-nav for anchor sections).
# Items are either an `anchor` on the landing page, or a `target` pointing at
# a distinct root-relative canonical page path.
nav_items <- list(
  list(key = "home", label = "Bright Spots", target = landing_output),
  list(
    key = "explore",
    label = "Explore the Data",
    target = "explore/index.html"
  ),
  list(
    key = "about-analysis",
    label = "About the Analysis",
    target = "explore/about-analysis.html"
  ),
  list(key = "research", label = "Research", target = "research/index.html")
)

build_nav <- function(current_key, is_home, up) {
  lines <- vapply(
    nav_items,
    function(item) {
      href <- if (!is.null(item$anchor)) {
        if (is_home) {
          paste0("#", item$anchor)
        } else {
          paste0(up, landing_output, "#", item$anchor)
        }
      } else {
        paste0(up, item$target)
      }
      current_attr <- if (
        !is.null(current_key) && identical(item$key, current_key)
      ) {
        ' aria-current="page"'
      } else {
        ""
      }
      sprintf('      <a href="%s"%s>%s</a>', href, current_attr, item$label)
    },
    character(1)
  )
  paste(lines, collapse = "\n")
}

# ---- Page manifest -----------------------------------------------------------
# nav_current: which primary nav item (if any) gets aria-current="page".
# is_home: TRUE only for the landing page (affects brand link + anchor prefixes).
pages <- list(
  list(
    id = "landing",
    output = landing_output,
    title = "AttendNC Bright Spots | NCDPI",
    description = "AttendNC Bright Spots highlights North Carolina schools making unusual progress in reducing chronic absenteeism and shares what educators are learning from them.",
    css = "styles/landing.css",
    body = "src/pages/landing-body.html",
    scripts = "src/pages/landing-scripts.html",
    nav_current = "home",
    is_home = TRUE
  ),
  list(
    id = "explore-data",
    output = "explore/index.html",
    title = "Explore the Data | AttendNC Bright Spots",
    description = "Explore the AttendNC Bright Spots map, dashboard, and analysis.",
    css = "styles/explore-data.css",
    body = "src/pages/explore-data-body.html",
    scripts = NULL,
    nav_current = "explore",
    is_home = FALSE
  ),
  list(
    id = "about-analysis",
    output = "explore/about-analysis.html",
    title = "About the Analysis | AttendNC Bright Spots",
    description = "How AttendNC identifies Bright Spot schools using context-adjusted chronic absenteeism analysis.",
    css = "styles/about-analysis.css",
    body = "src/pages/about-analysis-body.html",
    scripts = NULL,
    nav_current = "about-analysis",
    is_home = FALSE
  ),
  list(
    id = "research",
    output = "research/index.html",
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
    output = "spotlights/ecu-community/index.html",
    title = "ECU Community School | AttendNC Counts School Spotlight",
    description = "AttendNC Bright Spots school spotlight for ECU Community School, an East Carolina University laboratory school.",
    css = "styles/ecu-spotlight.css",
    body = "src/pages/ecu-spotlight-body.html",
    scripts = NULL,
    nav_current = NULL,
    is_home = FALSE
  ),
  list(
    id = "northwest-spotlight",
    output = "spotlights/northwest-elementary/index.html",
    title = "Northwest Elementary | AttendNC Counts School Spotlight",
    description = "AttendNC Bright Spots school spotlight for Northwest Elementary School in Pitt County Schools.",
    css = "styles/northwest-spotlight.css",
    body = "src/pages/northwest-spotlight-body.html",
    scripts = NULL,
    nav_current = NULL,
    is_home = FALSE
  ),
  list(
    id = "bright-spots-feasibility-study",
    output = "research/bright-spots-feasibility-study/index.html",
    title = "Bright Spots Feasibility Study | AttendNC Counts",
    description = "AttendNC Counts Bright Spots feasibility study information page.",
    css = "styles/bright-spots-feasibility-study.css",
    body = "src/pages/bright-spots-feasibility-study-body.html",
    scripts = NULL,
    nav_current = NULL,
    is_home = FALSE
  )
)

# ---- Build --------------------------------------------------------------
header_tpl <- read_file("src/partials/header.html")
footer_tpl <- read_file("src/partials/footer.html")

for (p in pages) {
  up <- up_prefix(p$output)
  brand_href <- if (p$is_home) "#top" else paste0(up, landing_output)

  header_html <- sub("__BRAND_HREF__", brand_href, header_tpl, fixed = TRUE)
  header_html <- sub(
    "__NAV_ITEMS__",
    build_nav(p$nav_current, p$is_home, up),
    header_html,
    fixed = TRUE
  )
  header_html <- resolve_assets(header_html, up)

  footer_html <- resolve_assets(footer_tpl, up)

  body_html <- resolve_links(read_file(p$body), up)
  scripts_html <- if (!is.null(p$scripts) && file.exists(p$scripts)) {
    read_file(p$scripts)
  } else {
    ""
  }

  out <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    sprintf("<meta name=\"description\" content=\"%s\">\n", p$description),
    sprintf("<title>%s</title>\n", p$title),
    sprintf("<link rel=\"stylesheet\" href=\"%sstyles/shared.css\">\n", up),
    sprintf("<link rel=\"stylesheet\" href=\"%s%s\">\n", up, p$css),
    "</head>\n<body>\n",
    header_html,
    "\n",
    body_html,
    "\n",
    footer_html,
    "\n",
    scripts_html,
    "\n</body>\n</html>\n"
  )

  out_dir <- dirname(p$output)
  if (!identical(out_dir, ".") && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  writeLines(out, p$output, useBytes = TRUE)
  message("Built ", p$output)
}
