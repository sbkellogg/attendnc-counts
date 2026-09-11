# =============================================================================
# global.R — 2025 Attendance Bright Spots Shiny App (v2)
# =============================================================================

# Libraries ####

library(shiny)
library(tidyverse)
library(leaflet)
library(reactable)
library(htmltools)
library(bslib)


# Constants ####

CANDIDATE_YEAR <- 2025
NCDPI_NAVY <- "#003A70"

GRADE_BAND_LEVELS <- c(
  "Elementary",
  "Elem-Mid",
  "Middle",
  "Mid-High",
  "High",
  "K-12/Combined",
  "Other/Unparsed"
)

NCDPI_BRAND_COLORS <- c(
  "Elementary" = "#003A70",
  "Middle" = "#922880",
  "High" = "#29B5D9",
  "Elem-Mid" = "#FF9015",
  "Mid-High" = "#74C04B",
  "K-12/Combined" = "#B7B9BB",
  "Other/Unparsed" = "#d0d0d0"
)

DECREASE_THRESHOLDS <- list(
  list(label = "\u226550% decrease", min = 0.50, max = Inf),
  list(label = "40\u201349% decrease", min = 0.40, max = 0.50),
  list(label = "30\u201339% decrease", min = 0.30, max = 0.40),
  list(label = "20\u201329% decrease", min = 0.20, max = 0.30),
  list(label = "10\u201319% decrease", min = 0.10, max = 0.20)
)

THRESHOLD_LABELS <- map_chr(DECREASE_THRESHOLDS, "label")

# Bright Spot category levels (ordered best → good)
PD_GROUP_LEVELS <- c(
  "Exceptional",
  "Strong",
  "Promising"
)

# Badge colours keyed by short label
PD_GROUP_COLORS <- c(
  "Exceptional" = "#922880",
  "Strong" = "#003A70",
  "Promising" = "#29B5D9"
)


# =============================================================================
# Grade Band Classification Helpers ####
# =============================================================================

normalize_span <- function(span) {
  span |>
    as.character() |>
    toupper() |>
    trimws() |>
    str_replace_all("\\s+", "") |>
    str_replace_all(":", "-")
}

parse_grade_token <- function(x) {
  x <- toupper(trimws(x))
  if (x %in% c("K", "KG", "0K")) {
    return(0L)
  }
  if (x %in% c("PK", "PREK")) {
    return(-1L)
  }
  if (str_detect(x, "^[0-9]{1,2}$")) {
    return(as.integer(x))
  }
  NA_integer_
}

grade_band_from_span <- function(span) {
  if (is.na(span) || trimws(span) == "") {
    return(NA_character_)
  }
  s <- normalize_span(span)
  parts <- str_split(s, "-", n = 2, simplify = TRUE)
  if (ncol(parts) != 2) {
    return("Other/Unparsed")
  }
  g1 <- parse_grade_token(parts[1])
  g2 <- parse_grade_token(parts[2])
  if (is.na(g1) || is.na(g2)) {
    return("Other/Unparsed")
  }
  g2 <- min(g2, 12L)
  if (g2 <= 5) {
    return("Elementary")
  }
  if (g1 >= 6 && g2 <= 8) {
    return("Middle")
  }
  if (g1 >= 9 && g2 <= 12) {
    return("High")
  }
  if (g1 <= 5 && g2 <= 8) {
    return("Elem-Mid")
  }
  if (g1 >= 6 && g2 >= 9) {
    return("Mid-High")
  }
  if (g1 <= 5 && g2 >= 9) {
    return("K-12/Combined")
  }
  "Other/Unparsed"
}

grade_band_from_span_vec <- Vectorize(grade_band_from_span, USE.NAMES = FALSE)


# =============================================================================
# Load App Data ####
# =============================================================================

app_data <- readRDS("data/app_data.rds")
nc_state_boundary <- readRDS("data/nc_state_boundary.rds")
nc_county_boundaries <- readRDS("data/nc_county_boundaries.rds")


# =============================================================================
# Filter Choice Vectors (used by ui.R) ####
# =============================================================================

app_county_choices <- sort(unique(na.omit(app_data$county)))
grade_band_choices <- GRADE_BAND_LEVELS[
  GRADE_BAND_LEVELS %in% unique(app_data$grade_band)
]
poverty_choices <- sort(unique(na.omit(app_data$poverty)))
enroll_range <- range(app_data$den, na.rm = TRUE)
eds_range <- range(app_data$pct_eds, na.rm = TRUE)


# =============================================================================
# Map Palettes ####
# =============================================================================

grade_band_palette <- colorFactor(
  palette = NCDPI_BRAND_COLORS,
  levels = GRADE_BAND_LEVELS,
  domain = app_data$grade_band
)


# =============================================================================
# Popup Builder ####
# =============================================================================

make_school_popups <- function(df) {
  unname(mapply(
    function(
      name,
      county,
      grade_band,
      poverty,
      den,
      year_peak,
      p_peak,
      p_current,
      improvement_pp,
      pct_decrease,
      expected_rate,
      residual_pp,
      pd_group
    ) {
      # Format model fields — NA-safe
      fmt_pct <- function(x) {
        if (is.na(x)) "N/A" else paste0(round(x * 100, 1), "%")
      }
      fmt_pp <- function(x) {
        if (is.na(x)) {
          return("N/A")
        }
        sign_str <- if (x < 0) "\u2212" else "+"
        paste0(sign_str, abs(round(x, 1)), " pp")
      }

      pd_badge <- if (!is.na(pd_group)) {
        badge_color <- PD_GROUP_COLORS[pd_group]
        if (is.na(badge_color)) {
          badge_color <- "#888888"
        }
        paste0(
          "<span style='background:",
          badge_color,
          "; color: white; ",
          "border-radius: 4px; padding: 2px 7px; font-size: 11px; ",
          "font-weight: 700; display: inline-block; margin-bottom: 8px;'>",
          pd_group,
          "</span><br>"
        )
      } else {
        ""
      }

      paste0(
        "<div style='font-family: Arial, sans-serif; min-width: 260px;'>",
        "<h4 style='margin: 0 0 6px 0; color: ",
        NCDPI_NAVY,
        ";'>",
        name,
        "</h4>",
        pd_badge,
        "<p style='margin: 5px 0;'>",
        "<strong>County:</strong> ",
        if_else(is.na(county), "Unknown", county),
        "<br>",
        "<strong>Grade Band:</strong> ",
        if_else(is.na(grade_band), "Unknown", grade_band),
        "<br>",
        "<strong>Poverty:</strong> ",
        if_else(is.na(poverty), "Unknown", poverty),
        "<br>",
        "<strong>Enrollment:</strong> ",
        as.integer(den),
        "</p>",
        "<hr style='margin: 8px 0; border-top: 1px solid #ccc;'>",
        "<p style='margin: 5px 0;'><strong>Chronic Absenteeism Rates:</strong><br>",
        "Peak (",
        as.integer(year_peak),
        "): ",
        "<span style='color: #e74c3c; font-weight: bold;'>",
        round(p_peak * 100, 1),
        "%</span><br>",
        "Current (",
        CANDIDATE_YEAR,
        "): ",
        "<span style='color: #27ae60; font-weight: bold;'>",
        round(p_current * 100, 1),
        "%</span>",
        "</p>",
        "<hr style='margin: 8px 0; border-top: 1px solid #ccc;'>",
        "<p style='margin: 5px 0;'><strong>Performance:</strong><br>",
        "Improvement: <strong>",
        round(improvement_pp, 1),
        " pp</strong><br>",
        "Percent Decrease: <strong>",
        round(pct_decrease * 100, 1),
        "%</strong>",
        "</p>",
        "<hr style='margin: 8px 0; border-top: 1px solid #ccc;'>",
        "<p style='margin: 5px 0;'><strong>Model Expectation:</strong><br>",
        "Expected Rate: <strong>",
        fmt_pct(expected_rate),
        "</strong><br>",
        "vs. Expected: <strong>",
        fmt_pp(residual_pp),
        "</strong>",
        "</p></div>"
      )
    },
    df$name,
    df$county,
    df$grade_band,
    df$poverty,
    df$den,
    df$year_peak,
    df$p_peak,
    df$p_current,
    df$improvement_pp,
    df$pct_decrease,
    df$expected_rate,
    df$residual_pp,
    df$pd_group,
    SIMPLIFY = TRUE
  ))
}


# =============================================================================
# Reactable Theme (shared) ####
# =============================================================================

ncdpi_table_theme <- reactableTheme(
  borderColor = "#dee2e6",
  stripedColor = "#f5f8fc",
  highlightColor = "#eaf1fb",
  headerStyle = list(
    background = "#ffffff",
    color = NCDPI_NAVY,
    fontWeight = "700",
    fontSize = "14px",
    borderBottom = paste0("3px solid ", NCDPI_NAVY),
    textTransform = "uppercase",
    letterSpacing = "0.05em"
  )
)
