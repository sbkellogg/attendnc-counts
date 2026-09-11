page_sidebar(
  title = tags$span(
    style = "color: white; font-weight: 700;",
    "2025 AttendNC Bright Spots"
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    primary = NCDPI_NAVY,
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro")
  ),
  tags$style(HTML(paste0(
    ".navbar, .bslib-page-title { background-color: ",
    NCDPI_NAVY,
    " !important; }",
    ".navbar *, .bslib-page-title * { color: white !important; }",
    ".nav-item:nth-child(2) .nav-link, .nav-item:nth-child(2) .nav-link * { color: #922880 !important; }",
    ".nav-item:nth-child(2) .nav-link.active, .nav-item:nth-child(2) .nav-link.active * { color: #922880 !important; }"
  ))),

  # ── Sidebar ──────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 290,

    # Summary badge — updated reactively
    uiOutput("filter_summary"),

    hr(),

    # --- County ------------------------------------------------------------
    selectInput(
      inputId = "county",
      label = "County",
      choices = c("All" = "", app_county_choices),
      selected = "",
      multiple = FALSE
    ),

    hr(),

    # --- Bright Spot Category ----------------------------------------------
    checkboxGroupInput(
      inputId = "pd_group",
      label = tags$span(
        icon("star", style = "color: #922880; margin-right: 4px;"),
        "Bright Spot Category"
      ),
      choices = PD_GROUP_LEVELS,
      selected = PD_GROUP_LEVELS
    ),

    checkboxInput(
      inputId = "show_all_schools",
      label = tags$span(
        style = "font-size: 13px; color: #555;",
        "Show all schools, not just Bright Spots"
      ),
      value = FALSE
    ),

    hr(),

    # --- Decrease threshold ------------------------------------------------
    checkboxGroupInput(
      inputId = "threshold",
      label = "Decrease since 2022 Peak",
      choices = THRESHOLD_LABELS,
      selected = THRESHOLD_LABELS
    ),

    hr(),

    # --- Enrollment range --------------------------------------------------
    sliderInput(
      inputId = "enrollment",
      label = "Enrollment",
      min = enroll_range[1],
      max = enroll_range[2],
      value = enroll_range,
      step = 10,
      sep = ","
    ),

    hr(),

    # --- Grade band --------------------------------------------------------
    checkboxGroupInput(
      inputId = "grade_band",
      label = "Grade Band",
      choices = grade_band_choices,
      selected = grade_band_choices
    ),

    hr(),

    # --- Poverty -----------------------------------------------------------
    checkboxGroupInput(
      inputId = "poverty",
      label = "Poverty Designation",
      choices = poverty_choices,
      selected = poverty_choices
    ),

    hr(),

    # --- % Economically Disadvantaged --------------------------------------
    sliderInput(
      inputId = "pct_eds",
      label = "% Economically Disadvantaged",
      min = floor(eds_range[1] * 100),
      max = ceiling(eds_range[2] * 100),
      value = c(floor(eds_range[1] * 100), ceiling(eds_range[2] * 100)),
      step = 1,
      post = "%"
    ),

    hr(),

    actionButton(
      "reset_filters",
      "Reset Filters",
      class = "btn-outline-secondary btn-sm w-100"
    )
  ),

  # ── Main panel ───────────────────────────────────────────────────────────
  navset_card_underline(
    nav_panel(
      title = "Map",
      icon = icon("map"),
      leafletOutput("bright_spots_map", height = "650px")
    ),

    nav_panel(
      title = "Table",
      icon = icon("table"),
      div(
        style = "margin-bottom: 10px; display: flex; gap: 10px; align-items: center;",
        downloadButton(
          "download_csv",
          "Download CSV",
          class = "btn-sm",
          style = paste0(
            "background:",
            NCDPI_NAVY,
            ";",
            "color: white;",
            "border: none;",
            "font-weight: 600;"
          )
        ),
        textOutput("table_count", inline = TRUE) |>
          tagAppendAttributes(style = "color: #666; font-size: 13px;")
      ),
      reactableOutput("bright_spots_table")
    )
  )
)
