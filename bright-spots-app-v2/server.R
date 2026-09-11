# =============================================================================
# server.R — 2025 Attendance Bright Spots Shiny App (v2)
#
# Key reactive flow:
#   filtered_data()  ← all sidebar inputs
#       │
#       ├─► leafletProxy  (updates markers without full map redraw)
#       ├─► renderReactable (filtered table)
#       ├─► filter_summary badge
#       └─► table_count label / download handler
#
# Filter logic (all conditions are AND):
#   1. pd_group filter — if show_all_schools is FALSE, restrict to selected
#      bright spot categories; schools with NA pd_group are included only
#      when show_all_schools is TRUE.
#   2. Decrease threshold — must match at least one selected bucket.
#   3. County, grade band, poverty, enrollment — standard sidebar filters.
# =============================================================================

function(input, output, session) {
  # ── Reset button ───────────────────────────────────────────────────────────
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "pd_group", selected = PD_GROUP_LEVELS)
    updateCheckboxInput(session, "show_all_schools", value = FALSE)
    updateCheckboxGroupInput(session, "threshold", selected = THRESHOLD_LABELS)
    updateSelectInput(session, "county", selected = "")
    updateCheckboxGroupInput(
      session,
      "grade_band",
      selected = grade_band_choices
    )
    updateCheckboxGroupInput(session, "poverty", selected = poverty_choices)
    updateSliderInput(session, "enrollment", value = enroll_range)
    updateSliderInput(
      session,
      "pct_eds",
      value = c(floor(eds_range[1] * 100), ceiling(eds_range[2] * 100))
    )
  })

  # ── Filtered dataset ───────────────────────────────────────────────────────
  filtered_data <- reactive({
    df <- app_data

    # --- Bright Spot Category (pd_group) ------------------------------------
    # If show_all_schools is FALSE, restrict to selected pd_group values.
    # Schools without a pd_group (NA) are excluded unless show_all_schools is TRUE.
    if (!isTRUE(input$show_all_schools)) {
      selected_groups <- input$pd_group
      if (length(selected_groups) == 0) {
        # No categories selected and not showing all → return empty
        return(df[0, ])
      }
      df <- df |> filter(pd_group %in% selected_groups)
    }

    # --- Decrease threshold -------------------------------------------------
    # Must satisfy at least one selected threshold bucket (OR within buckets,
    # AND with all other filters).
    if (length(input$threshold) > 0) {
      selected_thresholds <- DECREASE_THRESHOLDS[
        THRESHOLD_LABELS %in% input$threshold
      ]
      df <- df |>
        filter(purrr::reduce(
          purrr::map(selected_thresholds, function(t) {
            pct_decrease >= t$min & pct_decrease < t$max
          }),
          `|`
        ))
    }

    # --- County (single select; "" means "All") -----------------------------
    if (!is.null(input$county) && input$county != "") {
      df <- df |> filter(county == input$county)
    }

    # --- Grade band ---------------------------------------------------------
    if (length(input$grade_band) > 0) {
      df <- df |> filter(grade_band %in% input$grade_band)
    }

    # --- Poverty designation ------------------------------------------------
    if (length(input$poverty) > 0) {
      df <- df |> filter(poverty %in% input$poverty)
    }

    # --- Enrollment range ---------------------------------------------------
    df <- df |>
      filter(den >= input$enrollment[1], den <= input$enrollment[2])

    # --- % Economically Disadvantaged ---------------------------------------
    df <- df |>
      filter(
        pct_eds * 100 >= input$pct_eds[1],
        pct_eds * 100 <= input$pct_eds[2]
      )

    df
  })

  # ── Filter summary badge ───────────────────────────────────────────────────
  output$filter_summary <- renderUI({
    n <- nrow(filtered_data())
    n_bs <- sum(!is.na(filtered_data()$pd_group))
    n_other <- n - n_bs

    div(
      style = paste0(
        "background:",
        NCDPI_NAVY,
        ";",
        "color: white; border-radius: 6px;",
        "padding: 8px 12px; text-align: center; font-size: 13px;"
      ),
      strong(n),
      " schools",
      if (n_bs > 0 && n_other > 0) {
        tags$span(
          style = "display: block; font-size: 11px; opacity: 0.85; margin-top: 2px;",
          paste0(n_bs, " bright spot · ", n_other, " other")
        )
      }
    )
  })

  # ── Table row count label ──────────────────────────────────────────────────
  output$table_count <- renderText({
    paste0(nrow(filtered_data()), " schools")
  })

  # ── Initial map render ─────────────────────────────────────────────────────
  output$bright_spots_map <- renderLeaflet({
    leaflet() |>
      # CartoDB.Positron basemap, restored now that a CARTO API key is
      # available. addProviderTiles(providers$CartoDB.Positron, options =
      # providerTileOptions(key = ...)) does NOT work: leaflet-providers'
      # bundled CartoDB URL template has no {key} placeholder, so the key
      # is silently dropped and the "API KEY REQUIRED" watermark still
      # shows. Appending the key directly to a manual addTiles() URL is
      # the only way to actually get it into the tile request. Requires
      # CARTO_API_KEY to be set (see .Renviron in this app's directory).
      addTiles(
        urlTemplate = paste0(
          "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png?key=",
          Sys.getenv("CARTO_API_KEY")
        ),
        attribution = paste0(
          "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
          " contributors &copy; <a href='https://carto.com/attributions'>CARTO</a>"
        ),
        options = tileOptions(
          subdomains = "abcd",
          maxZoom = 20,
          detectRetina = TRUE
        )
      ) |>
      addPolygons(
        data = nc_county_boundaries,
        fillColor = "transparent",
        color = "#989a9cff",
        weight = 0.5,
        opacity = 0.5
      ) |>
      addPolygons(
        data = nc_state_boundary,
        color = "#989a9c72",
        weight = 0.5,
        opacity = 0.9
      ) |>
      addLegend(
        position = "bottomright",
        pal = grade_band_palette,
        values = factor(app_data$grade_band, levels = GRADE_BAND_LEVELS),
        title = "Grade Band",
        opacity = 0.8
      ) |>
      setView(lng = -79.5, lat = 35.5, zoom = 7)
  })

  # ── Update map markers when filters change ─────────────────────────────────
  observeEvent(filtered_data(), {
    df <- filtered_data()
    proxy <- leafletProxy("bright_spots_map") |> clearGroup("schools")

    if (nrow(df) == 0) {
      return(proxy)
    }

    # Pre-compute marker styling to avoid tilde/if_else on shinyapps.io
    df <- df |>
      mutate(
        marker_fill_opacity = if_else(!is.na(pd_group), 0.85, 0),
        marker_weight = if_else(!is.na(pd_group), 1, 2),
        marker_opacity = if_else(!is.na(pd_group), 0.9, 0.7)
      )

    proxy |>
      addCircleMarkers(
        data = df,
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        fillColor = ~ grade_band_palette(grade_band),
        fillOpacity = ~marker_fill_opacity,
        color = ~ grade_band_palette(grade_band),
        stroke = TRUE,
        weight = ~marker_weight,
        opacity = ~marker_opacity,
        popup = make_school_popups(df),
        label = ~name,
        group = "schools"
      )
  })

  # ── Reactable table ────────────────────────────────────────────────────────
  output$bright_spots_table <- renderReactable({
    filtered_data() |>
      select(
        name,
        county,
        grade_band,
        poverty,
        pct_eds,
        pd_group,
        den,
        p_peak,
        p_current,
        improvement_pp,
        pct_decrease,
        expected_rate,
        residual_pp
      ) |>
      arrange(desc(improvement_pp)) |>
      reactable(
        searchable = TRUE,
        filterable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        pagination = TRUE,
        defaultPageSize = 25,
        defaultSorted = list(improvement_pp = "desc"),
        style = list(
          fontFamily = "Source Sans Pro, Arial, sans-serif",
          fontSize = "14px"
        ),
        theme = ncdpi_table_theme,
        columns = list(
          name = colDef(
            name = "School",
            minWidth = 200,
            sticky = "left",
            filterMethod = JS(
              "function(rows, columnId, filterValue) {
                return rows.filter(row =>
                  row.values[columnId].toLowerCase()
                    .includes(filterValue.toLowerCase())
                );
              }"
            )
          ),
          county = colDef(
            name = "County",
            minWidth = 120,
            filterMethod = JS(
              "function(rows, columnId, filterValue) {
                return rows.filter(row =>
                  row.values[columnId].toLowerCase()
                    .includes(filterValue.toLowerCase())
                );
              }"
            )
          ),
          grade_band = colDef(
            name = "Grade Band",
            minWidth = 140,
            filterInput = JS(
              "function(column, state) {
                const options = ['', ...new Set(
                  state.data.map(row => row[column.id])
                )].sort();
                return React.createElement('select', {
                  onChange: e => column.setFilter(e.target.value || undefined),
                  style: { width: '100%', fontSize: '13px' }
                },
                options.map(opt =>
                  React.createElement('option', { value: opt, key: opt },
                    opt || 'All')
                ));
              }"
            )
          ),
          poverty = colDef(
            name = "Poverty",
            minWidth = 100,
            filterInput = JS(
              "function(column, state) {
                const options = ['', ...new Set(
                  state.data.map(row => row[column.id])
                )].sort();
                return React.createElement('select', {
                  onChange: e => column.setFilter(e.target.value || undefined),
                  style: { width: '100%', fontSize: '13px' }
                },
                options.map(opt =>
                  React.createElement('option', { value: opt, key: opt },
                    opt || 'All')
                ));
              }"
            )
          ),
          pct_eds = colDef(
            name = "% Econ. Disadvantaged",
            minWidth = 160,
            filterable = FALSE,
            format = colFormat(percent = TRUE, digits = 1),
            style = list(color = "#555")
          ),
          pd_group = colDef(
            name = "Bright Spot Category",
            minWidth = 180,
            na = "—",
            cell = function(value) {
              if (is.na(value)) {
                return("—")
              }
              badge_color <- PD_GROUP_COLORS[value]
              if (is.na(badge_color)) {
                badge_color <- "#888888"
              }
              tags$span(
                style = paste0(
                  "background:",
                  badge_color,
                  "; color: white;",
                  " border-radius: 4px; padding: 2px 7px;",
                  " font-size: 12px; font-weight: 700;"
                ),
                value
              )
            },
            filterInput = JS(
              "function(column, state) {
                const options = ['', ...new Set(
                  state.data.map(row => row[column.id])
                )].sort();
                return React.createElement('select', {
                  onChange: e => column.setFilter(e.target.value || undefined),
                  style: { width: '100%', fontSize: '13px' }
                },
                options.map(opt =>
                  React.createElement('option', { value: opt, key: opt },
                    opt || 'All')
                ));
              }"
            )
          ),
          den = colDef(
            name = "Enrollment",
            format = colFormat(separators = TRUE),
            minWidth = 110,
            filterable = FALSE
          ),
          p_peak = colDef(
            name = "Peak Rate (2022)",
            format = colFormat(percent = TRUE, digits = 1),
            minWidth = 140,
            filterable = FALSE,
            style = list(color = "#e74c3c", fontWeight = "600")
          ),
          p_current = colDef(
            name = "Current Rate (2025)",
            format = colFormat(percent = TRUE, digits = 1),
            minWidth = 150,
            filterable = FALSE,
            style = list(color = "#27ae60", fontWeight = "600")
          ),
          improvement_pp = colDef(
            name = "Improvement (pp)",
            minWidth = 150,
            filterable = FALSE,
            cell = function(value) paste0(round(value, 1), " pp")
          ),
          pct_decrease = colDef(
            name = "% Decrease",
            minWidth = 130,
            filterable = FALSE,
            cell = function(value) {
              bar_width <- paste0(round(value * 100), "%")
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                div(
                  style = paste0(
                    "background:",
                    NCDPI_NAVY,
                    ";",
                    "width:",
                    bar_width,
                    ";",
                    "height: 14px; border-radius: 2px; min-width: 2px;"
                  )
                ),
                span(
                  style = "font-weight: 600;",
                  paste0(round(value * 100, 1), "%")
                )
              )
            }
          ),
          expected_rate = colDef(
            name = "Expected Rate",
            minWidth = 140,
            filterable = FALSE,
            na = "N/A",
            format = colFormat(percent = TRUE, digits = 1),
            style = list(color = "#555")
          ),
          residual_pp = colDef(
            name = "vs. Expected",
            minWidth = 130,
            filterable = FALSE,
            na = "N/A",
            cell = function(value) {
              if (is.na(value)) {
                return("N/A")
              }
              color <- if (value < 0) "#27ae60" else "#e74c3c"
              sign_str <- if (value < 0) "\u2212" else "+"
              tags$span(
                style = paste0("color:", color, "; font-weight: 600;"),
                paste0(sign_str, abs(round(value, 1)), " pp")
              )
            }
          )
        )
      )
  })

  # ── CSV download ───────────────────────────────────────────────────────────
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("bright_spots_2025_v2_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      filtered_data() |>
        select(
          School = name,
          County = county,
          `Grade Band` = grade_band,
          Poverty = poverty,
          `% Econ. Disadvantaged` = pct_eds,
          `Bright Spot Category` = pd_group,
          Enrollment = den,
          `Peak Rate 2022 (%)` = p_peak,
          `Current Rate 2025 (%)` = p_current,
          `Improvement (pp)` = improvement_pp,
          `Pct Decrease (%)` = pct_decrease,
          `Expected Rate (%)` = expected_rate,
          `vs. Expected (pp)` = residual_pp
        ) |>
        mutate(
          `% Econ. Disadvantaged` = round(`% Econ. Disadvantaged` * 100, 1),
          `Peak Rate 2022 (%)` = round(`Peak Rate 2022 (%)` * 100, 1),
          `Current Rate 2025 (%)` = round(`Current Rate 2025 (%)` * 100, 1),
          `Expected Rate (%)` = round(`Expected Rate (%)` * 100, 1),
          `Improvement (pp)` = round(`Improvement (pp)`, 1),
          `Pct Decrease (%)` = round(`Pct Decrease (%)` * 100, 1),
          `vs. Expected (pp)` = round(`vs. Expected (pp)`, 1)
        ) |>
        write_csv(file)
    }
  )
}
