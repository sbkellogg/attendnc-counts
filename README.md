# attendnc-counts

Public companion site for the AttendNC Bright Spots initiative — a landing
page, host for the interactive dashboard/map, and (eventually) a publishing
outlet for analyses from the private `chronic-absenteeism` repo. Deployed via
GitHub Pages from the `main` branch root.

## Site structure

Pages are organized into topical subfolders, mirroring the
`innovation-leadership-council` site convention:

```
index.html                                   landing / home page
explore/
  index.html                                  "Explore the Data" overview (map + embedded Shiny dashboard)
  about-analysis.html                         methodology write-up
research/
  index.html                                  attendance research summary
spotlights/
  <school-slug>/index.html                    one folder per school spotlight
reports/
  <report-slug>/index.html                    future: published chronic-absenteeism analyses (placeholder for now)
styles/
  shared.css                                  design tokens, reset, header/footer CSS (shared by every page)
  <page-id>.css                                page-specific CSS, one file per page
images/                                        shared image assets (logo, hero photos)
src/
  partials/header.html, footer.html            shared header/footer templates
  pages/<page-id>-body.html                    per-page body content (+ *-scripts.html if a page needs inline JS)
  build.R                                      assembles every root-level HTML file from the partials + page sources
```

`styles/` and `images/` stay flat at the repo root. Every deployed page —
regardless of how deep its folder is — references them with a relative
`../` prefix that `build.R` computes automatically from the page's output
path, so don't hand-write `../` paths inside `src/pages/*-body.html`; write
root-relative paths (`images/foo.jpg`, `styles/foo.css`) and let the build
script handle depth.

## Building

The deployed HTML files (`index.html`, `explore/index.html`, `explore/about-analysis.html`, `research/index.html`,
`spotlights/*/index.html`, ...) are **generated output** — don't hand-edit
them directly, edit the sources under `src/` and `styles/` and rebuild:

```r
Rscript src/build.R
# or, from an R session already in the repo root:
source("src/build.R")
```

This overwrites every page listed in the `pages` manifest in `src/build.R`.
Re-run it after editing any partial, page body, or page-specific CSS.

## Adding a school spotlight

1. Add the new spotlight's body HTML at `src/pages/<slug>-spotlight-body.html`
   (copy an existing spotlight body as a starting point).
2. Add page-specific CSS at `styles/<slug>-spotlight.css` (or reuse an
   existing one if the layout is a close match).
3. Add any spotlight-specific hero/photo assets to `images/` (or to a
   spotlight-local `spotlights/<slug>/` folder if you'd rather keep them
   colocated — just update the image path in the body HTML to match).
4. In `src/build.R`, add a new entry to the `pages` list:
   ```r
   list(
     id = "<slug>-spotlight",
     output = "spotlights/<slug>/index.html",
     title = "<School Name> | AttendNC Counts School Spotlight",
     description = "...",
     css = "styles/<slug>-spotlight.css",
     body = "src/pages/<slug>-spotlight-body.html",
     scripts = NULL,
     nav_current = "spotlights",
     is_home = FALSE
   )
   ```
5. If the new spotlight should be linked from the landing page's spotlights
   section, add that link in `src/pages/landing-body.html` — write it as a
   root-relative path (`spotlights/<slug>/index.html`); `build.R` will
   resolve it relative to whichever page references it.
6. Rebuild (`Rscript src/build.R`) and spot-check the new page locally
   (e.g. `python3 -m http.server` from the repo root) before committing.

## Adding a report

The `reports/` folder is currently a placeholder (an empty `.gitkeep`)
reserving the URL shape for future published analyses from the private
`chronic-absenteeism` repo. Publishing workflow is manual: render the
analysis there, then bring the finished HTML into this repo.

1. Decide on a slug and add the page at `reports/<slug>/index.html`. A
   report can be either:
   - a standalone HTML export copied in as-is (simplest, no shared
     header/footer/theming), or
   - integrated into the `build.R` pipeline like any other page — add a
     `src/pages/<slug>-report-body.html`, a `styles/<slug>-report.css` if
     needed, and a manifest entry with `output = "reports/<slug>/index.html"`.
2. If the report should appear in site navigation (e.g. a future "Reports"
   index page, or a link from the landing/footer), add that link using a
   root-relative path and let `build.R` resolve it per-page as described
   above.
3. Rebuild and spot-check locally before committing.

## Notes

- `bright-spots-app-v2/` (the Shiny dashboard's source + data) has been
  removed from this repo — it's deployed and maintained separately on
  shinyapps.io. `explore/index.html` (the "Explore the Data" page) embeds it
  via an iframe pointing at the shinyapps.io URL, in the `#dashboard`
  section.
- `.nojekyll` disables GitHub Pages' Jekyll processing so files/folders are
  served as-is.
