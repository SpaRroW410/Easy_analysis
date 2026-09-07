# Roadmap

`easyanalysis` started as a set of R functions (2022) for building
"Table 1"-style research-reporting tables. The functions phase is now
consolidated into a package (see [NEWS.md](NEWS.md)). Everything below is
new work — a web app to turn analysis output into downloadable documents
was an early idea but was never built.

## Phase 0 — Package renovation (done)

- Consolidated four duplicate, filename-versioned scripts into a single
  package: `R/summary_table.R`, `R/check_normality.R`.
- Declared all dependencies explicitly (`DESCRIPTION`), removed `require()`
  calls, added `README.md`, `NEWS.md`, `.gitignore`, and a `testthat` smoke
  test suite.

## Phase 1 — Package hardening

- Install R tooling and actually run `devtools::check()` and
  `devtools::test()` — the tests added in Phase 0 have only been reviewed
  by hand, not executed, since no R environment was available during the
  renovation.
- Run `roxygen2::document()` to regenerate `NAMESPACE` and `man/` pages from
  the roxygen comments already in `R/*.R` (currently hand-maintained).
- Expand `testthat` coverage: missing/NA data, factor vs. numeric `y`,
  single- vs multi-variable headers, empty `ylab`.
- Add a GitHub Actions workflow (`R-CMD-check`) running install + check +
  tests on every push/PR.
- Add `lintr`/`styler` for consistent style, ideally as a pre-commit hook.

## Phase 2 — Shiny web app

Build the app the user originally envisioned, as a new `app/` directory
that depends on this package rather than duplicating its logic:

- **UI:** upload a dataset (CSV/Excel), pick a grouping variable and one or
  more outcome variables, toggle the same options `summary_table()` exposes
  (p-value, mean vs. median, labels, caption, spanning header).
- **Server:** call `summary_table()` on the uploaded data, render a live
  preview, and offer a download button that exports the result as a Word
  document (via `officer`/`flextable`), with PDF/HTML as stretch goals.
- **Deployment:** package the app in a Docker image; host on shinyapps.io
  or a self-hosted Shiny Server.

## Phase 3 — Feature expansion

- Additional table/report types beyond Table 1, e.g. regression result
  tables via `gtsummary::tbl_regression()`.
- Multi-table report documents using Quarto or R Markdown templates.
- Optional plot exports alongside tables.

## Phase 4 — Sharing & auth

Only if the app grows beyond personal/single-user use:

- User accounts and session handling.
- Saved report history.
- Upload size/rate limiting.
