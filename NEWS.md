# easyanalysis 0.2.0

- `summary_table()`'s p-value test for continuous variables (`p = TRUE`) is
  now chosen automatically based on normality, instead of always using
  `gtsummary`'s default test. A continuous variable is treated as normal
  only if it passes a Shapiro-Wilk test (p > 0.05) in *every* level of `x`;
  normal variables use `t.test`/`aov` (two vs. three-or-more groups),
  non-normal variables use `wilcox.test`/`kruskal.test`.
- New `parametric` argument on `summary_table()`: a character vector of
  variable names (from `y`) that should always use the parametric test,
  skipping the normality check — useful when Shapiro-Wilk is overly
  sensitive (e.g. large samples).
- Internal refactor: the Shapiro-Wilk logic in `check_normality()` is now
  shared with the new automatic test selection via two internal helpers
  (`shapiro_p_one_group()`, `all_groups_normal()`); `check_normality()`'s
  own behavior and output are unchanged.

# easyanalysis 0.1.0 (renovation)

The repository originally held four standalone, copy-pasted scripts instead
of a package, each a fuller iteration of the same function:

- `summary table.R` (2022-07-25) — first version of `tbl_s()`: build a
  `gtsummary`/`flextable` table, optional p-value column.
- `summary table_1.R` (2022-07-26) — added variable labelling (`ylab`).
- `summary table_1.01.R` (2022-08-03) — made the grouping variable optional,
  added `caption`, and added a `s.t()` Shapiro-Wilk normality helper.
- `summary table_1.02.R` (2022-10-11) — added spanning headers, a
  mean ± SD display option, and a toggle to return a raw `gtsummary` object
  instead of a `flextable`; this was the most feature-complete version, but
  dropped `s.t()` along the way.

This release consolidates that history into a single installable package:

- The four root scripts were removed; `tbl_s()` (from `summary table_1.02.R`)
  is now `summary_table()`, and `s.t()` (recovered from `summary table_1.01.R`)
  is now `check_normality()`, with its previous `print()`-only behavior
  changed to `return()` so it composes like `summary_table()` does.
- All dependencies (`dplyr`, `tidyr`, `broom`, `formattable`, `gtsummary`,
  `flextable`, `Hmisc`) are declared in `DESCRIPTION` and called via `::`
  instead of being loaded with `require()` inside the functions, so a
  missing dependency now fails loudly at load time instead of silently at
  call time.
- Added `README.md`, this `NEWS.md`, `ROADMAP.md`, a `.gitignore`, and a
  `testthat` smoke-test suite.
