# The General

The General is a Shiny application for exploring CSV datasets without writing code. Review suggested column roles, filter your data, create tables and plots, and pin useful results to a dashboard. Download commented R scripts to reproduce your analyses.

## Run from a checkout

Clone this repository, then open its root in Positron, RStudio or a terminal. The lockfile records R 4.5.2 and renv 1.1.4. Access to this GitHub repository is required while it is private.

In R, with the repository root as the working directory:

```r
renv::restore(prompt = FALSE)
dir.create("tmp", showWarnings = FALSE)
options(the_general.log_file = "tmp/app.log")
shiny::runApp(".", launch.browser = TRUE)
```

This checkout-based workflow includes the example datasets. The developer manual describes the current limitations of installed-package and Docker deployment.

## Use the app

1. Upload a CSV or choose an example dataset.
2. Open **Review columns**, review the suggested dates, numeric and categorical columns, move any misplaced columns, and confirm. Suggestions are editable and can be wrong.
3. Explore the analysis tabs and apply filters.
4. Pin results to **My Dashboard**, then preview or download them and their R scripts.
5. Open **Documentation** on Home for the user, statistics and developer guides.

The guides also work offline from [inst/app/www/docs/index.html](inst/app/www/docs/index.html).

## Develop and test

- `R/` contains the application, calculation helpers and `mod_*.R` Shiny modules.
- `inst/app/www/` contains CSS, JavaScript and documentation.
- `tests/testthat/` contains the regression tests and synthetic fixtures.
- `renv.lock` records the dependency versions.

Run the same test entry point as CI:

```r
dir.create("tmp", showWarnings = FALSE)
options(the_general.log_file = "tmp/test.log")
testthat::test_local(stop_on_failure = TRUE)
```

Create feature branches from `main` and open pull requests back to `main`. Use descriptive commits, for example `fix: preserve column choices after reopening`. R CI runs on pull requests, pushes to `main`, and manual workflow dispatches. A local commit alone does not start CI.

This is an independent repository: [tomh1988-8/the-general](https://github.com/tomh1988-8/the-general). It has no mirroring workflow. Keep credentials, deployment metadata and temporary reports out of Git. Use `tmp/` for local diagnostics.

For support or questions, open an issue in this repository.
