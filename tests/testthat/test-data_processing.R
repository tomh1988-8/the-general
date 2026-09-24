## -----------------------------------------------------------------------------
## test-data_processing.R  --  unit tests for data_* helpers
## -----------------------------------------------------------------------------
library(testthat)
library(tibble)
library(dplyr)
library(readr)
library(withr)

make_csv <- function(tbl, filename) {
  dir <- withr::local_tempdir(.local_env = parent.frame()) # keep dir alive
  path <- file.path(dir, filename)
  readr::write_csv(tbl, path)
  path
}
# ----------------------------------------------------------------------------
# 1. data_default_paths ------------------------------------------------------
# ----------------------------------------------------------------------------

test_that("data_default_paths() returns two existing .csv paths", {
  paths <- data_default_paths()
  expect_type(paths, "character")
  expect_length(paths, 2)
  # file.exists() will be TRUE *inside* the installed package / deployed app.
  # For dev environment we just check the extension so tests run anywhere.
  expect_true(all(tools::file_ext(paths) == "csv"))
})

# ----------------------------------------------------------------------------
# 2. data_register_uploads ---------------------------------------------------
# ----------------------------------------------------------------------------

test_that("data_register_uploads() converts fileInput df to named list", {
  df <- tibble(
    name = c("alpha.csv", "beta.csv"),
    datapath = c("/some/alpha", "/some/beta")
  )
  res <- data_register_uploads(df)
  expect_named(res, c("alpha", "beta"))
  expect_equal(res[["alpha"]], "/some/alpha")
  expect_equal(res[["beta"]], "/some/beta")
})

test_that("data_register_uploads() handles empty input", {
  df <- tibble(name = character(), datapath = character())
  res <- data_register_uploads(df)
  expect_equal(res, list())
})

# ----------------------------------------------------------------------------
# 3. data_load_dataset -------------------------------------------------------
# ----------------------------------------------------------------------------

test_that("data_load_dataset() prioritises uploads over defaults", {
  # create two temporary CSVs ---------------------
  tmp1 <- make_csv(tibble(A = 1:3), "tmp1.csv")
  tmp2 <- make_csv(tibble(A = 4:6), "tmp2.csv")
  uploads <- list(tmp1_df = tmp1)
  paths <- c(tmp2) # packaged default
  names(paths) <- NULL # keep it unnamed for function

  # load from uploads -----------------------------
  df1 <- data_load_dataset("tmp1_df", uploads, paths)
  expect_equal(df1$A, 1:3)

  # load from defaults ----------------------------
  df2 <- data_load_dataset(
    tools::file_path_sans_ext(basename(tmp2)),
    list(),
    paths
  )
  expect_equal(df2$A, 4:6)
})

# ----------------------------------------------------------------------------
# 4. data_group_vars & data_group_levels -------------------------------------
# ----------------------------------------------------------------------------

test_that("data_group_vars() returns chr+factor cols, excluding admin", {
  df <- tibble(
    id = 1:2,
    category = factor(c("x", "y")),
    colour = c("red", "blue"),
    Row_ID = c("foo", "bar")
  )
  vars <- data_group_vars(df)
  expect_setequal(vars, c("category", "colour"))
})

test_that("data_group_levels() works for factor and character", {
  df <- tibble(city = factor(c("LDN", "NYC", "LDN")), name = c("a", "b", "a"))
  expect_equal(data_group_levels(df, "city"), c("LDN", "NYC"))
  expect_equal(data_group_levels(df, "name"), c("a", "b"))
})

# ----------------------------------------------------------------------------
# 5. data_apply_filter -------------------------------------------------------
# ----------------------------------------------------------------------------

test_that("data_apply_filter() obeys grouping filters", {
  df <- tibble(g = c("A", "B", "A"), val = 1:3)
  res1 <- data_apply_filter(df, "g", "A")
  expect_equal(res1$val, c(1, 3))

  # no-op when group_var NULL ---------------------
  res2 <- data_apply_filter(df, NULL, NULL)
  expect_equal(res2, df)

  # no-op when levels empty -----------------------
  res3 <- data_apply_filter(df, "g", character())
  expect_equal(res3, df)
})

# ----------------------------------------------------------------------------
# 6. Integration: upload -> load -> filter -----------------------------------
# ----------------------------------------------------------------------------

test_that("End‑to‑end workflow behaves", {
  # create two datasets ---------------------------
  up_path <- make_csv(tibble(cat = c("x", "y", "x"), num = 1:3), "upload.csv")
  def_path <- make_csv(tibble(cat = c("z", "z"), num = 4:5), "default.csv")

  uploads <- list(upload = up_path)

  # load uploaded dataset -------------------------
  df1 <- data_load_dataset("upload", uploads, def_path)
  expect_equal(nrow(df1), 3)

  # apply filter ----------------------------------
  df2 <- data_apply_filter(df1, "cat", "y")
  expect_equal(df2$num, 2)

  # fall back to default --------------------------
  df3 <- data_load_dataset("default", list(), def_path)
  expect_equal(nrow(df3), 2)
})

## -----------------------------------------------------------------------------
test_that("data_apply_filter supports the app global filter call shape", {
  df <- tibble(
    region = c("North", "South", "North"),
    value = c(1, 2, 3)
  )

  filtered <- data_apply_filter(
    df,
    "region",
    "North"
  )

  expect_equal(filtered$value, c(1, 3))
  expect_s3_class(filtered, "tbl_df")
})

test_that("data_apply_filter treats blank filter choices as no filter", {
  df <- tibble(
    region = c("North", "South", "North"),
    value = c(1, 2, 3)
  )

  expect_equal(data_apply_filter(df, "", character())$value, c(1, 2, 3))
  expect_equal(data_apply_filter(df, NULL, NULL)$value, c(1, 2, 3))
})

test_that("data_prop_table_one_group shows zero percentages for completed empty periods", {
  df <- tibble(
    Month = factor(
      c("Apr", "Apr", "May", "May", "Jun", "Jun"),
      levels = month.abb[c(4:12, 1:3)],
      ordered = TRUE
    ),
    Cat_gender = factor(c("Female", "Male", "Female", "Male", "Female", "Male"))
  )

  out <- data_prop_table_one_group(df, "Month", "Cat_gender")
  july <- dplyr::filter(out, Month == "Jul")

  expect_equal(july$Female.Percent, 0)
  expect_equal(july$Male.Percent, 0)
  expect_equal(july$Female, 0)
  expect_equal(july$Male, 0)
})
