# Install & load usethis if needed
if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}
library(usethis)

# Scaffold the package in the current directory
usethis::create_package(
  path = ".",
  fields = list(
    Title = "The General",
    Description = "A shiny app where you can drop in your own data and interrogate through a flexible research tool.",
    `Authors@R` = 'person("Tom", "Hunter", email = "188674674+tomh1988-8@users.noreply.github.com", role = c("aut", "cre"))'
  ),
  open = FALSE
)
