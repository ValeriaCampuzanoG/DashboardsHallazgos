source("renv/activate.R")


options(repos = c(
  RSPM = "https://packagemanager.rstudio.com/cran/2024-01-15",
  CRAN = "https://cran.r-project.org/"
))

if (!interactive()) {
  options(pkgType = "binary")
  Sys.setenv(R_COMPILE_AND_INSTALL_PACKAGES = "never")
}
