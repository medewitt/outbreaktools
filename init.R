# Start Packages
usethis::use_description()
usethis::use_build_ignore("init.R")
usethis::use_build_ignore(".travis.yml")
usethis::use_build_ignore("Read-and-delete-me")
usethis::use_namespace()
usethis::use_package_doc()
usethis::use_readme_rmd()

# Packages
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("padr")
usethis::use_package("lubridate")
usethis::use_package("tidyr")
usethis::use_package("EnvStats")
usethis::use_package("cowplot")
usethis::use_package("ggpubr")
usethis::use_package("matrixStats")
usethis::use_package("ggpubr")
usethis::use_pipe()
# Add Vignettes
usethis::use_vignette("under-reporting", title = "Example Under Reporting Estimates")
usethis::use_vignette("short-term-predictions", title = "Short Term, Local Case and Reproductive Rates")

usethis::use_vignette("ifr-meta", title = "Meta-Analysis of Infection Fatality Ratio")


# Add PAckagedown
usethis::use_pkgdown()
pkgdown::build_site()
# Add CI
usethis::use_github_action("check-standard")
usethis::use_github_action("pkgdown")
usethis::use_github_action("test-coverage")
usethis::use_coverage()

# Raw Data
usethis::use_data_raw(name = "covid19_serial_interval")

rstantools::use_rstan(pkgdir = here::here())

roxygen2::roxygenise(load_code = "source")
