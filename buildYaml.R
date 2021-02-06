library(pacman)
p_load(devtools, usethis, magrittr, ymlthis, purrr)

usethis::use_pkgdown()


usethis::use_package("flextable")
usethis::use_package("randomcoloR")
usethis::use_package("openxlsx")
usethis::use_package("lubridate")
usethis::use_package("dplyr")
usethis::use_package("rlang")
usethis::use_package("stringr")
usethis::use_package("purrr")
usethis::use_package("officer")
usethis::use_pipe()
usethis::use_mit_license()


use_build_ignore("buildYaml.R")

load_all()


ymlthis::pkgdown_template() %>%
  yml_author("Harrison Tietze") %>%
  yml_date(lubridate::today()) -> yml1

usethis::use_description()
reference_list <- list(
  pkgdown_ref(
    title = "Flextable functions",
    desc = "Function to create the flextable",
    contents = list("matches('flex')")
  ) ,
  pkgdown_ref(
    title = "Excel functions",
    desc = "Functions to create excel workbooks",
    contents = list("matches('excel')")
  )
)

yml1$reference <- reference_list


yml1 %>%
  listviewer::jsonedit()

yml1 %>%
  use_pkgdown_yml()


check()
document()
build_site()

use_github()
