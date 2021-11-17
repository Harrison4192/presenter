
#' get piped name
#'
#'this function captures the name of an object piped into a function, and returns as a string. Powers the automatic naming found in presenter.
#'
#'The behavior doesn't work as expected when examples or vignettes are being knitted, so instead
#'you can specify a default output.
#'
#'
#' @param object an object
#' @param default_name string
#'
#' @return string
#' @export
#'
#' @examples
#'
#' ### on local machine the output will be "iris" for each example
#'
#' ### works if the object is piped or given as an argument
#' iris %>%
#' get_piped_name()
#'
#' get_piped_name(iris)
#'
#' ### can even extract name from multistep pipes
#' iris %>%
#' dplyr::select(1:3) %>%
#' get_piped_name()
#'
#' ### can be placed inside other functions to capture the name and save it
#'
#' find_name <- function(x){
#'  get_piped_name() -> new_name
#'
#'  new_name
#' }
#'
#' iris %>%
#' dplyr:select(1:3) %>%
#' find_name()
get_piped_name <- function(object, default_name = "Table") {

  calls <- sys.calls() %>%
    as.list()

  if(length(calls) > 22){
    start <- 23
  } else{
    start <- 1
  }


  calls %>%
    purrr::pluck(start) %>%
    as.character() %>%
    purrr::pluck(2) %>%
    stringr::str_split(pattern = stringr::boundary("word")) %>%
    purrr::pluck(1, 1) -> the_call

  if(the_call == "withCallingHandlers" | is.null(the_call)){

    the_call <- default_name
  }

  the_call

}

## debug function in vignettes

# new_fun <- function(x){
#
#   get_piped_name()
# }
#
# new_fun_pipe <- function(x){
#
#  new_pipe() -> cl
#   x %>%
#     dplyr::select(1:3) -> x
#
#   x %>%
#     new_pipe() -> stuff
#
#   stuff
#
#   1+1 -> h1
#
#   cl
# }
#
# new_pipe <- function(object){
#
#   calls <- sys.calls() %>%
#     as.list()
#
#   calls
# }
#
# ---
#   title: "pipedname"
# output: rmarkdown::html_vignette
# vignette: >
#   %\VignetteIndexEntry{pipedname}
# %\VignetteEngine{knitr::rmarkdown}
# %\VignetteEncoding{UTF-8}
# ---
#
#   ```{r, include = FALSE}
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>"
# )
# ```
#
# ```{r setup}
# library(presenter)
# library(dplyr)
# library(purrr)
# ```
#
# ```{r}
# iris %>%
#   mutate(Species = Species) %>%
#   new_fun_pipe() %>%
#   pluck(1)
# ```
#
# ```{r}
# iris %>%
#   mutate(Species = Species) %>%
#   get_piped_name()
# ```
#
# ```{r}
# iris %>%
#   mutate(Species = Species) %>%
#   new_fun() %>%
#   new_fun()
# ```
#
# ```{r}
# iris %>%
#   mutate(Species = Species) %>%
#   new_fun_pipe()
# ```
