
#' get piped name
#'
#'this function captures the name of an object piped into a function, and returns as a string. Powers the automatic naming found in presenter.
#'
#'
#'
#'
#' @param object an object
#' @param default_name string Attempts to return this string if an error occurs.
#'
#' @return string
#' @export
#'
#' @examples
#'
#' #necessary to specify this option when using get_piped_name in knitr
#' options(rlang_trace_top_env = rlang::current_env())
#'
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
#'
get_piped_name <- function(object, default_name = "Table") {

  rlang::trace_back() -> f_trace

  f_trace$calls -> f_calls

  length(f_calls) -> f_len

## if the option isn't specified, this workaround can bypass all the calls from knitr
 if(f_len > 7){
   f_calls[(f_len-2):f_len] -> f_calls
 }

  f_calls %>%
    purrr::pluck(1) %>%
    as.character() %>%
    purrr::pluck(2) %>%
    stringr::str_split(pattern = stringr::boundary("word")) %>%
    purrr::pluck(1, 1) -> the_call

  if(is.null(the_call)){

    the_call <- default_name
  }

  the_call

}

## debug function in vignettes

#' new_fun <- function(x){
#'
#'   get_piped_name()
#' }
#'
#'
#' #' new fun pipe
#' #'
#' #' @param x
#' #'
#' #' @export
#' new_fun_pipe <- function(x){
#'
#'  new_pipe() -> cl
#'
#' cl
#'
#' }
#'
#' #' new pipe
#' #'
#' #' @param object
#' #'
#' #' @export
#' new_pipe <- function(object){
#'
#'
#'   rlang::trace_back() -> tr
#'   tr$calls
#' }
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
