

#' get lhs
#'
#' @keywords internal
#'
get_lhs <- function(){
  calls <- sys.calls()

  call_firsts <- lapply(calls,`[[`,1)
  pipe_calls <- vapply(call_firsts,identical,logical(1),quote(`%>%`))
  if(all(!pipe_calls)){
    NULL
  } else {
    pipe_calls <- which(pipe_calls)
    pipe_calls <- pipe_calls[length(pipe_calls)]
    calls[[c(pipe_calls,2)]]
  }
}

#' get piped name
#'
#'this function captures the name of an object piped into a function, and returns as a string. Powers the automatic naming found in presenter.
#'
#'citation: \url{https://michaelbarrowman.co.uk/post/getting-a-variable-name-in-a-pipeline/}
#'
#'
#' @param objName an object
#' @param default_name default name if object name is not able to be accessed in case of a long pipe
#'
#' @return string
#' @export
#'
#' @examples
#'
#' ## works if the object is piped or given as an argument
#' iris %>%
#' get_piped_name
#'
#' get_piped_name(iris)
#'
#' ## does not work for multistep pipes. instead the 'default_name' is printed
#' iris %>%
#' dplyr::select(1:3) %>%
#' get_piped_name
get_piped_name <- function(objName, default_name = "Title") {
  z <- get_lhs()

  if (is.null(z)) {
    z <- rlang::enexpr(objName)
  }

  if (rlang::is_symbol(z)) {
    z <- rlang::as_name(z)
  } else{
    z <- default_name
  }
  z
}
