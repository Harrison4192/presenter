

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
#'this function captures the name of a data frame piped into a function as a string. Powers the automatic naming found in presenter.
#'
#'citation: <https://michaelbarrowman.co.uk/post/getting-a-variable-name-in-a-pipeline/>
#'
#'
#' @param objName an object
#' @param default_name default name if object name is not able to be accessed in case of a long pipe
#'
#' @return
#' @export
#'
#' @examples
get_piped_name <- function(objName, default_name = "Title") {
  lhs <- get_lhs()
  if (is.null(lhs)) {
    lhs <- rlang::ensym(objName)
  } else if (rlang::is_symbol(lhs)) {
    z <- rlang::as_name(lhs)
  } else{
    z <- default_name
  }
  z
}
