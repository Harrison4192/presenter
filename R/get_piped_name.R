

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
