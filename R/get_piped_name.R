

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
