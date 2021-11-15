

#' get lhs
#' @keywords internal
#' @export
get_lhs <- function(){
  calls <- sys.calls()

  lapply(calls, rlang::call_standardise) -> l1



  rlang::call_args(l1[[1]])  -> arg

  if(any((names(arg) == "objName"))){

    return(rlang::as_name(arg$objName))}

  arg$lhs -> c1

  while(rlang::is_call(c1)){
    c1 %>%
      rlang::call_args() %>%
      purrr::pluck(1) -> c1

  }

  rlang::as_name(c1)

}



#' get piped name
#'
#'this function captures the name of an object piped into a function, and returns as a string. Powers the automatic naming found in presenter.
#'
#'Inspiration for function: \url{https://michaelbarrowman.co.uk/post/getting-a-variable-name-in-a-pipeline/}
#'The function body has been rewritten to leverage \code{rlang}.
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
#' get_piped_name()
#'
#' get_piped_name(iris)
#'
#' ## can even extract name from multistep pipes
#' iris %>%
#' dplyr::select(1:3) %>%
#' get_piped_name()
#'
#' ## can be placed inside other functions to capture the name and save it
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
get_piped_name <- function(objName, default_name = "Title") {
  z <- get_lhs()



  if(is.character(z)){
    z <- z
  } else if (is.null(z)) {
    z <- rlang::enexpr(objName)
  } else if  (rlang::is_symbol(z)) {
    z <- rlang::as_name(z)
  } else{
    z <- default_name
  }

  z
}



