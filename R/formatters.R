#' format_red
#'
#' @param flextbl flextable
#' @param col string. col name
#' @param clr color
#' @keywords internal
#'
#' @return flextable
format_red <- function(flextbl, col, clr = "firebrick3"){

  rlang::new_formula(lhs = NULL, rhs = rlang::expr(!!rlang::expr(!!col) < 0)) -> form1
  rlang::new_formula(lhs = NULL, rhs = rlang::expr(!!col)) -> form2


  flextbl %>%
    flextable::color(form1, form2, color = clr, part = "body")

}
#' format percentage
#'
#' formats a number as a percentage. default selection uses a helper to detect numeric columns
#' in between -1 and 1.
#'
#' @param tbl dataframe
#' @param ... tidyselect
#' @param digits integer. trailing digits
#'
#' @return dataframe
#' @export
format_percent <- function(tbl, ..., digits = 0){

  tbl %>% select_otherwise(..., otherwise = where(is_percentage)) -> col_indx

  tbl %>% dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx), ~ifelse(. == 0, . + .0000000000001, .))) -> tbl


  tbl %>% dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx), ~formattable::percent(., digits = digits)))
}

#' format number
#'
#' formats a number accounting style by inserting commas
#'
#' @param tbl dataframe
#' @param ... tidyselect. default selection is integer.
#' @param digits integer. trailing digits
#'
#' @return dataframe
#' @export
format_number <- function(tbl, ..., digits = 0){

  tbl %>% select_otherwise(..., otherwise = where(rlang::is_integer)) -> col_indx

  tbl %>% dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx), ~formattable::accounting(., digits = digits)))
}
is_percentage <- function(x){

  suppressWarnings({
    all(dplyr::between(x, -1, 1), na.rm = T) & is.double(x)
  })

}
select_otherwise <- function(.data, ..., otherwise = NULL, col = NULL, return_type = c("index", "names", "df")){

  return_type <- return_type[1]

  .dots <- rlang::expr(c(...))


  col <- rlang::enexpr(col)
  otherwise = rlang::enexpr(otherwise)


  tidyselect::eval_select(.dots, data = .data) -> eval1

  if(length(eval1) == 0){
    tidyselect::eval_select( otherwise, data = .data) -> eval1
  }

  tidyselect::eval_select(col, data = .data) %>%
    c(eval1) %>% sort() -> eval1


  if(return_type == "df"){

    out <- .data %>% dplyr::select(tidyselect::any_of(eval1))
  } else if(return_type == "names"){
    out <- names(eval1)
  } else{
    out <- eval1
  }

  out
}

