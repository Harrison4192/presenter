


#' format number
#'
#' Number formatters to apply to a column in a dataframe. Helpful for displaying tibbles in console or in conjunction
#' with \code{\link{make_flextable}}.
#' Based off the \href{https://renkun-ken.github.io/formattable/}{formattable} package.
#'
#' \itemize{
#' \item{\code{format_number}}{ formats a number accounting style by inserting commas. default selection is integer columns}
#' \item{\code{format_percent}}{ formats a number as a percentage. default selection is numeric columns in between -1 and 1.}
#' \item{\code{format_currency}}{ formats a monetary value with the currency symbol. default currency symbol is yen.}}
#'
#' @param tbl dataframw
#' @param ... tidyselect. default selection is integer.
#' @param digits integer. trailing digits
#'
#' @return dataframe
#' @export
#' @importFrom framecleaner select_otherwise
#'
#' @examples
#'
#' tibble::tibble(
#' y = seq(1000L, 10000L, by = 1000L),
#' z = c(-.59, -.23, -.11, 0, .1, .21, .3, .4, .6, .9),
#' w = c(.1, 1.4, .23, -.10, 0, -2.3, .2,.3,.4,.5)) -> tbl1
#'
#' tbl1
#'
#' # automatically formats the integer column
#' tbl1 %>%
#' format_number()
#'
#' # automatically formats to yen
#' tbl1 %>%
#' format_currency(y)
#'
#' # automatically detects columns between -1 and 1 to convert to percentages
#' tbl1 %>%
#' format_percent()
#'
#' # select specific columns to convert.
#' tbl1 %>%
#' format_percent(z, w)
#'
format_number <- function(tbl, ..., digits = 0){

  tbl %>% select_otherwise(..., otherwise = where(rlang::is_integer)) -> col_indx

  tbl %>% dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx), ~formattable::accounting(., digits = digits)))
}

#' format percentage
#'
#'
#'
#' @rdname format_number
#' @param tbl dataframe
#' @param ... tidyselect
#' @param digits integer. trailing digits
#'
#' @return dataframe
#' @export
#'
#'
format_percent <- function(tbl, ..., digits = 0){

  tbl %>% select_otherwise(..., otherwise = where(is_percentage)) -> col_indx

  # tbl %>% dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx), ~ifelse(. == 0, . + .0000000000001, .))) -> tbl


  tbl %>% dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx), ~formattable::percent(., digits = digits)))
}

#' format currency
#'
#'
#' @rdname format_number
#' @param tbl dataframe
#' @param ... tidyselect.
#' @param symbol chr. currency symbol
#' @param digits integer. trailing digits
#'
#' @export
format_currency <- function(tbl, ..., symbol = "yen", digits = 0){

  if(symbol == "yen"){symbol <- "\u00A5"}

  tbl %>% select_otherwise(..., otherwise = tidyselect::matches("SALES|PRICE")) -> col_indx

  tbl %>% dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx), ~formattable::currency(as.double(.), symbol = symbol, digits = digits)))
}

#' is_percentage
#'
#' @param x numeric vector
#'
#' @return logical
#' @export
#' @keywords internal
#'
#' @examples
#'
#' library(presenter)
#'
#' c(.1, 0, .5) %>%
#' is_percentage
is_percentage <- function(x){

  suppressWarnings({
    all(dplyr::between(x, -1, 1), na.rm = T) & is.double(x)
  })

}




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
