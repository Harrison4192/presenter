
#' flex_create_headers
#'
#' create data frame of header specifications for flextable::set_header_df
#'
#' @param df a data frame
#' @param word_vec word vec
#' @keywords internal
#'
#' @return data frame
#'
flex_create_headers <- function(df, word_vec){

  stringr::str_c(word_vec, collapse = "|") -> strs

  df %>% names -> nms1
  nms1 %>% stringr::str_match(strs) %>% stringr::str_remove( "^[_\\.]|[_\\.]$")-> head1
  nms1 %>% stringr::str_remove(strs) %>% stringr::str_remove( "^[_\\.]|[_\\.]$")  -> head2


  headers <- data.frame(
    head = nms1,
    head1 = head1,
    head2 = head2,
    stringsAsFactors = F
  ) %>%
    dplyr::mutate(
      head1 = head1 %>% dplyr::coalesce(nms1),
      head2 = head2 %>% dplyr::coalesce(head1),
    )

  headers
}
#' charvec_to_formula
#'
#' @param chr character vector
#' @keywords internal
#'
#' @return a formula
#'
charvec_to_formula <- function(chr){

  if(rlang::is_empty(chr)){rlang::abort("requires a character column")}

  stringr::str_c(chr, collapse = " + ") %>%
    stringr::str_c("~ ", ., collapse = "")  %>%
    parse(text = .) %>%
    eval
}

#' flex_mergev1_formula
#'
#' @param df a dataframe
#' @keywords internal
#'
#' @return a formula
#'
flex_mergev1_formula <- function(df){

  df %>%
    purrr::map_lgl(~is.character(.)|is.factor(.)) %>%
    which(arr.ind = T) %>%
    names %>%
    charvec_to_formula(.)

}

## in table chain

#' charvec_to_formula
#'
#' @param x flextable
#' @param df data frame
#' @param word_vec  word
#' @keywords internal
#'
#' @return a flextable
#'
flex_set_headers <- function(x, df, word_vec){

  df %>%
    flex_create_headers(word_vec) -> headers

  flextable::set_header_df(x, mapping = headers, key = "head") %>%
    flextable::merge_h( part = "header") %>%
    flextable::merge_v( part = "header")
}
#' flex_mergev1
#'
#' @param x a flextable
#' @param df data frame
#' @keywords internal
#'
#' @return a flextable
#'
flex_mergev1 <- function(x, df){

  df %>%
    flex_mergev1_formula(.) -> my_form

  x %>%
    flextable::merge_v(j = my_form)

}

## make table

#' Make Flextable
#'
#' @param df data frame
#' @param header_words header words
#' @param last_id_col last id col
#'
#' @return a flextable
#' @export
#'
make_flextable <- function(df, header_words = NULL, last_id_col = NULL){


  flextable::flextable(df) -> f1

  if(!is.null(header_words)){

    flex_set_headers(f1, df, header_words) -> f1
  }


  if(!is.null(last_id_col)){

    df %>%
      dplyr::select(1:{{last_id_col}}) %>%
      names %>%
      charvec_to_formula(.) -> id_cols

    merge_cols <- id_cols
  }
  else{
    id_cols <- flex_mergev1_formula(df)
    merge_cols <- 1:length(names(df))
  }

  f1 %>%
    flextable::theme_zebra(
      odd_header = "steelblue3",
      even_header = "steelblue2",
      odd_body = "#A4D3EE",
      even_body = "azure2"
    ) %>%
    flextable::color( color = "white", part = "header") %>%
    flextable::color(color = "gray45", j = id_cols) %>%
    flextable::fontsize(size = 16, part = "header") %>%
    flextable::fontsize(size = 12, part = "body") %>%
    flextable::bold(j = id_cols) %>%
    flextable::merge_v(j = merge_cols) %>%
    flextable::border(border = officer::fp_border(color = "white", style = "solid", width = 1.5), part = "all") %>%
    flextable::align( align = "center", part= "all") %>%
    flextable::border_outer(part="all", border = officer::fp_border(width = 3) ) %>%
    flextable::fix_border_issues(.) %>%
    flextable::autofit(.)
}
