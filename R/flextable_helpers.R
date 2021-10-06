
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

  if(rlang::is_empty(chr)){return(NULL)} else{

  stringr::str_c(chr, collapse = " + ") %>%
    stringr::str_c("~ ", ., collapse = "")  %>%
    parse(text = .) %>%
    eval}
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


get_headers <- function(db){

  db %>%
    names() -> nms
  nms %>%
    stringr::str_extract("^.*(?=(_|\\.))") %>%
    table() %>%
    subset(subset = . > 1) %>%
    names() -> hdrs



  if(rlang::is_empty(hdrs)){
    nms1 <- nms
  } else{
    nms %>%
      stringr::str_subset(stringr::str_c(hdrs, collapse = "|"), negate = T) -> nms1

  }

  nms1 %>%
    stringr::str_extract("(?<=(_|\\.)).*$") %>%
    table() %>%
    subset(subset = . > 1) %>%
    names() -> hdrs1

unique(c(hdrs1, hdrs))

}
