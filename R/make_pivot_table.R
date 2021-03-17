#' Make pivot table
#'
#' @param tbl a data frame to pivot
#' @param col1 unqouted col 1
#' @param col2 unquoted col 2
#' @param show_percentages logical; show percentages with numbers
#' @param tbl_nm string to name table. If not given, automatically defaults to table name.
#' @param tabyl_denominator denominator to use when calculating percentages
#' @param theme string to choose a predefined theme
#'
#' @return a flextable
#' @export

make_pivot_table <- function(tbl,
                             col1,
                             col2,
                             show_percentages = T,
                             tbl_nm = NULL,
                             tabyl_denominator = c("all", "row", "col"),
                             theme = c("zebra_blue", "zebra_gold", "tron", "vader", "vanilla", "booktabs", "alafoli")){

  if(is.null(tbl_nm)){
  get_piped_name(tbl, "Pivot") -> tbl_nm} else {
    tbl_nm %>% stringr::str_replace_all(" ", "_") -> tbl_nm
  }

theme <- match.arg(theme)
tabyl_denominator <- math.arg(tabyl_denominator)

  col1 <- rlang::ensym(col1)
  col1_nm <- rlang::as_string(col1)
  col2 <- rlang::ensym(col2)
  col2_nm <- rlang::as_string(col2)
  tbl_nm1 <- tbl_nm %>% stringr::str_c(" ")



  tbl %>%
    dplyr::mutate(dplyr::across(c(!!col1, !!col2), as.factor)) %>%
    janitor::tabyl(!!col1, !!col2)  -> tbl1

  if(show_percentages){

    tbl1 %>%
    janitor::adorn_percentages(denominator = tabyl_denominator) %>%
    janitor::adorn_pct_formatting() %>%
    janitor::adorn_ns() -> tbl2} else{
      tbl1 -> tbl2
    }
  tbl2 %>%
    tibble::as_tibble() %>%
    dplyr::mutate("S" = col1_nm, .before = 1) %>%
    rlang::set_names(names(.) %>% stringr::str_c(col2_nm,"_", .) %>% replace(1:2, c(tbl_nm, tbl_nm1 ))) %>%
    make_flextable(last_id_col = 2, header_words = c(col2_nm, tbl_nm), theme = theme) ->f1
  f1 %>%
    flextable::merge_at(i = 1:2, j = 1:2, part = "header") %>%
    flextable::fix_border_issues() -> f1

  if(theme == "zebra_blue"){
    f1 %>%
    flextable::bg(i = 1:2, j = 1:2, bg = "steelblue3", part = "header") %>%
    flextable::bg(j = 1,  bg = "steelblue3") %>%
    flextable::bg(j = 2,  bg = "steelblue2") %>%
      flextable::color(j = 1:2, color = "white") -> f1}
  else if(theme == "zebra_gold"){

    odd_header = "darkgoldenrod2"
    even_header = "gold2"
    header_color = "black"

        f1 %>%
          flextable::bg(i = 1:2, j = 1:2, bg = odd_header, part = "header") %>%
          flextable::bg(j = 1,  bg = odd_header) %>%
          flextable::bg(j = 2,  bg = even_header) %>%
          flextable::color(j = 1:2, color = "black") -> f1}
  else if(theme %in% c("booktabs", "box", "vanilla")){
  f1 %>%
    flextable::color(j = 1:2, color = "black") -> f1}
  else if(theme %in% c("vader", "tron")){
    f1 %>%
      flextable::color(j = 1:2, color = "white") -> f1}
f1 %>%
  flextable::fontsize(j = 1:2, size = 16) %>%
  flextable::bold(j = 1:2) %>%
  flextable::rotate(j = 1, rotation = "btlr") %>%
  flextable::fix_border_issues() %>%
  flextable::width( j = 2:(ncol(tbl2)+1) , width = .5) %>%
  flextable::width(j = 1, width = .35)


}



