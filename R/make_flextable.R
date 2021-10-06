#' Make Flextable
#'
#' Turns a data frame into a flextable
#'
#' @param df data frame
#' @param header_words header words. Takes a character vector of header words. will be automatically generate via a heuristic if left NULL. can be completely disabled by the string "disable"
#' @param last_id_col last id col
#' @param merge_col_indices merge specific column indices
#' @param theme string to choose a preselected theme
#' @param dbl_digits integer. how many trailing digits should be displayed on dbls
#'
#' @return a flextable
#' @export
make_flextable <- function(df,
                           header_words = NULL,
                           last_id_col = NULL,
                           merge_col_indices = NULL,
                           dbl_digits = 2,
                           theme = c("zebra_blue", "zebra_gold", "tron", "vader", "vanilla", "booktabs", "alafoli")){


  odd_header = "steelblue3"
  even_header = "steelblue2"
  odd_body = "#A4D3EE"
  even_body = "azure2"
  header_color = "white"
  id_color = "grey45"
  header_font_size = 16
  body_font_size = 12
  cell_border_color = "white"
  border_outer_color = "black"
  border_outer_style = "solid"
  cell_border_style = "solid"


  #format p.value

  df %>%
    dplyr::mutate(dplyr::across(tidyselect::matches("p.value"), format.pval)) -> df

  # format percent (experimental)

  # df %>%
  #   format_percent(where(is.double) & matches("pct|PCT|percent|PERCENT")) -> df


  flextable::flextable(df) -> f1
theme <- match.arg(theme)
id_col_nums <- NULL


  if(!is.null(header_words) && "disable" %in% header_words){

    f1 <- f1


  } else if(!is.null(header_words)) {

    flex_set_headers(f1, df, header_words) -> f1

  } else {

    get_headers(df) -> header_words

    if(!rlang::is_empty(header_words)){
    flex_set_headers(f1, df, header_words) -> f1}
    else{
      f1 -> f1
    }
  }


  if(!is.null(last_id_col)){

    df %>%
      dplyr::select(1:{{last_id_col}}) %>%
      names -> id_nms

    last_id_col_num <- length(id_nms)

    (last_id_col_num+1):ncol(df) -> value_cols
    1:last_id_col_num -> id_col_nums

    id_nms %>%
      charvec_to_formula(.) -> id_cols

    merge_cols <- id_cols
  }
  else{
    id_col_nums <- NULL
    id_cols <- flex_mergev1_formula(df)
    merge_cols <- value_cols <- 1:length(names(df))
  }

  if(!is.null(merge_col_indices)){
    merge_cols <- merge_col_indices
  }


if(theme == "zebra_blue"){
  f1 %>%
    flextable::theme_zebra(
      odd_header = odd_header,
      even_header = even_header,
      odd_body = odd_body,
      even_body = even_body
    ) -> f1

  # if(!is.null(id_col_nums)){
  # f1 %>%
  #   flextable::bg(j = id_col_nums, bg = odd_body) -> f1}

} else if(theme == "zebra_gold"){
  f1 %>%
    flextable::theme_zebra(
      odd_header = "darkgoldenrod2",
      even_header = "gold2",
      odd_body = "gold",
      even_body = "wheat"
    ) -> f1

  cell_border_color <-  "black"
  header_color <- "black"


  # if(!is.null(id_col_nums)){
  # f1 %>%
  #   flextable::bg(j = id_col_nums, bg = "gold2" ) -> f1}


} else if(theme == "tron"){
  f1 %>% flextable::theme_tron() -> f1
  border_outer_color <- "skyblue"
  cell_border_color <- "skyblue"
} else if(theme == "vader"){
  f1 %>% flextable::theme_vader() -> f1
  border_outer_color <- "pink"
  cell_border_color <- "pink"
} else if(theme == "vanilla"){
  f1 %>% flextable::theme_vanilla() -> f1
  border_outer_color <- "darkgrey"
  cell_border_color <- "grey"
  header_color <- "black"
  cell_border_style <- "dashed"
} else if(theme == "booktabs"){
  f1 %>% flextable::theme_booktabs() -> f1
  border_outer_color <- "darkgrey"
  cell_border_color <- "grey"
  header_color <- "black"
  cell_border_style <- "solid"
} else if(theme == "alafoli"){
  f1 %>% flextable::theme_alafoli() -> f1
  border_outer_color <- "darkgrey"
  cell_border_color <- "grey"
  header_color <- "black"
  cell_border_style <- "dashed"
  header_color <- "black"
} else if(theme == "box"){
  f1 %>% flextable::theme_box() -> f1
  border_outer_color <- "darkgrey"
  cell_border_color <- "grey"
  header_color <- "black"
  cell_border_style <- "dashed"
  header_color <- "black"
} else{
  stop("You did not enter a valid theme. Choose from zebra_blue, zebra_gold, tron, vader, box, vanilla, booktabs, alafoli", call. = F)
}


  f1 %>%
    flextable::color( color = header_color, part = "header") %>%
    flextable::fontsize(size = header_font_size, part = "header") %>%
    flextable::fontsize(size = body_font_size, part = "body") -> f1

  if (!is.null(id_cols)) {
    f1 %>%
      flextable::bold(j = id_cols) %>%
      flextable::color(color = id_color, j = id_cols) -> f1
  }


  if(!is.null(merge_col_indices) | !is.null(last_id_col)){
    f1 %>% flextable::merge_v(j = merge_cols) -> f1}


# format integers
df %>%
  select_otherwise(where(rlang::is_bare_integer), return_type = "names") -> int_nms

f1 %>% flextable::colformat_int(j = int_nms) -> f1

# format doubles
df %>%
  select_otherwise(where(rlang::is_bare_double), return_type = "names") -> dbl_nms

if(!rlang::is_empty(dbl_nms)) {f1 %>% flextable::colformat_double(j = dbl_nms, digits = dbl_digits) -> f1}


# get char cols
df %>%
  select_otherwise(where(rlang::is_character)) -> chr_cols


# paint negative numbers red

df %>%
  select_otherwise(where(is.numeric), return_type = "names") -> numeric_cols

  for(nm in numeric_cols){

    my_nm <- rlang::sym(nm)

    f1 %>%
      format_red(my_nm) -> f1
  }

  f1 %>%
    flextable::align( align = "center", part= "all") %>%
    flextable::border_outer(part="all", border = officer::fp_border(width = 3, color = border_outer_color, style = border_outer_style) ) %>%
    flextable::fix_border_issues(.) %>%
    flextable::border_inner(border = officer::fp_border(color = border_outer_color, style = "solid", width = 1.5), part = "header") %>%
    flextable::border(j = last_id_col, border = officer::fp_border(color = border_outer_color, style = "solid", width = 1.5), part = "body") %>%
    flextable::border(j = value_cols, border = officer::fp_border(color = cell_border_color, style = cell_border_style, width = 1.5), part = "body") %>%
    flextable::border_outer(part="all", border = officer::fp_border(width = 3, color = border_outer_color, style = border_outer_style) ) %>%
    flextable::fix_border_issues(.) %>%
    flextable::autofit(.) -> f1


f1

}
