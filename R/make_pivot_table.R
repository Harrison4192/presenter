#' Make pivot table
#'
#' @param tbl a data frame to pivot
#' @param col1 unquoted col 1
#' @param col2 unquoted col 2
#' @param show_totals logical; show row and col totals
#' @param show_percentages string; denominator to use when calculating percentages
#' @param show_chi_test logical; show results of chi squared test in footnote
#' @param tbl_nm string to name table. If not given, automatically defaults to table name.
#' @param theme string to choose a predefined theme
#'
#' @return a flextable
#' @export

make_pivot_table <- function(tbl,
                             col1,
                             col2,
                             show_totals = TRUE,
                             show_percentages = c("none", "all", "row", "col"),
                             show_chi_test = FALSE,
                             theme = c("zebra_blue", "zebra_gold", "tron", "vader", "vanilla", "booktabs", "alafoli"),
                             tbl_nm = NULL){

  Total <- pct <- NULL

  if(is.null(tbl_nm)){
  tbl1 <- rlang::ensym(tbl)
  get_piped_name(!!tbl1, "Pivot") -> tbl_nm} else {
    tbl_nm %>% stringr::str_replace_all(" ", "_") -> tbl_nm
  }

theme <- match.arg(theme)
show_percentages <- match.arg(show_percentages)

  col1 <- rlang::ensym(col1)
  col1_nm <- rlang::as_string(col1)
  col2 <- rlang::ensym(col2)
  col2_nm <- rlang::as_string(col2)
  tbl_nm1 <- tbl_nm %>% stringr::str_c(" ")



  tbl %>%
    dplyr::mutate(dplyr::across(c(!!col1, !!col2), as.factor)) %>%
    janitor::tabyl(!!col1, !!col2)  -> tbl1

  suppressWarnings({
  tbl1 %>%
    janitor::chisq.test() %>%
    utils::capture.output() %>%
      purrr::pluck(length(.) - 1) -> chi_test_res
})

# create totals -----------------------------------------------------------

if(show_totals){
  ### col totals
  tbl1 %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(where(is.double), as.integer))-> tblx

  tblx %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Total = sum(dplyr::c_across())) -> sum1

  sum1 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(.fns = ~./Total)) %>%
    format_percent() %>%
    dplyr::mutate(dplyr::across(.fns = as.character)) %>%
    dplyr::ungroup() -> pct1

  dplyr::bind_rows(pct1, sum1 %>%  dplyr::mutate(dplyr::across(.fns = ~stringr::str_c("(", ., ")"))) ) %>%
    dplyr::summarize(dplyr::across(.fns = ~stringr::str_c(., collapse = " ")), .groups = "drop") -> col_totals


  ### row totals
  tblx %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Total = sum(dplyr::c_across(where(is.numeric)))) -> pct2

  pct2 %>%
    dplyr::mutate(pct = Total / sum(.$Total), .before = "Total") -> sum2

  sum2 %>%
    format_percent() %>%
    dplyr::mutate(Total = stringr::str_c("(", Total, ")")) %>%
    tidyr::unite(col = "Total", sep = " ", pct, Total) %>%
    dplyr::select(Total) -> row_totals
}

# create tabyl ------------------------------------------------------------




  if(show_percentages != "none"){

    tbl1 %>%
    janitor::adorn_percentages(denominator = show_percentages) %>%
    janitor::adorn_pct_formatting(digits = 0) %>%
    janitor::adorn_ns() -> tbl1}

  if(show_totals){

    tbl1 %>%
      dplyr::mutate(dplyr::across(.fns = as.character)) %>%
      dplyr::bind_cols(row_totals) %>%
      dplyr::bind_rows(col_totals) -> tbl1
  }


  tbl1 %>%
    dplyr::mutate(dplyr::across(where(is.double), as.integer)) %>%
    dplyr::mutate("S" = col1_nm, .before = 1) %>%
    rlang::set_names(names(.) %>% stringr::str_c(col2_nm,"_", .) %>% replace(1:2, c(tbl_nm, tbl_nm1 ))) -> tbl1

  if(show_totals){
    tbl1 %>% dplyr::rename(" " := tidyselect::last_col()) -> tbl1
    tbl1[nrow(tbl1), 1] <- " "}

# make flextable ----------------------------------------------------------


  tbl1 %>%
    make_flextable(last_id_col = 2, header_words = c(col2_nm, tbl_nm), theme = theme) ->f1

  f1 %>%
    flextable::merge_at(i = 1:2, j = 1:2, part = "header") %>%
    flextable::fix_border_issues() -> f1

  if(theme == "zebra_blue"){

    f1 %>%
    flextable::bg(i = 1:2, j = 1:2, bg = "steelblue3", part = "header") %>%
    flextable::bg(j = 1,  bg = "steelblue3") %>%
    flextable::bg(j = 2,  bg = "steelblue2") %>%
    flextable::color(j = 1:2, color = "white") %>%
    flextable::vline(j = 2, border = officer::fp_border(width = 3)) -> f1

    if (show_totals) {
      f1 %>%
        flextable::bg(j = 3:ncol(tbl1), bg = "#A4D3EE", part = "body") %>%
        flextable::bg(j = ncol(tbl1),  bg = "azure2") %>%
        flextable::bg(i = nrow(tbl1),
                      j = 3:ncol(tbl1),
                      bg = "azure2") -> f1

    }

    if(show_totals){
      f1 %>%
        flextable::color(i = nrow(tbl1), color = "gray43") %>%
        flextable::color(j = ncol(tbl1), color = "gray43") -> f1

      f1 %>%
        flextable::bg(i = nrow(tbl1), j = 1:2, bg = "white") %>%
        flextable::bg(i = 1:2, j = ncol(tbl1), bg = "white", part = "header") %>%
        flextable::vline(i = nrow(tbl1), j = 1, border = officer::fp_border(color = "white", width = 1.5), part = "body")-> f1

    }


    if (show_totals){
      f1 %>%
        flextable::border_inner(border = officer::fp_border(color = "black", style = "solid", width = 1), part = "body") -> f1
    }

    f1 %>%
    flextable::border(i = 1:(nrow(tbl1)), j = 1:2,  border = officer::fp_border(color = "black", style = "solid", width = 3), part = "body") %>%
      flextable::border(i = 1:2, j = 1:ncol(tbl1),  border = officer::fp_border(color = "black", style = "solid", width = 3), part = "header") %>%
      flextable::border(i = nrow(tbl1), j = 1,  border = officer::fp_border(color = "white", style = "solid", width = 3), part = "body") %>%
      flextable::border_outer(border = officer::fp_border(color = "black", style = "solid", width = 3)) -> f1
  }
  else if(theme == "zebra_gold"){

    if (show_totals){
      f1 %>%
        flextable::border_inner(border = officer::fp_border(color = "black", style = "solid", width = 1), part = "body") -> f1}

    odd_header = "darkgoldenrod2"
    even_header = "gold2"
    header_color = "black"

        f1 %>%
          flextable::bg(i = 1:2, j = 1:2, bg = odd_header, part = "header") %>%
          flextable::bg(j = 1,  bg = odd_header) %>%
          flextable::bg(j = 2,  bg = even_header) %>%
          flextable::color(j = 1:2, color = "black") %>%
          flextable::vline(j = 2, border = officer::fp_border(width = 3))-> f1

          if (show_totals) {
          f1 %>%
            flextable::bg(j = 3:ncol(tbl1), bg = "gold", part = "body") %>%
            flextable::bg(j = ncol(tbl1),  bg = "wheat") %>%
            flextable::bg(i = nrow(tbl1),
                          j = 3:ncol(tbl1),
                          bg = "wheat") -> f1

            if(show_totals){
              f1 %>%
                flextable::hline(i = (nrow(tbl1) - 1), border = officer::fp_border(color = "black", style = "solid", width = 3)) %>%
                flextable::vline(
                  j = ncol(tbl1) - 1,
                  border = officer::fp_border(color = "black", style = "solid", width = 3)) %>%
                flextable::fix_border_issues() %>%
                flextable::color(i = nrow(tbl1), color = "gray43") %>%
                flextable::color(j = ncol(tbl1), color = "gray43") -> f1

              f1 %>%
                flextable::bg(i = nrow(tbl1), j = 1:2, bg = "white") %>%
                flextable::bg(i = 1:2, j = ncol(tbl1), bg = "white", part = "header") %>%
                flextable::vline(i = nrow(tbl1), j = 1, border = officer::fp_border(color = "white", width = 1.5), part = "body")-> f1

            }


          }
        if (show_totals){
          f1 %>%
            flextable::border_inner(border = officer::fp_border(color = "black", style = "solid", width = 1), part = "body") -> f1
        }

        f1 %>%
          flextable::border(i = 1:(nrow(tbl1)), j = 1:2,  border = officer::fp_border(color = "black", style = "solid", width = 3), part = "body") %>%
          flextable::border(i = 1:2, j = 1:ncol(tbl1),  border = officer::fp_border(color = "black", style = "solid", width = 3), part = "header") %>%
          flextable::border(i = nrow(tbl1), j = 1,  border = officer::fp_border(color = "white", style = "solid", width = 3), part = "body") %>%
          flextable::border_outer(border = officer::fp_border(color = "black", style = "solid", width = 3)) -> f1
        }
  else if(theme %in% c("booktabs", "box", "vanilla")){
  f1 %>%
    flextable::color(j = 1:2, color = "black")-> f1

    if(show_totals){
      f1 %>%
        flextable::hline(i = (nrow(tbl1) - 1), border = officer::fp_border(color = "gray", style = "solid", width = 3)) %>%
        flextable::vline(
          j = ncol(tbl1) - 1,
          border = officer::fp_border(color = "gray", style = "solid", width = 3)) %>%
        flextable::fix_border_issues() %>%
        flextable::color(i = nrow(tbl1), color = "gray43") %>%
        flextable::color(j = ncol(tbl1), color = "gray43") -> f1

      f1 %>%
        flextable::bg(i = nrow(tbl1), j = 1:2, bg = "white") %>%
        flextable::bg(i = 1:2, j = ncol(tbl1), bg = "white", part = "header") %>%
        flextable::vline(i = nrow(tbl1), j = 1, border = officer::fp_border(color = "white", width = 1.5), part = "body")-> f1

    }


    f1 %>%
      flextable::border(i = 1:(nrow(tbl1)), j = 1:2,  border = officer::fp_border(color = "gray", style = "solid", width = 3), part = "body") %>%
      flextable::border(i = 1:2, j = 1:ncol(tbl1),  border = officer::fp_border(color = "gray", style = "solid", width = 3), part = "header") %>%
      flextable::border(i = nrow(tbl1), j = 1,  border = officer::fp_border(color = "white", style = "solid", width = 3), part = "body") %>%
      flextable::border_outer(border = officer::fp_border(color = "gray", style = "solid", width = 3)) -> f1

    }
  else if(theme %in% c( "tron")){
    f1 %>%
      flextable::color(j = 1:2, color = "white") -> f1

    if(show_totals){
      f1 %>%
        flextable::hline(i = (nrow(tbl1) - 1), border = officer::fp_border(color = "skyblue", style = "solid", width = 3)) %>%
        flextable::vline(
          j = ncol(tbl1) - 1,
          border = officer::fp_border(color = "skyblue", style = "solid", width = 3)) %>%
        flextable::fix_border_issues() %>%
        flextable::color(i = nrow(tbl1), color = "skyblue") %>%
        flextable::color(j = ncol(tbl1), color = "skyblue") -> f1}


    f1 %>%
      flextable::border(i = 1:(nrow(tbl1)), j = 1:2,  border = officer::fp_border(color = "skyblue", style = "solid", width = 3), part = "body") %>%
      flextable::border(i = 1:2, j = 1:ncol(tbl1),  border = officer::fp_border(color = "skyblue", style = "solid", width = 3), part = "header") %>%
      flextable::border(i = nrow(tbl1), j = 1,  border = officer::fp_border(color = "black", style = "solid", width = 3), part = "body") %>%
      flextable::border_outer(border = officer::fp_border(color = "skyblue", style = "solid", width = 3)) -> f1


  }else if(theme %in% c("vader")){
    f1 %>%
      flextable::color(j = 1:2, color = "white") -> f1

    if(show_totals){
      f1 %>%
        flextable::hline(i = (nrow(tbl1) - 1), border = officer::fp_border(color = "pink", style = "solid", width = 3)) %>%
        flextable::vline(
          j = ncol(tbl1) - 1,
          border = officer::fp_border(color = "pink", style = "solid", width = 3)) %>%
        flextable::fix_border_issues() %>%
        flextable::color(i = nrow(tbl1), color = "pink") %>%
        flextable::color(j = ncol(tbl1), color = "pink") -> f1}


    f1 %>%
      flextable::border(i = 1:(nrow(tbl1)), j = 1:2,  border = officer::fp_border(color = "pink", style = "solid", width = 3), part = "body") %>%
      flextable::border(i = 1:2, j = 1:ncol(tbl1),  border = officer::fp_border(color = "pink", style = "solid", width = 3), part = "header") %>%
      flextable::border(i = nrow(tbl1), j = 1,  border = officer::fp_border(color = "black", style = "solid", width = 3), part = "body") %>%
      flextable::border_outer(border = officer::fp_border(color = "pink", style = "solid", width = 3)) -> f1


  }
f1 %>%
  flextable::fontsize(j = 1:2, size = 16) %>%
  flextable::bold(j = 1:2) %>%
  flextable::rotate(j = 1, rotation = "btlr") %>%
  flextable::fix_border_issues() %>%
  flextable::width( j = 2:ncol(tbl1) , width = .5) %>%
  flextable::width(j = 1, width = .35) -> f1




if(show_percentages == "col"){
  f1 %>%
    flextable::add_footer_row(values = "* columns add to 100%", colwidths = ncol(tbl1)) -> f1
} else if(show_percentages == "row"){
  f1 %>%
    flextable::add_footer_row(values = "* rows add to 100%", colwidths = ncol(tbl1)) -> f1
}

if(show_chi_test){
  flextable::add_footer_row(f1, values = chi_test_res, colwidths = ncol(tbl1)) -> f1
}

f1

}



