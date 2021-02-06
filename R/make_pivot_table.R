make_pivot_table <- function(tbl, col1, col2, tbl_nm = NULL,
                             odd_header = "steelblue3",
                             even_header = "steelblue2",
                             odd_body = "#A4D3EE",
                             even_body = "azure2",
                             header_color = "white",
                             id_color = "grey45",
                             header_font_size = 16,
                             body_font_size = 12,
                             cell_border_color = "white",
                             title_cell_color = "steelblue3"){

  if(is.null(tbl_nm)){
  get_piped_name(tbl) -> tbl_nm} else {
    tbl_nm %>% stringr::str_replace_all(" ", "_") -> tbl_nm
  }




  col1 <- rlang::ensym(col1)
  col1_nm <- rlang::as_string(col1)
  col2 <- rlang::ensym(col2)
  col2_nm <- rlang::as_string(col2)
  tbl_nm1 <- tbl_nm %>% stringr::str_c(" ")



  tbl %>%
    janitor::tabyl(!!col1, !!col2)  %>%
    janitor::adorn_percentages(denominator = "all") %>%
    janitor::adorn_pct_formatting() %>%
    janitor::adorn_ns() %>%
    tibble::as_tibble() %>%
    dplyr::mutate("S" = col1_nm, .before = 1) %>%
    rlang::set_names(names(.) %>% stringr::str_c(col2_nm,"_", .) %>% replace(1:2, c(tbl_nm, tbl_nm1 ))) %>%
    make_flextable(last_id_col = 2, header_words = c(col2_nm, tbl_nm),
                   odd_header =       odd_header   ,
                   even_header =      even_header   ,
                   odd_body =         odd_body ,
                   even_body =       even_body ,
                   header_color =     header_color,
                   id_color =        id_color,
                   header_font_size = header_font_size,
                   body_font_size =   body_font_size ,
                   cell_border_color =cell_border_color) %>%
    flextable::bg(i = 1:2, j = 1:2, bg = title_cell_color, part = "header") %>%
    flextable::merge_at(i = 1:2, j = 1:2, part = "header") %>%
    flextable::bg(j = 1,  bg = "steelblue3") %>%
    flextable::bg(j = 2,  bg = "steelblue2") %>%
    flextable::color(j = 1:2, color = "white") %>%
    flextable::fontsize(j = 1:2, size = 16)
}


