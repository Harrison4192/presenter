#' Send Table to Powerpoint
#'
#' A table can be piped in to this function to be sent to a new ppt slide. Can be called with no arguments,
#' then a new ppt is created named after the table. If output_file is specified, table is sent to a new slide
#' on an existing powerpoint.
#'
#' @param tbl a data frame,  flextable, or a list thereof
#' @param output_file path to existing ppt
#' @param layout master layout
#' @param master master theme
#' @param show logical to open the ppt
#'
#' @return none
#' @export
make_powerpoint <- function(tbl,
                              output_file = NULL,
                              layout = "Two Content",
                              master = "Office Theme",
                              show = TRUE){


  tbl1 <- rlang::ensym(tbl)

  get_piped_name(!!tbl1) -> tbl_nm0

  stringi::stri_rand_strings(1, 4) -> tbl_id

  tbl_nm <- paste0(tbl_nm0, tbl_id)

  if(is.null(output_file)){
    output_file <- tbl_nm %>% stringr::str_c(".pptx")
    officer::read_pptx() -> ppt1

  } else{
    officer::read_pptx(output_file) -> ppt1

  }

  if(!rlang::is_bare_list(tbl)){
    rlang::list2(!!tbl_nm := tbl) -> tbl
  }


  for(i in seq_along(tbl)){



    title <- names(tbl[i])

    if(ggplot2::is.ggplot(tbl[[i]])){
      tbl[[i]] <- rvg::dml(ggobj = tbl[[i]])
    }


    title <- stringr::str_c("Presentation of ", title %>% stringr::str_replace_all("[_.]", " ")) %>% stringr::str_remove(tbl_id)

  ppt1 %>%
    officer::add_slide(layout = layout,
                       master = master) %>%
    officer::ph_with(value = title, location = officer::ph_location_type(type = "title")) %>%
    officer::ph_with(value = tbl[[i]], location = officer::ph_location(top = 1.5)) -> ppt1

}

print(ppt1, target= output_file)

if(show){
berryFunctions::openFile(output_file)}

}

