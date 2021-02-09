#' Send Table to Powerpoint
#'
#' A table can be piped in to this function to be sent to a new ppt slide. Can be called with no arguments,
#' then a new ppt is created named after the table. If `output_file` is specified, table is sent to a new slide
#' on an existing powerpoint.
#'
#' @param tbl a data frame or a flextable
#' @param output_file path to existing ppt
#' @param layout master layout
#' @param master master theme
#' @param title slide title
#' @param show logical to open the ppt
#'
#' @return none
#' @export
table_to_powerpoint <- function(tbl,
                              output_file = NULL,
                              layout = "Two Content",
                              master = "Office Theme",
                              title = NULL,
                              show = F){
  get_piped_name(tbl) -> tbl_nm

  if(is.null(output_file)){
    output_file <- tbl_nm %>% stringr::str_c(".pptx")
    officer::read_pptx() -> ppt1

  } else{
    officer::read_pptx(output_file) -> ppt1

  }


  if(is.null(title)){
    title <- stringr::str_c("Presentation of ", tbl_nm %>% stringr::str_replace_all("[_.]", " "))}
  ppt1 %>%
    officer::add_slide(layout = layout,
                       master = master) %>%
    officer::ph_with(value = title, location = officer::ph_location_type(type = "title")) %>%
    officer::ph_with(value = tbl, location = officer::ph_location(top = 1.5)) -> ppt2



print(ppt2, target= output_file)

if(show){
file.show(output_file)}

}

