export_powerpoint <- function(tbl,
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

