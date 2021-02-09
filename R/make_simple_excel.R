#' Make a simple excel
#'
#' Export a file to excel with minimal formatting and minimal effort.
#'
#' @param object a data frame
#' @param output_file string to name excel file. If not given, defaults to name of table.
#'
#' @return an .xlsx file
#' @export
make_simple_excel <- function(object, output_file = NULL){

  get_piped_name(object) -> filename

  if(is.null(output_file)){
  output_file <- filename %>% stringr::str_c(".xlsx", collapse = "")
  }
  openxlsx::write.xlsx(object, output_file,
                       headerStyle =  openxlsx::createStyle(textDecoration = "BOLD",
                                                  fontColour = "#FFFFFF",
                                                  fontSize=12, fontName="Arial Narrow",
                                                  fgFill = "#4F80BD"),
                       colNames = TRUE,
                       asTable = T,
                       borders = "rows",
                       tabColour = "snow2",
                       sheetName = filename,
                       borderColour = "steelblue3"

  )}
