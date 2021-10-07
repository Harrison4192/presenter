#' Make a simple excel
#'
#' Export a file to excel with minimal formatting and minimal effort.
#'
#' @param object a data frame or list thereof
#' @param  show logical. open excel upon completion?
#'
#' @return an .xlsx file
#' @export
make_simple_excel <- function(object, show = TRUE){

  sheetname1 <- NULL

  object1 <- rlang::ensym(object)
  get_piped_name(!!object1) -> sheetname

  if(is.data.frame(object)){
    object <- dplyr::rename_with(object, enc2utf8)
    sheetname1 <- sheetname}

  sheetname %>%
    paste0(stringi::stri_rand_strings(1, 4))  %>%
    stringr::str_c(".xlsx", collapse = "")-> output_file



  openxlsx::write.xlsx(object, output_file, overwrite = T,
                       headerStyle =  openxlsx::createStyle(textDecoration = "BOLD",
                                                  fontColour = "#FFFFFF",
                                                  fontSize=12, fontName="Arial Narrow",
                                                  fgFill = "#4F80BD"),
                       colNames = TRUE,
                       asTable = T,
                       borders = "rows",
                       tabColour = "snow2",
                       sheetName = sheetname1,
                       borderColour = "steelblue3"

  )

  if(show){
    openxlsx::openXL(output_file)}
  }

