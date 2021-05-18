# custom openxlsx ouputs --------------------------------------------------

#' style_header_sequence
#'
#' @param header_word header words
#' @param object object
#' @param id_col_range id col range
#' @param wb wb
#' @param sheet sheet
#'
#' @return excel headers
#' @keywords internal

style_header_sequence <- function(header_word, object, id_col_range, wb, sheet ){


  names(object) %>% stringr::str_which(header_word) %>% setdiff(id_col_range) -> after_cols


  value_header_format_2 <- openxlsx::createStyle(
    fgFill = randomcoloR::randomColor(),
    halign = "left",
    textDecoration = "Bold",
    border = c("bottom", "right"),
    fontColour = "black",
    fontName = "Arial",
    borderStyle = "thick",
    valign = "center",
    borderColour = "black",
    fontSize = 13,
    wrapText = T)

  openxlsx::addStyle(wb,
                     sheet = sheet ,
                     cols = after_cols,
                     rows = 1,
                     style = value_header_format_2)}



## create wb with chained  multiple sheets

#' create excel wb
#'
#' @param wb wb
#' @param object  object
#' @param last_id_col  index of last id col
#' @param header_word character vector of header words
#' @param widths col widths
#' @param random_color_seed seed for random color scheme
#'
#' @return excel wb object
#' @export
#'
make_excel_wb <- function(wb = NULL,
                            object,
                            last_id_col = NULL,
                            header_word = NULL,
                            widths = 13,
                            random_color_seed = 1){


  # create workbook -----------------------------------------------------


  file_name <- rlang::as_name(rlang::enexpr(object))
  wb_name <- file_name %>% stringr::str_c(".xlsx", collapse = "")

  object <- dplyr::rename_with(object, .fn = enc2utf8)

  if(!is.null(last_id_col)){
  id_col_range <- 1:(last_id_col) }
  else{id_col_range <- NULL}

  if(is.null(wb)){
    openxlsx::createWorkbook() -> wb}

set.seed(random_color_seed)
  openxlsx::addWorksheet(wb,
               file_name,
               gridLines = FALSE,
               tabColour = randomcoloR::randomColor())

  sheet <- file_name

  openxlsx::writeData(wb, sheet, object)


  # create formats ----------------------------------------------------------




  value_header_format_1 <- openxlsx::createStyle(
    fgFill = "darkolivegreen3",
    halign = "left",
    textDecoration = "Bold",
    border = c("bottom", "right"),
    fontColour = "black",
    fontName = "Arial",
    borderStyle = "thick",
    valign = "center",
    borderColour = "black",
    fontSize = 13,
    wrapText = T)


  text_header_format <- openxlsx::createStyle(
    fgFill = "honeydew3",
    halign = "left",
    textDecoration = "Bold",
    border = c("bottom", "right"),
    fontColour = "black",
    fontName = "Arial",
    borderStyle = "thick",
    valign = "center",
    borderColour = "black",
    fontSize = 13,
    wrapText = T)

  value_cell_integer_format <- openxlsx::createStyle(
    numFmt = "#;[Red]#;[Blue]-;",
    fgFill = "blanchedalmond",
    halign = "center",
    border =  c("top", "bottom", "left", "right"),
    fontColour = "black",
    borderStyle = "dashed",
    valign = "center",
    borderColour = "black",
    fontSize = 12,
    wrapText = T)

  value_cell_number_format <- openxlsx::createStyle(
    numFmt = "#.##;[Red]#.##;[Blue]-;",
    fgFill = "blanchedalmond",
    halign = "center",
    border =  c("top", "bottom", "left", "right"),
    fontColour = "black",
    borderStyle = "dashed",
    valign = "center",
    borderColour = "black",
    fontSize = 11,
    wrapText = T)

  value_cell_percent_format <- openxlsx::createStyle(
    numFmt = "###%;[Red]###%;[Blue]-",
    fgFill = "blanchedalmond",
    halign = "center",
    border =  c("top", "bottom", "left", "right"),
    fontColour = "black",
    textDecoration = "italic",
    borderStyle = "dashed",
    valign = "center",
    borderColour = "black",
    fontSize = 11,
    wrapText = T)

  value_cell_text_format <- openxlsx::createStyle(
    fgFill = "gray81",
    halign = "center",
    border =  c("top", "bottom", "left", "right"),
    textDecoration = "italic",
    fontColour = "black",
    borderStyle = "dashed",
    valign = "center",
    borderColour = "black",
    fontSize = 10,
    fontName = "Bell MT",
    wrapText = T)


  id_header_format <- openxlsx::createStyle(
    fgFill = "yellow",
    halign = "CENTER",
    textDecoration = "Bold",
    border = c("top", "bottom", "left", "right"),
    fontColour = "black",
    fontName = "Arial Narrow",
    borderStyle = "thick",
    valign = "center",
    borderColour = "black",
    fontSize = 13,
    wrapText = T)

  id_cell_format <- openxlsx::createStyle(
    fgFill = "khaki1",
    halign = "left",
    textDecoration = "Bold",
    border =  c("top", "bottom", "left", "right"),
    fontColour = "black",
    borderStyle = "thin",
    valign = "center",
    borderColour = "black",
    fontSize = 10,
    wrapText = T
  )
  date_cell_format <- openxlsx::createStyle(
    numFmt = "DATE",
    fgFill = "lightgoldenrod2",
    halign = "left",
    textDecoration = "Bold",
    border =  c("top", "bottom", "left", "right"),
    fontColour = "black",
    borderStyle = "thin",
    valign = "center",
    borderColour = "black",
    fontSize = 10,
    wrapText = T
  )



  # format sheets -----------------------------------------------------------

if(is.null(last_id_col)){last_id_col <- 0}
  value_col_range <- (last_id_col + 1):ncol(object)

  which(purrr::map_lgl(object, ~(is.character(.) | is.factor(.)))) %>%
    setdiff(id_col_range) -> character_col_range

  which(purrr::map_lgl(object, is.double)) %>%
    setdiff(id_col_range) -> numeric_col_range

  which(purrr::map_lgl(object, rlang::is_integerish)) %>%
    setdiff(id_col_range) -> integer_col_range

  which(purrr::map_lgl(object, lubridate::is.Date)) -> date_col_range

  row_range <- 2:(nrow(object) +1)





  openxlsx::addFilter(
    wb,
    sheet,
    rows = 1,
    cols = 1:ncol(object)
  )

  ## style id headers
  if(!is.null(id_col_range)){
  openxlsx::addStyle(wb,
           sheet = sheet ,
           cols = id_col_range,
           rows = 1,
           style = id_header_format)}

  # style id cells
  openxlsx::addStyle(wb,
           sheet = sheet ,
           cols = id_col_range %>% setdiff(date_col_range),
           rows = row_range,
           style = id_cell_format,
           gridExpand = T)

  # style date cells
  openxlsx::addStyle(wb,
           sheet = sheet ,
           cols = date_col_range,
           rows = row_range,
           style = date_cell_format,
           gridExpand = T)



  ## style value headers
  openxlsx::addStyle(wb,
           sheet = sheet ,
           cols = value_col_range,
           rows = 1,
           style = value_header_format_1)


  ## style value number cells
  openxlsx::addStyle(wb,
           sheet = sheet ,
           cols = numeric_col_range,
           rows = row_range,
           style = value_cell_number_format,
           gridExpand = T)

  ## style value integer cells
  openxlsx::addStyle(wb,
           sheet = sheet ,
           cols = integer_col_range,
           rows = row_range,
           style = value_cell_integer_format,
           gridExpand = T)

  ## style value percent cells

  names(object) %>% stringr::str_which("PERCENT|percent|ratio|RATIO|PCT|pct") -> percent_cols


  openxlsx::addStyle(wb,
           sheet = sheet ,
           cols = percent_cols,
           rows = row_range,
           style = value_cell_percent_format,
           gridExpand = T)

  ## style value text cells
  openxlsx::addStyle(wb,
           sheet = sheet ,
           cols = character_col_range,
           rows = row_range,
           style = value_cell_text_format,
           gridExpand = T)

  ## style text headers
  openxlsx::addStyle(wb,
           sheet = sheet ,
           cols = character_col_range,
           rows = 1,
           style = text_header_format)

  ## style 2nd headers
  if(!is.null(header_word)){
    for(word in header_word){

      style_header_sequence(word, object, id_col_range, wb, sheet)
    } }


  openxlsx::freezePane(
    wb,
    sheet,
    firstActiveCol = last_id_col + 1,
    firstActiveRow = 2
  )

  openxlsx::showGridLines(
    wb,
    sheet,
    showGridLines = FALSE
  )

  openxlsx::setColWidths(
    wb,
    sheet,
    unique(c(id_col_range, value_col_range)),
    widths = widths
  )



  if(length(date_col_range > 0)){

    openxlsx::setColWidths(
      wb,
      sheet,
      date_col_range,
      widths = 14
    )}

  wb
}

#' finish excel workbook
#'
#' @param wb wb
#' @param wb_name wb name
#'
#' @return an excel file
#' @export
#'
finish_excel_wb <- function(wb,
                            wb_name){

  wb_name <- wb_name %>% paste0(".xlsx")
  openxlsx::saveWorkbook(wb, wb_name, overwrite = T)
  openxlsx::openXL(wb_name)

}

#' Create Excel
#'
#' @param df data frame
#' @param last_id_col index of last id col
#' @param header_word character vector of header words
#' @param widths col widths
#' @param random_color_seed seed for random color scheme
#'
#' @return an excel file
#' @export
make_excel <- function(df,
                         last_id_col = NULL,
                         header_word = NULL,
                         widths = 13,
                         random_color_seed = 1){
  df1 <- rlang::ensym(df)

  df_name <- get_piped_name(!!df1)

  make_excel_wb(object = df,
                  last_id_col = last_id_col,
                  header_word = header_word,
                  widths = widths,
                  random_color_seed = random_color_seed) -> x1

  names(x1) <- df_name

  finish_excel_wb(x1, df_name)
}





