#'
#' #' style_header_sequence
#' #'
#' #' @param header_word A string to match against column names
#' #' @param object A data.frame-like object
#' #' @param id_col_range Columns to exclude from styling
#' #' @param wb A Workbook object from openxlsx2
#' #' @param sheet A string specifying the worksheet name
#' #'
#' #' @return Applies styles to matching headers in an openxlsx2 workbook
#' #' @keywords internal
#'
#' style_header_sequence <- function(header_word, object, id_col_range, wb, sheet) {
#'   # Find columns matching the header word, excluding id columns
#'   after_cols <- names(object) |>
#'     stringr::str_which(header_word) |>
#'     setdiff(id_col_range)
#'
#'   # Create a style using openxlsx2
#'   value_header_format_2 <- openxlsx2::create_style(
#'     fg_fill = randomcoloR::randomColor(),
#'     halign = "left",
#'     text_decorations = "bold",
#'     border = c("bottom", "right"),
#'     font_color = "black",
#'     font_name = "Arial",
#'     border_style = "thick",
#'     valign = "center",
#'     border_color = "black",
#'     font_size = 13,
#'     wrap_text = TRUE
#'   )
#'
#'   # Apply the style to the workbook
#'   openxlsx2::wb_add_style(
#'     wb = wb,
#'     sheet = sheet,
#'     style = value_header_format_2,
#'     rows = 1,
#'     cols = after_cols
#'   )
#' }
