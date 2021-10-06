#' Pivot Summary
#'
#' Wrapper around base r `t()` that returns a tibble.
#' Transposes a data frame, intended for use on the output of a  dplyr::summarize operation
#'
#' for an ungrouped summarize, ... argument can be left empty
#' for a grouped summarize, use column names or tidyselect to pivot the group names. Multiple groups
#' will be concatenated before pivoting.
#'
#' @param sumr A tibble
#' @param ... an optional tidyselect specification of grouping columns to pivot
#'
#' @return a tibble
#' @export

pivot_summary <- function(sumr, ...){

  sumr %>% dplyr::ungroup(.) -> sumr

  column <- rowname <- NULL

  if (!missing(..1)) {
    sumr %>%
      tidyr::unite(col = "column", ..., remove = T) %>% dplyr::relocate(column) -> sumr1

    sumr1 %>%
      dplyr::select(-1) %>% as.matrix() %>% typeof -> output_mode


  }
  else{
    sumr -> sumr1
  }

  sumr1 %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    dplyr::rename(column = rowname) -> sumr2

  if (!missing(..1)) {
    sumr2 %>%
      janitor::row_to_names(row_number = 1) %>%
      dplyr::mutate(dplyr::across(-1, ~as(., output_mode)))-> sumr3
  }
  else{
    sumr2 -> sumr3
  }

  sumr3

  }
