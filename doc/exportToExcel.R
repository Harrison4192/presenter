## ----setup, message=F, warning=F----------------------------------------------
library(presenter)
library(dplyr)

## ----eval=FALSE---------------------------------------------------------------
#  make_excel(df = iris,
#             header_word = c("Sepal", "Petal"),
#             last_id_col = NULL)
#  

## ----eval = FALSE-------------------------------------------------------------
#  iris %>%
#    make_simple_excel()

## ----eval=F-------------------------------------------------------------------
#  iris %>%
#    group_by(Species) %>%
#    summarize(across(where(is.numeric), sum)) -> iris_summary
#  
#  list(iris, iris_summary) -> iris_list
#  
#  iris_list %>%
#    make_simple_excel()

## ----eval=F-------------------------------------------------------------------
#  make_excel_wb(wb = NULL,
#                object = iris,
#                last_id_col = NULL,
#                header_word = c("Sepal", "Petal")) %>%
#    make_excel_wb(object = anscombe,
#                  last_id_col = NULL,
#                  header_word = NULL) %>%
#    finish_excel_wb(wb_name = "data_workbook")

