## ----setup, warning=F, message=F----------------------------------------------
library(presenter)
library(dplyr)

## -----------------------------------------------------------------------------
set.seed(1)

iris %>% 
  relocate("Species") %>% 
  sample_n(10) %>% 
  arrange(Species) -> iris_slice

header_words <- c("Sepal", "Petal")
last_id_col <- "Species"

make_flextable(iris_slice, header_words = header_words, last_id_col = last_id_col) -> myflex

myflex

## -----------------------------------------------------------------------------
tibble::tibble(my_letters = sample(letters[1:4], 100, T),
       my_numbers = sample(1:4, 100, T)) -> cross_table

cross_table %>% 
  make_pivot_table(my_letters, my_numbers, theme = "tron") -> tron_cross_table

tron_cross_table

## -----------------------------------------------------------------------------
iris %>% 
  dplyr::mutate(Species1 = stringr::str_c(Species, " very good")) %>% 
  make_pivot_table(Species1, Species, show_percentages = "none", tbl_nm = "gold table", theme = "zebra_gold") -> tbl 

tbl

## ----eval=F-------------------------------------------------------------------
#  
#  myflex %>%
#    export_powerpoint() # a new ppt is created, named after the table
#  
#  tron_cross_table %>%
#    export_powerpoint("myflex.pptx") # append this slide to the previous ppt
#  

