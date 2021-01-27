## ----setup, warning=F, message=F----------------------------------------------
library(dataExporter)
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

## ----eval=FALSE---------------------------------------------------------------
#  library(officer)
#  
#  
#  read_pptx("masterLayout.pptx") -> ppt1
#  
#  layout = "Two Content"
#  master = "Office Theme"
#  
#  
#  ppt1 %>%
#    add_slide(layout = layout,
#              master = master) %>%
#    ph_with(value = "Iris Slice", location = ph_location_type(type = "title")) %>%
#    ph_with(value = myflex, location = ph_location(left = 0, top = 1.5)) -> ppt2
#  
#  
#  
#  print(ppt2, target="irisSlice.pptx")
#  
#  file.show("irisSlice.pptx")

