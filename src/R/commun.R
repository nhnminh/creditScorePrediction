library(data.table)
library(stringr)

path_data <- file.path("data")
path_out <- file.path("out")


#Utilities functions ------ 


count_encoding <- function(dt, col){
  dt <- copy(train)
  col <- "FIELD_7"
  n_mod <- dt[, .N, keyby = .(values = get(col))]
  n_mod
  dt <- base::merge(dt, 
                    )
}