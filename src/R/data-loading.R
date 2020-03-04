source("src/R/commun.R")

train <- fread(file = file.path(path_data, "train.csv"))
train

test <- fread(file = file.path(path_data, "test.csv"))
test

