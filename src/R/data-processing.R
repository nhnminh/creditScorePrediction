source("src/R/data-loading.R")

# PART 0. EDA ------------------------------------------------------------------


train[, .N, keyby = .(label)]


train[is.na(age_source2)]

train[is.na(age_source2), lapply(.SD, function(x) sum(is.na(x))), .SDcols = 28:46]
train[is.na(age_source2), lapply(.SD, function(x) sum(is.na(x))), .SDcols = 28:47]



# PART 1 Feature engineer ------------------------------------------------------

lst_categorical <- c("province_cleaned", "disctrict_cleaned", "macv_cleaned_regroup",
                     'FIELD_7', 'FIELD_8', 'FIELD_9',
                     'FIELD_10', 'FIELD_13', 'FIELD_17', 
                     'FIELD_24', 'FIELD_35', 'FIELD_39', 
                     'FIELD_41', 'FIELD_42', 'FIELD_43', 
                     'FIELD_44')

lst_boolean <- c('FIELD_2', 'FIELD_18', 'FIELD_19', 
                 'FIELD_20', 'FIELD_23', 'FIELD_25', 
                 'FIELD_26', 'FIELD_27', 'FIELD_28', 
                 'FIELD_29', 'FIELD_30', 'FIELD_31', 
                 'FIELD_36', 'FIELD_37', 'FIELD_38', 
                 'FIELD_47', 'FIELD_48', 'FIELD_49')
lst_num <- setdiff(names(train), c(lst_categorical, lst_boolean, "id", "label", "province", "district", "maCV"))


# 1. Remove accents ------------------------------------------------------------

train[, maCv_cleaned := tolower(stringi::stri_trans_general(maCv,"Latin-ASCII"))]

train[, province_cleaned := stringi::stri_trans_general(province,"Latin-ASCII")]
train[, district_cleaned := stringi::stri_trans_general(district,"Latin-ASCII")]
train[,.N, keyby = .(maCv_cleaned)]
train[,.N, keyby = .(province_cleaned)]
train[,.N, keyby = .(district_cleaned)]



# 2. Standardize categorical labels -------------------------------------------


# ++ 2.1 Regroup the macv ---- 

train[, macv_cleaned_regroup := NULL]
train[grepl(maCv_cleaned, pattern = "cong n"), macv_cleaned_regroup := "cong nhan"]

train[grepl(maCv_cleaned, pattern = "cn") |
        grepl(maCv_cleaned, pattern = "tho ")|
        grepl(maCv_cleaned, pattern = "van hanh"), macv_cleaned_regroup := "cong nhan"]

train[grepl(maCv_cleaned, pattern = "cong")]
train[grepl(maCv_cleaned, pattern = "cong nhan"), .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)]

train[grepl(maCv_cleaned, pattern = "nhan vien") |
        grepl(maCv_cleaned, pattern = "nv"), macv_cleaned_regroup := "nhan vien"]
train[grepl(maCv_cleaned, pattern = "nhan vien ")|
        grepl(maCv_cleaned, pattern = "nv"), .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)]


train[grepl(maCv_cleaned, pattern = "ky su"), macv_cleaned_regroup := "ky su"]
train[grepl(maCv_cleaned, pattern = "ky su"), .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)]


train[grepl(maCv_cleaned, pattern = "ky thuat"), macv_cleaned_regroup := "ky thuat vien"]
train[grepl(maCv_cleaned, pattern = "ky thuat"), .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)]


train[grepl(maCv_cleaned, pattern = "lai xe") , macv_cleaned_regroup := "lai xe"]
train[grepl(maCv_cleaned, pattern = "lai xe") , .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)]

train[grepl(maCv_cleaned, pattern = "lao dong")|
        grepl(maCv_cleaned, pattern = "ldpt") |
        grepl(maCv_cleaned, pattern = "ld"), macv_cleaned_regroup := "lao dong pho thong"]
train[grepl(maCv_cleaned, pattern = "lao dong")|
        grepl(maCv_cleaned, pattern = "ldpt") |
        grepl(maCv_cleaned, pattern = "ld"), .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)]

train[grepl(maCv_cleaned, pattern = "giao v") , macv_cleaned_regroup := "giao vien"]
train[grepl(maCv_cleaned, pattern = "giao v"), .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)]
# 
# View(train[grepl(maCv_cleaned, pattern = "giam d"), .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)])
# View(train[grepl(maCv_cleaned, pattern = "y") & (macv_cleaned_regroup != "cong nhan" | is.na(macv_cleaned_regroup)) , .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)])
# View(train[grepl(maCv_cleaned, pattern = "tho ") & (macv_cleaned_regroup != "cong nhan" | is.na(macv_cleaned_regroup)) , .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)])
# View(train[grepl(maCv_cleaned, pattern = "^pho\\s"), .N, keyby = .(maCv, maCv_cleaned, macv_cleaned_regroup)])

train[grepl(maCv_cleaned, pattern = "^pho\\s") |
        grepl(maCv_cleaned, pattern = "giam d")|
        grepl(maCv_cleaned, pattern = "to truong"), macv_cleaned_regroup := "quan ly"]

train[grepl(maCv_cleaned, pattern = "chi huy"), macv_cleaned_regroup := "quan su"]

train[grepl(maCv_cleaned, pattern = "y t") |
        grepl(maCv_cleaned, pattern = "y s")|
        grepl(maCv_cleaned, pattern = "bac s"), macv_cleaned_regroup := "bac sy - y ta"]


train[is.na(macv_cleaned_regroup) & !(maCv %in% c("None", "")), macv_cleaned_regroup := "nganh nghe khac"]
train[is.na(macv_cleaned_regroup) & (maCv %in% c("None", "")), macv_cleaned_regroup := "Missing Value"]


train[, .N, keyby = .(macv_cleaned_regroup)]s
train[, .N, keyby = .(macv_cleaned_regroup, maCv)]


# ++ 