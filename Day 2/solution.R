library(data.table)
library(stringi)
library(stringr)

input <- data.table::fread("Day 2/input.csv"
                           , header = FALSE)

param <- input[
  
  j = .(value_min = as.numeric(as.character(stringr::str_extract(V1, '[^-]+'))),
        value_max = as.numeric(as.character(stringr::str_extract(V1, '\\b\\w+$'))),
        value = gsub(":", "", V2),
        string = V3)
  
]

output <- param[, count := stringr::str_count(string = string, pattern = as.character(value))][, check := ifelse(count >= value_min & count <= value_max, 1, 0)]
sum(output$check)