library(data.table)
library(stringi)
library(stringr)

input <- data.table::fread("Day 2/input.csv"
                           , header = FALSE)

# Part 1

param <- input[
  
  j = .(value_min = as.numeric(as.character(stringr::str_extract(V1, '[^-]+'))),
        value_max = as.numeric(as.character(stringr::str_extract(V1, '\\b\\w+$'))),
        value = gsub(":", "", V2),
        string = V3)
  
]

output <- param[, count := stringr::str_count(string = string, pattern = as.character(value))][, check := ifelse(count >= value_min & count <= value_max, 1, 0)]
sum(output$check)

# Part 2

param <- input[
  
  j = .(pos1 = as.numeric(as.character(stringr::str_extract(V1, '[^-]+'))),
        pos2 = as.numeric(as.character(stringr::str_extract(V1, '\\b\\w+$'))),
        char = gsub(":", "", V2),
        string = V3)
  
]

param[, pos1_char := substr(string, pos1, pos1)]
param[, pos2_char := substr(string, pos2, pos2)]
param[, pos1_match := ifelse(pos1_char == char, 1, 0)]
param[, pos2_match := ifelse(pos2_char == char, 1, 0)]
param[, total_matches := pos1_match + pos2_match]

solution <- param[total_matches == 1, .N]
