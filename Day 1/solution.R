library(data.table)

input <- data.table::fread("Day 1/input.csv"
                           , header = FALSE)

param <- input[
  
  ,
  j = .(value = V1,
        remain = 2020 - V1)
  
]

param[, check := ifelse(remain %in% value, TRUE, FALSE)]
solution <- param[check == TRUE][1, solution := value * remain]
