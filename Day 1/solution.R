library(data.table)

input <- data.table::fread("Day 1/input.csv"
                           , header = FALSE)
# Part 1

param <- input[
  
  ,
  j = .(value = V1,
        remain = 2020 - V1)
  
]

param[, check := ifelse(remain %in% value, TRUE, FALSE)]
solution <- param[check == TRUE][1, solution := value * remain]

# Part 2
values <- as.vector(input$V1)

differences1 <- as.matrix(outer(values, values, `+`))
differences2 <- 2020 - differences1

matches <- which(matrix(differences2 %in% values, dim(differences2)), arr.ind = TRUE)

pot_match1 <- as.numeric(matches[1, 1])
pot_match2 <- as.numeric(matches[2, 1])
pot_match3 <- as.numeric(matches[3, 1])

match_value1 <- values[pot_match1]
match_value2 <- values[pot_match2]
match_value3 <- values[pot_match3]

solution <- match_value1*match_value2*match_value3
