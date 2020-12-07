library(data.table)

# Part 1
input <- data.table::fread("Day 5//input.csv"
                           , header = FALSE)

row_column <- function(x) {
  
  step_row <- 0:127
  step_col <- 0:7
  
  ifelse(substr(x, 1, 1) == "F"
         , step_row <- step_row[1:(length(step_row) / 2)]
         , step_row <- step_row[((length(step_row) / 2) + 1):length(step_row)])
  
  ifelse(substr(x, 2, 2) == "F"
         , step_row <- step_row[1:(length(step_row) / 2)]
         , step_row <- step_row[((length(step_row) / 2) + 1):length(step_row)])
  
  ifelse(substr(x, 3, 3) == "F"
         , step_row <- step_row[1:(length(step_row) / 2)]
         , step_row <- step_row[((length(step_row) / 2) + 1):length(step_row)])
  
  ifelse(substr(x, 4, 4) == "F"
         , step_row <- step_row[1:(length(step_row) / 2)]
         , step_row <- step_row[((length(step_row) / 2) + 1):length(step_row)])
  
  ifelse(substr(x, 5, 5) == "F"
         , step_row <- step_row[1:(length(step_row) / 2)]
         , step_row <- step_row[((length(step_row) / 2) + 1):length(step_row)])
  
  ifelse(substr(x, 6, 6) == "F"
         , step_row <- step_row[1:(length(step_row) / 2)]
         , step_row <- step_row[((length(step_row) / 2) + 1):length(step_row)])
  
  ifelse(substr(x, 7, 7) == "F"
         , step_row <- step_row[1:(length(step_row) / 2)]
         , step_row <- step_row[((length(step_row) / 2) + 1):length(step_row)])
  
  ifelse(substr(x, 8, 8) == "L"
         , step_col <- step_col[1:(length(step_col) / 2)]
         , step_col <- step_col[((length(step_col) / 2) + 1):length(step_col)])
  
  ifelse(substr(x, 9, 9) == "L"
         , step_col <- step_col[1:(length(step_col) / 2)]
         , step_col <- step_col[((length(step_col) / 2) + 1):length(step_col)])
  
  ifelse(substr(x, 10, 10) == "L"
         , step_col <- step_col[1:(length(step_col) / 2)]
         , step_col <- step_col[((length(step_col) / 2) + 1):length(step_col)])
  
  unique_id <- (step_row * 8) + step_col
  return(unique_id)
  
}

solution_df <- data.table::data.table(input = input$V1
                                   , uniqe_id =sapply(input$V1, row_column))
solution <- max(solution_df$uniqe_id)

# Part 2
seat_range <- min(solution_df$uniqe_id):max(solution_df$uniqe_id)
solution <- setdiff(seat_range, solution_df$uniqe_id)
