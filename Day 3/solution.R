library(data.table)

# Part 1
input <- data.table::fread("Day 3//input.csv"
                           , header = FALSE)

param <- data.table::data.table(index = seq(1:nrow(input)),
                                string = input)

param[, horizon_pos := 1 + (3 * (.I - 1))]
param[, horizon_ind := ifelse(horizon_pos < 32, horizon_pos, horizon_pos %% 31)]
param[, horizon_ind := ifelse(horizon_ind == 0, 31, horizon_ind)]
param[, tree_check := substr(string.V1, horizon_ind, horizon_ind)]
param[, tree_safe := ifelse(tree_check %in% "#", 1, 0)]

solution <- sum(param$tree_safe)

# Part 2

param <- data.table::data.table(index = seq(1:nrow(input)),
                                string = input)

param[, horizon_pos := 1 + (1 * (.I - 1))]
param[, horizon_ind := ifelse(horizon_pos < 32, horizon_pos, horizon_pos %% 31)]
param[, horizon_ind := ifelse(horizon_ind == 0, 31, horizon_ind)]
param[, tree_check := substr(string.V1, horizon_ind, horizon_ind)]
param[, tree_safe := ifelse(tree_check %in% "#", 1, 0)]

solution_1_1 <- sum(param$tree_safe)

param <- data.table::data.table(index = seq(1:nrow(input)),
                                string = input)

param[, horizon_pos := 1 + (3 * (.I - 1))]
param[, horizon_ind := ifelse(horizon_pos < 32, horizon_pos, horizon_pos %% 31)]
param[, horizon_ind := ifelse(horizon_ind == 0, 31, horizon_ind)]
param[, tree_check := substr(string.V1, horizon_ind, horizon_ind)]
param[, tree_safe := ifelse(tree_check %in% "#", 1, 0)]

solution_3_1 <- sum(param$tree_safe)

param <- data.table::data.table(index = seq(1:nrow(input)),
                                string = input)

param[, horizon_pos := 1 + (5 * (.I - 1))]
param[, horizon_ind := ifelse(horizon_pos < 32, horizon_pos, horizon_pos %% 31)]
param[, horizon_ind := ifelse(horizon_ind == 0, 31, horizon_ind)]
param[, tree_check := substr(string.V1, horizon_ind, horizon_ind)]
param[, tree_safe := ifelse(tree_check %in% "#", 1, 0)]

solution_5_1 <- sum(param$tree_safe)

param <- data.table::data.table(index = seq(1:nrow(input)),
                                string = input)

param[, horizon_pos := 1 + (7 * (.I - 1))]
param[, horizon_ind := ifelse(horizon_pos < 32, horizon_pos, horizon_pos %% 31)]
param[, horizon_ind := ifelse(horizon_ind == 0, 31, horizon_ind)]
param[, tree_check := substr(string.V1, horizon_ind, horizon_ind)]
param[, tree_safe := ifelse(tree_check %in% "#", 1, 0)]

solution_7_1 <- sum(param$tree_safe)

param <- data.table::data.table(index = seq(1:nrow(input)),
                                string = input)

param <- param[seq(1, nrow(param), 2), ]

param[, horizon_pos := 1 + (1 * (.I - 1))]
param[, horizon_ind := ifelse(horizon_pos < 32, horizon_pos, horizon_pos %% 31)]
param[, horizon_ind := ifelse(horizon_ind == 0, 31, horizon_ind)]
param[, tree_check := substr(string.V1, horizon_ind, horizon_ind)]
param[, tree_safe := ifelse(tree_check %in% "#", 1, 0)]

solution_1_2 <- sum(param$tree_safe)

solution <- solution_1_1 * solution_3_1 * solution_5_1 * solution_7_1 * solution_1_2

