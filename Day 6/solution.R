library(data.table)

# Part 1
input <- data.table::fread("Day 6/input.txt"
                           , header = FALSE
                           , sep = "\n")

input_string <- paste(unlist(input), collapse =" ")
param <- data.table::data.table(string = unlist(strsplit(input_string, split = "  ")))

splits <- max(lengths(strsplit(param$string, " ")))

param[, paste0("person", 1:splits) := data.table::tstrsplit(string, " ", fixed = TRUE)]
param[, a_check := ifelse(grepl("a", person1) | grepl("a", person2) | grepl("a", person3) | grepl("a", person4) | grepl("a", person5), 1, 0)]
param[, b_check := ifelse(grepl("b", person1) | grepl("b", person2) | grepl("b", person3) | grepl("b", person4) | grepl("b", person5), 1, 0)]
param[, c_check := ifelse(grepl("c", person1) | grepl("c", person2) | grepl("c", person3) | grepl("c", person4) | grepl("c", person5), 1, 0)]
param[, d_check := ifelse(grepl("d", person1) | grepl("d", person2) | grepl("d", person3) | grepl("d", person4) | grepl("d", person5), 1, 0)]
param[, e_check := ifelse(grepl("e", person1) | grepl("e", person2) | grepl("e", person3) | grepl("e", person4) | grepl("e", person5), 1, 0)]
param[, f_check := ifelse(grepl("f", person1) | grepl("f", person2) | grepl("f", person3) | grepl("f", person4) | grepl("f", person5), 1, 0)]
param[, g_check := ifelse(grepl("g", person1) | grepl("g", person2) | grepl("g", person3) | grepl("g", person4) | grepl("g", person5), 1, 0)]
param[, h_check := ifelse(grepl("h", person1) | grepl("h", person2) | grepl("h", person3) | grepl("h", person4) | grepl("h", person5), 1, 0)]
param[, i_check := ifelse(grepl("i", person1) | grepl("i", person2) | grepl("i", person3) | grepl("i", person4) | grepl("i", person5), 1, 0)]
param[, j_check := ifelse(grepl("j", person1) | grepl("j", person2) | grepl("j", person3) | grepl("j", person4) | grepl("j", person5), 1, 0)]
param[, k_check := ifelse(grepl("k", person1) | grepl("k", person2) | grepl("k", person3) | grepl("k", person4) | grepl("k", person5), 1, 0)]
param[, l_check := ifelse(grepl("l", person1) | grepl("l", person2) | grepl("l", person3) | grepl("l", person4) | grepl("l", person5), 1, 0)]
param[, m_check := ifelse(grepl("m", person1) | grepl("m", person2) | grepl("m", person3) | grepl("m", person4) | grepl("m", person5), 1, 0)]
param[, n_check := ifelse(grepl("n", person1) | grepl("n", person2) | grepl("n", person3) | grepl("n", person4) | grepl("n", person5), 1, 0)]
param[, o_check := ifelse(grepl("o", person1) | grepl("o", person2) | grepl("o", person3) | grepl("o", person4) | grepl("o", person5), 1, 0)]
param[, p_check := ifelse(grepl("p", person1) | grepl("p", person2) | grepl("p", person3) | grepl("p", person4) | grepl("p", person5), 1, 0)]
param[, q_check := ifelse(grepl("q", person1) | grepl("q", person2) | grepl("q", person3) | grepl("q", person4) | grepl("q", person5), 1, 0)]
param[, r_check := ifelse(grepl("r", person1) | grepl("r", person2) | grepl("r", person3) | grepl("r", person4) | grepl("r", person5), 1, 0)]
param[, s_check := ifelse(grepl("s", person1) | grepl("s", person2) | grepl("s", person3) | grepl("s", person4) | grepl("s", person5), 1, 0)]
param[, t_check := ifelse(grepl("t", person1) | grepl("t", person2) | grepl("t", person3) | grepl("t", person4) | grepl("t", person5), 1, 0)]
param[, u_check := ifelse(grepl("u", person1) | grepl("u", person2) | grepl("u", person3) | grepl("u", person4) | grepl("u", person5), 1, 0)]
param[, v_check := ifelse(grepl("v", person1) | grepl("v", person2) | grepl("v", person3) | grepl("v", person4) | grepl("v", person5), 1, 0)]
param[, w_check := ifelse(grepl("w", person1) | grepl("w", person2) | grepl("w", person3) | grepl("w", person4) | grepl("w", person5), 1, 0)]
param[, x_check := ifelse(grepl("x", person1) | grepl("x", person2) | grepl("x", person3) | grepl("x", person4) | grepl("x", person5), 1, 0)]
param[, y_check := ifelse(grepl("y", person1) | grepl("y", person2) | grepl("y", person3) | grepl("y", person4) | grepl("y", person5), 1, 0)]
param[, z_check := ifelse(grepl("z", person1) | grepl("z", person2) | grepl("z", person3) | grepl("z", person4) | grepl("z", person5), 1, 0)]

sd_cols <- colnames(param[, c(7:32)])

param[, total_answered := sum(.SD, na.rm = TRUE), .SDcols = sd_cols, by = 1:nrow(param)]

solution <- sum(param$total_answered)

# Part 2
library(janitor)

input <- data.table::fread("Day 6/input.txt"
                           , header = FALSE
                           , sep = "\n")

input_string <- paste(unlist(input), collapse =" ")
param <- data.table::data.table(string = unlist(strsplit(input_string, split = "  ")))

splits <- max(lengths(strsplit(param$string, " ")))

param[, paste0("person", 1:splits) := data.table::tstrsplit(string, " ", fixed = TRUE)]

param[, a_check := ifelse(is.na(person2), ifelse(grepl("a", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("a", person1) & grepl("a", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("a", person1) & grepl("a", person2) & grepl("a", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("a", person1) & grepl("a", person2) & grepl("a", person3) & grepl("a", person4), 1, 0), ifelse(grepl("a", person1) & grepl("a", person2) & grepl("a", person3) & grepl("a", person4) & grepl("a", person5), 1, 0)))))]
param[, b_check := ifelse(is.na(person2), ifelse(grepl("b", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("b", person1) & grepl("b", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("b", person1) & grepl("b", person2) & grepl("b", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("b", person1) & grepl("b", person2) & grepl("b", person3) & grepl("b", person4), 1, 0), ifelse(grepl("b", person1) & grepl("b", person2) & grepl("b", person3) & grepl("b", person4) & grepl("b", person5), 1, 0)))))]
param[, c_check := ifelse(is.na(person2), ifelse(grepl("c", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("c", person1) & grepl("c", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("c", person1) & grepl("c", person2) & grepl("c", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("c", person1) & grepl("c", person2) & grepl("c", person3) & grepl("c", person4), 1, 0), ifelse(grepl("c", person1) & grepl("c", person2) & grepl("c", person3) & grepl("c", person4) & grepl("c", person5), 1, 0)))))]
param[, d_check := ifelse(is.na(person2), ifelse(grepl("d", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("d", person1) & grepl("d", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("d", person1) & grepl("d", person2) & grepl("d", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("d", person1) & grepl("d", person2) & grepl("d", person3) & grepl("d", person4), 1, 0), ifelse(grepl("d", person1) & grepl("d", person2) & grepl("d", person3) & grepl("d", person4) & grepl("d", person5), 1, 0)))))]
param[, e_check := ifelse(is.na(person2), ifelse(grepl("e", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("e", person1) & grepl("e", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("e", person1) & grepl("e", person2) & grepl("e", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("e", person1) & grepl("e", person2) & grepl("e", person3) & grepl("e", person4), 1, 0), ifelse(grepl("e", person1) & grepl("e", person2) & grepl("e", person3) & grepl("e", person4) & grepl("e", person5), 1, 0)))))]
param[, f_check := ifelse(is.na(person2), ifelse(grepl("f", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("f", person1) & grepl("f", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("f", person1) & grepl("f", person2) & grepl("f", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("f", person1) & grepl("f", person2) & grepl("f", person3) & grepl("f", person4), 1, 0), ifelse(grepl("f", person1) & grepl("f", person2) & grepl("f", person3) & grepl("f", person4) & grepl("f", person5), 1, 0)))))]
param[, g_check := ifelse(is.na(person2), ifelse(grepl("g", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("g", person1) & grepl("g", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("g", person1) & grepl("g", person2) & grepl("g", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("g", person1) & grepl("g", person2) & grepl("g", person3) & grepl("g", person4), 1, 0), ifelse(grepl("g", person1) & grepl("g", person2) & grepl("g", person3) & grepl("g", person4) & grepl("g", person5), 1, 0)))))]
param[, h_check := ifelse(is.na(person2), ifelse(grepl("h", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("h", person1) & grepl("h", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("h", person1) & grepl("h", person2) & grepl("h", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("h", person1) & grepl("h", person2) & grepl("h", person3) & grepl("h", person4), 1, 0), ifelse(grepl("h", person1) & grepl("h", person2) & grepl("h", person3) & grepl("h", person4) & grepl("h", person5), 1, 0)))))]
param[, i_check := ifelse(is.na(person2), ifelse(grepl("i", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("i", person1) & grepl("i", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("i", person1) & grepl("i", person2) & grepl("i", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("i", person1) & grepl("i", person2) & grepl("i", person3) & grepl("i", person4), 1, 0), ifelse(grepl("i", person1) & grepl("i", person2) & grepl("i", person3) & grepl("i", person4) & grepl("i", person5), 1, 0)))))]
param[, j_check := ifelse(is.na(person2), ifelse(grepl("j", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("j", person1) & grepl("j", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("j", person1) & grepl("j", person2) & grepl("j", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("j", person1) & grepl("j", person2) & grepl("j", person3) & grepl("j", person4), 1, 0), ifelse(grepl("j", person1) & grepl("j", person2) & grepl("j", person3) & grepl("j", person4) & grepl("j", person5), 1, 0)))))]
param[, k_check := ifelse(is.na(person2), ifelse(grepl("k", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("k", person1) & grepl("k", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("k", person1) & grepl("k", person2) & grepl("k", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("k", person1) & grepl("k", person2) & grepl("k", person3) & grepl("k", person4), 1, 0), ifelse(grepl("k", person1) & grepl("k", person2) & grepl("k", person3) & grepl("k", person4) & grepl("k", person5), 1, 0)))))]
param[, l_check := ifelse(is.na(person2), ifelse(grepl("l", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("l", person1) & grepl("l", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("l", person1) & grepl("l", person2) & grepl("l", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("l", person1) & grepl("l", person2) & grepl("l", person3) & grepl("l", person4), 1, 0), ifelse(grepl("l", person1) & grepl("l", person2) & grepl("l", person3) & grepl("l", person4) & grepl("l", person5), 1, 0)))))]
param[, m_check := ifelse(is.na(person2), ifelse(grepl("m", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("m", person1) & grepl("m", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("m", person1) & grepl("m", person2) & grepl("m", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("m", person1) & grepl("m", person2) & grepl("m", person3) & grepl("m", person4), 1, 0), ifelse(grepl("m", person1) & grepl("m", person2) & grepl("m", person3) & grepl("m", person4) & grepl("m", person5), 1, 0)))))]
param[, n_check := ifelse(is.na(person2), ifelse(grepl("n", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("n", person1) & grepl("n", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("n", person1) & grepl("n", person2) & grepl("n", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("n", person1) & grepl("n", person2) & grepl("n", person3) & grepl("n", person4), 1, 0), ifelse(grepl("n", person1) & grepl("n", person2) & grepl("n", person3) & grepl("n", person4) & grepl("n", person5), 1, 0)))))]
param[, o_check := ifelse(is.na(person2), ifelse(grepl("o", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("o", person1) & grepl("o", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("o", person1) & grepl("o", person2) & grepl("o", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("o", person1) & grepl("o", person2) & grepl("o", person3) & grepl("o", person4), 1, 0), ifelse(grepl("o", person1) & grepl("o", person2) & grepl("o", person3) & grepl("o", person4) & grepl("o", person5), 1, 0)))))]
param[, p_check := ifelse(is.na(person2), ifelse(grepl("p", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("p", person1) & grepl("p", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("p", person1) & grepl("p", person2) & grepl("p", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("p", person1) & grepl("p", person2) & grepl("p", person3) & grepl("p", person4), 1, 0), ifelse(grepl("p", person1) & grepl("p", person2) & grepl("p", person3) & grepl("p", person4) & grepl("p", person5), 1, 0)))))]
param[, q_check := ifelse(is.na(person2), ifelse(grepl("q", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("q", person1) & grepl("q", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("q", person1) & grepl("q", person2) & grepl("q", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("q", person1) & grepl("q", person2) & grepl("q", person3) & grepl("q", person4), 1, 0), ifelse(grepl("q", person1) & grepl("q", person2) & grepl("q", person3) & grepl("q", person4) & grepl("q", person5), 1, 0)))))]
param[, r_check := ifelse(is.na(person2), ifelse(grepl("r", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("r", person1) & grepl("r", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("r", person1) & grepl("r", person2) & grepl("r", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("r", person1) & grepl("r", person2) & grepl("r", person3) & grepl("r", person4), 1, 0), ifelse(grepl("r", person1) & grepl("r", person2) & grepl("r", person3) & grepl("r", person4) & grepl("r", person5), 1, 0)))))]
param[, s_check := ifelse(is.na(person2), ifelse(grepl("s", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("s", person1) & grepl("s", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("s", person1) & grepl("s", person2) & grepl("s", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("s", person1) & grepl("s", person2) & grepl("s", person3) & grepl("s", person4), 1, 0), ifelse(grepl("s", person1) & grepl("s", person2) & grepl("s", person3) & grepl("s", person4) & grepl("s", person5), 1, 0)))))]
param[, t_check := ifelse(is.na(person2), ifelse(grepl("t", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("t", person1) & grepl("t", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("t", person1) & grepl("t", person2) & grepl("t", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("t", person1) & grepl("t", person2) & grepl("t", person3) & grepl("t", person4), 1, 0), ifelse(grepl("t", person1) & grepl("t", person2) & grepl("t", person3) & grepl("t", person4) & grepl("t", person5), 1, 0)))))]
param[, u_check := ifelse(is.na(person2), ifelse(grepl("u", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("u", person1) & grepl("u", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("u", person1) & grepl("u", person2) & grepl("u", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("u", person1) & grepl("u", person2) & grepl("u", person3) & grepl("u", person4), 1, 0), ifelse(grepl("u", person1) & grepl("u", person2) & grepl("u", person3) & grepl("u", person4) & grepl("u", person5), 1, 0)))))]
param[, v_check := ifelse(is.na(person2), ifelse(grepl("v", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("v", person1) & grepl("v", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("v", person1) & grepl("v", person2) & grepl("v", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("v", person1) & grepl("v", person2) & grepl("v", person3) & grepl("v", person4), 1, 0), ifelse(grepl("v", person1) & grepl("v", person2) & grepl("v", person3) & grepl("v", person4) & grepl("v", person5), 1, 0)))))]
param[, w_check := ifelse(is.na(person2), ifelse(grepl("w", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("w", person1) & grepl("w", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("w", person1) & grepl("w", person2) & grepl("w", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("w", person1) & grepl("w", person2) & grepl("w", person3) & grepl("w", person4), 1, 0), ifelse(grepl("w", person1) & grepl("w", person2) & grepl("w", person3) & grepl("w", person4) & grepl("w", person5), 1, 0)))))]
param[, x_check := ifelse(is.na(person2), ifelse(grepl("x", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("x", person1) & grepl("x", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("x", person1) & grepl("x", person2) & grepl("x", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("x", person1) & grepl("x", person2) & grepl("x", person3) & grepl("x", person4), 1, 0), ifelse(grepl("x", person1) & grepl("x", person2) & grepl("x", person3) & grepl("x", person4) & grepl("x", person5), 1, 0)))))]
param[, y_check := ifelse(is.na(person2), ifelse(grepl("y", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("y", person1) & grepl("y", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("y", person1) & grepl("y", person2) & grepl("y", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("y", person1) & grepl("y", person2) & grepl("y", person3) & grepl("y", person4), 1, 0), ifelse(grepl("y", person1) & grepl("y", person2) & grepl("y", person3) & grepl("y", person4) & grepl("y", person5), 1, 0)))))]
param[, z_check := ifelse(is.na(person2), ifelse(grepl("z", person1), 1, 0), ifelse(is.na(person3), ifelse(grepl("z", person1) & grepl("z", person2), 1, 0), ifelse(is.na(person4), ifelse(grepl("z", person1) & grepl("z", person2) & grepl("z", person3), 1, 0), ifelse(is.na(person5), ifelse(grepl("z", person1) & grepl("z", person2) & grepl("z", person3) & grepl("z", person4), 1, 0), ifelse(grepl("z", person1) & grepl("z", person2) & grepl("z", person3) & grepl("z", person4) & grepl("z", person5), 1, 0)))))]

sd_cols <- colnames(param[, c(7:32)])

param[, total_answered := sum(.SD, na.rm = TRUE), .SDcols = sd_cols, by = 1:nrow(param)]

solution <- sum(param$total_answered)
