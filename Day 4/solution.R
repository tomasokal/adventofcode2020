library(data.table)
library(qdap)

# Part 1
input <- data.table::fread("Day 4/input.txt"
                           , header = FALSE
                           , sep = "\n")

input_string <- paste(unlist(input), collapse =" ")
input_df <- strsplit(input_string, split = "  ")

param <- data.table::data.table(string = unlist(strsplit(input_string, split = "  ")))

param[, byr := ifelse(grepl("byr:", string), 1, 0)]
param[, iyr := ifelse(grepl("iyr:", string), 1, 0)]
param[, eyr := ifelse(grepl("eyr:", string), 1, 0)]
param[, hgt := ifelse(grepl("hgt:", string), 1, 0)]
param[, hcl := ifelse(grepl("hcl:", string), 1, 0)]
param[, ecl := ifelse(grepl("ecl:", string), 1, 0)]
param[, pid := ifelse(grepl("pid:", string), 1, 0)]
param[, check := byr + iyr + eyr + hgt + hcl + ecl + pid]

solution <- nrow(param[check == 7, ])

# Part 2

input_2 <- param[
  
  i = check == 7
  , 
  j = .(string)
  
  ]

input_2[, byr_val := substr(sub(".*byr:", "", string), 1, 4)]
input_2[, iyr_val := substr(sub(".*iyr:", "", string), 1, 4)]
input_2[, eyr_val := substr(sub(".*eyr:", "", string), 1, 4)]
input_2[, hgt_val := substr(sub(".*hgt:", "", string), 1, 5)]
input_2[, hgt_cm_val := ifelse(substr(hgt_val, 4, 5) == "cm", substr(hgt_val, 1, 3), 0)]
input_2[, hgt_in_val := ifelse(substr(hgt_val, 3, 4) == "in", substr(hgt_val, 1, 2), 0)]
input_2[, hcl_val := substr(sub(".*hcl:", "", string), 1, 7)]
input_2[, ecl_val := substr(sub(".*ecl:", "", string), 1, 3)]
input_2[, pid_val1 := sub(".*pid:", "", string)]
input_2[, pid_val2 := gsub(" .*$", "", pid_val1)]

input_2[, byr_check := ifelse(as.numeric(byr_val) > 1919 & as.numeric(byr_val) < 2003, 1, 0)]
input_2[, iyr_check := ifelse(as.numeric(iyr_val) > 2009 & as.numeric(iyr_val) < 2021, 1, 0)]
input_2[, eyr_check := ifelse(as.numeric(eyr_val) > 2019 & as.numeric(eyr_val) < 2031, 1, 0)]
input_2[, hgt_cm_check := ifelse(as.numeric(hgt_cm_val) > 149 & as.numeric(hgt_cm_val) < 194, 1, 0)]
input_2[, hgt_in_check := ifelse(as.numeric(hgt_in_val) > 58 & as.numeric(hgt_in_val) < 77, 1, 0)]
input_2[, hgt_check := ifelse(hgt_cm_check == 1 | hgt_in_check == 1, 1, 0)]
input_2[, hcl_check := ifelse(grepl('#[0-9a-f]{6}$', hcl_val), 1, 0)]
input_2[, ecl_check := ifelse(ecl_val %chin% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"), 1, 0)]
input_2[, pid_check := ifelse(grepl('^[0-9]{9}$', pid_val2), 1, 0)]

input_2[, check := byr_check + iyr_check + eyr_check + hgt_check + hcl_check + ecl_check + pid_check]
solution <- nrow(input_2[check == 7, ])
