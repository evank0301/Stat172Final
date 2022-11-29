#Installing packages as we need them
rm(list = ls())


#Read in the data and examine the columns
fp = read.csv("Data/pbp-2021.csv", stringsAsFactors = TRUE)
str(fp)

