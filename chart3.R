# import packages
library("dplyr")
library("ggplot2")
library("scales")
library("stringr")


# load csv
checkouts_data <- read.csv("E:/Study/2023WI/Info201/2017-2023-10-Checkouts-SPL-Data.csv", 
                           stringsAsFactors = FALSE)

# This chart should reflect the trend of number of checkouts in different 
# UsageClass (physical or digital) by months (since 2023 only has one month)

# First add a column of date to distinguish year and month together
checkouts_data <- checkouts_data %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
checkouts_data$date <- as.Date(checkouts_data$date, format = "%Y-%m-%d")

# Create a data frame of total checkout number of each month
by_month <- checkouts_data %>% 
  group_by(date) %>% 
  summarize(total_checkout = sum(Checkouts))
