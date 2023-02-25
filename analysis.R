library("dplyr")

# load csv
checkouts_data <- read.csv("E:/Study/2023WI/Info201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# create a list to store summary statistics
summary_info <- list()

# find the number of types of checkouts
summary_info$num_of_type <- checkouts_data %>% 
  summarize(total = n_distinct(MaterialType, na.rm = TRUE)) %>% 
  pull(total)

# find the most checked out type of material
summary_info$max_type_num <- checkouts_data %>% 
  group_by(MaterialType) %>% 
  summarize(total = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(total == max(total, na.rm = TRUE)) %>% 
  pull(total)

# find the maximum number of check out of a single book in a month
summary_info$max_checkout <- checkouts_data %>% 
  summarize(highest = max(Checkouts, na.rm = TRUE)) %>% 
  pull(highest)

# find the year with the most checkouts
summary_info$max_year <- checkouts_data %>% 
  group_by(CheckoutYear) %>% 
  summarize(total = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(total == max(total, na.rm = TRUE)) %>% 
  pull(CheckoutYear)

# find whether people check out more physical or digital copies
summary_info$usage_class <- checkouts_data %>% 
  group_by(UsageClass) %>% 
  summarize(total = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(total == max(total, na.rm = TRUE)) %>% 
  pull(total)
