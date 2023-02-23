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

# Create a new data frame with only the total checkouts and type and date.
by_usage <- checkouts_data %>% 
  group_by(UsageClass, date) %>% 
  summarize(total_checkouts = sum(Checkouts))

# plot the line plot
ggplot(by_usage) +
  geom_line(aes(x = date, y = total_checkouts, color = UsageClass), size = 1.2) +
  scale_color_brewer(palette = "Accent") +
  labs(title = "Number of Checkouts in Digital and Physical Form", x = "Year", 
       y = "Number of Checkouts") +
  guides(color = guide_legend(title = "Checkout Form")) +
  scale_y_continuous(labels = label_number_si())
