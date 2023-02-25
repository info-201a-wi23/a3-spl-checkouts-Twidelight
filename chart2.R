# import packages
library("dplyr")
library("ggplot2")
library("scales")
library("stringr")


# load csv
checkouts_data <- read.csv("E:/Study/2023WI/Info201/2017-2023-10-Checkouts-SPL-Data.csv", 
                           stringsAsFactors = FALSE)

# First add a column of date to distinguish year and month together
checkouts_data <- checkouts_data %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
checkouts_data$date <- as.Date(checkouts_data$date, format = "%Y-%m-%d")

# Make a dataframe for each of the two authors I am interested in
hunter <- checkouts_data %>%
  filter(Creator %in% c("Erin Hunter", "Hunter, Erin")) %>% 
  group_by(date) %>% 
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  mutate(author = "Erin Hunter")

Rowling <- checkouts_data %>% 
  filter(Creator %in% c("J. K. Rowling", "Rowling, J. K.")) %>% 
  group_by(date) %>% 
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  mutate(author = "J. K. Rowling")

# merge them together into one dataframe
by_author <- rbind(hunter, Rowling)

# draw the chart
ggplot(data = by_author) +
  geom_line(aes(x = date, y = total_checkouts, color = author), size = 1.2) +
  scale_color_brewer(palette = "Paired") +
  labs(title = "Total checkouts of book by Erin Hunter and J. K. Rowling from Seattle Public Library in 2017-2023", 
       x = "Year", 
       y = "Number of Checkouts") +
  guides(color = guide_legend(title = "Author")) +
  scale_y_continuous(labels = label_number_si())
