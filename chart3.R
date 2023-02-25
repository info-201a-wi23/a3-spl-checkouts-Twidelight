# import packages
library("dplyr")
library("ggplot2")
library("ggrepel")
library("scales")
library("stringr")
library("tidyverse")

# load csv
checkouts_data <- read.csv("E:/Study/2023WI/Info201/2017-2023-10-Checkouts-SPL-Data.csv", 
                           stringsAsFactors = FALSE)


# Create a data frame of total checkout number of material type
by_material <- checkouts_data %>% 
  group_by(MaterialType) %>% 
  summarize(total_checkout = sum(Checkouts, na.rm = FALSE)) %>% 
  filter(total_checkout > 100000) %>% 
  mutate(percentage = round(100 * total_checkout / sum(total_checkout, 
                                                       na.rm = FALSE), 
                            digits = 1),
         csum = rev(cumsum(rev(total_checkout))),
         pos = total_checkout / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), total_checkout / 2, pos))



# draw a pie chart to see the main meterials people checked out from the library
ggplot(by_material, 
       aes(x = "", y = total_checkout, fill = fct_inorder(MaterialType))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = by_material, 
             aes(y = pos, label = paste0(percentage,"%")),
             size = 5,
             nudge_x = 1,
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Material")) +
  labs(title = "Total Checkouts by Material Type from Seattle Public Library
       (for Materials with Over 100k Checkouts) in 2017-2023") +
  theme_void()
