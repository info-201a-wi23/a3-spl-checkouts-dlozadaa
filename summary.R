library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)

spl_df <- read.csv("C:\\Users\\dhang\\Desktop\\info201\\2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>% mutate("date" = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

filtered_spl_df <- spl_df %>%
  filter(Creator %in% c("Audrey Niffenegger", "André Aciman", "John Green", "Nicola Yoon")) %>%
  filter(str_detect(Title, "Your Name|Everything|Our Stars|The Time Traveler's Wife"))

# Total number of checkouts for all books

Total_num_books <- sum(filtered_spl_df$Checkouts)

# The most popular book & author and number of checkouts

Most_popular <- filtered_spl_df %>%
  group_by(Creator) %>%
  summarize(Total_checkouts = sum(Checkouts)) %>%
  filter(Total_checkouts == max(Total_checkouts)) %>%
  pull(Creator)

Popular_book_checkouts <- filtered_spl_df %>%
  group_by(Creator) %>%
  summarize(Total_checkouts = sum(Checkouts)) %>%
  filter(Total_checkouts == max(Total_checkouts)) %>%
  pull(Total_checkouts)

# Least popular book & author and number of checkouts

Least_popular <- filtered_spl_df %>%
  group_by(Creator) %>%
  summarize(Total_checkouts = sum(Checkouts)) %>%
  filter(Total_checkouts == min(Total_checkouts)) %>%
  pull(Creator)

Least_popular_checkouts <- filtered_spl_df %>%
  group_by(Creator) %>%
  summarize(Total_checkouts = sum(Checkouts)) %>%
  filter(Total_checkouts == min(Total_checkouts)) %>%
  pull(Total_checkouts)

#- Year with the most & least checkouts 

Max_year <- filtered_spl_df %>%
  group_by(CheckoutYear) %>%
  summarize(year_checkouts = sum(Checkouts)) %>%
  filter(year_checkouts == max(year_checkouts)) %>%
  pull(CheckoutYear)

Min_year <- filtered_spl_df %>%
  group_by(CheckoutYear) %>%
  summarize(year_checkouts = sum(Checkouts)) %>%
  filter(year_checkouts == min(year_checkouts)) %>%
  pull(CheckoutYear)

# list
sum_info <- list()
sum_info$Total_num_books <- Total_num_books
sum_info$Most_popular <- Most_popular
sum_info$Popular_book_checkouts <- Popular_book_checkouts
sum_info$Least_popular <- Least_popular
sum_info$Least_popular_checkouts <- Least_popular_checkouts
sum_info$Max_year <- Max_year
sum_info$Min_year <- Min_year

  