
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


Checkouts_per_year <- filtered_spl_df %>% 
  group_by(CheckoutYear, Creator) %>%
  summarize(Total_checkouts = sum(Checkouts))


ggplot(data = Checkouts_per_year) +
  geom_point(
    mapping = aes(x = CheckoutYear, y = Total_checkouts, color = Creator)
  ) +
  geom_line(
    mapping = aes(x = CheckoutYear, y = Total_checkouts, color = Creator)
  ) +
  labs( title = "Author's Novel Checkouts Per Year",
        subtitle = "2017-2023",
        caption = "Line graph of each author's romance novel checkouts per year",
        x = "Year",
        y = "Total Checkouts")
