

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

Checkouts_during_20192020 <- filtered_spl_df %>% 
   filter(CheckoutYear == 2019|2020) %>%
    group_by(CheckoutMonth, Creator) %>%
    summarize(Total_checkouts = sum(Checkouts))


ggplot(data = Checkouts_during_20192020) +
  geom_col(
    mapping = aes(x = CheckoutMonth, y = Total_checkouts, fill = Creator)
  ) +
  labs( title = "Author's Novel Checkouts Per Month",
        subtitle = "2019-2020",
        caption = "Line graph of each author's romance novel total checkouts per month",
        x = "Month",
        y = "Total Checkouts") 