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

Audio_and_ebooks <- filtered_spl_df %>% 
  group_by(CheckoutYear, MaterialType) %>%
  summarize(Total_checkouts = sum(Checkouts))

ggplot(data = Audio_and_ebooks) +
  geom_point(
    mapping = aes(x = CheckoutYear, y = Total_checkouts, color = MaterialType)
  ) +
  geom_line(
    mapping = aes(x = CheckoutYear, y = Total_checkouts, color = MaterialType)
  ) +
  labs( title = "Audio vs Ebook Checkouts Per Year",
        subtitle = "2017-2023",
        caption = "Line graph of each material type checkouts per year",
        x = "Year",
        y = "Total Checkouts")