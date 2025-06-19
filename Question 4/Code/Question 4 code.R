library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)


# Load the data
billionaires <- read_csv("Question 4/Data/Billions/billionaires.csv")

glimpse(billionaires)

#Cleaning and preparing the data
# Add a decade column
billionaires_clean <- billionaires %>%
  filter(!is.na(year), !is.na(location.citizenship)) %>%
  mutate(
    decade = paste0(floor(year / 10) * 10, "s"),
    company.relationship = str_to_lower(company.relationship),
    company.sector = str_to_title(company.sector),
    country = str_to_upper(location.citizenship)
  )

#Claim 1:
#Trend of self made billionaires 
# Filtering for US data
us_inheritance_trends <- billionaires_clean %>%
  filter(country == "UNITED STATES") %>%
  group_by(decade, wealth.how.inherited) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(share = round(n / sum(n), 2))

# Visualization
ggplot(us_inheritance_trends, aes(x = decade, y = share, fill = wealth.how.inherited)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "US Billionaires by Inheritance Type Across Decades",
    x = "Decade",
    y = "Proportion of Billionaires",
    fill = "Inheritance Status"
  ) +
  theme_minimal()

ggsave("Question 4/Figures/us_billionaire_inheritance_by_decade.png", 
       width = 10, height = 6, dpi = 300)

# Shift in industries/sectors of self made billionaires 
# Simplifying and grouping low-frequency sectors
top_sectors <- billionaires_clean %>%
  filter(wealth.how.inherited == "not inherited") %>%
  count(company.sector, sort = TRUE) %>%
  slice_head(n = 8) %>%   # Adjust top N as needed
  pull(company.sector)

billionaires_clean <- billionaires_clean %>%
  mutate(
    simplified_sector = ifelse(company.sector %in% top_sectors, company.sector, "Other")
  )

billionaires_clean %>%
  filter(wealth.how.inherited == "not inherited") %>%
  count(decade, company.sector, sort = TRUE) %>%
  group_by(decade) %>%
  slice_max(n, n = 5) %>%
  ungroup()

# Filtering to self-made billionaires and group by decade and sector
selfmade_sector_trends <- billionaires_clean %>%
  filter(wealth.how.inherited == "not inherited") %>%
  group_by(decade, simplified_sector) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(decade) %>%
  mutate(share = n / sum(n))

# Plot
ggplot(selfmade_sector_trends, aes(x = decade, y = share, fill = simplified_sector)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Main Sectors of Self-Made Billionaires Across Decades",
    x = "Decade",
    y = "Proportion of Self-Made Billionaires",
    fill = "Sector"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9), legend.position = "bottom")

ggsave("Question 4/Figures/self_made_sectors_by_decade.png", width = 10, height = 6, dpi = 300)


library(knitr)

# Create table of top 5 sectors for self-made billionaires by decade
install.packages("gt")
library(gt)
top_sectors_table <- billionaires_clean %>%
  filter(
    wealth.how.inherited == "not inherited",
    !is.na(company.sector)
  ) %>%
  group_by(decade, company.sector) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(decade) %>%
  slice_max(order_by = count, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  pivot_wider(names_from = decade, values_from = count, values_fill = 0)

# View table in R
top_sectors_table %>%
  gt() %>%
  tab_header(
    title = "Split of top company Sectors for Self-Made Billionaires by Decade"
  ) %>%
  gtsave("Question 4/Figures/top_self_made_sectors.png")

