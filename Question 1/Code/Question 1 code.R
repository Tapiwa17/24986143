library(tidyverse)

# Load the datasets 
Baby_Names <- read_rds("data/US_Baby_names/Baby_Names_By_US_State.rds")
Top_100_Billboard <- read_rds("data/US_Baby_names/charts.rds")
HBO_titles <- read_rds("data/US_Baby_names/HBO_titles.rds")
HBO_credits <- read_rds("data/US_Baby_names/HBO_credits.rds")

glimpse(Baby_Names)

# Extracting top 25 names per year and gender across all states
top_25_names <- Baby_Names %>%
  group_by(Year, Gender, Name) %>%
  summarise(Total = sum(Count), .groups = "drop") %>%
  group_by(Year, Gender) %>%
  slice_max(order_by = Total, n = 25) %>%
  mutate(Rank = rank(-Total)) %>%
  select(Year, Gender, Name, Rank)

# Function to get spearman correlations between current year and year +1/2/3
get_spearman_for_year <- function(y, g) {
  base_year <- top_25_names %>% 
    filter(Year == y, Gender == g)
  
  result <- tibble()
  
  for (offset in 1:3) {
    compare_year <- top_25_names %>%
      filter(Year == y + offset, Gender == g)
    
    # Join on Name to keep only common names
    joined <- inner_join(base_year, compare_year, by = "Name", suffix = c("_base", "_comp"))
    
    if (nrow(joined) >= 5) {
      rho <- cor(joined$Rank_base, joined$Rank_comp, method = "spearman")
    } else {
      rho <- NA
    }
    
    result <- bind_rows(result, tibble(
      Year = y,
      Gender = g,
      Offset = offset,
      Future_Year = y + offset,
      Spearman = rho
    ))
  }
  
  return(result)
}

# Now we apply across all years
years <- unique(top_25_names$Year)
genders <- unique(top_25_names$Gender)

# Apply function to all (year, gender) pairs
correlation_results <- map_dfr(years, function(y) {
  map_dfr(genders, function(g) {
    get_spearman_for_year(y, g)
  })
})

# Visualization
ggplot(correlation_results, aes(x = Year, y = Spearman, color = as.factor(Offset))) +
  geom_line() +
  facet_wrap(~Gender) +
  labs(
    title = "Spearman Rank Correlation of Top 25 Baby Names Over Time",
    subtitle = "Correlation between each year's rankings and the next 3 years",
    x = "Base Year",
    y = "Spearman Correlation",
    color = "Years Ahead"
  ) +
  theme_minimal()

ggsave("Figures/spearman_name_correlation.png", width = 10, height = 6)

# Finding insights on surges : 

# Summarizing total counts per name, year, and gender
name_trends <- Baby_Names %>%
  group_by(Name, Year, Gender) %>%
  summarise(Total = sum(Count), .groups = "drop") %>%
  arrange(Name, Gender, Year)

# Calculating year-on-year percentage change
name_trends <- name_trends %>%
  group_by(Name, Gender) %>%
  mutate(YoY_change = 100 * (Total - lag(Total)) / lag(Total)) %>%
  ungroup()

# Top 10 surges by % increase (removing very low baselines)
top_surges <- name_trends %>%
  filter(!is.na(YoY_change), lag(Total) >= 50) %>%
  arrange(desc(YoY_change)) %>%
  slice_head(n = 10)

# Visualizing the surges
ggplot(top_surges, aes(x = Year, y = Total, color = Name)) +
  geom_line() +
  facet_wrap(~ Name + Gender, scales = "free_y") +
  labs(title = "Top Name Surges Over Time",
       y = "Total Babies Given Name",
       x = "Year") +
  theme_minimal()

ggsave("Figures/top_name_surges.png", width = 10, height = 6)

# Checking structure 
glimpse(Top_100_Billboard)
glimpse(HBO_titles)
glimpse(HBO_credits)

# Add Billboard and HBO links to top_surges data: 

# Add Billboard matches
top_surges <- top_surges %>%
  rowwise() %>%
  mutate(
    Billboard_Song_Match = any(str_detect(Top_100_Billboard$song, regex(Name, ignore_case = TRUE))),
    Billboard_Artist_Match = any(str_detect(Top_100_Billboard$artist, regex(Name, ignore_case = TRUE)))
  ) %>%
  ungroup()

# Join HBO credits and titles
hbo_data <- HBO_credits %>%
  left_join(HBO_titles, by = "id")

# Add HBO matches
top_surges <- top_surges %>%
  rowwise() %>%
  mutate(
    HBO_Name_Match = any(str_detect(hbo_data$name, regex(Name, ignore_case = TRUE))),
    HBO_Character_Match = any(str_detect(hbo_data$character, regex(Name, ignore_case = TRUE)))
  ) %>%
  ungroup()

colnames(top_surges)

top_surges_summary <- top_surges %>%
  mutate(
    Billboard_Link = if_else(
      is.na(Billboard_Song_Match) & is.na(Billboard_Artist_Match),
      FALSE,
      TRUE
    ),
    HBO_Link = if_else(
      is.na(HBO_Name_Match) & is.na(HBO_Character_Match),
      FALSE,
      TRUE
    )
  ) %>%
  select(Name, Gender, Year, YoY_change, Billboard_Link, HBO_Link)
# View Table
top_surges_summary

# Save top name surges summary table
write.csv(top_surges_summary, "Figures/top_surges_summary.csv", row.names = FALSE)

