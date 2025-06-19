# Load packages
library(tidyverse)
library(lubridate)
library(stringr)
library(tidytext)

# Load data
titles <- read_rds("Question 3/Data/netflix/titles.rds")
credits <- read_rds("Question 3/Data/netflix/credits.rds")
movie_info <- read_csv("Question 3/Data/netflix/netflix_movies.csv")


# Inspect structure
glimpse(titles)
glimpse(credits)
glimpse(movie_info)

library(dplyr)
library(tidyr)

# Clean titles data
titles_clean <- titles %>%
  mutate(
    genres = str_replace_all(genres, "\\[|\\]|'", ""),  # Remove brackets and quotes
    genres = str_split(genres, ",\\s*"),                # Split into list-column
    country = str_replace_all(production_countries, "\\[|\\]|'", ""),
    country = str_split(country, ",\\s*"),
    release_decade = floor(release_year / 10) * 10      # Create decade for grouping
  )

# Clean movie_info
movie_info_clean <- movie_info %>%
  mutate(
    duration_min = as.numeric(str_extract(duration, "\\d+")),  # Convert duration to numeric
    listed_in = str_split(listed_in, ",\\s*")                  # Genre listing
  )

# Quick look at cleaned data
glimpse(titles_clean)
glimpse(movie_info_clean)

# Let’s create a summary table showing the top 5 most common genres per country from movie_info_clean

# Identify top 6 countries by total titles
top_countries <- movie_info_clean %>%
  filter(!is.na(country)) %>%
  separate_rows(country, sep = ",\\s*") %>%
  count(country, sort = TRUE) %>%
  slice_max(n, n = 6) %>%
  pull(country)

# Prepare genre-country data for these countries only
genre_country <- movie_info_clean %>%
  filter(!is.na(country), !is.na(listed_in)) %>%
  separate_rows(country, sep = ",\\s*") %>%
  filter(country %in% top_countries) %>%
  unnest(listed_in) %>%
  count(country, listed_in, sort = TRUE) %>%
  group_by(country) %>%
  slice_max(n, n = 5) %>%
  ungroup()

# Clean genre labels for better readability
genre_country <- genre_country %>%
  mutate(listed_in = str_wrap(listed_in, width = 15))

# Plot improved version
ggplot(genre_country, aes(x = n, y = reorder(listed_in, n), fill = country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ country, scales = "free_y") +
  labs(
    title = "Top 5 Netflix Genres by Country",
    x = "Count",
    y = "Genre"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave("Question 3/Figures/top_genres_by_country.png", width = 10, height = 6, dpi = 300)

#Identify how IMDb ratings of Netflix titles vary by country. This helps inform which regions have a catalog of critically acclaimed content and where there may be room for quality improvement.

# Clean country codes
ratings_summary <- titles_clean %>%
  unnest(production_countries) %>%
  filter(!is.na(tmdb_score)) %>%
  mutate(production_countries = str_remove_all(production_countries, "\\[|\\]|'|\"")) %>%
  group_by(production_countries) %>%
  summarise(
    avg_rating = mean(tmdb_score, na.rm = TRUE),
    n_titles = n()
  ) %>%
  filter(n_titles >= 20) %>%
  arrange(desc(avg_rating))

ratings_summary <- ratings_summary %>%
  mutate(country_clean = gsub("\\[|\\]|'", "", production_countries))

# Plot
ggplot(ratings_summary, aes(x = avg_rating, y = fct_reorder(country_clean, avg_rating))) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Average IMDb Rating by Country",
    x = "Average Rating",
    y = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

ggsave("Question 3/Figures/imdb_avg_rating_by_country.png", width = 10, height = 6, dpi = 300)

#Star power impact 

# Merge datasets
netflix_data <- left_join(titles, credits, by = "id")

# Filter to actor rows only
actor_data <- netflix_data %>%
  filter(toupper(role) == "ACTOR")

# Identify top 10 most frequent actors
top_actors <- actor_data %>%
  count(name, sort = TRUE) %>%
  slice_max(n, n = 50)

# Mark which actor rows belong to top 10
actor_data <- actor_data %>%
  mutate(top_actor = name %in% top_actors$name)

# Collapse to one row per movie: TRUE if any top actor appears
movie_level_data <- actor_data %>%
  group_by(id, imdb_score) %>%
  summarise(any_top_actor = any(top_actor), .groups = "drop")

# View count distribution for sanity check (optional)
table(movie_level_data$any_top_actor)

# Boxplot of IMDb score distribution
ggplot(movie_level_data, aes(x = any_top_actor, y = imdb_score, fill = any_top_actor)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, width = 0.6) +
  labs(
    title = "IMDb Rating Distribution by Top Actor Presence",
    x = "Contains a Top 50 Actor",
    y = "IMDb Rating"
  ) +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  theme_minimal()

ggsave(
  filename = "star_power_boxplot.png",
  plot = last_plot(),
  path = "Question 3/Figures",
  width = 8, height = 6, dpi = 300
)

# Discovering which keywords appear more often in the descriptions of: high and low rated shows/films
# Filter for relevant descriptions and rating groups
desc_data <- titles %>%
  filter(!is.na(description), !is.na(imdb_score)) %>%
  mutate(
    rating_group = case_when(
      imdb_score >= 7.5 ~ "High-rated",
      imdb_score <= 5 ~ "Low-rated",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(rating_group))  # Keep only high or low

# Tokenizing the descriptions into words
word_tokens <- desc_data %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%  # Remove common stopwords like "the", "and"
  filter(str_detect(word, "^[a-z']+$"))  # Keep only clean words

# Counting word frequency by rating group
word_counts <- word_tokens %>%
  count(rating_group, word, sort = TRUE) %>%
  group_by(rating_group) %>%
  slice_max(n, n = 15)  # Top 15 words per group

# Plotting the top words in each group
ggplot(word_counts, aes(x = reorder_within(word, n, rating_group), y = n, fill = rating_group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ rating_group, scales = "free_y") +
  scale_x_reordered() +
  labs(
    title = "Most Frequent Words in Descriptions by IMDb Rating Group",
    x = NULL, y = "Word Count"
  ) +
  coord_flip() +
  theme_minimal()

ggsave(
  filename = "description_word_comparison.png",
  plot = last_plot(),
  path = "Question 3/Figures",
  width = 10, height = 6, dpi = 300
)

# Movies vs Series : Which receive higher ratings :
# Cleaning and filtering the data
type_comparison <- titles %>%
  filter(type %in% c("MOVIE", "SHOW"), !is.na(imdb_score)) %>%
  mutate(type = factor(type, levels = c("MOVIE", "SHOW")))  # for consistent ordering

# Summary statistics
type_comparison %>%
  group_by(type) %>%
  summarise(
    avg_rating = mean(imdb_score, na.rm = TRUE),
    median_rating = median(imdb_score, na.rm = TRUE),
    count = n()
  )

# Creating a boxplot
ggplot(type_comparison, aes(x = type, y = imdb_score, fill = type)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, width = 0.6) +
  labs(
    title = "IMDb Rating Distribution: Movies vs. Series",
    x = "Content Type",
    y = "IMDb Rating"
  ) +
  scale_fill_manual(values = c("MOVIE" = "#E69F00", "SHOW" = "#56B4E9")) +
  theme_minimal()

ggsave(
  filename = "movie_vs_series_ratings.png",
  plot = last_plot(),
  path = "Question 3/Figures",
  width = 8, height = 6, dpi = 300
)
