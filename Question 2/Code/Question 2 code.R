library(tidyverse)

# Load data
coldplay <- read_csv("data/Coldplay_vs_Metallica/Coldplay.csv")
metallica <- read_csv("data/Coldplay_vs_Metallica/metallica.csv")
spotify_trends <- read_rds("data/Coldplay_vs_Metallica/Broader_Spotify_Info.rds")
billboard_100 <- read_rds("data/Coldplay_vs_Metallica/charts.rds")

# Glimpse datasets
glimpse(coldplay)
glimpse(metallica)
glimpse(spotify_trends)
glimpse(billboard_100)

#Compare musical longegivity and progression of Coldplay and Metallica
colnames(coldplay)
colnames(metallica)

# Clean Coldplay
coldplay_clean <- coldplay %>%
  select(
    name,
    album = album_name,
    release_date,
    popularity,
    duration = duration,      # assuming already in seconds
    energy,
    tempo,
    valence,
    danceability,
    acousticness,
    liveness,
    speechiness,
    loudness
  ) %>%
  mutate(band = "Coldplay")
# Clean Metallica
metallica_clean <- metallica %>%
  select(
    name,
    album,
    release_date,
    popularity,
    duration_ms,
    energy,
    tempo,
    valence,
    danceability,
    acousticness,
    liveness,
    speechiness,
    loudness
  ) %>%
  # Cconvert duration_ms safely
  mutate(
    duration_sec = duration_ms / 1000,
    band = "Metallica"
  )

# Combine
combined_bands <- bind_rows(coldplay_clean, metallica_clean)

glimpse(coldplay_clean)
glimpse(metallica_clean)

# Combine datasets
combined_bands <- bind_rows(coldplay_clean, metallica_clean)


library(stringi)

combined_bands <- combined_bands %>%
  mutate(album_clean = stri_replace_all_regex(album, "[^\x20-\x7E]", "")) %>%  # Remove non-ASCII characters
  mutate(album_wrapped = str_wrap(album_clean, width = 20))

#Visualization
ggplot(combined_bands, aes(x = reorder(album_wrapped, popularity, median), y = popularity, fill = band)) +
  geom_boxplot() +
  coord_flip() +
  facet_grid(band ~ .) +
  labs(
    title = "Popularity Distribution by Album",
    subtitle = "Spotify Popularity Scores for Coldplay and Metallica",
    x = "Album",
    y = "Popularity Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

ggsave("Figures/popularity_by_album_initial.png", width = 10, height = 6, dpi = 300)

#We can get a more representable plot by simplifying album names
library(stringi)

combined_bands <- combined_bands %>%
  mutate(album_clean = stri_replace_all_regex(album, "[^\x20-\x7E]", "")) %>%
  mutate(album_short = str_trunc(album_clean, 25, side = "right"))

# Plot
ggplot(combined_bands, aes(x = popularity, y = reorder(album_short, popularity), fill = band)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~band, scales = "free_y") +
  labs(
    title = "Popularity Distribution by Album",
    subtitle = "Spotify Popularity Scores for Coldplay and Metallica",
    x = "Popularity Score",
    y = "Album"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )
ggsave("Figures/popularity_distribution_by_album.png", width = 12, height = 8)

#Looking at longegivity
# Average popularity by year
library(dplyr)
library(ggplot2)

band_trends <- bind_rows(coldplay_clean, metallica_clean) %>%
  mutate(year = lubridate::year(release_date)) %>%
  group_by(band, year) %>%
  summarise(avg_popularity = mean(popularity, na.rm = TRUE), .groups = "drop")

# Plot it
ggplot(band_trends, aes(x = year, y = avg_popularity, color = band)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Song Popularity Over Time",
    subtitle = "Trend in Spotify Popularity for Coldplay and Metallica",
    x = "Release Year",
    y = "Average Popularity Score",
    color = "Band"
  ) +
  theme_minimal(base_size = 13)

ggsave("Figures/avg_popularity_by_year.png", width = 10, height = 6)

#Compare track duration and tempo between the 2 bands

# Duration in seconds was already created as duration_sec
# Density plot: Track Duration
ggplot(combined_bands, aes(x = duration_sec, fill = band)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Track Duration",
    x = "Duration (seconds)",
    y = "Density",
    fill = "Band"
  ) +
  theme_minimal()

# Save the plot
ggsave("Figures/track_duration_distribution.png", width = 8, height = 5)

colnames(coldplay)
colnames(metallica)

# Density plot: Tempo
ggplot(combined_bands, aes(x = tempo, fill = band)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Track Tempo",
    x = "Tempo (BPM)",
    y = "Density",
    fill = "Band"
  ) +
  theme_minimal()

# Save the plot
ggsave("Figures/track_tempo_distribution.png", width = 8, height = 5)


# Next Metric : Energy
# Plot energy distribution
ggplot(combined_bands, aes(x = energy, fill = band)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Track Energy",
    subtitle = "Comparison of Coldplay and Metallica",
    x = "Energy",
    y = "Density",
    fill = "Band"
  ) +
  theme_minimal()

# Save the plot
ggsave("Figures/track_energy_distribution.png", width = 8, height = 5)

#Valence distribution
# Plot valence distribution
ggplot(combined_bands, aes(x = valence, fill = band)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Track Valence",
    subtitle = "Comparison of Emotional Positivity in Coldplay and Metallica Tracks",
    x = "Valence (Emotional Positivity)",
    y = "Density",
    fill = "Band"
  ) +
  theme_minimal()

# Save the plot
ggsave("Figures/track_valence_distribution.png", width = 8, height = 6)

# Plot danceability
colnames(combined_bands)
ggplot(combined_bands, aes(x = danceability, fill = band)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Track Danceability",
    subtitle = "Comparison of Coldplay and Metallica",
    x = "Danceability",
    y = "Density",
    fill = "Band"
  ) +
  theme_minimal()

ggsave("Figures/track_danceability_distribution.png", width = 8, height = 5)


# Plot Loudness Distribution
ggplot(combined_bands, aes(x = loudness, fill = band)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Track Loudness",
    subtitle = "Comparison of Coldplay and Metallica",
    x = "Loudness (dB)",
    y = "Density",
    fill = "Band"
  ) +
  theme_minimal()

# Save plot
ggsave("Figures/track_loudness_distribution.png", width = 10, height = 6)

# Summarise key audio features per band
feature_summary <- combined_bands %>%
  group_by(band) %>%
  summarise(
    avg_valence = mean(valence, na.rm = TRUE),
    avg_energy = mean(energy, na.rm = TRUE),
    avg_tempo = mean(tempo, na.rm = TRUE),
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_loudness = mean(loudness, na.rm = TRUE),
    avg_duration_sec = mean(duration_sec, na.rm = TRUE)
  )

# Save table as CSV
write.csv(feature_summary, "Figures/audio_feature_summary.csv", row.names = FALSE)

file.exists("Figures/popularity_by_album_initial.png")
