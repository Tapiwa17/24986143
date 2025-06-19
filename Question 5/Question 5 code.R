library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)

#Load the data
health <- read_csv("Question 5/Data/Health/HealthCare.csv")
glimpse(health)

# Sleep vs Physical Activity Level impact on final weight (Comparison between groups)
ggplot(health, aes(x = `Sleep Quality`, y = `Final Weight (lbs)`, fill = `Physical Activity Level`)) +
  geom_boxplot() +
  labs(
    title = "Final Weight by Sleep Quality and Activity Level",
    x = "Sleep Quality", y = "Final Weight (lbs)", fill = "Activity Level"
  ) +
  theme_minimal()
ggsave("Question 5/Figures/final_weight_by_sleep_and_activity.png",
       width = 10, height = 6, dpi = 300)

#Is sleep a stronger factor than excercise 
#Whether people with lower stress have better weight outcomes across all activity types.
ggplot(health, aes(x = `Stress Level`, y = `Final Weight (lbs)`)) +
  geom_jitter(aes(color = `Physical Activity Level`), width = 0.3, alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  labs(
    title = "Impact of Stress and Activity on Final Weight",
    subtitle = "Stress level vs final weight across different activity levels", 
    x = "Stress Level (1 = Low, 10 = High",
    y = "Final Weight (lbs)",
    color = "Activity Level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10))
  )

ggsave("Question 5/Figures/Impact of Stress and Activity on Final Weight.png",
       width = 10, height = 6, dpi = 300)

#Modelling final weight as a function of : Sleep quality, Physical Activity Level, Stress Level, Duration of intervention (weeks)

#Ensuring categorical variables are treated as factors 
health_clean <- health %>%
  mutate(
    `Sleep Quality` = as.factor(`Sleep Quality`),
    `Physical Activity Level` = as.factor(`Physical Activity Level`)
  )

# Run the linear model
model <- lm(
  `Final Weight (lbs)` ~ `Sleep Quality` + `Physical Activity Level` + `Stress Level` + `Duration (weeks)`,
  data = health_clean
)

# View summary
summary(model)

install.packages("rmarkdown")
library(rmarkdown)



