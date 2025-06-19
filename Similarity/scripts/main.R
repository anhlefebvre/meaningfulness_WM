rm(list = ls())

library(brms)
library(tidyverse)
library(here)
library(ggplot2)
library(dplyr)

similarity_data_path = here("Similarity", "similarity_data.txt")
similarity_data = read_delim(similarity_data_path)
view(similarity_data)

long_similarity_data = similarity_data %>%
  pivot_longer(
    cols = starts_with("similarity_response"),
    names_to = "response_slot",
    names_pattern = "similarity_response(\\d)_image",
    values_to = "similarity"
  ) %>%
  mutate(
    selected = as.integer(similarity == similarity_selected_image)
  )

### Similarity between selected response and target
model = glm(selected ~ similarity * condition, data = long_similarity_data, family = binomial())
summary(model)

ggplot(long_similarity_data, aes(x = similarity, y = selected)) +
  geom_jitter(height = 0.05, alpha = 0.2) +
  stat_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~condition) +
  labs(x = "Similarity to Probe", y = "Probability of Selection")

