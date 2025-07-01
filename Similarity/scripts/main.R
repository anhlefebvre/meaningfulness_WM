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
similarity_response_target = glm(selected ~ similarity * condition, data = long_similarity_data, family = binomial())
summary(similarity_response_target)

similarity_response_target_plot = ggplot(long_similarity_data, aes(x = similarity, y = selected)) +
  geom_jitter(height = 0.05, alpha = 0.2) +
  stat_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~condition) +
  labs(x = "Similarity to Probe", y = "Probability of Selection")

### Similarity between response options
similarity_summary = similarity_data%>%
  rowwise()%>%
  mutate(
    mean_similarity_response = mean(c_across(starts_with("similarity_response")), na.rm=TRUE),
    min_similarity_response = min(c_across(starts_with("similarity_response")), na.rm=TRUE),
    max_similarity_response = max(c_across(starts_with("similarity_response")), na.rm=TRUE),
    sd_similarity_response = sd(c_across(starts_with("similarity_response")), na.rm=TRUE),
  )%>%
  ungroup()

response_similarity = glm(correct ~ mean_similarity_response * condition, 
                          data = similarity_summary,
                          family = binomial())
summary(response_similarity)

ggplot(long_similarity_data, aes(x = similarity, y = selected)) +
  geom_jitter(height = 0.05, alpha = 0.2) +
  stat_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~condition) +
  labs(x = "Similarity to Probe", y = "Probability of Selection")


### Compare similarity - accuracy across conds
summary_data = similarity_summary %>%
  group_by(condition) %>%
  summarise(
    p_correct = mean(correct, na.rm = TRUE),
    p_correct_se = sd(correct, na.rm = TRUE) / sqrt(n()),
    mean_similarity = mean(mean_similarity_response, na.rm = TRUE),
    mean_similarity_se = sd(mean_similarity_response, na.rm = TRUE) / sqrt(n())
  )

plot_data = summary_data %>%
  pivot_longer(
    cols = c(p_correct, mean_similarity),
    names_to = "measure",
    values_to = "y"
  ) %>%
  mutate(
    se = if_else(measure == "p_correct", p_correct_se, mean_similarity_se)
  )

plot_summary = ggplot(plot_data, aes(x = condition, y = y, color = condition)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = y - se, ymax = y + se), width = 0.1) +
  facet_wrap(~measure, scales = "free_y") +
  labs(
    title = "Similarity – Mean Accuracy and Mean Distance in Experiment 1:",
    x = "Condition",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

