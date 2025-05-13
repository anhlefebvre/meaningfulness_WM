rm(list = ls())

library(brms)
library("tidyverse")
library(dplyr)
library(ggplot2)
library(here)


source("plot_summary_functions.R")

get_main_data = function(data_path) {
  data = read_delim(data_path)
  data_main = filter(data, phase == "Main"  & cheating == "no" & seriousness == "yes")
  artificial = filter(data_main, condition == "artificial")
  scram = filter(data_main, condition == "scram")
  real = filter(data_main, condition == "real")
  return(bind_rows(artificial, scram, real))
}

get_p_response = function(main_data) {
  main_data = main_data %>%
    mutate(
      is_target = if_else(selected_type == "target", 1, 0),
      is_within = if_else(selected_type == "within_list", 1, 0),
      is_extra = if_else(selected_type == "extra_list", 1, 0)
    )
  
  p_target = summarize_plot(main_data, DV = is_target, IV_within = condition, ID = participant_id, name_DV = "proportion")
  p_within = summarize_plot(main_data, DV = is_within, IV_within = condition, ID = participant_id, name_DV = "proportion")
  p_extra = summarize_plot(main_data, DV = is_extra, IV_within = condition, ID = participant_id, name_DV = "proportion")
  
  p_target$selected_type = "target"
  p_within$selected_type = "within-list"
  p_extra$selected_type = "extra-list"
  
  summary_all = bind_rows(p_target, p_within, p_extra)
  
  return (summary_all)
}

### Main ###
data_path = here("data", "data_raw.txt")
main_data = read_delim(data_path) %>%
  filter(phase == "Main", cheating == "no", seriousness == "yes") %>%
  filter(condition %in% c("artificial", "real", "scram"))

summary_p_correct = summarize_plot(
  .data = main_data,
  DV = correct,
  IV_within = condition,
  ID = participant_id,
  name_DV = "P(correct)"
)

summary_all_response_types = get_p_response(main_data)

### Plot ###

#Plot P(correct)
ggplot(summary_p_correct, aes(x = condition, y = `P(correct)`, fill = condition)) +
  geom_col(width = 0.6, color = "black") +  
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +  
  labs(
    title = "Proportion Correct by Condition",
    x = "Condition",
    y = "P(correct)"
  )

#Plot response types by condition
ggplot(summary_all_response_types, aes(x = condition, y = proportion, color = selected_type, group = selected_type, shape = selected_type)) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Proportion of Selected Response Types by Condition",
    x = "Condition",
    y = "Proportion",
    color = "Response Type",
    shape = "Response Type"
  ) +
  theme_minimal()