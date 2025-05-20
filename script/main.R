rm(list = ls())

library(brms)
library(bmm)
library("tidyverse")
library(dplyr)
library(ggplot2)
library(here)
library(tidybayes)
library(bridgesampling)
library(logspline)
#library(polspline)
library(bayestestR)


source(here("script", "data_preprocessing.R"))
source(here("script", "plot_summary_functions.R"))

### Functions

# Get p(correct) for each conditions
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
main_data = get_main_data(data_path)


### Analysis
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
  ) + 
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

#Plot response types by condition
ggplot(summary_all_response_types, aes(x = condition, y = proportion, color = selected_type, group = selected_type, shape = selected_type)) +
  geom_point(size = 4, stroke = 1.2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Proportion of Selected Response Types by Condition",
    x = "Condition",
    y = "Proportion",
    color = "Response Type",
    shape = "Response Type"
  ) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# Bayesian 
data_m3 = main_data %>%
  group_by(participant_id, condition) %>%
  summarise(
    target = sum(selected_type == "target"),
    within_list = sum(selected_type == "within_list"),
    extra_list = sum(selected_type == "extra_list"),
  ) %>%
  ungroup()

data_m3 = data_m3 %>%
  rename(
    target = target,
    within = within_list,
    extra = extra_list,
    id = participant_id,
    cond = condition
  )

#Initiate m3 model objects
m3_model = m3(
  resp_cats = c("target", "within", "extra"),
  num_options = c(1, 2, 3),
  choice_rule = "softmax",
  links = list(
    a = "log",
    c = "log"
  ),
  default_priors = list(
    a = list(main = "normal(0, 0.7)", effect = "normal(0, 0.7)"),
    c = list(main = "normal(0, 0.7)", effect = "normal(0, 0.7)")
  )
)

#specify the model formula 
m3_formula = bmf(
  target ~ b + a + c,
  within ~ b + a,
  extra  ~ b,
  a ~ 0 + cond + (0 + cond || id),
  c ~ 0 + cond + (0 + cond || id)
)


#fit the model
m3_fit = bmm::bmm(
  formula = m3_formula,
  data = data_m3,
  model = m3_model,
  cores = 4,
  chains = 4,
  iter = 12500,
  warmup = 1000,
  init = 1
)

summary(m3_fit)

#Transform back to exponential
posterior_summary = as.data.frame(summary(m3_fit)$fixed)
posterior_summary$Estimate_exp_scale = exp(posterior_summary$Estimate)
posterior_summary$CI_low_exp = exp(posterior_summary$`l-95% CI`)
posterior_summary$CI_high_exp = exp(posterior_summary$`u-95% CI`)

#extract posterior sample + take relevant data and exponential_value
posterior_sample_m3 = gather_draws(m3_fit, `b_a_.*`, `b_c_.*`, regex = TRUE) %>%
  mutate(value_exponential = .value)

### For item memory ### 
a_draws = posterior_sample_m3 %>%
  filter(str_detect(.variable, "b_a_")) %>%
  select(.draw, .variable, value_exponential) %>%
  pivot_wider(names_from = .variable, values_from = value_exponential)

# compute the posterior distribution of each pairwise difference 
a_draws = a_draws %>%
  mutate(diff_real_scram = b_a_condreal - b_a_condscram,
         diff_real_artificial = b_a_condreal - b_a_condartificial,
         diff_artificial_scram = b_a_condartificial - b_a_condscram)

#Plot for real - scram
a_draws %>%
  ggplot(aes(x = diff_real_scram)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Posterior distribution: a-real-a-scram")
#Plot for real-artificial
a_draws %>%
  ggplot(aes(x = diff_real_artificial)) + 
  geom_density(fill = "green", alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Posterior distribution: a-real - a-artificial")

#Plot for artificial - scram
a_draws %>%
  ggplot(aes(x = diff_artificial_scram)) + 
  geom_density(fill = "orange", alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Posterior distribution: a-artificial - a-scram")

### Bayes Factors
# Prior density
prior_density = dnorm(0, mean = 0, sd = sqrt(2) * 0.7)

posterior_sample_m3_log = gather_draws(m3_fit, `b_a_.*`, `b_c_.*`, regex = TRUE)


a_draws_log = posterior_sample_m3_log %>%
  filter(str_detect(.variable, "b_a_")) %>%
  select(.draw, .variable, .value) %>%
  pivot_wider(names_from = .variable, values_from = .value)

a_draws_log_diff = a_draws_log %>%
  mutate(
    real_scram = b_a_condreal - b_a_condscram,
    real_artificial = b_a_condreal - b_a_condartificial,
    artificial_scram = b_a_condartificial - b_a_condscram
  )

pd_real_scram_a = density_at(a_draws_log_diff$real_scram, 0, extend = TRUE)
pd_real_artificial_a = density_at(a_draws_log_diff$real_artificial, 0, extend = TRUE)
pd_artificial_scram_a = density_at(a_draws_log_diff$artificial_scram, 0, extend = TRUE)

BF_real_scram_a = prior_density / pd_real_scram_a
BF_real_artificial_a = prior_density / pd_real_artificial_a
BF_artificial_scram_a = prior_density / pd_artificial_scram_a



c_draws_log = posterior_sample_m3_log %>%
  filter(str_detect(.variable, "b_c_")) %>%
  select(.draw, .variable, .value) %>%
  pivot_wider(names_from = .variable, values_from = .value)

c_draws_log_diff = c_draws_log %>%
  mutate(
    real_scram = b_c_condreal - b_c_condscram,
    real_artificial = b_c_condreal - b_c_condartificial,
    artificial_scram = b_c_condartificial - b_c_condscram
  )

pd_real_scram_c = density_at(c_draws_log_diff$real_scram, 0, extend = TRUE)
pd_real_artificial_c = density_at(c_draws_log_diff$real_artificial, 0, extend = TRUE)
pd_artificial_scram_c = density_at(c_draws_log_diff$artificial_scram, 0, extend = TRUE)

BF_real_scram_c = prior_density / pd_real_scram_c
BF_real_artificial_c = prior_density / pd_real_artificial_c
BF_artificial_scram_c = prior_density / pd_artificial_scram_c
