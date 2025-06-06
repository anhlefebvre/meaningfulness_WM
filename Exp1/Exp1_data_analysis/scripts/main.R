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
main_data = get_main_data(data_path, exclude_potential_cheaters = FALSE)
print(length(unique(main_data$participant_id)))


### Analysis
summary_p_correct = summarize_plot(
  .data = main_data,
  DV = correct,
  IV_within = condition,
  ID = participant_id,
  name_DV = "P(correct)"
)

summary_all_response_types = get_p_response(main_data)

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
    c = "logit"
  ),
  default_priors = list(
    a = list(main = "normal(0, 0.7)", effect = "normal(0, 0.7)"),
    c = list(main = "normal(0, 0.7)", effect = "normal(0, 0.7)")
  )
)

#specify the model formula 
# m3_formula = bmf(
#   target ~ b + a + c,
#   within ~ b + a,
#   extra  ~ b,
#   a ~ 0 + cond + (0 + cond || id),
#   c ~ 0 + cond + (0 + cond || id)
# )

m3_formula_correlation = bmf(
  target ~ b + a + c,
  within ~ b + a,
  extra  ~ b,
  a ~ 0 + cond + (0 + cond | id),
  c ~ 0 + cond + (0 + cond | id)
)

#fit the model
m3_fit_correlation = bmm::bmm(
  formula = m3_formula_correlation,
  data = data_m3,
  model = m3_model,
  cores = 4,
  chains = 4,
  iter = 12500,
  warmup = 1000,
  init = 1,
)

summary(m3_fit_correlation)

#Transform back to exponential
posterior_summary = as.data.frame(summary(m3_fit_correlation)$fixed)
posterior_summary$Estimate_exp_scale = exp(posterior_summary$Estimate)
posterior_summary$CI_low_exp = exp(posterior_summary$`l-95% CI`)
posterior_summary$CI_high_exp = exp(posterior_summary$`u-95% CI`)

#extract posterior sample + take relevant data and exponential_value
posterior_sample_m3 = gather_draws(m3_fit_correlation, `b_a_.*`, `b_p_.*`, regex = TRUE) %>%
  mutate(value_exponential = .value)

### For item memory ### 
a_draws = posterior_sample_m3 %>%
  filter(str_detect(.variable, "b_a_")) %>%
  select(.draw, .variable, value_exponential) %>%
  pivot_wider(names_from = .variable, values_from = value_exponential)

# compute the posterior distribution of each pairwise difference 
a_draws = a_draws %>%
  mutate(diff_real_scram_a = b_a_condreal - b_a_condscram,
         diff_real_artificial_a = b_a_condreal - b_a_condartificial,
         diff_artificial_scram_a = b_a_condartificial - b_a_condscram)

### For binding memory ###
c_draws = posterior_sample_m3 %>%
  filter(str_detect(.variable, "b_p_")) %>%
  select(.draw, .variable, value_exponential) %>%
  pivot_wider(names_from = .variable, values_from = value_exponential)

# compute the posterior distribution of each pairwise difference 
c_draws = c_draws %>%
  mutate(diff_real_scram_c = b_c_condreal - b_c_condscram,
         diff_real_artificial_c = b_c_condreal - b_c_condartificial,
         diff_artificial_scram_c = b_c_condartificial - b_c_condscram)

# Compare both a-c
plot_data = posterior_sample_m3 %>%
  filter(str_detect(.variable, "b_a_") | str_detect(.variable, "b_p_")) %>%
  mutate(
    param = if_else(str_detect(.variable, "b_a_"), "a", "p"),
    condition = str_remove(.variable, "b_[ap]_cond")
  )

plot_summary = plot_data %>%
  group_by(param, condition) %>%
  summarise(
    mean = mean(exp(.value)),
    lower = quantile(exp(.value), 0.025),
    upper = quantile(exp(.value), 0.975),
    .groups = "drop"
  )

### Bayes Factors
# Prior density
prior_density = dnorm(0, mean = 0, sd = sqrt(2) * 0.7)

posterior_sample_m3_log = gather_draws(m3_fit_correlation, `b_a_.*`, `b_p_.*`, regex = TRUE)


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
  filter(str_detect(.variable, "b_p_")) %>%
  select(.draw, .variable, .value) %>%
  pivot_wider(names_from = .variable, values_from = .value)

c_draws_log_diff = c_draws_log %>%
  mutate(
    real_scram = b_p_condreal - b_p_condscram,
    real_artificial = b_p_condreal - b_p_condartificial,
    artificial_scram = b_p_condartificial - b_p_condscram
  )

pd_real_scram_c = density_at(c_draws_log_diff$real_scram, 0, extend = TRUE)
pd_real_artificial_c = density_at(c_draws_log_diff$real_artificial, 0, extend = TRUE)
pd_artificial_scram_c = density_at(c_draws_log_diff$artificial_scram, 0, extend = TRUE)

BF_real_scram_c = prior_density / pd_real_scram_c
BF_real_artificial_c = prior_density / pd_real_artificial_c
BF_artificial_scram_c = prior_density / pd_artificial_scram_c