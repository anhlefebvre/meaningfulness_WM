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


source(here("Exp2/Exp2_data_analysis/scripts", "data_preprocessing.R"))
source(here("Exp1/Exp1_data_analysis/scripts", "plot_summary_functions.R"))

### Functions

# Get p(correct) for each conditions
get_p_response= function(main_data) {
  main_data = main_data %>%
    mutate(
      is_both = if_else(total_correct == 2, 1, 0),
      is_2AFC = if_else(total_correct == 1, 1, 0),
      is_none = if_else(total_correct == 0, 1, 0)
    )
  
  p_both  = summarize_plot(main_data,DV = is_both, IV_within = condition, ID = participant_id, name_DV = "proportion")
  p_2AFC  = summarize_plot(main_data,DV = is_2AFC, IV_within = condition, ID = participant_id, name_DV = "proportion")
  p_none  = summarize_plot(main_data,DV = is_none, IV_within = condition, ID = participant_id, name_DV = "proportion")


  p_both$response_type = "both_correct"
  p_2AFC$response_type = "2AFC_correct"
  p_none$response_type = "none_correct"

  summary_proportions = bind_rows(p_both, p_2AFC, p_none)
  return(summary_proportions)
}


### Main ###
main_data = get_main_data(data_path, exclude_potential_cheaters = FALSE)

### Analysis

summary_all_response_types = get_p_response(main_data)

#P(correct) - for the plot
summary_p_correct = summarize_plot(
  .data = main_data,
  DV = if_else(total_correct == 2, 1, 0),
  IV_within = condition,
  ID = participant_id,
  name_DV = "P(correct)"
)

# Bayesian
data_m3 = main_data %>%
  group_by(participant_id, condition) %>%
  summarise(
    both = sum(if_else(total_correct == 2, 1, 0)),
    item = sum(if_else(total_correct == 1, 1, 0)),
    none = sum(if_else(total_correct == 0, 1, 0))
  ) %>%
  ungroup() %>%
  rename(
    id = participant_id,
    cond = condition
  )

# data_m3 = data_m3 %>%
#   rename(
#     is_both = target,
#     within = within_list,
#     extra = extra_list,
#     id = participant_id,
#     cond = condition
#   )

#Initiate m3 model objects
m3_model = m3(
  resp_cats = c("both", "item", "none"),
  num_options = c(1, 5, 6),
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
  both ~ b + a + c,
  item ~ b + a,
  none  ~ b,
  c ~ 0 + cond + (0 + cond | id),
  a ~ 0 + cond + (0 + cond | id)
)

model_path = here("Exp2/Exp2_data_analysis/models", "m3_fit_correlation.rds")
dir.create(dirname(model_path), showWarnings = FALSE, recursive = TRUE)
##file.remove(model_path)

if (file.exists(model_path)) {
  cat("Already computed, loading ", model_path, "\n")
  m3_fit = readRDS(model_path)
} else {
  cat("Fitting model\n")
  m3_fit = bmm::bmm(
    formula = m3_formula,
    data = data_m3,
    model = m3_model,
    cores = 4,
    chains = 4,
    iter = 13500,
    warmup = 1000,
    init = 0,
  )
  saveRDS(m3_fit, model_path)
  cat("Save the model:", model_path, "\n")
}

summary(m3_fit)

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
  mutate(diff_real_scram_a = b_a_condreal - b_a_condscram,
         diff_real_artificial_a = b_a_condreal - b_a_condartificial,
         diff_artificial_scram_a = b_a_condartificial - b_a_condscram)

### For binding memory ###
c_draws = posterior_sample_m3 %>%
  filter(str_detect(.variable, "b_c_")) %>%
  select(.draw, .variable, value_exponential) %>%
  pivot_wider(names_from = .variable, values_from = value_exponential)

# compute the posterior distribution of each pairwise difference 
c_draws = c_draws %>%
  mutate(diff_real_scram_c = b_c_condreal - b_c_condscram,
         diff_real_artificial_c = b_c_condreal - b_c_condartificial,
         diff_artificial_scram_c = b_c_condartificial - b_c_condscram)

# Compare both a-c
plot_data = posterior_sample_m3 %>%
  filter(str_detect(.variable, "b_a_") | str_detect(.variable, "b_c_")) %>%
  mutate(
    param = if_else(str_detect(.variable, "b_a_"), "a", "c"),
    condition = str_remove(.variable, "b_[ac]_cond")
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

pd_real_scram_a = density_at(a_draws_log_diff$real_scram, 0, extend = TRUE, extend_scale = 0.5)
pd_real_artificial_a = density_at(a_draws_log_diff$real_artificial, 0, extend = TRUE, extend_scale = 0.5)
pd_artificial_scram_a = density_at(a_draws_log_diff$artificial_scram, 0, extend = TRUE, extend_scale = 0.5)

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

pd_real_scram_c = density_at(c_draws_log_diff$real_scram, 0, extend = TRUE, extend_scale = 0.5)
pd_real_artificial_c = density_at(c_draws_log_diff$real_artificial, 0, extend = TRUE, extend_scale = 0.5)
pd_artificial_scram_c = density_at(c_draws_log_diff$artificial_scram, 0, extend = TRUE, extend_scale = 0.5)

BF_real_scram_c = prior_density / pd_real_scram_c
BF_real_artificial_c = prior_density / pd_real_artificial_c
BF_artificial_scram_c = prior_density / pd_artificial_scram_c