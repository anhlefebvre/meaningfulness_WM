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


source(here("Exp1/Exp1_data_analysis/scripts", "data_preprocessing.R"))
source(here("Exp1/Exp1_data_analysis/scripts", "plot_summary_functions.R"))

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

# P(correct)
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
  c ~ 0 + cond + (0 + cond | id),  
  a ~ 0 + cond + (0 + cond | id)
)


#fit the model
model_path = here("Exp1/Exp1_data_analysis/models", "m3_fit_correlation.rds")
dir.create(dirname(model_path), showWarnings = FALSE, recursive = TRUE)
#file.remove(model_path)

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

pd_real_scram_c = density_at(c_draws_log_diff$real_scram, 0, extend = TRUE, extend_scale = 1.5)
pd_real_artificial_c = density_at(c_draws_log_diff$real_artificial, 0, extend = TRUE, extend_scale = 1.5)
pd_artificial_scram_c = density_at(c_draws_log_diff$artificial_scram, 0, extend = TRUE, extend_scale = 1.5)

BF_real_scram_c = prior_density / pd_real_scram_c
BF_real_artificial_c = prior_density / pd_real_artificial_c
BF_artificial_scram_c = prior_density / pd_artificial_scram_c
#BF_real_scram = dnorm(0, mean = 0, sd = 1) / dnorm(0, mean = mean(c_draws_log_diff$real_scram), sd = sd(c_draws_log_diff$real_scram))

# BF for accuracy part
accuracy_path = here("Exp1/Exp1_data_analysis/models", "accuracy_model.rds")
dir.create(dirname(accuracy_path), showWarnings = FALSE, recursive = TRUE)

if (file.exists(accuracy_path)) {
  cat("Already computed, loading ", accuracy_path, "\n")
  accuracy_model = readRDS(accuracy_path)
} else {
  cat("Run the model\n")
  accuracy_model = brm(
    correct ~ 0 + condition + (0 + condition | participant_id),
    data = main_data,
    family = bernoulli(),
    prior = c(
      prior(normal(0, 0.7), class = "b"),
      prior(exponential(1), class = "sd") 
    ),
    chains = 4,
    cores = 4,
    iter = 13500,
    warmup = 1000
  )
  
  saveRDS(accuracy_model, accuracy_path)
  cat("Save the model:", accuracy_path, "\n")
}

summary(accuracy_model)

posterior_accuracy = as_draws_df(accuracy_model)

posterior_accuracy = posterior_accuracy%>%
  mutate(
    diff_real_scram = b_conditionreal - b_conditionscram,
    diff_real_artificial = b_conditionreal - b_conditionartificial,
    diff_artificial_scram = b_conditionartificial - b_conditionscram
  )

prior_density_accuracy = dnorm(0, mean = 0, sd = sqrt(2) * 0.7)

pd_real_scram_accuracy = dnorm(
  0,
  mean = mean(posterior_accuracy$diff_real_scram),
  sd = sd(posterior_accuracy$diff_real_scram)
)
pd_real_artificial_accuracy = dnorm(
  0,
  mean = mean(posterior_accuracy$diff_real_artificial),
  sd = sd(posterior_accuracy$diff_real_artificial)
)

pd_artificial_scram_accuracy = dnorm(
  0,
  mean = mean(posterior_accuracy$diff_artificial_scram),
  sd = sd(posterior_accuracy$diff_artificial_scram)
)

BF_real_scram_accuracy = prior_density_accuracy/pd_real_scram_accuracy
BF_real_artificial_accuracy = prior_density_accuracy/pd_real_artificial_accuracy
BF_artificial_scram_accuracy = prior_density_accuracy/pd_artificial_scram_accuracy


# BF for accuracy part - difference in error types
H1_err_path = here("Exp1/Exp1_data_analysis/models", "H1_error_type_model.rds")
H0_err_path = here("Exp1/Exp1_data_analysis/models", "H0_error_type_model.rds")
BF_err_path = here("Exp1/Exp1_data_analysis/models", "BF_error_type_H1_vs_H0.rds")

# H1
if (file.exists(H1_err_path)) {
  cat("Already computed, loading ", H1_err_path, "\n")
  H1_err = readRDS(H1_err_path)
} else {
  cat("Fitting H1 error-type model\n")
  H1_err = brm(
    err_is_within ~ 0 + condition + (0 + condition | participant_id),
    data = err_data,
    family = bernoulli(),
    prior = c(
      prior(normal(0, 0.7), class = "b"),
      prior(exponential(1), class = "sd")
    ),
    chains = 4, cores = 4, iter = 13500, warmup = 1000,
    save_pars = save_pars(all = TRUE)
  )
  saveRDS(H1_err, H1_err_path)
}

# H0
if (file.exists(H0_err_path)) {
  cat("Already computed, loading ", H0_err_path, "\n")
  H0_err = readRDS(H0_err_path)
} else {
  cat("Fitting H0 error-type model\n")
  H0_err = brm(
    err_is_within ~ 1 + (1 | participant_id),
    data = err_data,
    family = bernoulli(),
    prior = c(
      prior(normal(0, 0.7), class = "Intercept"),
      prior(exponential(1), class = "sd")
    ),
    chains = 4, cores = 4, iter = 13500, warmup = 1000,
    save_pars = save_pars(all = TRUE)
  )
  saveRDS(H0_err, H0_err_path)
}

if (file.exists(BF_err_path)) {
  cat("Already computed, loading ", BF_err_path, "\n")
  BF_err_10 = readRDS(BF_err_path)
} else {
  cat("Computing BF for error-type models\n")
  br_H1 = bridge_sampler(H1_err)
  br_H0 = bridge_sampler(H0_err)
  BF_err_10 = bf(br_H1, br_H0) 
  saveRDS(BF_err_10, BF_err_path)
}
print(BF_err_10)

### Test BF for different responses across conditions
error_data = main_data %>%
  filter(selected_type != "target") %>%
  mutate(err_is_within = as.integer(selected_type == "within_list"))

H1_err = brm(
  err_is_within ~ 0 + condition + (0 + condition | participant_id),
  data = error_data,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 0.7), class = "b"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4, cores = 4, iter = 13500, warmup = 1000,
  save_pars = save_pars(all = TRUE)
)

H0_err = brm(
  err_is_within ~ 1 + (1 | participant_id),
  data = error_data,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 0.7), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4, cores = 4, iter = 13500, warmup = 1000,
  save_pars = save_pars(all = TRUE)
)

bridge_H1 = bridge_sampler(H1_err)
bridge_H0 = bridge_sampler(H0_err)
BF_error_global = bf(bridge_H1, bridge_H0)

print(BF_error_global)

posterior_error = as_draws_df(H1_err)

posterior_error = posterior_error %>%
  mutate(
    diff_real_scram = b_conditionreal - b_conditionscram,
    diff_real_artificial = b_conditionreal - b_conditionartificial,
    diff_artificial_scram = b_conditionartificial - b_conditionscram
  )

prior_density_error = dnorm(0, mean = 0, sd = sqrt(2) * 0.7)

pd_real_scram_error = dnorm(
  0,
  mean = mean(posterior_error$diff_real_scram),
  sd = sd(posterior_error$diff_real_scram)
)
pd_real_artificial_error = dnorm(
  0,
  mean = mean(posterior_error$diff_real_artificial),
  sd = sd(posterior_error$diff_real_artificial)
)
pd_artificial_scram_error = dnorm(
  0,
  mean = mean(posterior_error$diff_artificial_scram),
  sd = sd(posterior_error$diff_artificial_scram)
)

BF_real_scram_error = prior_density_error / pd_real_scram_error
BF_real_artificial_error = prior_density_error / pd_real_artificial_error
BF_artificial_scram_error = prior_density_error / pd_artificial_scram_error
