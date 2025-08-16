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

#P(correct) - 2AFC
summary_p_correct_2AFC = summarize_plot(
  .data = main_data,
  DV = correct_2AFC,
  IV_within = condition,
  ID = participant_id,
  name_DV = "P(correct)"
)

#P(correct) - 6AFC
summary_p_correct_6AFC = summarize_plot(
  .data = main_data,
  DV = correct_6AFC,
  IV_within = condition,
  ID = participant_id,
  name_DV = "P(correct)"
)

summary_p_correct$type = "Total Correct (Both)"
summary_p_correct_2AFC$type = "2AFC Correct"

summary_combined = bind_rows(summary_p_correct, summary_p_correct_2AFC)


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

### Accuracy

#2AFC
accuracy_2AFC_path = here("Exp2/Exp2_data_analysis/models", "accuracy_2AFC_model.rds")
dir.create(dirname(accuracy_2AFC_path), showWarnings = FALSE, recursive = TRUE)

if (file.exists(accuracy_2AFC_path)) {
  cat("Already computed, loading ", accuracy_2AFC_path, "\n")
  accuracy_2AFC_model = readRDS(accuracy_2AFC_path)
} else {
  cat("Run 2AFC model")
  accuracy_2AFC_model = brm(
    correct_2AFC ~ 0 + condition + (0 + condition | participant_id),
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
  saveRDS(accuracy_2AFC_model, accuracy_2AFC_path)
  cat("Save the model: ", accuracy_2AFC_path, "\n")
}

summary(accuracy_2AFC_model)

pd_2AFC = as_draws_df(accuracy_2AFC_model) %>%
  mutate(
    diff_real_scram_2AFC      = b_conditionreal - b_conditionscram,
    diff_real_artificial_2AFC = b_conditionreal - b_conditionartificial,
    diff_artificial_scram_2AFC = b_conditionartificial - b_conditionscram
  )

prior_density = dnorm(0, mean = 0, sd = sqrt(2) * 0.7)

pd_real_scram_2AFC = dnorm(
  0,
  mean = mean(pd_2AFC$diff_real_scram_2AFC),
  sd   = sd(pd_2AFC$diff_real_scram_2AFC)
)
pd_real_artificial_2AFC = dnorm(
  0,
  mean = mean(pd_2AFC$diff_real_artificial_2AFC),
  sd   = sd(pd_2AFC$diff_real_artificial_2AFC)
)
pd_artificial_scram_2AFC = dnorm(
  0,
  mean = mean(pd_2AFC$diff_artificial_scram_2AFC),
  sd   = sd(pd_2AFC$diff_artificial_scram_2AFC)
)

BF_real_scram_accuracy_2AFC      = prior_density / pd_real_scram_2AFC
BF_real_artificial_accuracy_2AFC = prior_density / pd_real_artificial_2AFC
BF_artificial_scram_accuracy_2AFC = prior_density / pd_artificial_scram_2AFC

###Acuracy different across conditions - 2AFC
H1_2AFC_path = here("Exp2/Exp2_data_analysis/models", "H1_accuracy_2AFC_model.rds")
H0_2AFC_path = here("Exp2/Exp2_data_analysis/models", "H0_accuracy_2AFC_model.rds")
BF_2AFC_path = here("Exp2/Exp2_data_analysis/models", "BF_2AFC_H1_vs_H0.rds")
dir.create(dirname(H1_2AFC_path), showWarnings = FALSE, recursive = TRUE)

if (file.exists(H1_2AFC_path)) {
  cat("Already computed, loading ", H1_2AFC_path, "\n")
  H1_2AFC = readRDS(H1_2AFC_path)
} else {
  cat("Fitting H1 2AFC model\n")
  H1_2AFC = brm(
    correct_2AFC ~ 0 + condition + (0 + condition | participant_id),
    data = main_data,
    family = bernoulli(),
    prior = c(
      prior(normal(0, 0.7), class = "b"),
      prior(exponential(1), class = "sd")
    ),
    chains = 4, cores = 4, iter = 13500, warmup = 1000,
    save_pars = save_pars(all = TRUE)
  )
  saveRDS(H1_2AFC, H1_2AFC_path)
}

if (file.exists(H0_2AFC_path)) {
  cat("Already computed, loading ", H0_2AFC_path, "\n")
  H0_2AFC = readRDS(H0_2AFC_path)
} else {
  cat("Fitting H0 2AFC model\n")
  H0_2AFC = brm(
    correct_2AFC ~ 1 + (1 | participant_id),
    data = main_data,
    family = bernoulli(),
    prior = c(
      prior(normal(0, 0.7), class = "Intercept"),
      prior(exponential(1), class = "sd")
    ),
    chains = 4, cores = 4, iter = 13500, warmup = 1000,
    save_pars = save_pars(all = TRUE)
  )
  saveRDS(H0_2AFC, H0_2AFC_path)
}

if (file.exists(BF_2AFC_path)) {
  cat("Already computed, loading ", BF_2AFC_path, "\n")
  BF_2AFC_10 = readRDS(BF_2AFC_path)
} else {
  cat("Computing bridge-sampling BF for 2AFC models\n")
  bridge_H1_2AFC = bridge_sampler(H1_2AFC)
  bridge_H0_2AFC = bridge_sampler(H0_2AFC)
  BF_2AFC_10 = bf(bridge_H1_2AFC, bridge_H0_2AFC)
  saveRDS(BF_2AFC_10, BF_2AFC_path)
}

### Total

accuracy_total_path = here("Exp2/Exp2_data_analysis/models", "accuracy_total_model.rds")
dir.create(dirname(accuracy_total_path), showWarnings = FALSE, recursive = TRUE)

if (file.exists(accuracy_total_path)) {
  cat("Already have it, loading ", accuracy_total_path, "\n")
  accuracy_total_model = readRDS(accuracy_total_path)
} else {
  cat("Run the model\n")
  accuracy_total_model = brm( 
    formula = brms::bf(
      total_correct | trials(2) ~ 0 + condition + (0 + condition | participant_id)
    ), 
    data = main_data, 
    family = binomial(), 
    prior = c( prior(normal(0, 0.7), class = "b"), prior(exponential(1), class = "sd") ), 
    chains = 4, 
    cores = 4, 
    iter = 13500, 
    warmup = 1000 
  )
  saveRDS(accuracy_total_model, accuracy_total_path)
  cat("Save the model: ", accuracy_total_path, "\n")
}

summary(accuracy_total_model)

pd_total = as_draws_df(accuracy_total_model) %>%
  mutate(
    diff_real_scram_total = b_conditionreal - b_conditionscram,
    diff_real_artificial_total = b_conditionreal - b_conditionartificial,
    diff_artificial_scram_total = b_conditionartificial - b_conditionscram
  )

pd_real_scram_total = dnorm(
  0,
  mean = mean(pd_total$diff_real_scram_total),
  sd = sd(pd_total$diff_real_scram_total)
)
pd_real_artificial_total = dnorm(
  0,
  mean = mean(pd_total$diff_real_artificial_total),
  sd = sd(pd_total$diff_real_artificial_total)
)
pd_artificial_scram_total = dnorm(
  0,
  mean = mean(pd_total$diff_artificial_scram_total),
  sd = sd(pd_total$diff_artificial_scram_total)
)

BF_real_scram_accuracy_total = prior_density / pd_real_scram_total
BF_real_artificial_accuracy_total = prior_density / pd_real_artificial_total
BF_artificial_scram_accuracy_total = prior_density / pd_artificial_scram_total

### total - difference across conditions
H1_total_path = here("Exp2/Exp2_data_analysis/models", "H1_accuracy_total_model.rds")
H0_total_path = here("Exp2/Exp2_data_analysis/models", "H0_accuracy_total_model.rds")
BF_total_path = here("Exp2/Exp2_data_analysis/models", "BF_total_H1_vs_H0.rds")
dir.create(dirname(H1_total_path), showWarnings = FALSE, recursive = TRUE)

if (file.exists(H1_total_path)) {
  cat("Already computed, loading ", H1_total_path, "\n")
  H1_total = readRDS(H1_total_path)
} else {
  cat("Fitting H1 total model\n")
  H1_total = brm(
    formula = brms::bf(
      total_correct | trials(2) ~ 0 + condition + (0 + condition | participant_id)
    ),
    data = main_data,
    family = binomial(),
    prior = c(
      prior(normal(0, 0.7), class = "b"),
      prior(exponential(1), class = "sd")
    ),
    chains = 4, cores = 4, iter = 13500, warmup = 1000,
    save_pars = save_pars(all = TRUE)
  )
  saveRDS(H1_total, H1_total_path)
}

if (file.exists(H0_total_path)) {
  cat("Already computed, loading ", H0_total_path, "\n")
  H0_total = readRDS(H0_total_path)
} else {
  cat("Fitting H0 total model\n")
  H0_total = brm(
    formula = brms::bf(
      total_correct | trials(2) ~ 1 + (1 | participant_id)
    ),
    data = main_data,
    family = binomial(),
    prior = c(
      prior(normal(0, 0.7), class = "Intercept"),
      prior(exponential(1), class = "sd")
    ),
    chains = 4, cores = 4, iter = 13500, warmup = 1000,
    save_pars = save_pars(all = TRUE)
  )
  saveRDS(H0_total, H0_total_path)
}

if (file.exists(BF_total_path)) {
  cat("Already computed, loading ", BF_total_path, "\n")
  BF_total_10 = readRDS(BF_total_path)
} else {
  cat("Computing bridge-sampling BF for TOTAL models\n")
  br_H1_total = bridge_sampler(H1_total)
  br_H0_total = bridge_sampler(H0_total)
  BF_total_10 = bf(br_H1_total, br_H0_total)  
  saveRDS(BF_total_10, BF_total_path)
}

print(BF_total_10)


