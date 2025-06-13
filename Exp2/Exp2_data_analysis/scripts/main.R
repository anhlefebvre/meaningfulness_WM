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
      is_both    = if_else(total_correct == 2, 1, 0),
      is_2AFC    = if_else(total_correct == 1, 1, 0),
      is_none    = if_else(total_correct == 0, 1, 0)
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

# # Bayesian 
# data_m3 = main_data %>%
#   group_by(participant_id, condition) %>%
#   summarise(
#     target = sum(selected_type == "target"),
#     within_list = sum(selected_type == "within_list"),
#     extra_list = sum(selected_type == "extra_list"),
#   ) %>%
#   ungroup()
# 
# data_m3 = data_m3 %>%
#   rename(
#     target = target,
#     within = within_list,
#     extra = extra_list,
#     id = participant_id,
#     cond = condition
#   )
# 
# #Initiate m3 model objects
# m3_model = m3(
#   resp_cats = c("target", "within", "extra"),
#   num_options = c(1, 2, 3),
#   choice_rule = "softmax",
#   links = list(
#     a = "log",
#     c = "log"
#   ),
#   default_priors = list(
#     a = list(main = "normal(0, 0.7)", effect = "normal(0, 0.7)"),
#     c = list(main = "normal(0, 0.7)", effect = "normal(0, 0.7)")
#   )
# )
# 
# #specify the model formula 
# m3_formula = bmf(
#   target ~ b + a + c,
#   within ~ b + a,
#   extra  ~ b,
#   c ~ 0 + cond + (0 + cond | id),  
#   a ~ 0 + cond + (0 + cond | id)
# )
