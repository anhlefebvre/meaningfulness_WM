rm(list = ls())

library(brms)
library("tidyverse")
library(here)

get_main_data = function(data_path, exclude_potential_cheaters = TRUE, cut_off = 0.9, exclude_chance_performers = TRUE,
                         min_correct_chance = 13) {
  data = read_delim(data_path)
  print(length(unique(data$participant_id)))
  
  all_main = data %>% filter(phase == "Main")
  n_all = n_distinct(all_main$participant_id)
  
  data_main = data %>%
    filter(cheating == "no" & seriousness == "yes") %>%
    filter(condition %in% c("artificial", "scram", "real")) %>%
    select(participant_id, phase, trial_number, condition, probe_index, probe_image,
           selected_image, selected_type, correct, response_time)
  
  n_after_initial_filter = n_distinct(data_main$participant_id)
  cat("Removed due to cheating or low seriousness:", n_all - n_after_initial_filter, "participants\n")
  
  write_delim(data_main, here("data", "main_data_without_exclusion.txt"), delim = "\t")
  
  cleaned_data = data_main
  
  if (exclude_potential_cheaters) {
    p_correct_per_participant = data_main %>%
      group_by(participant_id) %>%
      summarise(p_correct = mean(correct), .groups = "drop")
    
    potential_cheater = p_correct_per_participant %>%
      filter(p_correct >= cut_off) %>%
      pull(participant_id)
    
    cat("Cheaters removed: \n")
    print(potential_cheater)
    
    cleaned_data = cleaned_data %>%
      filter(!(participant_id %in% potential_cheater))
    
    print_alert = data_main %>%
      left_join(p_correct_per_participant, by = "participant_id") %>%
      mutate(alert_cheating = p_correct >= cut_off)
    write_delim(print_alert, here("data", "alert_cheating.txt"), delim = "\t")
  }
  
  if (exclude_chance_performers) {
    chance_performers = data_main %>%
      group_by(participant_id, condition) %>%
      summarise(n_correct = sum(correct), .groups = "drop") %>%
      group_by(participant_id) %>%
      summarise(min_correct_across_condition = min(n_correct), .groups = "drop") %>%
      filter(min_correct_across_condition <= min_correct_chance) %>%
      pull(participant_id)
    
    cleaned_data = cleaned_data %>%
      filter(!(participant_id %in% chance_performers))
    
    cat("Chance performers removed:\n")
    print(chance_performers)
  }
  
  total_removed = union(
    if (exists("potential_cheater")) potential_cheater else character(0),
    if (exists("chance_performers")) chance_performers else character(0)
  )
  cat("\nNumber of participants removed due to exclusion criteria:", length(total_removed), "\n")
  
  write_delim(cleaned_data, here("data", "main_data_without_cheaters"), delim = "\t")
  
  return(cleaned_data)
}


### Main ###
data_path = here("data", "data_raw.txt")
main_data = get_main_data(data_path, exclude_potential_cheaters = FALSE)


