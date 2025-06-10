rm(list = ls())

library(brms)
library("tidyverse")
library(here)

get_main_data = function(data_path, exclude_potential_cheaters = TRUE, cut_off = 0.9, exclude_chance_performers = TRUE,
                         min_correct_chance = 31) {
  data = read_delim(data_path)
  cat("Total number of participants: ", length(unique(data$participant_id)), "\n")
  
  check_duplicate(data)
  data_main = check_missing_trials(data)

  all_main = data %>% filter(phase == "Main")
  n_all = n_distinct(all_main$participant_id)
  
  data_main = data_main %>%
    filter(cheating == "no" & seriousness == "yes") %>%
    filter(condition %in% c("artificial", "scram", "real")) %>%
    select(participant_id, phase, component_id, sender, sender_id, trial_number, condition,
           probe_index, probe_location, probe_image,
           selected_location_2AFC, selected_image_2AFC, selected_type_2AFC, correct_2AFC,
           selected_location_6AFC, selected_index_6AFC, selected_image_6AFC, correct_6AFC, 
           total_correct,
           item0_index, item0_location, item0_image,
           item1_index, item1_location, item1_image,
           item2_index, item2_location, item2_image,
           item3_index, item3_location, item3_image,
           item4_index, item4_location, item4_image,
           item5_index, item5_location, item5_image,
           response0_location, response0_image, response0_type,
           response1_location, response1_image, response1_type,
           start_duration, fixation_duration, presentation_duration, retention_duration,
           warnings, cheating, seriousness)
  
  n_after_initial_filter = n_distinct(data_main$participant_id)
  cat("Number of participant after removing cheating and seriousness", n_after_initial_filter, "\n")
  ### REMOVE CHANCE PERFORMERS ###
  
  if (exclude_chance_performers) {
    chance_performers = data_main %>%
      group_by(participant_id, condition) %>%
      summarise(n_correct = sum(correct_2AFC), .groups = "drop") %>%
      group_by(participant_id) %>%
      summarise(min_correct_across_condition = min(n_correct), .groups = "drop") %>%
      filter(min_correct_across_condition < min_correct_chance) %>%
      pull(participant_id)
    
    data_main = data_main %>%
      filter(!(participant_id %in% chance_performers))
    
    cat("Removed participants who is chance performers:", chance_performers, "\n")
    
  } else {
    cat("No exclusion for chance performance was applied")
  }
  
  cat("Total participants after cleaning ", n_distinct(data_main$participant_id))

  #write_delim(cleaned_data, here("Exp2/data", "main_data_without_cheaters"), delim = "\t")
  
  return(data_main)
}

### CHECK FOR DUPLICATION ###
check_duplicate = function(main_data) {
  data_checked = main_data %>%
    rowwise() %>%
    mutate(
      item_set_signature = paste0(
        sort(c_across(matches("^item[0-5]_image$"))),
        collapse = "_"
      )
    ) %>%
    ungroup()
  
  duplicates = data_checked %>%
    count(item_set_signature) %>%
    filter(n > 1)
  
  if (nrow(duplicates) > 0) {
    cat("WARNING: some stimuli are used more than once - check duplicate_set.txt")
    
    duplicated_trials = data_checked %>%
      filter(item_set_signature %in% duplicates$item_set_signature) %>%
      select(participant_id, trial_number, item_set_signature, starts_with("item"))
    
    print(duplicated_trials)
    write_delim(duplicated_trials, here("Exp2/data", "duplicate_set.txt"), delim = "\t")
  } else {
    cat("All stimuli are used only once!")
  }
}


### REMOVE PARTICIPANTS WITH MISSING TRIALS ###
check_missing_trials = function(data, trials_total = 0:149) {
  incomplete_participants = data %>%
    group_by(participant_id) %>%
    summarise(
      missing_trials = list(setdiff(trials_total, trial_number)),
      n_missing = length(setdiff(trials_total, trial_number))
    )%>%
    filter(n_missing>0)
  if (nrow(incomplete_participants) > 0) {
    cat("WARNING: Following participants data trials are missing: ")
    for (i in seq_len(nrow(incomplete_participants))){
      cat("Participant:", incomplete_participants$participant_id[i], "& missing trials:", toString(incomplete_participants$missing_trials[[i]]), "\n")
    }
    data_clean = data %>%
      filter(!(participant_id %in% incomplete_participants$participant_id))
    cat("Number of participants removed due to missing try ", nrow(incomplete_participants), "\n")
    
  } else {
    cat("No missing trials!")
    data_clean = data
  }
  
  cat("Total participants now: ", n_distinct(data_clean$participant_id), "\n")

  return(data_clean)
    
}

### Main ###
data_path = here("Exp2/Exp2_data", "data_raw.txt")
main_data = get_main_data(data_path, exclude_potential_cheaters = FALSE)

