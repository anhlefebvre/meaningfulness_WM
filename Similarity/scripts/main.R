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
    filter(condition %in% c("scram")) %>%
    select(participant_id, phase, trial_number, condition, probe_image, selected_image, correct,
           item0_image, item1_image, item2_image, item3_image, item4_image, item5_image, 
           response0_image, response1_image, response2_image, response3_image, response4_image, response5_image
           )

  n_after_initial_filter = n_distinct(data_main$participant_id)
  cat("Removed due to cheating or low seriousness:", n_all - n_after_initial_filter, "participants\n")
  
  cleaned_data = data_main
  
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
  
  return(cleaned_data)
}


### Main ###
data_path = here("Exp1/Exp1_data", "data_raw.txt")
main_data = get_main_data(data_path, exclude_potential_cheaters = FALSE)

similarity_path = here("Similarity/CNN pairwise", "similarities_scrambled_long.txt")
similarity_data = read_delim(similarity_path)%>%
  mutate(pair_key = paste(file1, file2, sep="_"),
         pair_key_rev = paste(file2, file1, sep="_"))


### Lookup similarity for each item_image
similarities_lookup = function(main_data, similarity_data, new_cols, prefix){
  similarity_results = main_data %>%
    pivot_longer(cols = all_of(new_cols),
                 names_to = "slot", values_to = "compared_image") %>%
    mutate(pair1 = paste(probe_image, compared_image, sep = "_"),
           pair2 = paste(compared_image, probe_image, sep = "_"))
  
  similarity_results = similarity_results %>%
    left_join(similarity_data %>% select(pair_key, similarity), by = c("pair1" = "pair_key")) %>%
    rename(sim1 = similarity) %>%
    left_join(similarity_data %>% select(pair_key_rev, similarity), by = c("pair2" = "pair_key_rev")) %>%
    mutate(similarity = coalesce(sim1, similarity)) %>%
    select(-pair1, -pair2, -sim1, -compared_image)
  
  similarity_results = similarity_results %>%
    pivot_wider(names_from = slot, values_from = similarity, names_prefix = paste0("sim_", prefix, "_"))
  
  similarity_results = bind_cols(main_data, similarity_results %>% select(-participant_id, -trial_number))
  return(similarity_results)
}


if (!exists("check_similarity_stimuli")) {
  check_similarity_stimuli = similarities_lookup(
    main_data,
    similarity_data,
    new_cols = paste0("item", 0:5, "_image"),
    prefix = "item"
  ) %>%
    select(-starts_with("item"), -starts_with("response"))
}

if (!exists("check_similarity_response")) {
  check_similarity_response = similarities_lookup(
    main_data,
    similarity_data,
    new_cols = paste0("response", 0:5, "_image"),
    prefix = "resp"
  ) %>%
    select(-starts_with("item"), -starts_with("response"))
}
