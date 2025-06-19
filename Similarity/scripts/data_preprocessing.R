rm(list = ls())

library(tidyverse)
library(here)

# Load and clean the data
get_main_data = function(data_path, exclude_potential_cheaters = TRUE, cut_off = 0.9,
                         exclude_chance_performers = TRUE, min_correct_chance = 13) {
  data = read_delim(data_path)
  print(length(unique(data$participant_id)))
  
  all_main = data %>% filter(phase == "Main")
  n_all = n_distinct(all_main$participant_id)
  
  data_main = data %>%
    filter(cheating == "no", seriousness == "yes") %>%
    filter(condition %in% c("artificial", "real", "scram")) %>%
    select(participant_id, phase, trial_number, condition, probe_image, selected_image, correct,
           starts_with("item"), starts_with("response"))
  
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
  
  # Remove unwanted columns
  cleaned_data = cleaned_data %>%
    select(-matches("item[0-9]+_index"),
           -matches("item[0-9]+_location"),
           -matches("response[0-9]+_location"),
           -matches("response[0-9]+_type"),
           -matches("response_time"))
  
  return(cleaned_data)
}

# Load and prepare similarity data
similarity_lookup = function(similarity_data) {
  similarity_data %>%
    mutate(pair = map2_chr(file1, file2, ~ paste(sort(c(.x, .y)), collapse = "_"))) %>%
    select(pair, similarity) %>%
    deframe()
}

get_similarity_condition = function(condition) {
  data_name = switch(condition,
                     "scram" = "similarities_scrambled_long.txt",
                     "artificial" = "similarities_artificial_long.txt",
                     "real" = "similarities_real_long.txt",
                     stop("Invalid condition"))
  similarity_path = here("Similarity/CNN pairwise", data_name)
  read_delim(similarity_path)
}

similarity_condition = list(
  artificial = similarity_lookup(get_similarity_condition("artificial")),
  real = similarity_lookup(get_similarity_condition("real")),
  scram = similarity_lookup(get_similarity_condition("scram"))
)

get_similarity_vec = function(condition_vec, probe_vec, selected_vec) {
  keys = map2_chr(probe_vec, selected_vec, ~ paste(sort(c(.x, .y)), collapse = "_"))
  map2_dbl(condition_vec, keys, ~ similarity_condition[[.x]][[.y]] %||% NA_real_)
}

data_path = here("Exp1/Exp1_data", "data_raw.txt")
main_data = get_main_data(data_path, exclude_potential_cheaters = FALSE)

main_data = main_data %>%
  mutate(similarity_selected_image = get_similarity_vec(condition, probe_image, selected_image)) %>%
  select(-selected_image)
for (i in 0:6) {
  item_col <- paste0("item", i, "_image")
  sim_col <- paste0("similarity_item", i, "_image")
  
  if (item_col %in% colnames(main_data)) {
    main_data[[sim_col]] <- get_similarity_vec(main_data$condition, main_data$probe_image, main_data[[item_col]])
    main_data[[item_col]] <- NULL
  }
}
for (i in 0:6) {
  response_col <- paste0("response", i, "_image")
  sim_col <- paste0("similarity_response", i, "_image")
  
  if (response_col %in% colnames(main_data)) {
    main_data[[sim_col]] <- get_similarity_vec(main_data$condition, main_data$probe_image, main_data[[response_col]])
    main_data[[response_col]] <- NULL
  }
}

file=write_delim(main_data, here("Similarity", "similarity_data.txt"))
print("Finished.")
view(file)