rm(list = ls())

library(brms)
library("tidyverse")
library(here)
library(ggplot2)


get_main_data = function(data_path, exclude_potential_cheaters = TRUE, cut_off = 0.9, exclude_chance_performers = TRUE,
                         min_correct_chance = 13) {
  data = read_delim(data_path)
  print(length(unique(data$participant_id)))
  
  all_main = data %>% filter(phase == "Main")
  n_all = n_distinct(all_main$participant_id)
  
  data_main = data %>%
    filter(cheating == "no" & seriousness == "yes") %>%
    filter(condition %in% c("artificial", "real","scram")) %>%
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

similarity_lookup = function(similarity_data){
  similarity_data %>%
    mutate(pair = map2_chr(file1, file2, ~ paste(sort(c(.x, .y)), collapse = "_"))) %>%
    select(pair, similarity) %>%
    deframe()
}

get_similarity_condition = function(condition){
  data_name = switch(condition,
                     "scram" = "similarities_scrambled_long.txt",
                     "artificial" = "similarities_artificial_long.txt",
                     "real" = "similarities_real_long.txt",
                     stop("Invalid condition: must be 'scram', 'artificial', or 'real'"))
  similarity_path = here("Similarity/CNN pairwise", data_name)
  read_delim(similarity_path)
}

similarity_condition = list(
  artificial = similarity_lookup(get_similarity_condition("artificial")),
  real = similarity_lookup(get_similarity_condition("real")),
  scram = similarity_lookup(get_similarity_condition("scram"))
)

get_similarity_score = function(condition, probe, selected) {
  key = paste(sort(c(probe, selected)), collapse = "_")
  sim_table = similarity_condition[[condition]]
  return(sim_table[[key]] %||% NA)
}

### Main Exp 1 ###
data_path = here("Exp1/Exp1_data", "data_raw.txt")
main_data = get_main_data(data_path, exclude_potential_cheaters = FALSE)
similarity_data_path = here("Exp1/Exp1_data_analysis/models", "similarity_data.rds")


if (file.exists(similarity_data_path)) {
  similarity_data = readRDS(similarity_data_path)
} else {
  similarity_data = main_data %>%
    rowwise() %>%
    mutate(
      similarity_probed_selected = get_similarity_score(condition, probe_image, selected_image),
      similarity_probed_response0 = get_similarity_score(condition, probe_image, response0_image),
      similarity_probed_response1 = get_similarity_score(condition, probe_image, response1_image),
      similarity_probed_response2 = get_similarity_score(condition, probe_image, response2_image),
      similarity_probed_response3 = get_similarity_score(condition, probe_image, response3_image),
      similarity_probed_response4 = get_similarity_score(condition, probe_image, response4_image),
      similarity_probed_response5 = get_similarity_score(condition, probe_image, response5_image)
    ) %>%
    ungroup()
  
  saveRDS(similarity_data, file = similarity_data_path)
}  


#test similarity between probed and selected items in incorrect trials
test_probed_selected = function(main_data, cond) {
  cat("Condition:", cond, "\n")
  incorrect_data = main_data%>%
    filter(condition == cond, correct == 0)
  cat("Mean:", mean(incorrect_data$similarity_probed_selected, na.rm = TRUE), "\n")
  test = t.test(incorrect_data$similarity_probed_selected, mu = 0, alternative = "greater")
  print(test)
}

for (cond in c("real", "artificial", "scram")) {
  test_probed_selected(similarity_data, cond)
}

# test similarity between responses

