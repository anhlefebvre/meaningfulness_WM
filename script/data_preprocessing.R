rm(list = ls())

library(brms)
library("tidyverse")
library(here)

get_main_data = function(data_path) {
  data = read_delim(data_path)
  
  data_main = data %>%
    filter(phase == "Main", cheating == "no" & seriousness == "yes") %>%
    filter(condition %in% c("artificial", "scram", "real")) %>%
    select(participant_id, phase, trial_number, condition, probe_index, probe_image,
           selected_image, selected_type, correct, response_time)
  return(data_main)
}

### Main ###
data_path = here("data", "data_raw.txt")
main_data = get_main_data(data_path)

write_delim(main_data, here("data", "main_data.txt"), delim = "\t")

