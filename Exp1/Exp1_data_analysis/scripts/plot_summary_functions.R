####write a function to calculate 95% (within) CI in a tidyverse framework by correcting for within-subject factors
summarize_plot = function(.data, DV, IV_within = NULL, IV_between = NULL, ID, CI = 0.95, name_DV = "mean"){
  
  ### if we only have between-subject variables, compute the usual confidence interval without any correction
  if (missing(IV_within)) {
    .data %>%
      ###first aggregate the data on the level of participants
      dplyr::group_by(dplyr::across(c({{ ID }}, {{ IV_between }}))) %>%
      dplyr::summarize(
        sub_DV = base::mean({{ DV }}, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      ###aggregate data on level of plot
      dplyr::group_by(dplyr::across(c({{ IV_between }}))) %>%
      dplyr::summarize(
        mean = base::mean(sub_DV, na.rm = TRUE),
        n = dplyr::n(),
        sd = stats::sd(sub_DV, na.rm = TRUE),
        se = stats::sd(sub_DV, na.rm = TRUE)/base::sqrt(n),
        CI_factor = stats::qt(1 - ((1 - CI) / 2), n - 1),
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(CI_low = mean - CI_factor*se,
                    CI_high = mean + CI_factor*se) %>%
      dplyr::rename({{ name_DV }} := mean)
  }
  
  
  ### if we only have within-subject variables, compute the within-subject confidence interval and apply the Morey-correction
  else if (missing(IV_between)) {
    .data %>%
      ###first aggregate the data on the level of participants
      dplyr::group_by(dplyr::across(c({{ ID }}, {{ IV_within }}))) %>%
      dplyr::summarize(
        sub_DV = base::mean({{ DV }}, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      ###adjust the data to compute the within-subject se
      dplyr::group_by(dplyr::across(c({{ ID }}))) %>%
      dplyr::mutate(participant_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(grand_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
      dplyr::mutate(DV_adjusted = sub_DV - participant_mean + grand_mean) %>%
      ###aggregate data on level of plot
      dplyr::group_by(dplyr::across(c({{ IV_within }}))) %>%
      dplyr::summarize(
        mean = base::mean(DV_adjusted, na.rm = TRUE),
        n = dplyr::n(),
        sd = stats::sd(DV_adjusted, na.rm = TRUE),
        se = stats::sd(DV_adjusted, na.rm = TRUE)/base::sqrt(n),
        CI_factor = stats::qt(1 - ((1 - CI) / 2), n - 1),
      ) %>%
      dplyr::ungroup() %>%
      ###add the Morey-correction to the data
      dplyr::mutate(morey_correction = base::sqrt(dplyr::n()/(dplyr::n() - 1)),
                    se = se * morey_correction,
                    sd = sd * morey_correction) %>%
      dplyr::mutate(CI_low = mean - CI_factor*se,
                    CI_high = mean + CI_factor*se) %>%
      dplyr::rename({{ name_DV }} := mean) %>% 
      dplyr::select(-morey_correction)
  }
  
  
  ### if we get both, between- and within-subject variables, compute the mixed confidence interval by correcting for the within-subject variables
  else{
    .data %>%
      ###first aggregate the data on the level of participants
      dplyr::group_by(dplyr::across(c({{ ID }}, {{ IV_between }}, {{ IV_within }}))) %>%
      dplyr::summarize(
        sub_DV = base::mean({{ DV }}, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      ###adjust the data to compute the within-subject se
      dplyr::group_by(dplyr::across(c({{ ID }}, {{ IV_between }}))) %>%
      dplyr::mutate(participant_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dplyr::across(c({{ IV_between }}))) %>%
      dplyr::mutate(grand_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(DV_adjusted = sub_DV - participant_mean + grand_mean) %>%
      ###aggregate data on level of plot
      dplyr::group_by(dplyr::across(c({{ IV_between }}, {{ IV_within }}))) %>%
      dplyr::summarize(
        mean = base::mean(DV_adjusted, na.rm = TRUE),
        n = dplyr::n(),
        sd = stats::sd(DV_adjusted, na.rm = TRUE),
        se = stats::sd(DV_adjusted, na.rm = TRUE)/base::sqrt(n),
        CI_factor = stats::qt(1 - ((1 - CI) / 2), n - 1),
      ) %>%
      dplyr::ungroup() %>%
      ###add the Morey-correction to the se
      dplyr::group_by(dplyr::across(c({{ IV_between }}))) %>%
      dplyr::mutate(morey_correction = base::sqrt(dplyr::n()/(dplyr::n() - 1)),
                    se = se * morey_correction,
                    sd = sd * morey_correction) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(CI_low = mean - CI_factor*se,
                    CI_high = mean + CI_factor*se) %>%
      dplyr::select(-morey_correction) %>% 
      dplyr::rename({{ name_DV }} := mean)
  }
}