generate_cost_data <- function(mileage_rate, labour_rate,
                               trans_dist_1, trans_dist_2,
                               trans_dist_3, trans_dist_4, survey_speed_input,
                               study_day1, study_day2, study_day3) {
  
  four_transect_distance <- c(trans_dist_1, trans_dist_2, trans_dist_3, trans_dist_4)
  three_study_days <- c(study_day1, study_day2, study_day3)

  survey_lookup <- as_tibble(expand.grid(
    survey_interval = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 15),
    study_duration_days = three_study_days
  )) %>%
    rowwise() %>%
    mutate(optimal_num_surveys = length(seq(from = 1, to = study_duration_days, by = 1 + survey_interval)))

  cost <- expand.grid(
    persistence = c("1", "2", "3", "5", ">10"),
    carcass_detection = c(50, 75, 100),
    study_duration_days = three_study_days,
    transect_distance = four_transect_distance,
    survey_speed = survey_speed_input
  ) %>%
    as_tibble() %>%
    mutate(survey_interval = case_when(
      persistence == "1" & carcass_detection == 100 ~ 0,
      persistence == "2" & carcass_detection == 100 ~ 0,
      persistence == "3" & carcass_detection == 100 ~ 0,
      persistence == "5" & carcass_detection == 100 ~ 0,
      persistence == ">10" & carcass_detection == 100 ~ 0,
      persistence == "1" & carcass_detection == 75 ~ 1,
      persistence == "2" & carcass_detection == 75 ~ 2,
      persistence == "3" & carcass_detection == 75 ~ 4,
      persistence == "5" & carcass_detection == 75 ~ 5,
      persistence == ">10" & carcass_detection == 75 ~ 8,
      persistence == "1" & carcass_detection == 50 ~ 3,
      persistence == "2" & carcass_detection == 50 ~ 4,
      persistence == "3" & carcass_detection == 50 ~ 6,
      persistence == "5" & carcass_detection == 50 ~ 8,
      persistence == ">10" & carcass_detection == 50 ~ 15
    )) %>%
    left_join(survey_lookup, by = c(
      "survey_interval" = "survey_interval",
      "study_duration_days" = "study_duration_days"
    )) %>%
    mutate(
      total_distance_driven = transect_distance * optimal_num_surveys,
      hours_worked = total_distance_driven / survey_speed,
      hours_per_day = hours_worked / optimal_num_surveys,
      mileage_cost = mileage_rate * total_distance_driven,
      labour_cost = labour_rate * hours_worked,
      total_cost = labour_cost + mileage_cost
    ) %>%
    mutate_at(vars(
      persistence, carcass_detection,
      transect_distance, study_duration_days,
      survey_speed
    ), as_factor)

  return(cost)
}
