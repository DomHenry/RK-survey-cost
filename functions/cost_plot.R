cost_plot <- function(x, survey_speed_input,
                      maxcost, maxcost_amount) {
  cbp2 <- c(
    "#000000", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
  )

  detect_labs <- c(
    "50" = "50% detection",
    "75" = "75% detection"
  )

  p <- x %>%
    mutate(
      carcass_detection = as.character(carcass_detection),
      study_duration_days = glue("{study_duration_days} days")
    ) %>%
    filter(!carcass_detection == 100) %>%
    ggplot(aes(x = persistence, y = total_cost, color = transect_distance, group = carcass_detection)) +
    geom_point(size = 2) +
    geom_line(aes(group = transect_distance), size = 1) +
    labs(title = glue("Survey speed: {survey_speed_input}km/h")) +
    scale_colour_manual(values = cbp2[c(3, 4, 5, 7)]) +
    labs(color = str_wrap("Transect distance (km)", width = 15)) +
    xlab("Mean carcass persistence (days)") +
    ylab("Total study cost") +
    theme_bw() +
    theme(
      title = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      strip.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    ) +
    scale_y_continuous(labels = scales::dollar_format()) +
    facet_grid(carcass_detection ~ study_duration_days,
      labeller = labeller(carcass_detection = detect_labs)
    )

  if (maxcost == "yes") {
    p <- p +
      geom_hline(yintercept = maxcost_amount, col = "red", linetype = "dashed", size = 0.8)
    return(p)
  } else {
    return(p)
  }
}
