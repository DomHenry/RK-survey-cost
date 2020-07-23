cost_max_plot <- function(x,
                          three_study_duration,
                          four_transect_distance) {

  three_study_duration <-  as.character(sort(three_study_duration))
  four_transect_distance <- sort(four_transect_distance)
  
  cbp2 <- c(
    "#000000", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
  )
  x %>%
    filter(carcass_detection == 100) %>%
    distinct(transect_distance, study_duration_days, total_cost) %>%
    ggplot(aes(x = study_duration_days, y = total_cost, fill = transect_distance)) +
    geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.8)) +
    scale_x_discrete(limits = three_study_duration) +
    xlab("Study duration (days)") +
    ylab("Total study cost") +
    scale_fill_manual(values = cbp2[c(3, 4, 5, 7)], limits = four_transect_distance) +
    labs(fill = str_wrap("Transect distance (km)", width = 15)) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      strip.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    ) +
    scale_y_continuous()
}
