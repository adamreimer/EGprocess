#' @title Expected Yield Plot
#' @description
#' Produces a SR plot with an overlay of Smsy and the goal range.
#'
#' @param profile_data Output of the get_profile function.
#' @param brood_data A dataframe containing year (yr), Spawners (S), and Recruits (R) to be included in the plot. The data frame should include years without empirical observations of S and R.
#' @param goal_data  A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. If the updated analysis resulted in a new escapement goal finding the new finding should be included as the last row with the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#'
#' @return A figure
#'
#' @import tidyverse
#'
#' @examples
#'
#' p_Igushik <- make.age(data_Igushik, 3, 8)
#' brood_Igushik <- make.brood(data_Igushik, p_Igushik)
#'
#' profile_Igushik <- get_profile(post_Igushik_byr63-15, multiplier = 1e-5)
#'
#' plot_ey(profile_Igushik, brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon")
#'
#' @export

plot_ey <- function(profile_data,
                    brood_data,
                    goal_data,
                    title){
  plot_dat <-
    profile_data %>%
    dplyr::select(s, dplyr::starts_with("SY")) %>%
    dplyr::group_by(s) %>%
    dplyr::summarise(median.SY = median(SY, na.rm = TRUE),
                     p25.SY = quantile(SY, probs = 0.25, na.rm = TRUE),
                     p75.SY = quantile(SY, probs = 0.75, na.rm = TRUE)
    ) %>%
    tidyr::gather(Productivity, SY, median.SY)

  goal_range <- as.numeric(goal_data[dim(goal_data)[1], c(2, 3)])

  # Identify brood years added since the last goal change.
  # Likely fragile. Need Hamachan to output some of this stuff.
  max_age <- length(brood_data$yr[is.na(brood_data$R) & !is.na(brood_data$S)])
  byr_modified <- suppressWarnings(max(as.numeric(goal_data$yr[goal_data$yr != "new"])) - max_age)
  brood_data <-
    brood_data %>%
    mutate(update = ifelse(yr > byr_modified, "updated", "existing"),
           Y = R - S) %>%
    select(yr, S, Y, update) %>%
    filter(complete.cases(.))

  ymax <- max(brood_data$Y) * 1.05
  ymin <- if(min(brood_data$Y) < 0){min(brood_data$Y) * 1.05} else{0}
  xmax <- max(brood_data$S) * 1.05

  cap_width = 85
  cap <-
    case_when(
      sum(brood_data$update == "existing") == 0 ~ str_wrap("Note: The current escapement goal range is shaded gray.", width = cap_width),
      sum(brood_data$update == "updated") > 0 ~ str_wrap("Note: Filled circles indicated observations added to the dataset since the escapement goal was last modified. The current escapement goal range is shaded gray.", width = cap_width)
    )

  ggplot2::ggplot(brood_data, aes(x = S, y = Y)) +
    geom_point(aes(shape = update), size = 2) +
    ggplot2::geom_line(ggplot2::aes(x = s, y = SY, color = Productivity), data = plot_dat) +
    ggplot2::geom_ribbon(ggplot2::aes(x = s, ymin = p25.SY, ymax = p75.SY), data = plot_dat, inherit.aes = FALSE, alpha = 0.1) +
    ggplot2::geom_rect(ggplot2::aes(xmin = lb, xmax = ub, ymin = -Inf, ymax = Inf),
                       data = goal_data[dim(goal_data)[1], ],
                       inherit.aes = FALSE, fill = "gray", alpha = 0.2) +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, xmax), ylim = c(ymin, ymax)) +
    ggplot2::scale_color_manual(guide = "none", values = "black") +
    scale_shape_manual(values = c("updated" = 16, "existing" = 1)) +
    ggplot2::theme_bw(base_size = 16) +
    labs(
      title = title,
      subtitle = paste0("Brood Years:", min(brood_data$yr), " - ", max(brood_data$yr)),
      x = "Escapement",
      y = "Yield",
      caption = cap) +
    theme(text = element_text(family = "sans"),
          plot.caption = element_text(
            hjust = 0,
            size = 10),
          plot.subtitle = element_text(size = 10),
          plot.caption.position = "plot",
          legend.position = "none")
}
