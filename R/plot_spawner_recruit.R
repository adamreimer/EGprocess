#' @title Spawner-Recruit Plot
#' @description
#' Produces a SR plot with an overlay of Smsy and the goal range.
#'
#' @param posterior_data A dataframe containing lnalpha, beta, phi, and sigma.
#' Can handle point estimates (input as a single row) or mcmc samples (input as multiple rows)
#' @param brood_data A dataframe containing year (yr), Spawners (S), and Recruits (R)
#' to be included in the plot. The data frame should include years without empirical
#' observations of S and R.
#' @param goal_data  A dataframe containing calendar year (yr), the escapement goal
#' lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include
#' years where the goal changed. If the updated analysis resulted in a new escapement
#' goal finding the new finding should be included as the last row with the year
#' labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#' @param multiplier The Shiny app uses a multiplier to scale beta. Input that here. Defaults to 1.
#'
#' @return A figure
#'
#' @import dplyr tibble tidyr ggplot2 stringr
#' @importFrom magrittr %>%
#' @importFrom scales comma
#'
#' @examples
#'
#' p_Igushik <- make_age(data_Igushik, 3, 8)
#' brood_Igushik <- make_brood(data_Igushik, p_Igushik)
#'
#' plot_SR(posterior_data = post_Igushik_byr63_15, brood_data = brood_Igushik,
#' goal_data = goal_Igushik, title = "Igushik River Sockeye Salmon", multiplier = 1e-5)
#'
#' @export

plot_SR <- function(posterior_data,
                    brood_data,
                    goal_data,
                    title,
                    multiplier){
  param_50 <-
    data.frame(beta = posterior_data[["beta"]] * multiplier,
               lnalpha = posterior_data[["lnalpha"]],
               phi = posterior_data[["phi"]],
               sigma = posterior_data[["sigma"]]) %>%
    dplyr::summarise(beta = median(beta),
              lnalpha = median(lnalpha),
              phi = median(phi),
              sigma = median(sigma),
              Smsy = lnalpha / beta * (0.5000002 - 0.07 * lnalpha))

  # Identify brood years added since the last goal change.
  # Likely fragile. Need Hamachan to output some of this stuff.
  max_age <- length(brood_data$yr[is.na(brood_data$R) & !is.na(brood_data$S)])
  byr_modified <- suppressWarnings(max(as.numeric(goal_data$yr[goal_data$yr != "new"])) - max_age)
  brood_data <-
    brood_data %>%
    mutate(update = ifelse(yr > byr_modified, "updated", "existing")) %>%
    select(yr, S, R, update) %>%
    filter(complete.cases(.))

  upper_x = max(brood_data$S) * 1.05
  upper_y = max(brood_data$R) * 1.05

  cap_width = 85
  cap <-
    case_when(
      sum(brood_data$update == "existing") == 0 ~ str_wrap("Note: The dashed line represents
      the 1:1 line; points above this line represent spawning events that produced a
      harvestable surplus. The vertical line shows Smsy. The current escapement
      goal range is shaded gray.", width = cap_width),
      sum(brood_data$update == "updated") > 0 ~ str_wrap("Note: Filled circles
      indicated observations added to the dataset since the escapement goal last
      changed. The dashed line represents the 1:1 line; points above this line
      represent spawning events that produced a harvestable surplus. The vertical
      line shows Smsy. The current escapement goal range is shaded gray.", width = cap_width)
    )

  ggplot2::ggplot(brood_data, ggplot2::aes(x = S, y = R)) +
    ggplot2::geom_point(aes(shape = update), size = 2) +
    ggplot2::stat_function(fun=function(x){x * exp(param_50$lnalpha - param_50$beta * x)},
                           linewidth = 1.5,
                           xlim = c(0, upper_x)) +
    ggplot2::scale_x_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper_x), ylim = c(0, upper_y)) +
    ggplot2::geom_abline(slope = 1, linewidth = 1, linetype = 2) +
    ggplot2::geom_vline(xintercept = param_50$Smsy) +
    ggplot2::geom_rect(ggplot2::aes(xmin = lb, xmax = ub, ymin = -Inf, ymax = Inf),
                       data = goal_data[dim(goal_data)[1], ],
                       inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
    ggplot2::theme_bw(base_size = 16) +
    scale_shape_manual(values = c("updated" = 16, "existing" = 1)) +
    labs(
      title = title,
      subtitle = paste0("Brood Years:", min(brood_data$yr), " - ", max(brood_data$yr)),
      x = "Escapement",
      y = "Recruitment",
      caption = cap) +
    theme(text = element_text(family = "sans"),
          plot.caption = element_text(
            hjust = 0,
            size = 10),
          plot.subtitle = element_text(size = 10),
          plot.caption.position = "plot",
          legend.position = "none")
}

