#' @title Historical Escapement Plot
#' @description
#' Produces a plot of historical escapements with an overlay of the goal range.
#'
#' @param brood_dat A dataframe containing calendar year (yr) and escapement(S).
#' @param goal_dat  A dataframe containing calendar year (yr), the escapement goal
#' lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include
#' years where the goal changed. If the updated analysis resulted in a new
#' escapement goal finding the new finding should be included as the last row with
#' the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#'
#' @return A figure
#'
#' @import tidyverse
#'
#' @examples
#'
#' p_Igushik <- make_age(data_Igushik, 3, 8)
#' brood_Igushik <- make_brood(data_Igushik, p_Igushik)
#'
#' plot_escapement(brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon")
#'
#' @export
plot_escapement <- function(brood_dat,
                   goal_dat,
                   title){
  brood_dat <- brood_dat %>% filter(!is.na(S))

  yr_max <- if(sum(goal_dat$yr == "new") == 0){max(brood_dat$yr)}else{max(brood_dat$yr) + 2}
  goal <-
    goal_dat %>%
    mutate(across(c(lb, ub), function(x){ifelse(is.na(x), -99, x)}),
           yr = as.numeric(ifelse(yr == "new", max(brood_dat$yr) + 1, yr))) %>%
    complete(yr = full_seq(c(yr, yr_max), 1)) %>%
    fill(lb, ub, .direction = "down") %>%
    mutate(across(c(lb, ub), function(x){ifelse(x == -99, NA, x)})) %>%
    pivot_longer(cols = c(lb, ub), names_to = "bound", values_to = "S_bound")

  cap <- str_wrap("Note: Escapement goal lower and upper bounds are shown as solid
                  and dashed lines, respectively. Escapements below the lower bound
                  of the contemporaneous escapement goal are indicated with black fill.",
                  width = 85)

  brood_dat %>%
    select(yr, S) %>%
    left_join(goal[goal$bound == "lb", ], by = "yr") %>%
    mutate(miss = ifelse(S >= S_bound | is.na(S_bound), FALSE, TRUE)) %>%
    ggplot(aes(x = yr)) +
    geom_bar(aes(y = S, fill = miss), stat = "identity") +
    geom_line(aes(y = S_bound, linetype = bound), data = goal) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("grey75", "black")) +
    theme_bw(base_size = 16) +
    labs(
      title = title,
      x = "Year",
      y = "Escapement",
      caption = cap) +
    theme(text = element_text(family = "sans"),
          plot.caption = element_text(
            hjust = 0,
            size = 10),
          plot.caption.position = "plot",
          legend.position = "none")
}

# For backwards compatibility
plot_S <- plot_escapement
