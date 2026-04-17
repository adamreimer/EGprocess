#' @title OYP Plot
#' @description
#'  Produces a plot of the OYP with an overlay of the goal range.
#'
#' @param profile_data Output of the get_profile function
#' @param goal_data  A dataframe containing calendar year (yr), the escapement
#' goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs
#' to include years where the goal changed. If the updated analysis resulted in
#' a new escapement goal finding the new finding should be included as the last
#' row with the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#' @param limit Upper bound of spawners for plot. Default (NULL) will use 2.25 times S.msy.
#'
#' @seealso [plot_profile_facet()]
#'
#' @return A figure
#'
#' @import dplyr tibble tidyr ggplot2 stringr
#' @importFrom magrittr %>%
#' @importFrom scales comma
#'
#' @examples
#'
#' post_list <-
#'   list(
#'     'Brood Years: 1963-2005' = post_Igushik_byr63_05,
#'     'Brood Years: 1963-2015' = post_Igushik_byr63_15
#'   )
#' profile_list <- lapply(post_list, get_profile, multiplier = 1e-5)
#'
#' plot_profile(profile_data = profile_list[[2]], goal_data = goal_Igushik,
#' title = "Igushik River Sockeye Salmon")
#'
#' @export

plot_profile <- function(profile_data,
                         goal_data,
                         title,
                         limit = NULL){
  S.msy50 <- median(profile_data$S.msy)
  n_OYP <- sum(grepl("OYP\\d+", names(profile_data)))
  name_alternate <- names(profile_data)[!(names(profile_data) %in% c("s", "OYP90", "SY", "S.msy"))]
  pct_alternate <- gsub("\\D", "", name_alternate)

  if(is.null(limit)){
    xmax <- S.msy50 * 2.25
  }
  else xmax <- limit

  goal_range <- as.numeric(goal_data[dim(goal_data)[1], c(2, 3)])

  cap_width = 85
  cap <-
    case_when(
      n_OYP == 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability
                           (under average productivity) of achieving 90% of maximum
                           sustained yield (MSY) relative to the number of salmon
                           escaped. The probability of achieving 90% of MSY is the standard
                           criteria used to describe an escapement goal range.", width = cap_width),
      n_OYP == 2 ~ str_wrap(paste0("Note: Optimal Yield Profiles (OYP) show the probability
                                  (under average productivity) of achieving ",
                                   pct_alternate,
                                   "% (dashed line) and 90% (solid line) of maximum sustained yield (MSY)
                                  relative to the number of salmon escaped. The probability of achieving
                                  90% of MSY is the standard criteria used to describe an escapement
                                   goal range."),
                            width = cap_width)
    )

  ref_lines0 <-
    goal_data[dim(goal_data)[1], c(2, 3)] %>%
    pivot_longer(cols = lb:ub, values_to = "xend") %>%
    rowwise() %>%
    mutate(y90 = profile_data[["OYP90"]][which.min(abs(profile_data$s - xend))],
           x = -Inf)

  if(n_OYP == 1){}
  else{
    varname <- paste0("y", pct_alternate)
    ref_lines0 <-
      ref_lines0 %>%
      mutate(!!varname := profile_data[[name_alternate]][which.min(abs(profile_data$s - xend))])
  }

  ref_lines <-
    ref_lines0 %>%
    pivot_longer(dplyr::starts_with("y"),
                 values_to = "y",
                 names_to = "max_pct",
                 names_prefix = "y")

  profile_data %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    tidyr::gather("key", "prob", -s, -S.msy, -SY, factor_key = TRUE) %>%
    dplyr::mutate(max_pct = gsub("[A-Z]+([0-9]+)", "\\1", key)) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::geom_segment(aes(x = x, xend = xend, y = y), data = ref_lines, linewidth = 0.25) +
    ggplot2::geom_rect(ggplot2::aes(xmin = lb, xmax = ub, ymin = -Inf, ymax = Inf),
                       data = goal_data[dim(goal_data)[1], ],
                       inherit.aes = FALSE, fill = "gray", alpha = 0.2) +
    ggplot2::annotate("segment",
                      x = -Inf, xend = goal_range[[1]],
                      y = profile_data[["OYP90"]][which.min(abs(profile_data$s - goal_range[[1]]))],
                      linewidth = 0.25) +
    ggplot2::annotate("segment",
                      x = -Inf, xend = goal_range[[2]],
                      y = profile_data[["OYP90"]][which.min(abs(profile_data$s - goal_range[[2]]))],
                      linewidth = 0.25) +
    ggplot2::scale_x_continuous(limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_manual(values = if(n_OYP == 2){c("dashed", "solid")}else("solid"))+
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::ggtitle(title) +
    labs(
      title = title,
      x = "Escapement",
      y = "Probability",
      caption = cap) +
    theme(text = element_text(family = "sans"),
          plot.caption = element_text(
            hjust = 0,
            size = 10),
          plot.caption.position = "plot",
          legend.position = "none")
}
