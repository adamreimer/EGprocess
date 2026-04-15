#' @title Comparative OYP Plot
#' @description
#'  Produces faceted OYP plots representing the prior and updated OYPs for the stock, both overlain with escapement goal ranges. If a new escapement goal finding was made as a result of the updated analysis the new escapement goal will be overlay the updated OYP while the prior escapement goal range will overlay the prior OYP.
#'
#' @param profile_data A list of 2 OYPs that you wish to plot. The name of each list object will be used as the facet title. Recommend "Brood Year: xxxx - YYYY" for each item. The function assumes the first item in the list is the old OYP and the second item in the list is the updated OYP.
#' @param goal_data A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. If the updated analysis resulted in a new escapement goal finding the new finding should be included as the last row with the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#' @param limit Upper bound of spawners for plot. Default (NULL) will use 2.25 times S.msy.
#'
#' @seealso [plot_profile()]
#'
#' @return A figure
#'
#' @import tidyverse
#'
#' @examples
#'
#' post_list <-
#'   list(
#'     'Brood Years: 1963-2005' = post_Igushik_byr63_05,
#'     'Brood Years: 1963-2017' = post_Igushik_byr63-15
#'   )
#' profile_list <- lapply(post_list, get_profile, multiplier = 1e-5)
#'
#' plot_profile_facet(profile_data = profile_list, goal_data = goal_Igushik,
#' title = "Igushik River Sockeye Salmon")
#'
#' @export

plot_profile_facet <- function(profile_data,
                          goal_data,
                          title,
                          limit = NULL){
  S.msy50 <- max(sapply(profile_data, function(x) median(x[["S.msy"]])))
  n_OYP <- sapply(profile_data, function(x) sum(grepl("OYP\\d+", names(x))))
  name_OYP <- names(profile_data[[2]])[!(names(profile_data[[2]]) %in% c("s", "OYP90", "SY", "S.msy"))]

  if(is.null(limit)){
    xmax <- S.msy50 * 2.25
  }
  else xmax <- limit

  goal_data <-
    if(goal_data$yr[dim(goal_data)[1]] == "new")
    {goal_data[(dim(goal_data)[1] - 1):dim(goal_data)[1], ]}else{
      goal_data[rep(dim(goal_data)[1], 2), ]}
  goal_data$profile <- names(profile_data)

  cap_width = 85
  cap <-
    case_when(
      max(n_OYP) == 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving 90% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width),
      max(n_OYP) == 2 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving X% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width)
    )

  ref_lines0 <-
    data.frame(profile = names(profile_data), lb = goal_data$lb, ub = goal_data$ub) %>%
    pivot_longer(cols = lb:ub, values_to = "xend") %>%
    rowwise() %>%
    mutate(y90 = profile_data[[profile]][["OYP90"]][which.min(abs(profile_data[[profile]]$s - xend))],
           x = -Inf)

  if(length(name_OYP) == 0){}
  else(ref_lines0 <-
         ref_lines0 <-
         ref_lines0 %>%
         mutate(y00 = profile_data[[profile]][[name_OYP]][which.min(abs(profile_data[[profile]]$s - xend))])
  )

  ref_lines <-
    ref_lines0 %>%
    pivot_longer(dplyr::starts_with("y"),
                 values_to = "y",
                 names_to = "max_pct",
                 names_prefix = "y") %>%
    mutate(max_pct = ifelse(max_pct == "00", gsub("OYP(\\d+)", "\\1", name_OYP), max_pct))

  lapply(1:length(profile_data), function(x) mutate(profile_data[[x]], profile = names(profile_data)[x])) %>%
    do.call(rbind, .) %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    tidyr::gather("key", "prob", -s, -S.msy, -SY, - profile, factor_key = TRUE) %>%
    dplyr::mutate(max_pct = gsub("[A-Z]+([0-9]+)", "\\1", key)) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::geom_segment(aes(x = x, xend = xend, y = y), data = ref_lines, linewidth = 0.25) +
    ggplot2::geom_rect(ggplot2::aes(xmin = lb, xmax = ub, ymin = -Inf, ymax = Inf),
                       data = goal_data,
                       inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
    ggplot2::facet_grid(. ~ profile) +
    ggplot2::scale_x_continuous(limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_manual(name = "Percent of MSY",
                                   values = if(max(n_OYP) == 2){c("dashed", "solid")}else("solid"))+
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
          plot.caption.position = "plot")
}
