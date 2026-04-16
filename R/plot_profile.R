#' @title OYP Plot
#' @description
#'  Produces a plot of the OYP with an overlay of the goal range.
#'
#' @param profile_dat Output of the get_profile function
#' @param goal_data  A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. If the updated analysis resulted in a new escapement goal finding the new finding should be included as the last row with the year labeled as "new". Use ub = NA for lower bound SEGs.
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
#'     'Brood Years: 1963-2017' = post_Igushik_byr63_15
#'   )
#' profile_list <- lapply(post_list, get_profile, multiplier = 1e-5)
#'
#' plot_profile(profile_list[[2]], goal_Igushik, "Igushik River Sockeye Salmon")
#'
#' @export

plot_profile <- function(profile_dat,
                         goal_data,
                         title,
                         limit = NULL){
  S.msy50 <- median(profile_dat$S.msy)
  n_OYP <- sum(grepl("OYP\\d+", names(profile_dat)))

  if(is.null(limit)){
    xmax <- S.msy50 * 2.25
  }
  else xmax <- limit

  goal_range <- as.numeric(goal_data[dim(goal_data)[1], c(2, 3)])

  cap_width = 85
  OYP2 <- sum(!(names(profile_dat) %in% c("s", "OYP90", "SY", "S.msy")))
  cap <-
    case_when(
      OYP2 == 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving 90% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width),
      OYP2 != 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving X% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width)
    )

  plot <- profile_dat %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    tidyr::gather("key", "prob", -s, -S.msy, -SY, factor_key = TRUE) %>%
    dplyr::mutate(max_pct = gsub("[A-Z]+([0-9]+)", "\\1", key)) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::geom_rect(ggplot2::aes(xmin = lb, xmax = ub, ymin = -Inf, ymax = Inf),
                       data = goal_data[dim(goal_data)[1], ],
                       inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
    ggplot2::annotate("segment",
                      x = -Inf, xend = goal_range[[1]],
                      y = profile_dat[["OYP90"]][which.min(abs(profile_dat$s - goal_range[[1]]))],
                      linewidth = 0.25) +
    ggplot2::annotate("segment",
                      x = -Inf, xend = goal_range[[2]],
                      y = profile_dat[["OYP90"]][which.min(abs(profile_dat$s - goal_range[[2]]))],
                      linewidth = 0.25) +
    ggplot2::scale_x_continuous(limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_manual(name = "Percent of MSY",
                                   values = if(n_OYP == 2){c("dashed", "solid")}else("solid"))+
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

  if(OYP2 == 1){
    col <- names(profile_dat)[!(names(profile_dat) %in% c( "s", "OYP90", "SY", "S.msy"))]
    plot +
      ggplot2::annotate("segment",
                        x = -Inf, xend = goal_range[[1]],
                        y = profile_dat[[col]][which.min(abs(profile_dat$s - goal_range[[1]]))],
                        linewidth = 0.25,
                        linetype = 2) +
      ggplot2::annotate("segment",
                        x = -Inf, xend = goal_range[[2]],
                        y = profile_dat[[col]][which.min(abs(profile_dat$s - goal_range[[2]]))],
                        linewidth = 0.25,
                        linetype = 2)
  }
  else plot
}
